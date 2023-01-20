(* étiquettes
     F_function      entrée fonction
     E_function      sortie fonction
     L_xxx           sauts
     S_xxx           chaîne

   expression calculée avec la pile si besoin, résultat final dans %rdi

   fonction : arguments sur la pile, résultat dans %rax ou sur la pile

            res k
            ...
            res 1
            arg n
            ...
            arg 1
            adr. retour
   rbp ---> ancien rbp
            ...
            var locales
            ...
            calculs
   rsp ---> ...

*)

open Format
open Ast
open Tast
open X86_64

exception Anomaly of string

let debug = ref false

let strings = Hashtbl.create 32
let alloc_string =
  let r = ref 0 in
  fun s ->
    incr r;
    let l = "S_" ^ string_of_int !r in
    Hashtbl.add strings l s;
    l

let malloc n = movq (imm n) (reg rdi) ++ call "malloc"
let allocz n = movq (imm n) (reg rdi) ++ call "allocz"

let sizeof = Typing.sizeof

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r

type env = {
  exit_label: string;
  ofs_this: int;
  nb_locals: int ref; (* maximum *)
  next_local: int; (* 0, 1, ... *)
  size_local_variable: int ref;
  pos_return: int
}

let empty_env =
  { exit_label = ""; ofs_this = -1; nb_locals = ref 0; next_local = 0; size_local_variable = ref 0; pos_return = 0 }

let mk_bool d = { expr_desc = d; expr_typ = Tbool }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
let compile_bool f =
  let l_true = new_label () and l_end = new_label () in
  f l_true ++
  movq (imm 0) (reg rdi) ++ jmp l_end ++
  label l_true ++ movq (imm 1) (reg rdi) ++ label l_end

let rec push_typ rbegin ty =
  let size = sizeof ty in
  if size = 8 then pushq !%rbegin
  else
    let i = ref nop in
    for j = 0 to (size-1) / 8 do
      i := !i ++ pushq (ind ~ofs:(-j*8) rbegin);
    done;
    !i

let rec mov_typ rbegin rend ty =
  let size = sizeof ty in
  if size = 8 then movq (ind rbegin) (ind rend)
  else
    let i = ref nop in
    for j = 0 to size / 8 do
      i := !i ++ movq (ind ~ofs:(-j) rbegin) (ind ~ofs:(-j) rend);
    done;
    !i

let rec print_type ty = match ty with
  | Tbool -> movq (ind ~ofs:0 rsp) !%rdi ++ call "print_bool"
  | Tint -> movq (ind ~ofs:0 rsp) !%rdi ++ call "print_int"
  | Tstring -> movq (ind ~ofs:0 rsp) !%rdi ++ call "print_string"
  | Tptr (Tstruct s)  -> leaq (lab "Samp") rdi
                        ++ call "print_string"
                        ++ leaq (ind rsp) rdi
                        ++ push_typ rdi (Tstruct s)
                        ++ print_type (Tstruct s)
                        ++ addq (imm (sizeof (Tstruct s))) !%rsp
  | Tptr ptr_ty ->
    movq (ind ~ofs:0 rsp) !%rdi
    ++ call "print_hexa"
  | Tstruct s ->
    begin
      let rec print_field_struct fieldl = match fieldl with
        | [] -> nop
        | t::q ->
          (
            match t.f_typ with
            | Tint | Tstring | Tbool ->
              pushq (ind ~ofs:(sizeof (Tstruct s) - t.f_ofs - 8) rsp)
              ++ print_type t.f_typ
              ++ addq (imm 8) !%rsp
            | Tptr _ -> movq (ind ~ofs:0 rsp) !%rdi
                        ++ call "print_hexa"
            | fty ->  push_typ rdi fty
                    ++ print_type fty
                    ++ addq (imm (sizeof fty)) !%rsp
            )
          ++
          if q = [] then
            leaq (lab "Srb") rdi
            ++ call "print_string"
          else
            call "print_space"
            ++ print_field_struct q
      in
      let sorted_argument = List.sort (fun x y -> compare x.f_ofs y.f_ofs)
          (Hashtbl.fold (fun k v l -> v::l) s.s_fields []) in
       leaq (lab "Slb") rdi
      ++ call "print_string"
      ++ print_field_struct sorted_argument
    end
  | Twild -> failwith "Not implemented"
  | Tmany l -> failwith "Not implemented"

let rec expr env e = match e.expr_desc with
  | TEskip ->
    nop
  | TEconstant (Cbool true) ->
    movq (imm 1) (reg rdi)
  | TEconstant (Cbool false) ->
    movq (imm 0) (reg rdi)
  | TEconstant (Cint x) ->
    movq (imm64 x) (reg rdi)
  | TEnil ->
    xorq (reg rdi) (reg rdi)
  | TEconstant (Cstring s) ->
    (* TODO code pour constante string *)
    begin
      leaq (lab (alloc_string s)) rdi
    end
  | TEbinop (Band, e1, e2) ->
    (* TODO code pour ET logique lazy *)
    begin
      let i1 = expr env e1 in
      let labf = new_label () in
      let i2 = expr env e2 in
      i1
      ++ jz labf
      ++ i2
      ++ label labf
    end
  | TEbinop (Bor, e1, e2) ->
    (* TODO code pour OU logique lazy *)
    begin
      let i1 = expr env e1 in
      let labt = new_label () in
      let i2 = expr env e2 in
      i1
      ++ jnz labt
      ++ i2
      ++ label labt
    end
  | TEbinop (Blt | Ble | Bgt | Bge as op, e1, e2) ->
    (* TODO code pour comparaison ints *)
    begin
      let i1 = expr env e1 in
      let i2 = expr env e2 in
      let iop = match op with
        | Blt -> setl
        | Ble -> setle
        | Bgt -> setg
        | Bge -> setge
        | _ -> failwith "impossible"
      in
      i2
      ++ pushq !%rdi
      ++ i1
      ++ popq rsi
      ++ cmpq !%rdi !%rsi
      ++ iop !%dil
      ++ movzbq !%dil rdi
    end
  | TEbinop (Badd | Bsub | Bmul | Bdiv | Bmod as op, e1, e2) ->
    (* TODO code pour arithmetique ints *)
    begin
      let i1 = expr env e1 in
      let i2 = expr env e2 in
      let iop r1 r2= match op with
        | Badd -> addq !%r2 !%r1
        | Bsub -> subq !%r2 !%r1
        | Bmul -> imulq !%r2 !%r1
        | Bdiv -> movq !%r1 !%rax ++ cqto ++ idivq !%r2 ++ movq !%rax !%rdi
        | Bmod -> movq !%r1 !%rax ++ cqto ++ idivq !%r2 ++ movq !%rdx !%rdi
        | _ -> failwith "impossible"
      in
      i2
      ++ pushq !%rdi
      ++ i1
      ++ popq rsi
      ++ iop rdi rsi
    end
  | TEbinop (Beq | Bne as op, e1, e2) ->
    (* TODO code pour egalite toute valeur *)
    begin
      let i1 = expr env e1 in
      let i2 = expr env e2 in
      let iop,ijmp = match op with
        | Beq -> sete  , jne
        | Bne -> setne , je
        | _ -> failwith "impossible"
      in
      let i = ref nop in
      let ty = if e1.expr_typ == Twild then e2.expr_typ
        else e1.expr_typ in
            i2
      ++ pushq !%rdi
      ++ i1
      ++ popq rsi
      ++ match ty with
      | Tstruct s -> begin
                      let end_lab = new_label () in
                      let false_lab = new_label () in
                      let n = sizeof ty in
                      for j = 0 to n/8 do
                        i := !i
                             ++ cmpq !%rdi !%rsi
                             ++ ijmp false_lab
                             ++ subq (imm 8) !%rdi
                             ++ subq (imm 8) !%rsi
                      done;
                      !i
                      ++ movq (imm 1) !%rdi
                      ++ jmp end_lab
                      ++ label false_lab
                      ++ movq (imm 0) !%rdi
                      ++ label end_lab
                     end
      | Tstring -> call "compare_string" ++ movq !%rax !%rdi
      | _ -> cmpq !%rdi !%rsi ++ iop !%dil ++ movzbq !%dil rdi
    end
  | TEunop (Uneg, e1) ->
    (* TODO code pour negation ints *)
    begin
      expr env e1
      ++  negq !%rdi
    end
  | TEunop (Unot, e1) ->
    (* TODO code pour negation bool *)
    begin
      expr env e1
      ++ notq !%rdi
    end
  | TEunop (Uamp, e1) ->
    (* TODO code pour & *)
    begin
      let addr = left_expr env e1 in
      leaq (ind ~ofs:addr rbp) rdi
    end
  | TEunop (Ustar, e1) ->
    (* TODO code pour * *) assert false
  | TEprint el ->
    (* TODO code pour Print *)
    (*List.fold_right (fun e i -> i ++ expr env e ++ print_type e.expr_typ) el nop*)
    begin
      let expr_ty = ref Tstring in
      List.fold_right (fun e1 i ->
          i
          ++ expr env e1
          ++ push_typ rdi e1.expr_typ )
        el nop
      ++ List.fold_left (fun i e ->
          i
          ++
          (if !expr_ty <> Tstring
          && e.expr_typ <> Tstring then
           (expr_ty := e.expr_typ; call "print_space")
          else (expr_ty := e.expr_typ ; nop)
         )
          ++ print_type e.expr_typ
          ++ addq (imm (sizeof e.expr_typ)) !%rsp; )
       nop el
        (* clean local variable *)
    end

  | TEident x ->
    (* TODO code pour x *)
    begin
      match x.v_typ with
      | Tstruct s -> leaq (ind ~ofs:(left_expr env e) rbp) rdi
      | _ -> movq (ind ~ofs:(left_expr env e) rbp) !%rdi
    end
  (*| TEassign ([{expr_desc=TEident x}], [e1]) ->
    (* TODO code pour x := e *) assert false *)
  | TEassign ([lv], [e1]) ->
    (* TODO code pour x1,... := e1,... *)
    begin
      let addr = left_expr env lv in
      expr env e1
      ++ movq !%rdi (ind ~ofs:addr rbp)
    end
  | TEassign (_, _) ->
      assert false
  | TEblock el ->
     (* TODO code pour block *)
    begin
      let actual_size_local_variable = !(env.size_local_variable) in
      let rec fold_block i e1 = match e1.expr_desc with
        | TEvars (vl, el)-> begin
            i ++ match el with
            | [] -> begin
                subq (imm (List.fold_right (fun v s ->
                    v.v_addr <- - !(env.size_local_variable) - 8;
                    env.size_local_variable := !(env.size_local_variable) + sizeof v.v_typ;
                    s + sizeof v.v_typ) vl 0)) !%rsp
              end
            | [{expr_desc = TEcall _} as er] -> begin
                let _ = List.fold_right (fun v s ->
                    v.v_addr <- - !(env.size_local_variable) - 8;
                    env.size_local_variable := !(env.size_local_variable) + sizeof v.v_typ;
                    s - sizeof v.v_typ) vl 0 in
                expr env er
              end
            | _ -> begin
                let _ = List.fold_left (fun s v ->
                    v.v_addr <- - !(env.size_local_variable) - 8;
                    env.size_local_variable := !(env.size_local_variable) + sizeof v.v_typ;
                    s - sizeof v.v_typ) 0 vl in
                List.fold_left (fun i e -> i ++ expr env e ++ push_typ rdi e.expr_typ) nop el
                end
          end
        | _ -> i ++ expr env e1
      in
      let i1 = List.fold_left fold_block nop el in
      i1
        (* clean local variable *)
      ++ addq (imm (!(env.size_local_variable) - actual_size_local_variable)) !%rsp
    end
  | TEif (e1, e2, e3) ->
     (* TODO code pour if *)
    begin
      let i1 = expr env e1 in
      let lab_endif = new_label () in
      let lab_false = new_label () in
      let i2 = expr env e2 in
      let i3 = expr env e3 in
      i1
      ++ jz lab_false
      ++ i2
      ++ jmp lab_endif
      ++ label lab_false
      ++ i3
      ++ label lab_endif
    end
  | TEfor (e1, e2) ->
     (* TODO code pour for *)
    begin
      let lab_loop = new_label () in
      let lab_cond = new_label () in
      let i1 = expr env e1 in
      let i2 = expr env e2 in
      jmp lab_cond
      ++ label lab_loop
      ++ i2
      ++ label lab_cond
      ++ i1
      ++ jnz lab_loop
    end
  | TEnew ty ->
     (* TODO code pour new S *)
      allocz (sizeof ty)
      ++ movq !%rax !%rdi
  | TEcall (f, el) ->
     (* TODO code pour appel fonction *)
    begin
      (* resultats *)
      subq (imm (List.fold_left (fun s ty -> s + sizeof ty) 0 f.fn_typ)) !%rsp
      (* arguments *)
      ++ List.fold_right (fun e1 i -> i ++ expr env e1 ++ push_typ rdi e1.expr_typ ) el nop
      ++ call f.fn_name
      (* efface les arguments *)
      ++ addq (imm (List.fold_left (fun s v -> s + sizeof v.v_typ) 0 f.fn_params)) !%rsp
    end

  | TEdot (e1, {f_ofs=ofs}) ->
     (* TODO code pour e.f *)
    begin
      let addr = left_expr env e in
      movq (ind ~ofs:addr rbp) !%rdi
    end
  | TEvars _ ->
     assert false (* fait dans block *)
  | TEreturn [] ->
    (* TODO code pour return e *)
    jmp env.exit_label
  | TEreturn [e1] ->
    (* TODO code pour return e1,... *)
    jmp env.exit_label
  | TEreturn el ->
     begin
       fst (List.fold_left (fun (i, pos) e ->
         (
           i
           ++ expr env e
           ++ leaq (ind ~ofs:(pos+sizeof e.expr_typ) rbp) rsi
           ++ mov_typ rdi rbp e.expr_typ , pos + sizeof e.expr_typ)
         )
           (nop, env.pos_return) el
         )
         ++ jmp env.exit_label
     end
  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- *)
    begin
      let a1 = left_expr env e1 in
      let iop = match op with
        | Inc -> incq
        | Dec -> decq
      in
      iop (ind ~ofs:a1 rbp)
    end

and left_expr env e = match e.expr_desc with
  | TEdot (e1,f1) -> (*leaq (ind ~ofs:-f1.f_ofs !%rsi) !%rsi*)
    left_expr env e1 - f1.f_ofs
  | TEunop (Ustar, e1) -> (*leaq !%rsi !%rsi*) left_expr env e1
  | TEident v ->(*leaq (ind ~ofs:v.v_addr !%rbp) !%rsi*) v.v_addr
  | _ -> failwith "not yet"

let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  (* TODO code pour fonction *)
  let s = f.fn_name in
  let env = {
    exit_label = "E_" ^ s;
    ofs_this= 0;
    nb_locals = ref 0; (* maximum *)
    next_local= 0; (* 0, 1, ... *)
    size_local_variable= ref 0;
    pos_return = List.fold_left (fun s v -> s + sizeof v.v_typ ) 8 f.fn_params;
  }
  in
  label ("F_" ^ s)
  ++ pushq !%rbp
  ++ movq !%rsp !%rbp
  ++ expr env e
  ++ label env.exit_label
    (* delete local variable *)
  ++ movq !%rbp !%rsp
  ++ popq rbp
  ++ ret

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code


let xprint_int =
  label "print_int"
  ++ movq !%rdi !%rsi
  ++ leaq (lab "S_int") rdi
  ++ xorq !%rax !%rax
  ++ call "printf"
  ++ ret

let xprint_hexa =
  label "print_hexa"
  ++ movq !%rdi !%rsi
  ++ leaq (lab "S_hexa") rdi
  ++ xorq !%rax !%rax
  ++ call "printf"
  ++ ret



let xprint_bool =
  label "print_bool"
  ++ testq !%rdi !%rdi
  ++ jnz "print_bool_true"
  ++ leaq (lab "Sfalse") rdi
  ++ jmp "print_bool_printf"
  ++ label "print_bool_true"
  ++ leaq (lab "Strue") rdi
  ++ label "print_bool_printf"
  ++ xorq !%rax !%rax
  ++ call "printf"
  ++ ret

let xprint_string =
  label "print_string"
  ++ movq !%rdi !%rsi
  ++ leaq (lab "S_string") rdi
  ++ xorq !%rax !%rax
  ++ call "printf"
  ++ ret

let xprint_space =
  label "print_space"
  ++ leaq (lab "Sspace") rdi
  ++ xorq !%rax !%rax
  ++ call "printf"
  ++ ret

let xcompare_string =
  let false_lab = new_label () in
  let true_lab = new_label () in
  let loop_lab = new_label () in
  label "compare_string"
  ++ cmpq !%rdi !%rsi     (* test si rdi et rsi pointe la meme string *)
  ++ jz true_lab
  ++ label loop_lab
  ++ movq (ind rdi) !%r12
  ++ movq (ind rsi) !%r11
  ++ cmpq !%r12 !%r11     (* test si les characteres sont les memes *)
  ++ jnz false_lab
  ++ cmpq (imm 0) !%r12   (* test si on est a la fin de la string *)
  ++ jz true_lab
  ++ addq (imm 8) !%rdi   (*next char*)
  ++ addq (imm 8) !%rdi
  ++ jmp loop_lab
  ++ label true_lab
  ++ movq (imm 1) !%rax
  ++ ret
  ++ label false_lab
  ++ movq (imm 0) !%rax
  ++ ret




let file ?debug:(b=false) dl =
  debug := b;
  (* TODO calcul offset champs *)
  (* TODO code fonctions *) let funs = List.fold_left decl nop dl in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq (reg rax) (reg rax) ++
      ret ++
      funs ++
(*      inline "
print_int:
        movq    %rdi, %rsi
        movq    $S_int, %rdi
        xorq    %rax, %rax
        call    printf
        ret
        "
++*)
      xprint_int ++
xprint_string
++
xprint_space
++
xprint_bool
++
xprint_hexa
++
label "allocz"
  ++ movq !%rdi !%rbx
  ++ call "malloc"
  ++ testq !%rbx !%rbx
  ++ jnz "alloc_zero"
  ++ ret
  ++ label "alloc_zero"
  ++ movb (imm 0) (ind ~index:rbx rax)
  ++ decq !%rbx
  ++ jnz "alloc_zero"
  ++ ret
;
    data =
      label "S_int" ++ string "%ld" ++
      label "S_string" ++ string "%s" ++
      (* Constantes pour l'appel de print *)
      label "S_hexa" ++ string "0x%lx" ++
      label "Sspace" ++ string " " ++
      label "Samp" ++ string "&" ++
      label "Slb" ++ string "{" ++
      label "Srb" ++ string "}" ++
      label "Strue" ++ string "true" ++
      label "Sfalse" ++ string "false" ++
      label "S_nil" ++ string "<nil>" ++
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }
