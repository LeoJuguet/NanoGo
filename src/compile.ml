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

(** f reçoit le label correspondant à ``renvoyer vrai'' *)
let compile_bool f =
  let l_true = new_label () and l_end = new_label () in
  f l_true ++
  movq (imm 0) (reg rdi) ++ jmp l_end ++
  label l_true ++ movq (imm 1) (reg rdi) ++ label l_end

(** push le type dans rbegin*)
let rec push_typ rbegin ty =
  let size = sizeof ty in
  if size = 8 then pushq !%rbegin
  else
    let i = ref nop in
    for j = (size / 8) - 1 downto 0 do
      i := !i ++ pushq (ind ~ofs:(j*8) rbegin);
    done;
    !i

(** move la valeur de rbegin dans rend *)
let rec mov_typ rbegin rend ty =
  let size = sizeof ty in
  if size = 8 then
    movq !%rbegin (ind rend)
  else
    let i = ref nop in
    for j = (size / 8) - 1 downto 0 do
      i := !i
           ++ movq (ind ~ofs:(j*8) rbegin) !%r11
           ++ movq !%r11 (ind ~ofs:(j*8) rend);
    done;
    !i

(** move la valeur a l'addresse rbegin dans rend *)
let rec mov_addr_typ rbegin rend ty =
  let size = sizeof ty in
  if size = 8 then
    movq (ind rbegin) !%r11
    ++ movq !%r11  (ind rend)
  else
    let i = ref nop in
    for j = (size / 8) - 1 downto 0 do
      i := !i
           ++ movq (ind ~ofs:(j*8) rbegin) !%r11
           ++ movq !%r11 (ind ~ofs:(j*8) rend);
    done;
    !i

(** move la valeur dans rdi *)
let mov_rdi_typ rbegin ty =
  let size = sizeof ty in
  if size = 8 then
    movq (ind rbegin) !%rdi
  else push_typ rbegin ty ++ movq !%rsp !%rdi

(** genere un print pour le type ty
    Suppose que la valeur est dans la stack
                             *)
let rec print_type ty = match ty with
  | Tbool -> movq (ind ~ofs:0 rsp) !%rdi ++ call "print_bool"
  | Tint -> movq (ind ~ofs:0 rsp) !%rdi ++ call "print_int"
  | Tstring -> movq (ind ~ofs:0 rsp) !%rdi ++ call "print_string"
  | Tptr (Tstruct s)  -> leaq (lab "Samp") rdi
                        ++ call "print_string"
                        ++ movq (ind rsp) !%rdi
                        ++ push_typ rdi (Tstruct s)
                        ++ print_type (Tstruct s)
                        ++ addq (imm (sizeof (Tstruct s))) !%rsp
  | Tptr Twild -> leaq (lab "S_nil") rdi ++ call "print_string"
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
              pushq (ind ~ofs:(t.f_ofs) rsp)
              ++ print_type t.f_typ
              ++ addq (imm 8) !%rsp
            | Tptr _ -> movq (ind ~ofs:0 rsp) !%rdi
                        ++ call "print_hexa"
            | fty -> leaq (ind ~ofs:t.f_ofs rsp) rdi
                    ++ mov_rdi_typ rdi fty
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
      i1
      ++ pushq !%rdi
      ++ i2
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
      let iop,ijmp,sop = match op with
        | Beq -> sete  , jne,setne
        | Bne -> setne , je,sete
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
      | Tstring -> call "compare_string"
                   ++ cmpq (imm 0) !%rax
                   ++ sop !%dil
                   ++ movzbq !%dil rdi
      | _ -> cmpq !%rdi !%rsi ++ iop !%dil ++ movzbq !%dil rdi
    end
  | TEunop (Uneg, e1) ->
    begin
      expr env e1
      ++  negq !%rdi
    end
  | TEunop (Unot, e1) ->
    begin
      expr env e1
      ++ notq !%rdi
    end
  | TEunop (Uamp, e1) ->
    (* TODO code pour & *)
    begin
      left_expr env e1
      ++ leaq (ind rsi) rdi
    end
  | TEunop (Ustar, e1) ->
    (* TODO code pour * *)
    left_expr env e ++ mov_rdi_typ rsi e.expr_typ
  | TEprint el ->
    begin
      let expr_ty = ref Tstring in
      (match el with
      | [{expr_desc = TEcall _ } as e1] -> expr env e1
      | _ ->List.fold_right (fun e1 i ->
          i
          ++ expr env e1
          ++ match e1.expr_typ with
          | Tstring | Tint | Tbool | Twild | Tptr _ -> push_typ rdi e1.expr_typ
          | Tstruct _ | Tmany _ -> nop)
        el nop
      )
      ++ List.fold_left (fun i etyp ->
          i
          ++
          (if !expr_ty <> Tstring
          && etyp <> Tstring then
           (expr_ty := etyp; call "print_space")
          else (expr_ty := etyp ; nop)
         )
          ++ print_type etyp
          ++ addq (imm (sizeof etyp)) !%rsp; )
       nop (match el with
            | [{expr_typ = Tmany l}] -> l
            | _ -> List.map (fun e -> e.expr_typ ) el)
        (* clean local variable *)
    end

  | TEident x ->
    begin
      match x.v_typ with
      | Tstring | Tbool | Tint | Tptr _ -> movq (ind ~ofs:x.v_addr rbp) !%rdi
      | _ -> leaq (ind ~ofs:x.v_addr rbp) rsi
             ++ mov_rdi_typ rsi x.v_typ
             ++ movq !%rsp !%rdi
    end
  (*| TEassign ([{expr_desc=TEident x}], [e1]) ->
    (* TODO code pour x := e *) assert false *)
  | TEassign ([lv], [e1]) ->
    (* TODO code pour x1,... := e1,... *)
    begin
      let addr = left_expr env lv in
      expr env e1
      ++ addr
      ++ mov_typ rdi rsi e1.expr_typ
    end
  | TEassign (lv, el) ->
        begin
          let i1, s = List.fold_left (fun (i,s) e1 ->
                i
                ++ expr env e1
                ++ (match e1.expr_typ with
                    | Tint | Tbool | Tstring | Tptr _ -> pushq !%rdi
                    |  _ -> nop
                  ), s + sizeof e1.expr_typ
            ) (nop,0) el
          in
          i1
          ++ (match el with
              (* functions *)
              | [{expr_typ = Tmany lety}] ->
                leaq (ind rsp) rdi
                ++ List.fold_right2 (fun v ety i ->
                    (match v.expr_desc with
                    | TEident {v_name = "_"} -> nop
                    | _ -> left_expr env v
                           ++ mov_addr_typ rdi rsi ety)
                           ++ addq (imm (sizeof ety)) !%rdi
                           ++ i
                  ) lv lety nop
              (* expressions *)
              | _ ->
                leaq (ind ~ofs:(s) rsp) rdi
                ++List.fold_right2 (fun v e i ->
                    match v.expr_desc with
                     | TEident {v_name = "_"} ->
                       subq (imm (sizeof e.expr_typ)) !%rdi
                       ++ i
                     | _ ->
                       left_expr env v
                       ++ subq (imm (sizeof e.expr_typ)) !%rdi
                       ++ mov_addr_typ rdi rsi e.expr_typ
                       ++ i
                ) lv el nop
            )
            (*delete right expression*)
          ++ addq (imm s) !%rsp
        end
  | TEblock el ->
     (* TODO code pour block *)
    begin
      let actual_size_local_variable = !(env.size_local_variable) in
      let rec fold_block i e1 =
        i ++
        match e1.expr_desc with
        | TEvars (vl, el)-> begin
            match el with
            | [] -> begin
                let rec get_type l = match l with
                  | [] -> 8
                  | {v_name = "_"}::q -> get_type q
                  | t::q -> sizeof t.v_typ
                in
                let sizety = get_type vl in
                let s =  (List.fold_right (fun v s ->
                    env.size_local_variable := !(env.size_local_variable) + sizety;
                    v.v_addr <- - !(env.size_local_variable);
                    s + sizety) vl 0) in
                let rec clear s =
                  if s = 0 then movq (imm 0) (ind ~ofs:0 rsp)
                  else movq (imm 0) (ind ~ofs:s rsp) ++ clear (s-8) in
                subq (imm s)!%rsp
                ++ clear (s-8)
              end
            | [{expr_desc = TEcall (f,_)} as er] -> begin
                let _ = List.fold_right2 (fun v fty s ->
                    env.size_local_variable := !(env.size_local_variable) + sizeof fty;
                    v.v_addr <- - !(env.size_local_variable);
                    s - sizeof fty) vl f.fn_typ 0 in
                expr env er
              end
            | _ -> begin
                let _ = List.fold_left2 (fun s v e->
                    env.size_local_variable := !(env.size_local_variable) + sizeof e.expr_typ;
                    v.v_addr <- - !(env.size_local_variable);
                    s - sizeof e.expr_typ) 0 vl el in
                List.fold_left (fun i e -> i ++ expr env e ++ push_typ rdi e.expr_typ) nop el
                end
          end
        | _ -> expr env e1
      in
      let i1 = List.fold_left fold_block nop el in
      let var_loc = !(env.size_local_variable) - actual_size_local_variable in
      i1
        (* clean local variable *)
      ++ if var_loc <> 0 then addq (imm var_loc) !%rsp
          else nop
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
      ++ match e1.expr_desc with
      | TEcall _ -> movq !%rsp !%rdi
      | _ -> nop
      ++ testq !%rdi !%rdi
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
      ++ match e1.expr_desc with
      | TEcall _ -> movq !%rsp !%rdi
      | _ -> nop
      ++ testq !%rdi !%rdi
      ++ jnz lab_loop
    end
  | TEnew ty ->
     (* TODO code pour new S *)
      allocz (sizeof ty)
      ++ movq !%rax !%rdi
  | TEcall (f, el) ->
     (* TODO code pour appel fonction *)
    begin
      let return_size = List.fold_left (fun s ty -> s + sizeof ty) 0 f.fn_typ in
      let arg_size = List.fold_left (fun s v -> s + sizeof v.v_typ) 0 f.fn_params in
      (* resultats *)
      (if return_size <> 0 then subq (imm return_size) !%rsp
      else nop)
      (* arguments *)
      ++ ( match el with
          | [{expr_desc = TEcall _} as e1] -> expr env e1
          | _ -> List.fold_right (fun e1 i -> i
                                              ++ expr env e1
                                              ++ match e1.expr_typ with
                                              | Tstruct _ -> nop
                                              | _ -> push_typ rdi e1.expr_typ
                                 ) el nop
         )
      ++ call ("F_"^f.fn_name)
      (* efface les arguments *)
      ++ (if arg_size <> 0 then addq (imm arg_size) !%rsp
          else nop)
      ++ (if return_size = 8 then movq (ind rsp) !%rdi
          else nop)
    end

  | TEdot (e1, {f_ofs=ofs}) ->
     (* TODO code pour e.f *)
    begin
      left_expr env e
      ++ movq (ind rsi) !%rdi
    end
  | TEvars _ ->
     assert false (* fait dans block *)
  | TEreturn [] ->
    (* TODO code pour return e *)
    jmp env.exit_label
  (*| TEreturn [e1] ->
    (* TODO code pour return e1,... *)
    jmp env.exit_label*)
  | TEreturn el ->
     begin
       fst (List.fold_left (fun (i, pos) e ->
         (
           i
           ++ expr env e
           ++ leaq (ind ~ofs:pos rbp) rsi
           ++ mov_typ rdi rsi e.expr_typ
         , pos + sizeof e.expr_typ)
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
      a1
      ++ iop (ind rsi)
    end
(** genere le code pour récupérer l'addresse de la l-value e *)
and left_expr env e = match e.expr_desc with
  | TEdot (e1,f1) ->
    begin
      (match e1.expr_typ with
      | Tptr _ -> left_expr env e1
                  ++ movq (ind rsi) !%rsi
      | _ ->left_expr env e1)
      ++ leaq (ind ~ofs:f1.f_ofs rsi) rsi
    end
  | TEunop (Ustar, e1) -> left_expr env e1
                          ++ movq (ind rsi) !%rsi
  | TEident v -> leaq (ind ~ofs:v.v_addr rbp) rsi
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
    pos_return = List.fold_left (fun s v -> s + sizeof v.v_typ ) 16 f.fn_params;
  }
  in
  label ("F_" ^ s)
  ++ pushq !%rbp
  ++ movq !%rsp !%rbp
  ++ expr env e
  ++ label env.exit_label
    (* delete local variable *)
  ++ leave
  ++ ret

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

(** fonction pour afficher un int*)
let xprint_int =
  label "print_int"
  ++ movq !%rdi !%rsi
  ++ leaq (lab "S_int") rdi
  ++ xorq !%rax !%rax
  ++ call "printf"
  ++ ret

(** fonction pour afficher en hexadecimal (pour les pointeurs)*)
let xprint_hexa =
  label "print_hexa"
  ++ movq !%rdi !%rsi
  ++ leaq (lab "S_hexa") rdi
  ++ xorq !%rax !%rax
  ++ call "printf"
  ++ ret

(** fonction pour afficher un booleen*)
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

(** fonction pour afficher un string*)
let xprint_string =
  label "print_string"
  ++ movq !%rdi !%rsi
  ++ leaq (lab "S_string") rdi
  ++ xorq !%rax !%rax
  ++ call "printf"
  ++ ret

(** fonction pour afficher un espace*)
let xprint_space =
  label "print_space"
  ++ leaq (lab "Sspace") rdi
  ++ xorq !%rax !%rax
  ++ call "printf"
  ++ ret

(** fonction pour allocz*)
let xallocz =
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

 (** fonction pour comparer l'égalite de deux string*)
let xcompare_string =
  let false_lab = new_label () in
  let true_lab = new_label () in
  let loop_lab = new_label () in
  label "compare_string"
  ++ cmpq !%rdi !%rsi     (* test si rdi et rsi pointe la meme string *)
  ++ jz true_lab
  ++ label loop_lab
  ++ movb (ind rdi) !%r10b
  ++ movb (ind rsi) !%r11b
  ++ cmpb !%r10b !%r11b     (* test si les characteres sont les memes *)
  ++ jnz false_lab
  ++ cmpb (imm 0) !%r10b   (* test si on est a la fin de la string *)
  ++ jz true_lab
  ++ addq (imm 1) !%rdi   (*next char*)
  ++ addq (imm 1) !%rsi
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
      xprint_int ++
      xprint_string ++
      xprint_space ++
      xprint_bool ++
      xprint_hexa ++
      xallocz ++
      xcompare_string
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
