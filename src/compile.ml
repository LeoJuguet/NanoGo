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
}

let empty_env =
  { exit_label = ""; ofs_this = -1; nb_locals = ref 0; next_local = 0 }

let mk_bool d = { expr_desc = d; expr_typ = Tbool }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
let compile_bool f =
  let l_true = new_label () and l_end = new_label () in
  f l_true ++
  movq (imm 0) (reg rdi) ++ jmp l_end ++
  label l_true ++ movq (imm 1) (reg rdi) ++ label l_end

let rec print_type ty = match ty with
  | Tbool -> call "print_bool"
  | Tint -> call "print_int"
  | Tstring -> call "print_string"
  | Tptr ptr_ty ->
    movq (lab "Samp") !%rdi
    ++ print_type Tstring
    ++ print_type ptr_ty
  | Tstruct s ->
    begin
      let rec print_field_struct fieldl = match fieldl with
        | [] -> nop
        | t::[] -> movq (ind rbp) !%rdi
                   ++ print_type t.f_typ
                   ++ movq (lab "Srb") !%rdi
                   ++ print_type Tstring
        | t::q -> print_type t.f_typ
                  ++ movq (lab "Ssapace") !%rdi
                  ++ print_type Tstring
                  ++ print_field_struct q
      in
      movq (lab "Slb") !%rdi
      ++ print_type Tstring
      ++ print_field_struct (Hashtbl.fold (fun k v l -> []) s.s_fields [])
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
      movq (lab (alloc_string s)) !%rdi
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
    (* TODO code pour egalite toute valeur *) assert false 
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
    (* TODO code pour & *) assert false 
  | TEunop (Ustar, e1) ->
    (* TODO code pour * *) assert false 
  | TEprint el ->
    (* TODO code pour Print *)
    List.fold_left (fun i e -> i ++ expr env e) nop el
  | TEident x ->
    (* TODO code pour x *) assert false 
  | TEassign ([{expr_desc=TEident x}], [e1]) ->
    (* TODO code pour x := e *) assert false 
  | TEassign ([lv], [e1]) ->
    (* TODO code pour x1,... := e1,... *) assert false 
  | TEassign (_, _) ->
     assert false
  | TEblock el ->
     (* TODO code pour block *)
    begin
      let rec fold_block i e1 = match e1.expr_desc with
        | TEvars _-> nop
        | _ -> i ++ expr env e1
      in
      List.fold_left fold_block nop el
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
    malloc (sizeof ty)
    ++ movq !%rax !%rdi
  | TEcall (f, el) ->
     (* TODO code pour appel fonction *)
    begin
      List.fold_left (fun i e1 -> i ++ pushq !%rdi ++ expr env e1 ) nop el
      ++ call f.fn_name
    end

  | TEdot (e1, {f_ofs=ofs}) ->
     (* TODO code pour e.f *) assert false
  | TEvars _ ->
     assert false (* fait dans block *)
  | TEreturn [] ->
    (* TODO code pour return e *)
    jmp env.exit_label
  | TEreturn [e1] ->
    (* TODO code pour return e1,... *)
    jmp env.exit_label
  | TEreturn _ ->
     assert false
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
  | TEdot (e1,f1) -> left_expr env e1 + f1.f_ofs
  | TEunop (Ustar, e1) -> left_expr env e1
  | TEident v -> v.v_addr
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
  }
  in
  label ("F_" ^ s)
  ++ pushq !%rbp
  ++ movq !%rsp !%rbp
  ++ expr env e
  ++ label env.exit_label
  ++ popq rbp
  ++ ret

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code


let xprint_string =
  label "print_string"
  ++ xorq !%rax !%rax
  ++ call "printf"
  ++ ret

let xprint_bool =
  label "print_bool"
  ++ xorq !%rax !%rax
  ++ call "printf"
  ++ ret

let print ty = 0

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
      inline "
print_int:
        movq    %rdi, %rsi
        movq    $S_int, %rdi
        xorq    %rax, %rax
        call    printf
        ret
"; (* TODO print pour d'autres valeurs *)
   (* TODO appel malloc de stdlib *)
    data =
      label "S_int" ++ string "%ld" ++
      label "S_string" ++ string "%s" ++
      (* Constantes pour l'appel de print *)
      label "Sspace" ++ string " " ++
      label "Samp" ++ string "&" ++
      label "Slb" ++ string "{" ++
      label "Srb" ++ string "}" ++
      label "S_nil" ++ string "<nil>" ++
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }
