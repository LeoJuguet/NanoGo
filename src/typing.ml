
open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string
exception Anomaly of string

let error loc e = raise (Error (loc, e))

let anomaly s = raise (Anomaly s)
(* TODO environnement pour les types structure *)

let new_struct =
  fun x ->
  {s_name = x; s_fields = Hashtbl.create 0; s_size = 0}

module EnvS = struct
  module M = Map.Make(String)
  type t = structure M.t
  let empty = M.empty
  let find = M.find
  let add envs struc = envs := M.add struc.s_name struc !envs

  let all_structs = ref []

  let struc x envs=
    let struc = new_struct x in
    all_structs := struc :: ! all_structs;
    add envs struc;
    struc
end

let envs = ref EnvS.empty


(* TODO environnement pour les fonctions *)
let new_func =
  fun x var ty ->
  {fn_name = x; fn_params = var; fn_typ = ty}

module EnvF = struct
  module M = Map.Make(String)
  type t = function_ M.t
  let empty = M.empty
  let find = M.find
  let add envf fn = M.add fn.fn_name fn envf

  let all_funcs = ref []

  let func x var ty envf=
    let fn = new_func x var ty in
    all_funcs := fn :: !all_funcs;
    envf :=add !envf fn;
    fn
end

let envf = ref EnvF.empty


let rec type_type = function
  | PTident { id = "int" } -> Tint
  | PTident { id = "bool" } -> Tbool
  | PTident { id = "string" } -> Tstring
  | PTptr ty -> Tptr (type_type ty)
  | PTident { loc = loc; id = ids} -> try Tstruct(EnvS.find ids !envs)
                                      with Not_found -> error loc ("struture "^ids^" is undefined")
  | _ -> error dummy_loc ("unknown struct ") (* TODO type structure *)

let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | Tmany ty1, Tmany ty2 -> List.for_all2 eq_type ty1 ty2
  | Twild, _ | _, Twild -> true
  | _ -> false
    (* TODO autres types *)

let rec string_of_type = function
    | Tint -> "int"
    | Tbool -> "bool"
    | Tstring -> "string"
    | Tptr x -> "*"^string_of_type x
    | Tstruct st -> st.s_name
    | Tmany [] -> "void"
    | Tmany lt -> "Tmany ("^string_of_type_list lt^")"
    | _ -> "unknow_type"

and string_of_type_list = function
    [] -> ""
  |t::[] -> string_of_type t
  |t::q -> string_of_type t^", "^string_of_type_list q

let fmt_used = ref false
let fmt_imported = ref false

let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

let new_var =
  let id = ref 0 in
  fun x loc ?(used=false) ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty; v_used = used; v_addr = 0; v_depth = 0 }

module Env = struct
  module M = Map.Make(String)
  type t = var M.t
  let empty = M.empty
  let find = M.find
  let add env v = M.add v.v_name v env

  let all_vars = ref []
  let check_unused () =
    let check v =
      if v.v_name <> "_" && (* TODO used *) v.v_used then error v.v_loc "unused variable" in
    List.iter check !all_vars


  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  (* TODO type () et vecteur de types *)
    let fn = ref {fn_name = ""; fn_params = []; fn_typ = []}
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid

let simplify_expr_list_typ = function
    | [{expr_desc = TEcall(fn,exp)}] -> fn.fn_typ
    | t -> List.map (fun e -> e.expr_typ) t

let rec expr env e =
 let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty }, rt

and expr_desc env loc = function
  | PEskip ->
     TEskip, tvoid, false
  | PEconstant c -> begin
    (* TODO Ok*)
      let constant_type = match c with
          Cbool _ -> Tbool
        | Cint _ -> Tint
        | Cstring _ -> Tstring
      in
      TEconstant c, constant_type, false
    end

  | PEbinop (op, e1, e2) ->
    (* TODO Ok*)
     begin
       let expr1, rt1 = expr env e1 and
           expr2, rt2 = expr env e2 in
       let type_in, type_out = match op with
         | Band | Bor ->  Tbool, Tbool
         | Badd | Bsub | Bmul | Bdiv | Bmod -> Tint, Tint
         | Beq | Bne -> if expr1.expr_desc <> TEnil then expr1.expr_typ, Tbool
                        else if expr2.expr_desc <> TEnil then expr2.expr_typ, Tbool
                        else error loc "both variables are nil"
         | Blt | Bgt | Ble | Bge-> Tint, Tbool
       in
       if eq_type expr1.expr_typ expr2.expr_typ then
         if eq_type expr1.expr_typ type_in then (TEbinop(op,expr1,expr2), type_out, false)
         else error loc ("wrong type for binary operation")
       else error loc "The two expressions must be of the same type"
     end
  | PEunop (Uamp, e1) ->
    (* TODO Ok*)
     begin
       let expr1, rt = expr env e1 in
       let var = match expr1.expr_desc with
         | TEident v -> v
         | _ -> error loc "variable is expected"
       in
       try
         let v = Env.find var.v_name env in
         TEunop(Uamp,expr1), Tptr v.v_typ, false
       with Not_found -> error loc "variable didn't exist"
          | _ -> error loc "error with &"
     end
  | PEunop (Uneg | Unot | Ustar as op, e1) ->
    (* TODO Ok*)
     begin
       let expr1, rt1 = expr env e1 in
       let type_in, type_out = match op with
         |Uneg -> Tint, Tint
         |Unot -> Tbool, Tbool
         |Ustar -> begin
             match expr1.expr_typ with
               | Tptr x -> Tptr x, x
               | _ -> error loc "wrong type for *, pointer is expected"
             end
         |Uamp -> assert false
       in
       if eq_type expr1.expr_typ type_in then
         (TEunop(op, expr1), type_out, false)
       else error loc "wrong type for unit operation"
    end
  | PEcall ({id = "fmt.Print"}, el) ->
    (* TODO Ok*)
     let lt = List.map (fun x->fst (expr env x)) el in
     fmt_used := true;
     TEprint lt, tvoid, false
  | PEcall ({id="new"}, [{pexpr_desc=PEident {id}}]) ->
     let ty = match id with
       | "int" -> Tint | "bool" -> Tbool | "string" -> Tstring
       | _ -> (* TODO *)
          try
            let st = EnvS.find id !envs in
            Tstruct st
          with Not_found -> error loc ("no such type " ^ id)
     in TEnew ty, Tptr ty, false
  | PEcall ({id="new"}, _) ->
     error loc "new expects a type"
  | PEcall (id, el) ->
     (* TODO *)
     begin
       let fn = try EnvF.find id.id !envf
       with Not_found -> error loc ("unknown function "^id.id)
       in
       let exprl = List.map (fun e -> fst (expr env e)) el
       in
           try
           if List.for_all2 (fun ety v-> eq_type ety v.v_typ)
               (simplify_expr_list_typ exprl) fn.fn_params
            then TEcall(fn,exprl),Tmany fn.fn_typ, false
           else raise (Invalid_argument "")
           with Invalid_argument _ -> error loc ("The function "^id.id^" has parameters of type "^string_of_type_list (List.map (fun v -> v.v_typ) fn.fn_params)^" but function is call with type "^string_of_type_list (List.map (fun x -> x.expr_typ) exprl));
     end
  | PEfor (e, b) ->
     (* TODO *)
     begin
       let expr1,rt1 = expr env e and
           expr2,rt2 = expr env b in
       if expr1.expr_typ = Tbool then
         TEfor(expr1,expr2), tvoid, false
       else error loc "bool is expected for condition"
     end
  | PEif (e1, e2, e3) ->
     (* TODO *)
     begin
       let expr1, rt1 = expr env e1 in
       if expr1.expr_typ = Tbool then
         begin
           let expr2, rt2 = expr env e2 and
               expr3, rt3 = expr env e3 in
           TEif(expr1,expr2,expr3), tvoid, rt2 && rt3 (*change ty*)
         end
       else error loc "wrong type for if condition, bool is expected"
     end
  | PEnil ->
     (* TODO *)
     TEnil, Tptr Twild ,false
  | PEident {id=id} ->
     (* TODO *) (try
         let v = Env.find id env in TEident v, v.v_typ, false
      with Not_found -> error loc ("unbound variable " ^ id))
  | PEdot (e, id) ->
     (* TODO *)
     begin
       let expr1, rt1 = expr env e in
       match expr1.expr_typ with
         | Tptr (Tstruct st) -> begin
            if expr1.expr_desc = TEnil then
              error loc "expression is nil but pointer not nul is expected"
            else
              try
                let field = Hashtbl.find st.s_fields id.id in
                TEdot(expr1,field), field.f_typ, false
              with Not_found -> error loc ("field "^id.id^" didn't exist in structure "^st.s_name)
           end
         | Tstruct st -> begin
             try
               let field = Hashtbl.find st.s_fields  id.id in
               TEdot(expr1,field),field.f_typ, false
             with Not_found -> error loc ("field "^id.id^" didn't exist in structure "^st.s_name)
           end
         | _ -> error loc "the expression must be a structure"
     end
  | PEassign (lvl, el) ->
     (* TODO *)
     begin
       let vl = List.map (fun x -> fst (expr env x)) lvl in
       let exl = List.map (fun x -> fst (expr env x)) el in
       (match exl with
         [] -> error loc "expression is expected"
       | [{expr_desc = TEcall(fn,exprfl); expr_typ = typex}] ->
            List.iter2 (fun v ty -> if not (eq_type v.expr_typ ty) then
                                      error loc ("The "^string_of_type v.expr_typ^" type of the variable and the "^string_of_type ty^" type of the output of the function "^fn.fn_name ^" do not match")) vl fn.fn_typ
       | _ -> List.iter2 (fun v e -> if not (eq_type v.expr_typ e.expr_typ) then
                                       error loc "variables and expressions types didn't match") vl exl);
       TEassign (vl, exl), tvoid, false
     end
  | PEreturn el ->
     (* TODO Ok*)
     begin
     let exprl = List.map (fun x->fst (expr env x)) el in
     try
       if List.for_all2 (fun ex ty -> eq_type ex.expr_typ ty)
            exprl !(Env.fn).fn_typ then
       TEreturn exprl, tvoid, true
       else raise (Invalid_argument "")
     with Invalid_argument _ -> error loc ("The function "^ !(Env.fn).fn_name^" is of type "^string_of_type_list !(Env.fn).fn_typ^" but a type "^string_of_type_list (List.map (fun x -> x.expr_typ) exprl)^" is returned")
     end
  | PEblock el ->
     (* TODO *)
     begin
       if el = [] then (TEblock [], tvoid, false)
       else begin
           let (expr1,rt1), nenv =  match el with
             | [] -> assert false
             | {pexpr_desc = PEvars (idl,tyo,pexprl); pexpr_loc = locvars} :: ql -> begin
                 let exprl = List.map (fun x -> fst (expr env x)) pexprl in
                 match tyo,exprl with
                   | None, [] -> error locvars "Unknow type for variables"
                   | None, [{expr_desc = TEcall (fn,exprfl); expr_typ = tyl}] ->
                      begin
                        (*if List.length fn.fn_typ <> List.length idl then error locvars "Wrong number of variable";*)
                        let new_env = ref env in
                        try
                          let varl = List.map2 (fun id ty ->
                                         let tenv, var = Env.var id.id id.loc ty !new_env
                                         in new_env := tenv; var) idl fn.fn_typ
                          in (stmt (TEvars(varl,exprl)),false), !new_env
                        with Invalid_argument _-> error locvars ("The function "^ fn.fn_name^" is of type "^string_of_type_list fn.fn_typ^" but "^string_of_int (List.length idl) ^" variables are defined")
                      end
                   | None, _ ->
                      begin
                        if List.length exprl <> List.length idl then error locvars ("The "^string_of_int (List.length idl)^" variables defined do not correspond to the "^string_of_int (List.length exprl)^" expressions");
                        let new_env = ref env in
                        let varl = List.map2 (fun id expr->
                                       let tenv, var = Env.var id.id id.loc expr.expr_typ !new_env in
                                       new_env := tenv; var) idl exprl
                        in (stmt (TEvars(varl,exprl)),false), !new_env
                      end
                   | Some ty, [] ->
                      begin
                        let new_env = ref env in
                        let varl = List.map (fun id -> let tenv,var = Env.var id.id id.loc (type_type ty) !new_env
                                     in new_env := tenv; var) idl
                        in (stmt (TEvars(varl,[])),false), !new_env
                      end
                   | Some ty, [{expr_desc = TEcall (fn, exprl); expr_typ = tyl}]->
                      begin
                        if List.length fn.fn_typ <> List.length idl then error loc "Wrong number of variable";
                        if not (List.for_all (fun x -> x = type_type ty) fn.fn_typ) then error locvars "Wrong type of funcion return";
                        let new_env = ref env in
                        let varl = List.map2 (fun id expr ->
                                       let tenv, var = Env.var id.id id.loc expr.expr_typ !new_env
                                       in new_env := tenv; var) idl exprl
                        in (stmt (TEvars(varl,exprl)),false), !new_env
                      end
                   | Some x,_ ->
                      begin
                        if List.length exprl <> List.length idl then error locvars "Wrong number of varaible and expression";
                        let new_env = ref env in
                        let varl = List.map2 (fun id expr->
                                       if type_type x <> expr.expr_typ then error locvars "Type sepciefied and expression type didn't match";
                                       let tenv, var = Env.var id.id id.loc expr.expr_typ !new_env in
                                       new_env := tenv; var) idl exprl
                        in (stmt (TEvars(varl,exprl)),false), !new_env
                      end
               end
             | e::ql -> expr env e , env
           in
           match el with
           |[] -> assert false
           | e::[] -> TEblock [expr1], tvoid, rt1
           | e::ql -> if rt1 then error loc "instruction never executed"
                      else
                        let expr2,ty,rt2 = expr_desc nenv loc (PEblock ql) in
                        let exprl = match expr2 with
                          | TEblock q -> q
                          | _ -> error loc "impossible ?" in
                        TEblock (expr1::exprl), tvoid, rt2
         end
     end
  | PEincdec (e, op) ->
     (* TODO ok*)
     begin
       let expr1, _ = expr env e in
       match expr1.expr_desc, expr1.expr_typ with
         | TEident _, Tint -> TEincdec(expr1,op), tvoid, false
         | TEident _, _ -> error loc "type int is expected"
         | _, _ -> error loc "variable is expected"
     end
  | PEvars _ ->
     (* TODO *) error loc "variables must be defined in a block"


let found_main = ref false

(* 1. declare structures *)
let phase1 = function
  | PDstruct { ps_name = { id = id; loc = loc }} -> (* TODO *)
     begin
       try
         let _ = EnvS.find id !envs in error loc "Structure already exist"
       with Not_found -> let _ =  EnvS.struc id envs in ()
     end
  | PDfunction _ -> ()

let sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | Tstruct st -> st.s_size
  | _ -> (* TODO *) assert false

(* 2. declare functions and type fields *)
let phase2 = function
  | PDfunction { pf_name={id; loc}; pf_params=pl; pf_typ=tyl; } ->
     (* TODO *)
     begin
       try
         let _ = EnvF.find id !envf in error loc ("function "^id^" already exist")
       with
         Not_found -> begin
           let rec variable_list paraml= match paraml with
               | [] -> []
               | (idp,ty)::q ->
                  let varl = variable_list q in
                  try
                    let _ = List.find (fun var -> var.v_name = idp.id)  varl
                    in error loc (idp.id^" is not an unique parameter in function declaration of "^id)
                  with Not_found -> (new_var idp.id idp.loc (type_type ty))::varl
           in
           let vl = variable_list pl  and
               ttyl = List.map type_type tyl in
           if id = "main" then
             if vl = [] then
               if ttyl = [] then
                 found_main := true
               else error loc "main didn't return any type"
             else error loc "main didn't take entry";
           let _ = EnvF.func id vl ttyl envf in ()
           end
     end
  | PDstruct { ps_name = {id}; ps_fields = fl } ->
     (* TODO *)
     begin
       try
         let st = EnvS.find id !envs in
         List.iter (fun (idf,ty) ->
             try
               let _ = Hashtbl.find st.s_fields idf.id in
               error idf.loc ("The field "^idf.id^" is defined several times in the structure "^id)
             with Not_found ->
               Hashtbl.add st.s_fields idf.id {f_name = idf.id;
                                               f_typ = type_type ty;
                                               f_ofs = 0} ) fl
       with Not_found -> error dummy_loc "Structure not found"
     end

(* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=tyl } ->
    (* TODO check name and type *)
    let f = EnvF.find id !envf  in
    let env = ref Env.empty in
    Env.fn := f;
    List.iter (fun v -> env := Env.add !env v) f.fn_params;
    let e, rt = expr !env e in
    if f.fn_name = "main" then begin
        if rt then error loc "main has return but not return is expected";
        if e.expr_typ <> tvoid then error loc "main";
        if f.fn_params <> [] then error loc "main params";
        if f.fn_typ <> [] then error loc "main type void";
      end;
    TDfunction (f, e)
  | PDstruct {ps_name={id}} ->
     (* TODO *) (*let s = { s_name = id; s_fields = Hashtbl.create 5; s_size = 0 } in*)
     let rec no_recursive_and_size stl =
       Hashtbl.fold (fun sf f s-> match f.f_typ with
                                   | Tstruct fst->
                                      if List.exists (fun st -> fst.s_name = st.s_name) stl
                                      then error dummy_loc ("structure "^id^" is recursive, but struture can't be recursive")
                                      else begin
                                          f.f_ofs <- s;
                                          s + no_recursive_and_size (fst::stl)
                                           end
                                   | typ -> f.f_ofs <- s;  s + sizeof typ )
         (List.hd stl).s_fields 0
     in let st = (EnvS.find id !envs) in
        st.s_size <- no_recursive_and_size [st];
        TDstruct st

let file ~debug:b (imp, dl) =
  debug := b;
  (* fmt_imported := imp; *)
  List.iter phase1 dl;
  List.iter phase2 dl;
  if not !found_main then error dummy_loc "missing method main";
  let dl = List.map decl dl in
  Env.check_unused (); (* TODO variables non utilisees *)
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl
