
open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string
exception Anomaly of string

let error loc e = raise (Error (loc, e))

(* TODO environnement pour les types structure *)

(* TODO environnement pour les fonctions *)
module EnvFunction = struct
  module M = Map.Make(String)
  type t = func M.t
  let empty = M.empty
  let find = M.find
  let add env fn = M.add fn.fn_name fn env

  let all_function = ref []
end



let rec type_type = function
  | PTident { id = "int" } -> Tint
  | PTident { id = "bool" } -> Tbool
  | PTident { id = "string" } -> Tstring
  | PTptr ty -> Tptr (type_type ty)
  | _ -> error dummy_loc ("unknown struct ") (* TODO type structure *)

let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | _ -> false
    (* TODO autres types *)

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
      if v.v_name <> "_" && (* TODO used *) true then error v.v_loc "unused variable" in
    List.iter check !all_vars


  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  (* TODO type () et vecteur de types *)
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid

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
         | Band | Bor -> Tbool, Tbool
         | Badd | Bsub | Bmul | Bdiv | Bmod -> Tint, Tint
         | Beq | Bne | Blt | Bgt | Ble | Bge-> Tint, Tbool
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
    (* TODO NF*)
     begin
       let expr1, rt1 = expr env e1 in
       let type_in, type_out = match op with
         |Uneg -> Tint, Tint
         |Unot -> Tbool, Tbool
         |Ustar -> assert false
         |Uamp -> assert false
       in
       if eq_type expr1.expr_typ type_in then
         (TEunop(op, expr1), type_out, false)
       else error loc "wrong type for unit operation"
    end
  | PEcall ({id = "fmt.Print"}, el) ->
    (* TODO Ok*)
     let lt = List.map (fun x->fst (expr env x)) el in
     TEprint lt, tvoid, false
  | PEcall ({id="new"}, [{pexpr_desc=PEident {id}}]) ->
     let ty = match id with
       | "int" -> Tint | "bool" -> Tbool | "string" -> Tstring
       | _ -> (* TODO *) error loc ("no such type " ^ id) in
     TEnew ty, Tptr ty, false
  | PEcall ({id="new"}, _) ->
     error loc "new expects a type"
  | PEcall (id, el) ->
     (* TODO *) assert false
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
           TEif(expr1,expr2,expr3), tvoid, false (*change ty*)
         end
       else error loc "wrong type for if condition, bool is expected"
     end
  | PEnil ->
     (* TODO *) assert false
  | PEident {id=id} ->
     (* TODO *) (try let v = Env.find id env in TEident v, v.v_typ, false
      with Not_found -> error loc ("unbound variable " ^ id))
  | PEdot (e, id) ->
     (* TODO *) assert false
  | PEassign (lvl, el) ->
     (* TODO *) TEassign ([], []), tvoid, false
  | PEreturn el ->
     (* TODO Ok*)
     let er = List.map (fun x->fst (expr env x)) el in
     TEreturn er, tvoid, true
  | PEblock el ->
     (* TODO *)
     let er = List.map (fun x->fst (expr env x)) el in
     TEblock er, tvoid, false
  | PEincdec (e, op) ->
     (* TODO *) assert false
  | PEvars _ ->
     (* TODO *) assert false

let found_main = ref false

(* 1. declare structures *)
let phase1 = function
  | PDstruct { ps_name = { id = id; loc = loc }} -> (* TODO *) ()
  | PDfunction _ -> ()

let sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | _ -> (* TODO *) assert false

(* 2. declare functions and type fields *)
let phase2 = function
  | PDfunction { pf_name={id; loc}; pf_params=pl; pf_typ=tyl; } ->
     (* TODO *) ()
  | PDstruct { ps_name = {id}; ps_fields = fl } ->
     (* TODO *) ()

(* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=tyl } ->
    (* TODO check name and type *)
    let f = { fn_name = id; fn_params = []; fn_typ = []} in
    let e, rt = expr Env.empty e in
    if f.fn_name = "main" then begin
        if rt then error loc "main return";
        if e.expr_typ <> tvoid then error loc "main";
        if f.fn_params <> [] then error loc "main params";
        if f.fn_typ <> [] then error loc "main type void";
      end;
    TDfunction (f, e)
  | PDstruct {ps_name={id}} ->
    (* TODO *) let s = { s_name = id; s_fields = Hashtbl.create 5; s_size = 0 } in
     TDstruct s

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