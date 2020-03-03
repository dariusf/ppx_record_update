open Ppxlib
open Ast_builder.Default

let name = "record"

let fail ~loc msg =
  [%expr [%ocaml.error [%e estring ~loc msg]]]

exception Failure of Location.t * string
let die ~loc s = raise (Failure (loc, s))

let rec pexp_field_to_list e =
  match e with
  | { pexp_desc = Pexp_field (e, ident); _ } ->
    (* TODO n^2 *)
    pexp_field_to_list e @ [Loc.txt ident]
  | { pexp_desc = Pexp_ident ident; _ } ->
    [Loc.txt ident]
  | _ ->
    (* This is probably guaranteed by the parser *)
    failwith "pexp_field_to_list: not a field or ident"

type path = string list list

let show_ident xs =
  String.concat "->" xs

let show_path xs =
  List.map (String.concat ".") xs |> show_ident

let check_assignment_exp e =
  match e with
  | { pexp_desc = Pexp_setfield _; _ }
  | { pexp_desc = Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "<~"; _ }; _ }, [ _; _]); _ } ->
    e
  | { pexp_loc = loc; _ } ->
    die ~loc "expected a field assignment using <- or <~"

let rec pexp_sequence_to_list e =
  match e with
  | { pexp_desc = Pexp_sequence (h, t); _ } ->
    check_assignment_exp h :: pexp_sequence_to_list t
  | x -> [check_assignment_exp x]

type rhs =
  | Expr of expression
  | Func of expression

let show_rhs = function
  | Expr e
  | Func e -> Pprintast.string_of_expression e

type assignment = {
  lhs : path;
  rhs : rhs;
  loc : Location.t;
}

(** This should be kept in sync with check_assignment_exp *)
let to_assignment e =
  match e with
  | { pexp_desc = Pexp_setfield ({ pexp_loc = loc; _ } as lhs, f, rhs); _ } ->
    let fs = pexp_field_to_list lhs in
    {
      lhs = List.map Longident.flatten_exn (fs @ [f |> Loc.txt]);
      rhs = Expr rhs;
      loc
    }
  | { pexp_desc = Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "<~"; _ }; pexp_loc = loc; _ }, [_, i; _, f]); _ } ->
    let fs = pexp_field_to_list i in
    {
      lhs = List.map Longident.flatten_exn fs;
      rhs = Func f;
      loc
    }
  | _ ->
    failwith "to_assignment: invalid field assignment"

let loc = Location.none

let wrap_loc ~loc a =
  Location.{ txt = a; loc; }

let list_to_pexp_field (e : Longident.t list) =
  match e with
  | [] -> failwith "list_to_pexp_field: empty list"
  | x :: xs ->
    List.fold_left (fun t c -> pexp_field ~loc t (wrap_loc ~loc c))
      (pexp_ident ~loc (wrap_loc ~loc x)) xs

let show_longident i = Longident.flatten_exn i |> String.concat "->"

let unflatten xs =
  match xs with
  | [] -> failwith "unflatten: empty list"
  | [x] -> Lident x
  | x :: xs -> List.fold_left (fun t c -> Ldot (t, c)) (Lident x) xs

module Trie = struct

  type t =
    | Node of string list * neighbour
  and neighbour =
    | Branches of t list
    | Terminal of rhs

  let rec show (Node (ss, n)) =
    let open Printf in
    let id = ss |> String.concat "." in
    match n with
    | Branches ts -> sprintf "Node (%s, Branches [%s])" id (List.map show ts |> String.concat ";")
    | Terminal e -> sprintf "Node (%s, Terminal %s)" id (show_rhs e)

  let rec from is e =
    match is with
    | [] -> failwith "from: empty argument"
    | [x] -> Node (x, Terminal e)
    | x :: xs -> Node (x, Branches [from xs e])

  let insert node ({ loc; _ } as assignment) =
    let rec run (Node (id, nx)) { lhs; rhs; _ } =
      match nx with
      | Terminal _ ->
        invalid_arg "expressions can only live at leaves"
      | Branches ts ->
        match lhs with
        | [] ->
          failwith "insert: empty assignment"
        | [lid] ->
          if List.exists (fun (Node (n, _)) -> Caml.(n = lid)) ts then
            die ~loc "this value is already assigned"
          else
            Node (id, Branches (Node (lid, Terminal rhs) :: ts))
        | lid :: ids ->
          match List.partition (fun (Node (i, _)) -> Caml.(i = lid)) ts with
          | [], _ ->
            let r = run (Node (lid, Branches [])) { lhs = ids; rhs; loc } in
            Node (id, Branches (r :: ts))
          | [n], rest ->
            let r = run n { lhs = ids; rhs; loc } in
            Node (id, Branches (r :: rest))
          | _ -> failwith @@ "insert: invariant broken; more than one node with name " ^ show_ident lid
    in
    let Node (id, _) = node in
    match assignment.lhs with
    | [] -> failwith "insert: empty assignment"
    | lid :: lhs when Caml.(lid = id) -> run node { assignment with lhs }
    | _ -> die ~loc "there can be only one root node"

  let to_record node =
    let rec run ctx (Node (id, nx)) =
      let field = List.map unflatten (List.rev ctx @ [id]) |> list_to_pexp_field in
      match nx with
      | Terminal r ->
        (* print_endline @@ "terminal"; *)
        (* print_endline @@ show_path [id]; *)
        id, (match r with
            | Expr e -> e
            | Func f -> pexp_apply ~loc f [Nolabel, field])
      | Branches ts ->
        (* print_endline @@ "branches"; *)
        (* print_endline @@ show_path [id]; *)
        let res = List.map (fun t -> run (id :: ctx) t) ts in
        let fs = res |> List.map (fun (i, r) ->
            wrap_loc ~loc (unflatten i), r)
        in
        id, pexp_record ~loc fs (Some field)

    in run [] node

end

let handle ~loc ~path:_ (e : expression) =
  try
    let es = pexp_sequence_to_list e in

    let assignments = es |> List.map to_assignment in
    (match assignments with
     | { lhs = ai; rhs = ae; _ } :: a1 ->

       (* print_endline @@ show_path ai; *)
       (* print_endline @@ Pprintast.string_of_expression ae; *)

       let tr = List.fold_right (fun c t -> Trie.insert t c) a1 (Trie.from ai ae) in

       (* print_endline @@ Trie.show @@ tr; *)
       let e = snd @@ Trie.to_record tr in

       (* Suppress warning about using with clause when all fields are present *)
       { e with pexp_attributes = [
             { attr_loc = loc; attr_name = Loc.make ~loc "ocaml.warning"; attr_payload = PStr [pstr_eval ~loc (estring ~loc "-23") []]}]
       }

     | _ -> failwith "handle: empty sequence")
  with Failure (loc, s) ->
    fail ~loc s

let ext =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    handle

let () =
  Driver.register_transformation name ~extensions:[ext]
