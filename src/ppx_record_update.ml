open Ppx_core
open Ast_builder.Default

let name = "record"

let fail ~loc msg =
  [%expr [%ocaml.error [%e estring ~loc msg]]]

exception Failure of Location.t * string
let die ~loc s = raise (Failure (loc, s))

let rec pexp_field_to_list e =
  match e with
  | { pexp_desc = Pexp_field (e, ident) } ->
    (* TODO n^2 *)
    pexp_field_to_list e @ [Loc.txt ident]
  | { pexp_desc = Pexp_ident ident } ->
    [Loc.txt ident]
  | _ ->
    (* This is probably guaranteed by the parser *)
    failwith "pexp_field_to_list: not a field or ident"

let check_assignment_exp e =
  match e with
  | { pexp_desc = Pexp_setfield _ } ->
    (* | { pexp_desc = Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident ":=" } }, _) } -> *)
    e
  | { pexp_loc = loc } ->
    die ~loc "field assignment expected"

let rec pexp_sequence_to_list e =
  match e with
  | { pexp_desc = Pexp_sequence (h, t); pexp_loc; pexp_attributes } ->
    check_assignment_exp h :: pexp_sequence_to_list t
  | x -> [check_assignment_exp x]

type path = string list list

let show_path xs = List.map ~f:(String.concat ~sep:".") xs |> String.concat ~sep:"->"

type assignment = {
  lhs : path;
  rhs : expression;
  loc : Location.t;
}

(** This should be kept in sync with check_assignment_exp *)
let to_assignment e =
  match e with
  | { pexp_desc = Pexp_setfield (lhs, f, rhs); pexp_loc = loc } ->
    let fs = pexp_field_to_list lhs in
    {
      lhs = List.map ~f:Longident.flatten_exn (fs @ [f |> Loc.txt]);
      rhs;
      loc
    }
  | _ ->
    failwith "to_assignment: not a field assignment"

let loc = Location.none

let wrap_loc ~loc a =
  Location.{ txt = a; loc; }

let rec list_to_pexp_field (e : Longident.t list) =
  match e with
  | [] -> failwith "list_to_pexp_field: empty list"
  | x :: xs ->
    List.fold_left ~init:(pexp_ident ~loc (wrap_loc ~loc x))
      ~f:(fun t c -> pexp_field ~loc t (wrap_loc ~loc c)) xs

let show_longident i = Longident.flatten_exn i |> String.concat ~sep:"->"

let unflatten xs =
  match xs with
  | [] -> failwith "unflatten: empty list"
  | [x] -> Lident x
  | x :: xs -> List.fold_left ~init:(Lident x) ~f:(fun t c -> Ldot (t, c)) xs

module Trie = struct

  type t =
    | Node of string list * neighbour
  and neighbour =
    | Branches of t list
    | Terminal of expression

  let rec show (Node (ss, n)) =
    let open Printf in
    let id = ss |> String.concat ~sep:"." in
    match n with
    | Branches ts -> sprintf "Node (%s, Branches (%s))" id (List.map ~f:show ts |> String.concat ~sep:",")
    | Terminal e -> sprintf "Node (%s, Terminal %s)" id (Pprintast.string_of_expression e)

  let rec from is e =
    match is with
    | [] -> failwith "from: empty argument"
    | [x] -> Node (x, Terminal e)
    | x :: xs -> Node (x, Branches [from xs e])

  let rec insert (Node (id, nx)) { lhs; rhs; loc } =
    match lhs with
    | [] -> failwith "trie_insert: empty"
    | [id] -> Node (id, Terminal rhs)
    | id :: ids ->
      (match nx with
       | Branches ts ->
         if List.exists ts ~f:(fun (Node (n, _)) -> Caml.(n = id)) then
           (* invalid_arg "this value is already assigned" *)
           die ~loc "this value is already assigned"
         else
           Node (id, Branches (insert (Node (id, Branches [])) { lhs = ids; rhs; loc } :: ts))
       | Terminal _ ->
         invalid_arg "expressions can only live at leaves")

  let to_record node =
    let rec run ctx (Node (id, nx)) =
      match nx with
      | Terminal e ->
        (* print_endline @@ "terminal"; *)
        (* print_endline @@ show_path [id]; *)
        id, e
      | Branches ts ->
        (* print_endline @@ "branches"; *)
        (* print_endline @@ show_path [id]; *)
        let res = List.map ~f:(fun t -> run (id :: ctx) t) ts in
        let fs = res |> List.map ~f:(fun (i, r) ->
            wrap_loc ~loc (unflatten i), r)
        in
        id, pexp_record ~loc fs (Some (List.map ~f:unflatten (List.rev ctx @ [id]) |> list_to_pexp_field))

    in run [] node

end

let handle ~loc ~path:_ (e : expression) =
  try
    let es = pexp_sequence_to_list e in

    let assignments = es |> List.map ~f:to_assignment in
    (match assignments with
     | { lhs = ai; rhs = ae } :: a1 ->

       (* print_endline @@ show_path ai; *)
       (* print_endline @@ Pprintast.string_of_expression ae; *)

       let tr = List.fold_right ~init:(Trie.from ai ae)
           ~f:(fun c t ->
               Trie.insert t c) a1 in

       (* print_endline @@ Trie.show @@ tr; *)
       let e = snd @@ Trie.to_record tr in
       (* suppress warning about using with clause when all fields are present *)
       [%expr [%e e] [@ocaml.warning "-23"]]

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
  Ppx_driver.register_transformation name ~extensions:[ext]