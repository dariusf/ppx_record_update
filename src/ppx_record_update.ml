open Ppxlib
open Ast_builder.Default

type string = label

module Errors = struct
  let already_assigned = "this field has already been assigned"
end

(* signals that a case should be impossible *)
let error fmt = Format.ksprintf failwith fmt

let error_node ~loc msg = [%expr [%ocaml.error [%e estring ~loc msg]]]

exception Failure of Location.t * string

(* every instance of this should have a test case *)
let malformed_user_input ~loc s = raise (Failure (loc, s))

let rec pexp_field_to_list e =
  match e with
  | { pexp_desc = Pexp_field (e, ident); _ } ->
    (* TODO n^2 *)
    pexp_field_to_list e @ [Loc.txt ident]
  | { pexp_desc = Pexp_ident ident; _ } -> [Loc.txt ident]
  | _ ->
    (* This is probably guaranteed by the parser *)
    error "pexp_field_to_list: not a field or ident"

type ident = string list

(* this is really a list of longidents.
   the inner lists are qualified identifiers (Longident),
   but represented as lists via Longident.flatten_exn *)
type path = ident list

let show_ident xs = String.concat "->" xs

let show_path xs = List.map (String.concat ".") xs |> show_ident

let check_assignment_exp e =
  match e with
  | { pexp_desc = Pexp_setfield _; _ }
  | {
      pexp_desc =
        Pexp_apply
          ({ pexp_desc = Pexp_ident { txt = Lident "<~"; _ }; _ }, [_; _]);
      _;
    } ->
    e
  | { pexp_loc = loc; _ } ->
    malformed_user_input ~loc "expected a field assignment using <- or <~"

let rec pexp_sequence_to_list e =
  match e with
  | { pexp_desc = Pexp_sequence (h, t); _ } ->
    check_assignment_exp h :: pexp_sequence_to_list t
  | x -> [check_assignment_exp x]

type rhs =
  | Expr of expression
  | Func of expression

let show_rhs = function Expr e | Func e -> Pprintast.string_of_expression e

type assignment = {
  lhs : path;
  rhs : rhs;
  loc : Location.t;
}

(** This should be kept in sync with check_assignment_exp *)
let to_assignment e =
  match e with
  | { pexp_desc = Pexp_setfield (({ pexp_loc = loc; _ } as lhs), f, rhs); _ } ->
    let fs = pexp_field_to_list lhs in
    {
      lhs = List.map Longident.flatten_exn (fs @ [f |> Loc.txt]);
      rhs = Expr rhs;
      loc;
    }
  | {
   pexp_desc =
     Pexp_apply
       ( { pexp_desc = Pexp_ident { txt = Lident "<~"; _ }; pexp_loc = loc; _ },
         [(_, i); (_, f)] );
   _;
  } ->
    let fs = pexp_field_to_list i in
    { lhs = List.map Longident.flatten_exn fs; rhs = Func f; loc }
  | _ -> error "to_assignment: invalid field assignment"

let loc = Location.none

let wrap_loc ~loc a = Location.{ txt = a; loc }

let list_to_pexp_field (e : Longident.t list) =
  match e with
  | [] -> error "list_to_pexp_field: empty list"
  | x :: xs ->
    List.fold_left
      (fun t c -> pexp_field ~loc t (wrap_loc ~loc c))
      (pexp_ident ~loc (wrap_loc ~loc x))
      xs

let show_longident i = Longident.flatten_exn i |> String.concat "->"

let unflatten xs =
  match xs with
  | [] -> failwith "unflatten: empty list"
  | [x] -> Lident x
  | x :: xs -> List.fold_left (fun t c -> Ldot (t, c)) (Lident x) xs

module Trie = struct
  type t = Node of string list * neighbour

  and neighbour =
    | Branches of t list
    | Terminal of rhs

  let rec show (Node (ss, n)) =
    let open Printf in
    let id = ss |> String.concat "." in
    match n with
    | Branches ts ->
      sprintf "Node (%s, Branches [%s])" id
        (List.map show ts |> String.concat ";")
    | Terminal e -> sprintf "Node (%s, Terminal %s)" id (show_rhs e)

  let rec from is e =
    match is with
    | [] -> error "from: empty argument"
    | [x] -> Node (x, Terminal e)
    | x :: xs -> Node (x, Branches [from xs e])

  let insert node ({ loc; _ } as assignment) =
    let rec run (Node (id, nx)) { lhs; rhs; _ } =
      match nx with
      | Terminal _ -> malformed_user_input ~loc Errors.already_assigned
      | Branches br ->
      match lhs with
      | [] ->
        (* this should be impossible, as if assignment is nonempty,
           we'll fail or succeed before this point *)
        error "insert: empty assignment"
      | [lid] ->
        (* lid is the final record field we're assigning.
           we want to create a terminal here to store the rhs,
           so something being here already signals an error. *)
        if List.exists (fun (Node (n, _)) -> n = lid) br then
          malformed_user_input ~loc "this value is already assigned"
        else
          Node (id, Branches (Node (lid, Terminal rhs) :: br))
      | lid :: ids ->
      (* we're not at the end, so just traverse down, creating or extending existing nodes *)
      match List.partition (fun (Node (i, _)) -> i = lid) br with
      | ([], _) ->
        (* no matching node, so create one and continue *)
        let r = run (Node (lid, Branches [])) { lhs = ids; rhs; loc } in
        Node (id, Branches (r :: br))
      | ([n], rest) ->
        (* exactly one matching node; take it *)
        let r = run n { lhs = ids; rhs; loc } in
        Node (id, Branches (r :: rest))
      | _ -> error "insert: more than one node with name %s" (show_ident lid)
    in
    (* an assignment looks like { lhs = x.y.z; rhs = 10 },
       where x is the variable name of the record variable we're assigning.
       the topmost trie node's id (below) will be x. *)
    let (Node (id, _)) = node in
    match assignment.lhs with
    | [] -> error "insert: empty assignment"
    | lid :: lhs when lid = id -> run node { assignment with lhs }
    | _ ->
      (* the root of the trie is not x *)
      malformed_user_input ~loc
        "only one record can be assigned to per update expression"

  let to_record node =
    let rec run ctx (Node (id, nx)) =
      let field =
        List.map unflatten (List.rev ctx @ [id]) |> list_to_pexp_field
      in
      match nx with
      | Terminal r ->
        ( id,
          (match r with
          | Expr e -> e
          | Func f -> pexp_apply ~loc f [(Nolabel, field)]) )
      | Branches ts ->
        let res = List.map (fun t -> run (id :: ctx) t) ts in
        let fs =
          res |> List.map (fun (i, r) -> (wrap_loc ~loc (unflatten i), r))
        in
        (id, pexp_record ~loc fs (Some field))
    in
    run [] node
end

let handle ~loc:_ ~path:_ (e : expression) =
  try
    let es = pexp_sequence_to_list e in
    let assignments = List.map to_assignment es in
    match assignments with
    | { lhs = v; rhs = e; _ } :: a1 ->
      (* fold left over the tail (with head as init) to preserve order,
         so errors are reported from left to right *)
      let tr =
        List.fold_left
          (fun t c ->
            let r = Trie.insert t c in
            (* easy way to understanding what's going on *)
            (* print_endline (Trie.show r); *)
            r)
          (Trie.from v e) a1
      in
      let (_, e) = Trie.to_record tr in
      (* suppress warning about using with clause when all fields are present *)
      {
        e with
        pexp_attributes =
          [
            {
              attr_loc = loc;
              attr_name = Loc.make ~loc "ocaml.warning";
              attr_payload = PStr [pstr_eval ~loc (estring ~loc "-23") []];
            };
          ];
      }
    | _ -> error "handle: empty sequence"
  with Failure (loc, s) -> error_node ~loc s

let ext =
  Extension.declare "record" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    handle

let () = Driver.register_transformation "ppx_record_update" ~extensions:[ext]
