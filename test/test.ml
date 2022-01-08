module Person = struct
  type t = {
    id : int;
    name : string;
    age : int;
    child : t option;
  }
  [@@deriving show]
end

module Book = struct
  type t = {
    borrowed_by : Person.t;
    title : string;
    in_queue : Person.t list;
  }
  [@@deriving show]
end

type state = { mutable local : int }

let _ =
  let p = { Person.id = 10; name = "bob"; age = 20; child = None } in
  let p1 = { Person.id = 11; name = "trudy"; age = 1; child = None } in
  let b = Book.{ borrowed_by = p; title = "anna karenina"; in_queue = [] } in

  let succ x = x + 1 in

  (* let _change_id = { b with borrowed_by = { b.borrowed_by with id = 3 } } in *)
  print_endline
  @@ Book.show
       [%record
         (* b.title <~ fun x -> x; *)
         (b.in_queue <~ fun p -> p1 :: p);
         b.borrowed_by.id <- 10;
         b.Book.title <- "a tale of two cities";
         b.borrowed_by.child <-
           Some { Person.id = 1; name = "trudy"; age = 7; child = None };
         (* already_assigned *)
         (* b.borrowed_by.child.age <- 1; *)
         (* b.Book.borrowed_by.Person.child <- Some { Person.id = 1; name = "trudy"; age = 7; child = None }; *)
         b.borrowed_by.age <~ succ
         (* p.id <- 1; *)

         (* () *)];

  let s = { local = 1 } in
  s.local <- 1;
  (* the fun can also be parenthesized but ocamlformat produces this *)
  ( s.local <~ fun s -> s + 1 );
  (* this wil apply to the rest of a seq *)
  Format.printf "%d@." s.local;
  s.local <~ fun s ->
  ();
  s + 1

(* print_endline @@ Book.show [%record
   b.Book.borrowed_by.id <- 3;
   p.id <- 3;
   ] *)
