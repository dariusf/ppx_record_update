
module Person = struct
  type t = { id : int; name : string; age : int; child : t option }
  [@@deriving show]
end

module Book = struct
  type t = { borrowed_by : Person.t; title : string }
  [@@deriving show]
end

let _ =

  let p = { Person.id = 10; name = "bob"; age = 20; child = None } in
  let b = Book.{ borrowed_by = p; title = "anna karenina" } in

  let succ x = x + 1 in

  (* let _change_id = { b with borrowed_by = { b.borrowed_by with id = 3 } } in *)
  print_endline @@ Book.show [%record
     (* b.title <~ fun x -> x *)
    b.borrowed_by.id <- 10;
    b.Book.title <- "a tale of two cities";
    b.borrowed_by.child <- Some { Person.id = 1; name = "trudy"; age = 7; child = None };
    b.borrowed_by.age <~ succ;

    (* p.id <- 1; *)

    (* () *)
  ];

  (* print_endline @@ Book.show [%record
     b.Book.borrowed_by.id <- 3;
     p.id <- 3;
     ] *)
