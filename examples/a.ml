
module Person = struct
  type t = { id : int; name : string }
  [@@deriving show]
end
module Book = struct
  type t = { borrowed_by : Person.t; title : string }
  [@@deriving show]
end

let _ =

  let p = { Person.id = 10; name = "bob" } in
  let b = Book.{ borrowed_by = p; title = "anna karenina" } in

  let _change_id = { b with borrowed_by = { b.borrowed_by with id = 3 } } in
  print_endline @@ Book.show [%record
    b.Book.borrowed_by.id <- 3;
    (* b.title <- ""; *)
    a.id <- 1;
  ]