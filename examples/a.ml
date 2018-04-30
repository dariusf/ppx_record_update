
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

  print_endline @@ Book.show [%record
    b.borrowed_by.name <- "lol";
    b.borrowed_by.id <~ fun x -> x + 1;
  ]