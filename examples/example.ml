module Person = struct
  type t = {
    id : int;
    name : string;
  }
  [@@deriving show]
end

module Book = struct
  type t = {
    borrowed_by : Person.t;
    title : string;
  }
  [@@deriving show]
end

module Library = struct
  type t = { book : Book.t } [@@deriving show]
end

let _ =
  let p = { Person.id = 10; name = "Alyssa" } in
  let b = { Book.borrowed_by = p; title = "Wizard" } in
  let lib = { Library.book = b } in

  let print l = l |> Library.show |> print_endline in
  print lib;
  print
    [%r
      lib.book.borrowed_by.name <- "Louis";
      lib.book.title <- "Dragon";
      lib.book.borrowed_by.id <~ fun x -> x + 1]
