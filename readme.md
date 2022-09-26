
# ppx_record_update

Syntax for updates to nested records.

```ocaml
let _ =
  let p = { Person.id = 10; name = "Alyssa" } in
  let b = Book.{ borrowed_by = p; title = "Wizard" } in
  let lib = Library.{ book = b } in

  let print l = l |> Library.show |> print_endline in
  print lib;
  print
    [%r
      lib.book.borrowed_by.name <- "Louis";
      lib.book.title <- "Dragon";
      lib.book.borrowed_by.id <~ fun x -> x + 1]
```

`<-` assigns a value to a possibly-nested field.

`<~` applies a function to a field. `{ record with field = f record.field }` becomes `[%r record.field <~ f]`.

All updates are fused into a single expression:

```sh
dune describe pp examples/example.ml | sed -n '/let _ =/,$p'
```

```ocaml
let _ =
  let p = { Person.id = 10; name = "Alyssa" } in
  let b = { Book.borrowed_by = p; title = "Wizard" } in
  let lib = { Library.book = b } in
  let print l = (l |> Library.show) |> print_endline in
  print lib;
  print
    (({
        lib with
        book =
          {
            (lib.book) with
            borrowed_by =
              {
                ((lib.book).borrowed_by) with
                id = ((fun x -> x + 1) ((lib.book).borrowed_by).id);
                name = "Louis"
              };
            title = "Dragon"
          }
      })[@ocaml.warning "-23"])
```

# Usage

`make` to build the example project and see its output.
