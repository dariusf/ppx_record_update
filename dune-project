(lang dune 2.7)

(name ppx_record_update)
(generate_opam_files true)

(source
 (github dariusf/ppx_record_update))

(authors "Darius Foo")

(maintainers "darius.foo.tw@gmail.com")

(package
 (name ppx_record_update)
 (synopsis "A ppx for nicer record update syntax")
 (description "Provides syntax for assigning individual fields of a record, mapping over fields, and fusing several such updates into a single one."))
