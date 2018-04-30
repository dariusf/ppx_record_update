
let () =
  [%record
    ()
  ]

let () =
  [%record
    a.b <- 1;
    b.c <- "asd";
  ]

let () =
  [%record
    a.b <- 1;
    a.b <- 2;
  ]

(* Unhandled *)
let () =
  [%record
    a.b <- 1;
    a.A.b <- 2;
  ]
