
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
