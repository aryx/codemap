module Foo = struct
  type color = Red | Green | Blue

  let factorial n =
    if n = 0 then 1
    else n * factorial (n - 1)

  let msg = "hello"
  let x = 42
  let b = true
end

open Foo

let () = print_int (factorial 5)
