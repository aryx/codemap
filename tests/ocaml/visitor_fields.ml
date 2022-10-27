let foo () =
  let visitor = { V.x with
                  kfld1 = (fun _ -> ());
                  kfld2 = (fun _ -> ()); }
  in
  2

let foo2 () =
  let hooks = { V.x with
                V.kfld1 = (fun _ -> ());
                V.kfld2 = (fun _ -> ()); }
  in
  ()

