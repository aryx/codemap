let foo () =
  let visitor = { V.x with
                  kfld1 = (fun _ -> ());
                  kfld2 = (fun _ -> ()); }
  in
  2
