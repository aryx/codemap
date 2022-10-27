let visit_program ast =
  let hooks =
    { V.default_visitor with

      V.kdef = (fun (k, _) x ->
      x
      );

      V.ktparam = (fun (k, _) x ->
        k x
      );

    }
  in
  let v = V.mk_visitor hooks in
  v (Pr ast);
  ()
