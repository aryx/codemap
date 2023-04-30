(*s: ui_layers.mli *)
val choose_layer: 
  root:Common.filename ->
  string option (* layer title we want *) -> 
  Model2.world -> 
  unit
(*e: ui_layers.mli *)
