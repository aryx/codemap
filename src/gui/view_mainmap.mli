(*s: view_mainmap.mli *)

val paint: Model.drawing -> Model.model Async.t -> unit

val zoom_pan_scale_map: Cairo.context -> Model.drawing -> unit

val device_to_user_area: Model.drawing -> Figures.rectangle

val with_map: Model.drawing -> (Cairo.context -> 'a) -> 'a

val button_action:
   Model.world -> GdkEvent.Button.t -> bool

(*e: view_mainmap.mli *)
