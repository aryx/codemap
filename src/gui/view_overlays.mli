(*s: view_overlays.mli *)

val draw_searched_rectangles:
  dw:Model.drawing -> unit

val motion_notify:
  Model.world -> GdkEvent.Motion.t -> bool

val paint_initial:
  Model.drawing -> unit
val hook_finish_paint:
  Model.world -> unit

(*e: view_overlays.mli *)
