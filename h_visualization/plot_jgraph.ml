open Common
module Color = Simple_color

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* http://www.cs.utk.edu/~plank/plank/jgraph/jgraph.html
*)

let tmpfile = "/tmp/test.jgr"

let prelude () =
  "  newgraph\n\
  \      xaxis\n\
  \      size 7in\n\
  \      min 0\n\
  \      hash 1 mhash 0\n\
   (* autogenerated by jgraph.ml *)\n"

let xaxis_font () = "hash_labels hjr font Helvetica fontsize 10 rotate 45"

let yaxis_label s =
  spf
    "\n\
    \      yaxis\n\
    \      hash_labels fontsize 12 label fontsize 12 : %s\n\
    \      " s

let xaxis_label xpos s =
  spf "\n      newstring x %d y -4 fontsize 12 : %s\n   " xpos s

(*****************************************************************************)
(* communicating with a pipe with R seems harder so use R batch mode *)
let jgraph_program file = Common.command2 (spf "jgraph %s > %s.ps" file file)

let gv_command file =
  let _status = Unix.system (spf "gv %s.ps &" file) in
  (* zarb: I need this when I launch the program via eshell, otherwise gv
     do not get the chance to be launched
  *)
  Unix.sleep 1;
  ()

(*****************************************************************************)

(* the colomns will be x, the lines will be the y *)
let plot_matrix ~lines ~columns ~xlabel ~ylabel matrix =
  let limx = List.length columns - 1 in
  let limy = List.length lines - 1 in

  Common.with_open_outfile tmpfile (fun (pr_no_nl, _chan) ->
      let pr s = pr_no_nl (s ^ "\n") in

      pr (prelude ());

      Common.index_list columns
      |> List.iter (fun (s, i) -> pr (spf "hash_label at %d : %s" i s));
      let middle = limx / 2 in
      pr (xaxis_font ());
      pr (yaxis_label ylabel);
      pr (xaxis_label middle xlabel);
      pr "";

      for j = 0 to limy do
        let color =
          let r, g, b = Color.rainbow_color j in
          spf "color %f %f %f" r g b
        in
        let lbl = List.nth lines j in
        pr (spf "newcurve marktype x linetype solid %s label : %s" color lbl);
        pr "pts";

        for i = 0 to limx do
          pr (spf "%d %f" i matrix.(j).(i))
        done
      done);

  jgraph_program tmpfile;
  gv_command tmpfile;
  ()
