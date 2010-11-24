let js =
  <:mls<
    let f x = x + x in
    f 3
  >>

let my_script =
  Printf.printf "<SCRIPT>\n%s\n</SCRIPT>\n%!" (Js.to_string js)
