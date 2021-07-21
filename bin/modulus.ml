open Modulus_lib.Loop

let () =
  if Array.length Sys.argv > 1 then
    if Sys.argv.(1) = "-i" then
      ignore (repl ())
    else
      ignore (batch Sys.argv.(1))
  else begin
    Printf.eprintf "usage: %s ( -i | <file> )\n" Sys.argv.(0);
    flush stderr;
    exit 1
  end 