open Modulus_lib.Loop

let () =
  if Array.length Sys.argv > 1 then
    if Sys.argv.(1) = "-i" then
      exec ()
    else begin
      Printf.eprintf "unknown command '%s'\n" Sys.argv.(1);
      flush stderr;
      exit 1
    end
  else begin
    Printf.eprintf "batch mode is'nt supported yet\n";
    flush stderr;
    exit 1
  end 