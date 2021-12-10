type 'a bench = { result : 'a; time : float }

let time (f : unit -> 'a) : 'a bench =
  let t = Sys.time () in
  let r = f () in
  { result = r; time = Sys.time () -. t }