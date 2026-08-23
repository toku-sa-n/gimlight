let () =
  try Terminal.with_raw_mode (fun () -> failwith "expected terminal exception")
  with Failure _ -> print_endline "terminal restored"
