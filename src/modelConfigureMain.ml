module C = Configurator.V1

let () =
  let cc = ref "cc" in
  let c_flags = ref [] in
  let args = Arg.[
      ("--cc", Set_string cc, "");
      ("-m", String (fun s -> c_flags := ("-m" ^ s) :: !c_flags), "");
    ]
  in
    C.main ~name:"model" ~args (fun c ->
      Printf.printf "%s" (ModelConfigure.generateModel c !cc !c_flags |> ModelCommon.model_to_yojson |> Yojson.Safe.to_string)
    )
