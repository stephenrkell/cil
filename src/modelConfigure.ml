module C = Configurator.V1

let () =
  let cc = ref "" in
  let m = ref "" in
  let args = Arg.[
      ("--cc", Set_string cc, "");
      ("-m", Set_string m, "");
    ]
  in
  C.main ~name:"model" ~args (fun c ->
      let exe = "./machdep-ml" ^ !m ^ ".exe" in
      let {C.Process.exit_code; stdout; stderr} = C.Process.run c !cc ["-D_GNUCC"; "-include"; "machdep" ^ !m ^ "-config.h"; "-m" ^ !m; "machdep-ml.c"; "-o"; exe] in
      if exit_code = 0 then (
        let {C.Process.stdout; stderr; exit_code} = C.Process.run c exe ["--env"] in
        assert (exit_code = 0);
        print_string stdout;
      )
      else 
        Printf.printf "nogcc%smodel\n" !m
    )
