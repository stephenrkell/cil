open GoblintCil
open ModelConfigure
open Configurator.V1

module C = Configurator.V1

let compilersToTest = [
  "gcc";
  "gcc-10";
  "gcc-11";
  "gcc-12";
  "gcc-13";
  "gcc-14";
  "gcc-15";
  "clang";
  "clang-14";
  "clang-15";
  "clang-16";
  "clang-17";
  "clang-18";
  "clang-19";
  "clang-20";
]

let getModelFromMacroDefs (c: C.t) (cc: string) (c_flags: string list) : Model.model =
  let tempfile = Filename.temp_file "modeltest" ".i" in
  let compiler_ok = C.Process.run_ok c cc (c_flags @ ["-Wp,-dD"; "-xc"; "-E"; "/dev/null"; "-o"; tempfile]) in
    if not compiler_ok then begin
      Sys.remove tempfile;
      raise (Failure (Printf.sprintf "Failed to run %s %s" cc (String.concat " " c_flags)))
    end
    else
      ignore (GoblintCil.Frontc.parse tempfile ());
      Sys.remove tempfile;
      !GoblintCil.Model.theModel

let is_compiler_ok (c: C.t) (cc: string) (c_flags: string list) : bool =
  let (tempfile, ch) = Filename.open_temp_file "modeltest" ".c" in
  output_string ch "int main() { return 0; }\n";
  close_out ch;
  let ret = C.Process.run_ok c cc (c_flags @ [tempfile; "-o"; "/dev/null"]) in
  Sys.remove tempfile;
  ret

let () =
  GoblintCil.initCIL ();
  C.main ~name:"model" ~args:Arg.[] (fun c ->
    compilersToTest |> List.iter (fun cc ->
      if not (C.Process.run_ok c cc ["--version"]) then
        Printf.printf "Compiler %s not found, skipping.\n" cc
      else
        let target = C.Process.run_capture_exn c cc ["-dumpmachine"] in
        let arch = String.trim target |> String.split_on_char '-' |> List.hd in
        let cflags_to_test = if arch = "x86_64" then [["-m32"]; ["-m64"]; []] else [[]] in

        Printf.printf "Testing %s %s\n" cc (String.concat "/" (List.map (String.concat " ") cflags_to_test));
        cflags_to_test |> List.iter (fun cflags ->
          if not (is_compiler_ok c cc cflags) then
            Printf.printf "%s %s doesn't work, skipping.\n" cc (String.concat " " cflags)
          else
            let model1 = ModelConfigure.generateModel c cc cflags in
            let model2 = getModelFromMacroDefs c cc cflags in
            if model1 <> model2 then begin
              Printf.printf "Model inconsistent for %s %s\n" cc (String.concat " " cflags);
              Printf.printf "Model 1 = %s\n\n" (model1 |> Model.model_to_yojson |> Yojson.Safe.to_string);
              Printf.printf "Model 2 = %s\n\n" (model2 |> Model.model_to_yojson |> Yojson.Safe.to_string);
              raise (Failure "Model inconsistent")
            end else
              Printf.printf "Model consistent for %s %s\n" cc (String.concat " " cflags);
        )
    )
  )
  