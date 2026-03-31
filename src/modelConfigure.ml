module C = Configurator.V1
module MC = ModelCommon
module H = Hashtbl

type itemty = Int | Str

type item = {
  ident: string;
  pre_stmt: string option;
  expr: string;
  pre_cond: string option;
  ty: itemty;
}

type itemval = IntVal of int | StrVal of string

let unwrapIntVal = function IntVal i -> i | _ -> raise (Failure "Expected IntVal")
let unwrapStrVal = function StrVal s -> s | _ -> raise (Failure "Expected StrVal")

type programspec = {
  header: string;
  items: item list;
}

let identForName (lowercase: bool) (s: string): string =
  let is_sep = function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> false
    | _ -> true
  in
    String.to_seq s
    |> List.of_seq
    |> List.filter (fun c -> not (is_sep c))
    |> List.to_seq
    |> String.of_seq
    |> (if lowercase then String.lowercase_ascii else String.uppercase_ascii)

let generateSource (p: programspec): string =
  let p1 = p.items |> List.map (fun {ident; ty; _} -> (match ty with Int -> "int " | Str -> "char *") ^ ident ^ ";" ) |> String.concat "\n" in
  let p2 = p.items |> List.map (fun {ident; pre_stmt; expr; pre_cond; ty} ->
      let pre_stmt = match pre_stmt with Some s -> s | None -> "" in
      let pre_cond = match pre_cond with Some s -> s | None -> "1" in
      let default = match ty with Int -> "0" | Str -> "\"\"" in
      Format.sprintf {|#if %s
{
  %s
  %s = %s;
}
#else
  %s = %s;
#endif|} pre_cond pre_stmt ident expr ident default
    )
  |> String.concat "\n" in
  let p3 = p.items 
    |> List.map (fun {ident; ty; _} -> 
      let fmtspec = match ty with Int -> "%d" | Str -> "%s" in 
      Format.sprintf "printf(\"%s=%s\\n\", %s);" ident fmtspec ident)
    |> String.concat "\n" in
  Format.sprintf {|%s
#define XSTR(x) STR(x)
#define STR(x) #x
#include <stdio.h>
#include <stdlib.h>

int main() {
%s
return 0;
}
|} p.header (p1 ^ "\n" ^ p2 ^ "\n" ^ p3)

type testitem = {
  name: string;
  (* Set to 1 if the test program compiles, 0 otherwise *)
  testsrc: string;
}

let cTest (c: C.t) (cc: string) ?(c_flags: string list = []) (f: string): bool =
  let fn, ch = Filename.open_temp_file "cil-model-cfg-test" ".c" in
  output_string ch f;
  close_out ch;
  let r = C.Process.run_ok c cc (["-o"; Filename.null; fn] @ c_flags) in 
  Sys.remove fn;
  r

let compileAndRun (c: C.t) (cc: string) ?(c_flags: string list = []) (f: string): string =
  let srcfn, srcch = Filename.open_temp_file "cil-model-cfg" ".c" in
  let outfn = Filename.temp_file "cil-model-cfg" ".out" in
  output_string srcch f;
  close_out srcch;
  if (C.Process.run c cc (["-o"; outfn; srcfn] @ c_flags)).exit_code != 0 then begin
    Sys.remove srcfn;
    Sys.remove outfn;
    raise (Failure "Compilation failed");
  end;
  C.Process.run_capture_exn c outfn []


let testAndGenerateHeader (c: C.t) (cc: string) ?(c_flags: string list = []) (hl: testitem list): string =
  hl |> List.map (fun {name; testsrc} ->
      let value = if cTest c cc ~c_flags testsrc then "1" else "0" in
      Format.sprintf "#define %s %s" name value
    )
  |> String.concat "\n"

type gen = {
  test_gen: unit -> testitem list;
  prog_gen: unit -> item list;
}

type 'a modelgen = {
  gen: gen;
  parse: (string, itemval) Hashtbl.t -> 'a;
} 

let parseOutput (items: item list) (out: string): (string, itemval) Hashtbl.t =
  let lines = String.split_on_char '\n' out in
  let values_str = lines |> List.filter_map (fun line -> match String.split_on_char '=' line with
      | ident :: rest -> Some (ident, String.concat "=" rest)
      | _ -> None
    ) |> List.to_seq |> Hashtbl.of_seq in
  items |> List.map (fun {ident; ty; _} ->
      let value_str = Hashtbl.find values_str ident in
      let value = match ty with Int -> IntVal (int_of_string value_str) | Str -> StrVal value_str in
      (ident, value)
    ) |> List.to_seq |> Hashtbl.of_seq

let typeinfoGen = {
  gen = {
    test_gen = (fun () -> MC.allBasicTyps |>
      List.filter_map (fun ty -> let m = MC.metaOfBasicType ty in
        if m.optional then
          Some {
            name = "HAVE_" ^ (identForName false (Option.get m.c_type));
            testsrc = Format.sprintf "%s x; int main() { return 0; }" (Option.get m.c_type)
          }
        else None
      ));
    prog_gen = (fun () ->
      MC.allBasicTyps |> List.concat_map (fun ty -> 
        let m = MC.metaOfBasicType ty in 
        let ident: string = MC.nameOfBasicType ty in
        let specsForSizeAndAlignment ?(sizeof_override: int option = None) ?(use_alignof: bool = false) (ident: string) (pre_cond: string option) (expr: string): item list = 
          let sizeof_expr = (match sizeof_override with Some n -> string_of_int n | None -> Format.sprintf "sizeof(%s)" expr) in
          [
            { ident = "have_" ^ ident ; pre_cond; pre_stmt = None; expr = "1"; ty = Int };
            { ident = "sizeof_" ^ ident ; pre_cond; pre_stmt = None; expr = sizeof_expr; ty = Int };
            { ident = "alignof_" ^ ident ; pre_cond; 
              pre_stmt = (
                if use_alignof then
                  None
                else
                  Some (Format.sprintf "struct s { char c; %s x; };" expr)
              );
              expr = (
                if use_alignof then 
                  Format.sprintf "__alignof(%s)" expr
                else
                  Format.sprintf "(int)(&((struct s*)0)->x)"
              ); ty = Int };
          ] in
        match m.c_type with
          | Some c_type -> 
            let pre_cond = if m.optional then Some ("HAVE_" ^ (identForName false c_type)) else None in
            specsForSizeAndAlignment ~use_alignof:(ty = Void) ident pre_cond c_type
          
          | None -> begin
            match ty with
            | Ptr -> specsForSizeAndAlignment ~use_alignof:true ident None "int*"
            | Str -> specsForSizeAndAlignment ~use_alignof:true ~sizeof_override:(Some 0) ident None "\"str\""
            | Fun -> specsForSizeAndAlignment ~use_alignof:true ident (Some "__GNUC__") "main"
            | _ -> raise (Failure ("Unexpected type without c_type: " ^ ident))
          end
      )
    );
  };
  parse = (fun tbl ->
    MC.allBasicTyps |> List.filter_map (fun ty ->
      let ident = MC.nameOfBasicType ty in
      if H.find tbl ("have_" ^ ident) |> unwrapIntVal > 0 then
        let sizeof = H.find tbl ("sizeof_" ^ ident) |> unwrapIntVal in
        let alignof = H.find tbl ("alignof_" ^ ident) |> unwrapIntVal in
        Some (ty, {MC.sizeof; alignof})
      else None
    ) |> List.to_seq |> H.of_seq
  );
}

let charIsUnsignedGen = {
  gen = {
    test_gen = (fun () -> []);
    prog_gen = (fun () -> [
        { ident = "char_is_unsigned"; pre_cond = None; pre_stmt = None; expr = "((char)0xff) > 0"; ty = Int }
      ]);
  };
  parse = (fun tbl -> H.find tbl "char_is_unsigned" |> unwrapIntVal > 0);
}

let littleEndianGen = {
  gen = {
    test_gen = (fun () -> []);
    prog_gen = (fun () -> [
        { ident = "little_endian"; pre_cond = None; pre_stmt = Some "int e = 0x11223344;"; expr = "(0x44 == *(char*)&e) ? 1 : ((0x11 == *(char*)&e) ? 0 : (exit(1), 0))"; ty = Int }
    ]);
  };
  parse = (fun tbl -> H.find tbl "little_endian" |> unwrapIntVal > 0);
}

let threadIsKeywordGen = {
  gen = {
    test_gen = (fun () -> [{ name = "HAVE___THREAD"; testsrc = "int __thread a; int main() { return 0; }" }]);
    prog_gen = (fun () -> [{ ident = "thread_is_keyword"; pre_cond = None; pre_stmt = None; expr = "HAVE___THREAD"; ty = Int }]);
  };
  parse = (fun tbl -> H.find tbl "thread_is_keyword" |> unwrapIntVal > 0);
}

let builtinVaListGen = {
  gen = {
    test_gen = (fun () -> [{ name = "HAVE___BUILTIN_VA_LIST"; testsrc = "int main() { if (sizeof (__builtin_va_list)) { return 0; } return 0; }" }]);
    prog_gen = (fun () -> [{ ident = "builtin_va_list"; pre_cond = None; pre_stmt = None; expr = "HAVE___BUILTIN_VA_LIST"; ty = Int }]);
  };
  parse = (fun tbl -> H.find tbl "builtin_va_list" |> unwrapIntVal > 0);
}

let alignOfAlignedGen = {
  gen = {
    test_gen = (fun () -> []);
    prog_gen = (fun () -> [{ ident = "alignof_aligned"; pre_cond = Some "__GNUC__"; pre_stmt = Some {|
char __attribute__((aligned)) c;
long double  __attribute__((aligned)) ld;
if (__alignof(c) != __alignof(ld)) {
  fprintf(stderr, "__attribute__((aligned)) has a different effect on different types.  alignments may be computed incorrectly.\n");
}
|}; expr = "__alignof(c)"; ty = Int }]);
  };
  parse = (fun tbl -> H.find tbl "alignof_aligned" |> unwrapIntVal);
}

let stdcVerGen = {
  gen = {
    test_gen = (fun () -> []);
    prog_gen = (fun () -> [{ ident = "stdc_ver"; pre_cond = None; pre_stmt = None; expr = "__STDC_VERSION__"; ty = Int }]);
  };
  parse = (fun tbl -> H.find tbl "stdc_ver" |> unwrapIntVal);
}

let sizeTypeGen = {
  gen = {
    test_gen = (fun () -> []);
    prog_gen = (fun () -> [{ ident = "size_type"; pre_cond = None; pre_stmt = None; expr = "XSTR(__SIZE_TYPE__)"; ty = Str }]);
  };
  parse = (fun tbl -> H.find tbl "size_type" |> unwrapStrVal);
}

let wcharTypeGen = {
  gen = {
    test_gen = (fun () -> []);
    prog_gen = (fun () -> [{ ident = "wchar_type"; pre_cond = None; pre_stmt = None; expr = "XSTR(__WCHAR_TYPE__)"; ty = Str }]);
  };
  parse = (fun tbl -> H.find tbl "wchar_type" |> unwrapStrVal);
}

let gccVerGen = {
  gen = {
    test_gen = (fun () -> []);
    prog_gen = (fun () -> [
        { ident = "gcc_major"; pre_cond = Some "__GNUC__"; pre_stmt = None; expr = "__GNUC__"; ty = Int };
        { ident = "gcc_minor"; pre_cond = Some "__GNUC__"; pre_stmt = None; expr = "__GNUC_MINOR__"; ty = Int };
        { ident = "gcc_patch"; pre_cond = Some "__GNUC__"; pre_stmt = None; expr = "__GNUC_PATCHLEVEL__"; ty = Int };
      ]);
  };
  parse = (fun tbl -> {
      MC.major = H.find tbl "gcc_major" |> unwrapIntVal;
      minor = H.find tbl "gcc_minor" |> unwrapIntVal;
      patch = H.find tbl "gcc_patch" |> unwrapIntVal;
    });
}

let clangVerGen = {
  gen = {
    test_gen = (fun () -> []);
    prog_gen = (fun () -> [
        { ident = "is_clang"; pre_cond = Some "__clang__"; pre_stmt = None; expr = "1"; ty = Int };
        { ident = "clang_major"; pre_cond = Some "__clang__"; pre_stmt = None; expr = "__clang_major__"; ty = Int };
        { ident = "clang_minor"; pre_cond = Some "__clang__"; pre_stmt = None; expr = "__clang_minor__"; ty = Int };
        { ident = "clang_patch"; pre_cond = Some "__clang__"; pre_stmt = None; expr = "__clang_patchlevel__"; ty = Int };
      ]);
  };
  parse = (fun tbl ->
    if H.find tbl "is_clang" |> unwrapIntVal > 0 then
      Some {
        MC.major = H.find tbl "clang_major" |> unwrapIntVal;
        minor = H.find tbl "clang_minor" |> unwrapIntVal;
        patch = H.find tbl "clang_patch" |> unwrapIntVal;
      }
    else None
  );
}

let generateModel (c: C.t) (cc: string) (c_flags: string list): MC.model = 
  let allGens: gen list = [
    typeinfoGen.gen;
    charIsUnsignedGen.gen;
    littleEndianGen.gen;
    threadIsKeywordGen.gen;
    builtinVaListGen.gen;
    alignOfAlignedGen.gen;
    stdcVerGen.gen;
    sizeTypeGen.gen;
    wcharTypeGen.gen;
    gccVerGen.gen;
    clangVerGen.gen;
  ] in
  let all_tests = allGens |> List.concat_map (fun g -> g.test_gen ()) in
  let header = testAndGenerateHeader c cc ~c_flags all_tests in
  let all_items = allGens |> List.concat_map (fun g -> g.prog_gen ()) in
  let prog = { header; items = all_items } in
  let prog_src = generateSource prog in
  let prog_out = compileAndRun c cc ~c_flags prog_src in
  let out = parseOutput all_items prog_out in
  {
    typeinfo = typeinfoGen.parse out;
    misc = {
      char_is_unsigned = charIsUnsignedGen.parse out;
      little_endian = littleEndianGen.parse out;
      thread_is_keyword = threadIsKeywordGen.parse out;
      builtin_va_list = builtinVaListGen.parse out;
      alignof_aligned = alignOfAlignedGen.parse out;
      stdc_ver = stdcVerGen.parse out;
      size_type = sizeTypeGen.parse out;
      wchar_type = wcharTypeGen.parse out;
    };
    gcc_ver = gccVerGen.parse out;
    clang_ver = clangVerGen.parse out;
  }

