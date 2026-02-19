module C = Configurator.V1

let c_flags = ref []

let base_code = {|
int main() { return 0; }
|}

let has_header_code f =
  Format.sprintf {|
#include <%s>
int main() { return 0; } // Just so that dune-configurator linking works
  |} f

let c_test (c: C.t) (cc: string) ?(c_flags: string list = []) (f: string): bool =
  let fn, ch = Filename.open_temp_file "cil-machdep-cfg" ".c" in
  output_string ch f;
  close_out ch;
  C.Process.run_ok c cc (["-x"; "c"; "-o"; Filename.null; fn] @ c_flags)

let has_header c cc f =
  c_test c cc ~c_flags:!c_flags (has_header_code f)

let builtin_va_list_code = {|
int
main (void)
{
if (sizeof (__builtin_va_list))
	 return 0;
  ;
  return 0;
}
|}

let thread_is_keyword_code = {|
int main(int __thread) { return 0; }
|}

let have_type (type_name: string) = Printf.sprintf {|
int main() {
  %s x;
  return 0;
}
|} type_name

let cil_check_integer_type_type_code t1 t2 =
  Format.sprintf {|
#include <stddef.h>
#include <wchar.h>
#include <stdint.h>
#if __APPLE__
  // C11 7.28 defines these to be the same as uint_least16_t and uint_least32_t.
  // The standard mandates a uchar.h file to contain these typedefs, but Mac does
  // not have that header file
  typedef uint_least16_t char16_t;
  typedef uint_least32_t char32_t;
#else
  #include <uchar.h>
#endif
/* We define a prototype with one type and the function with
   another type.  This will result in compilation error
   unless the types are really identical. */
%s foo(%s x);
%s foo(%s x) { return x; }

int main() { return 0; } // Just so that dune-configurator linking works
  |} t2 t2 t1 t1

exception FoundType of string

let cil_check_integer_type_type c cc t1 t2 =
  if c_test c cc ~c_flags:!c_flags (cil_check_integer_type_type_code t1 t2) then
    raise (FoundType t2)

let cil_check_integer_type_signs c cc t1 t2 =
  cil_check_integer_type_type c cc t1 t2;
  cil_check_integer_type_type c cc t1 ("unsigned " ^ t2)

let cil_check_integer_type c cc t1 =
  try
    cil_check_integer_type_signs c cc t1 "int";
    cil_check_integer_type_signs c cc t1 "long";
    cil_check_integer_type_signs c cc t1 "long long";
    cil_check_integer_type_signs c cc t1 "short";
    cil_check_integer_type_signs c cc t1 "char";
    failwith ("cannot find definition of " ^ t1)
  with FoundType t2 ->
    t2

let () =
  let fname = ref "machdep-config.h" in
  let cc = ref "" in
  let args = Arg.[
      ("--cc", Set_string cc, "");
      ("-m", String (fun s ->
          c_flags := ("-m" ^ s) :: !c_flags;
          fname := "machdep" ^ s ^ "-config.h";
        ), "");
    ]
  in
  C.main ~name:"machdep" ~args (fun c ->
      if c_test c !cc ~c_flags:!c_flags base_code then (
        let have_builtin_va_list = c_test c !cc ~c_flags:!c_flags builtin_va_list_code in
        let thread_is_keyword = not @@ c_test c !cc ~c_flags:!c_flags thread_is_keyword_code in
        let have_float128 = c_test c !cc ~c_flags:!c_flags (have_type "_Float128") in
        let have_float64 = c_test c !cc ~c_flags:!c_flags (have_type "_Float64") in
        let have_float64x = c_test c !cc ~c_flags:!c_flags (have_type "_Float64x") in
        let have_float32 = c_test c !cc ~c_flags:!c_flags (have_type "_Float32") in
        let have_float32x = c_test c !cc ~c_flags:!c_flags (have_type "_Float32x") in
        let have_float16 = c_test c !cc ~c_flags:!c_flags (have_type "_Float16") in
        let have_bf16 = c_test c !cc ~c_flags:!c_flags (have_type "__bf16") in

        C.C_define.gen_header_file c ~fname:!fname [
          ("HAVE_STDLIB_H", Switch (has_header c !cc "stdlib.h"));
          ("HAVE_WCHAR_H", Switch (has_header c !cc "wchar.h"));
          ("HAVE_STDBOOL_H", Switch (has_header c !cc "stdbool.h"));
          ("HAVE_INTTYPES_H", Switch (has_header c !cc "inttypes.h"));
          ("HAVE_STDINT_H", Switch (has_header c !cc "stdint.h"));

          ("HAVE_BUILTIN_VA_LIST_DEF", Switch have_builtin_va_list);
          ("THREAD_IS_KEYWORD_DEF", Switch thread_is_keyword);
          ("HAVE_FLOAT128_DEF", Switch have_float128);
          ("HAVE_FLOAT64_DEF", Switch have_float64);
          ("HAVE_FLOAT64X_DEF", Switch have_float64x);
          ("HAVE_FLOAT32_DEF", Switch have_float32);
          ("HAVE_FLOAT32X_DEF", Switch have_float32x);
          ("HAVE_FLOAT16_DEF", Switch have_float16);
          ("HAVE_BF16_DEF", Switch have_bf16);

          ("TYPE_SIZE_T", String (cil_check_integer_type c !cc "size_t"));
          ("TYPE_WCHAR_T", String (cil_check_integer_type c !cc "wchar_t"));
          ("TYPE_CHAR16_T", String (cil_check_integer_type c !cc "char16_t"));
          ("TYPE_CHAR32_T", String (cil_check_integer_type c !cc "char32_t"));
        ]
      )
      else (
        C.C_define.gen_header_file c ~fname:!fname []
      )
    )
