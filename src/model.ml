open ModelCommon

module H = Hashtbl
module E = Errormsg

let cver (major: int) (minor: int) (patch: int): compilerver = { major; minor; patch }


let strValOfMacro (md: (string, string) H.t) (t: string): string = H.find md t

let intValOfMacro (md: (string, string) H.t) (t: string): int =
  let remove_parens (s: string): string =
    if String.length s >= 2 && String.get s 0 = '(' && String.get s (String.length s - 1) = ')' then
      String.sub s 1 (String.length s - 2)
    else s in
      strValOfMacro md t |> remove_parens |> int_of_string

let parseStdcVer (s: string): int =
  try int_of_string (String.sub s 0 (String.length s - 1)) with Failure _ -> ignore (E.warn "Failed to parse __STDC_VERSION__: %s, assuming 199711L\n" s); 199711

let macroExists (md: (string, string) H.t) (t: string): bool = H.mem md t

let typeInfoFromMacroDefs (md: (string, string) H.t) (t: basictyp) (gcc_ver: compilerver) (clang_ver: compilerver option): basictypinfo option =
  let is_i386 = macroExists md "__i386__" in
  let is_x86_64 = macroExists md "__x86_64__" in
  let is_aarch64 = macroExists md "__aarch64__" in
  let is_arm = macroExists md "__arm__" in
  let is_riscv = macroExists md "__riscv__" in
  let has_sse2 = macroExists md "__SSE2__" in
  let is_clang = macroExists md "__clang__" in
  let alignof_generic (t: int): int = if is_i386 && t < 16 then min 4 t else t in
  let essential (sizeof_macro: string) (alignof_f: int -> int): basictypinfo option = 
    let sizeof_t = intValOfMacro md sizeof_macro in
    Some { sizeof = sizeof_t; alignof = alignof_f sizeof_t } in
  let fixed (sizeof: int) (alignof: int): basictypinfo option = Some { sizeof; alignof } in
  let float (bits: int): basictypinfo option =
    let ident = "__FLT" ^ string_of_int bits ^ "_MAX__" in
    if not (macroExists md ident) then None else match bits with
    | 16 -> Some { sizeof = 2; alignof = alignof_generic 2 }
    | 32 -> Some { sizeof = 4; alignof = alignof_generic 4 }
    | 64 -> Some { sizeof = 8; alignof = alignof_generic 8 }
    | 128 -> Some { sizeof = 16; alignof = alignof_generic 16 }
    | _ -> E.s (E.bug "Unexpected float type with %d bits" bits) in
  let float_info ?(x: bool = false) (ident: int): (int * int * int) option =
    let s = string_of_int ident ^ (if x then "X" else "") in
    if not (macroExists md ("__FLT" ^ s ^ "_MANT_DIG__")) then
      None
    else
      let mant_dig = intValOfMacro md ("__FLT" ^ s ^ "_MANT_DIG__") in
      let max_exp = intValOfMacro md ("__FLT" ^ s ^ "_MAX_EXP__") in
      let min_exp = intValOfMacro md ("__FLT" ^ s ^ "_MIN_EXP__") in
      Some (mant_dig, max_exp, min_exp) in
  let floatx (ident: int): basictypinfo option =
    match float_info ~x:true ident with
    | Some x ->
      let ret =
        if Some x = float_info 32 then { sizeof = 4; alignof = alignof_generic 4 } else
        if Some x = float_info 64 then { sizeof = 8; alignof = alignof_generic 8 } else
        if Some x = float_info 128 then { sizeof = 16; alignof = alignof_generic 16 } else
        if (is_i386 || is_x86_64) && x = (64, 16384, -16381) then
          (* x86 extended precision float *)
          let sz = intValOfMacro md "__SIZEOF_FLOAT80__" in { sizeof = sz; alignof = alignof_generic sz }
        else
          E.s (E.bug "Failed to detect _Float%dx type from macros" ident)
        in
      Some ret
    | None -> None in
  let clang_bf16 (): basictypinfo option =
    if is_clang then
      let clang_ver = Option.get clang_ver in
      if ((clang_ver >= cver 11 0 0) && (is_aarch64 || is_arm)) ||
        (* __bf16 is introduced to AArch64 and ARM in Clang 11:
         * https://releases.llvm.org/11.0.1/docs/ReleaseNotes.html#changes-to-the-aarch64-backend
         *)
        ((clang_ver >= cver 15 0 0) && has_sse2) ||
        (* and it is later introduced to SSE2-capable x86 in Clang 15:
         * https://github.com/llvm/llvm-project/commit/e4888a37d36780872d685c68ef8b26b2e14d6d39
         *)
        ((clang_ver >= cver 18 1 0) && is_riscv)
        (* and it is later introduced to RISC-V in Clang 18.1:
         * https://github.com/llvm/llvm-project/commit/a5791bfef4e4bcc159ef9bf40d88262e5f409766
         *)
        then Some { sizeof = 2; alignof = 2 }
      else None
    else if ((gcc_ver >= cver 10 0 0) && (is_aarch64 || is_arm)) ||
      (* __bf16 is introduced to AArch64 and ARM in GCC 10
       * https://gcc.gnu.org/gcc-10/changes.html#aarch64
       *)
      ((gcc_ver >= cver 13 0 0) && has_sse2) ||
      (* and it is later introduced to SSE2-capable x86 in GCC 13
       * https://gcc.gnu.org/gcc-13/changes.html#x86
       *)
      ((gcc_ver >= cver 14 0 0) && (is_i386 || is_x86_64))
       (* it is supported on x86 independent of SSE2 in GCC 14
        * https://gcc.gnu.org/gcc-14/changes.html#x86
        *)
      then Some { sizeof = 2; alignof = 2 }
    else None in
  match t with
  | Short -> essential "__SIZEOF_SHORT__" alignof_generic
  | Int -> essential "__SIZEOF_INT__" alignof_generic
  | Bool -> fixed 1 1
  | Long -> essential "__SIZEOF_LONG__" alignof_generic
  | LongLong -> essential "__SIZEOF_LONG_LONG__" alignof_generic
  | Ptr -> essential "__SIZEOF_POINTER__" alignof_generic
  | Float -> essential "__SIZEOF_FLOAT__" alignof_generic
  | Double -> essential "__SIZEOF_DOUBLE__" alignof_generic
  | LongDouble -> essential "__SIZEOF_LONG_DOUBLE__" alignof_generic
  | Float16 -> float 16
  | Float32 -> float 32
  | Float64 -> float 64
  | Float128 -> float 128
  | Float32x -> floatx 32
  | Float64x -> floatx 64
  | Bf16 -> clang_bf16 ()
  | Void -> fixed 1 1
  | Fun -> fixed 1 (if is_clang (* clang uses 4-byte alignment for function pointers *) || not (is_i386 || is_x86_64) then 4 else 1)
  | Str -> fixed 0 1

let modelMiscFromMacroDefs (md: (string, string) H.t) (v: compilerver): modelmisc = {
  char_is_unsigned = macroExists md "__CHAR_UNSIGNED__";
  little_endian = (let o = strValOfMacro md "__BYTE_ORDER__" in o = "1234" || o = "__ORDER_LITTLE_ENDIAN__");
  thread_is_keyword = (v >= cver 3 3 0); (* TODO: verify this *)
  builtin_va_list = (v >= cver 2 96 0); (* TODO: verify this *)
  alignof_aligned = intValOfMacro md "__BIGGEST_ALIGNMENT__";
  stdc_ver = parseStdcVer (strValOfMacro md "__STDC_VERSION__");
  size_type = strValOfMacro md "__SIZE_TYPE__";
  wchar_type = strValOfMacro md "__WCHAR_TYPE__";
}

let modelFromMacroDefs (md: (string, string) H.t): model =
  if not (macroExists md "__STDC__") then
    E.s (E.error "Macro definitions not detected. Have you called cc with -Wp,-dD?\n");
  let gcc_ver = cver (intValOfMacro md "__GNUC__") (intValOfMacro md "__GNUC_MINOR__") (intValOfMacro md "__GNUC_PATCHLEVEL__") in
  let clang_ver = if macroExists md "__clang__" then Some (cver (intValOfMacro md "__clang_major__") (intValOfMacro md "__clang_minor__") (intValOfMacro md "__clang_patchlevel__")) else None in
  let misc = modelMiscFromMacroDefs md gcc_ver in
  let typeinfo = H.create 16 in
  let all_typeinfos = List.map (fun t -> (t, typeInfoFromMacroDefs md t gcc_ver clang_ver)) allBasicTyps in
  List.iter (fun (t, info) -> match info with Some x -> H.add typeinfo t x | None -> ()) all_typeinfos;
  { typeinfo; misc; gcc_ver; clang_ver }

let uninitModel: model = {
  typeinfo = H.create 0;
  misc = {
    char_is_unsigned = false;
    little_endian = false;
    thread_is_keyword = false;
    builtin_va_list = false;
    alignof_aligned = 0;
    stdc_ver = 0;
    size_type = "";
    wchar_type = "";
  };
  gcc_ver = cver 0 0 0;
  clang_ver = None;
}

let gcc10x64Model: model = {
  typeinfo = (let h = H.create 16 in
    let add t sizeof alignof = H.add h t { sizeof; alignof } in
    add Str 0 1;
    add Int 4 4;
    add Float32x 8 8;
    add Short 2 2;
    add Float64x 16 16;
    add LongLong 8 8;
    add Ptr 8 8;
    add Float128 16 16;
    add Void 1 1;
    add Long 8 8;
    add Double 8 8;
    add LongDouble 16 16;
    add Fun 1 1;
    add Bool 1 1;
    add Float 4 4;
    h);

  misc = {
    char_is_unsigned = false;
    little_endian = true;
    thread_is_keyword = true;
    builtin_va_list = true;
    alignof_aligned = 16;
    stdc_ver = 201710;
    size_type = "long unsigned int";
    wchar_type = "int";
  };

  gcc_ver = cver 10 5 0;
  clang_ver = None;
}

let theModel: model ref = ref uninitModel

let sizeOf (k: basictyp): int = (H.find !theModel.typeinfo k).sizeof
let alignOf (k: basictyp): int = (H.find !theModel.typeinfo k).alignof
let typeExists (k: basictyp): bool = H.mem !theModel.typeinfo k

type modelsrc = 
| MMacroDefs (* model detected from macro defintions in preprocessor output via -Wp,-dD *)
| MFixed of model (* model specified by the user via CIL_MACHINE environment variable *)

let modelSource : modelsrc ref = ref MMacroDefs

let initModelFromMacroDefs (md: (string, string) H.t): unit =
  theModel := modelFromMacroDefs md
