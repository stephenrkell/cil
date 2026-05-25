module H = Hashtbl

type basictyp =
  | Short
  | Int
  | Bool
  | Long
  | LongLong
  | Ptr
  | Float
  | Double
  | LongDouble
  | Float16 (* _Float16 *)
  | Float32 (* _Float32 *)
  | Float64 (* _Float64 *)
  | Float128 (* _Float128 *)
  | Float32x (* _Float32x *)
  | Float64x (* _Float64x *)
  | Bf16 (* __bf16 *)
  | Void
  | Fun
  | Str

type basictypemeta = {
  c_type: string option;
  optional: bool;
}

val metaOfBasicType : basictyp -> basictypemeta
val nameOfBasicType : basictyp -> string

type basictypinfo = {
  sizeof: int;
  alignof: int;
}

type compilerver = {
  major: int;
  minor: int;
  patch: int;
}

type modelmisc = {
  char_is_unsigned: bool; (* Whether "char" is unsigned *)
  little_endian: bool; (* whether the machine is little endian *)
  thread_is_keyword: bool; (* whether __thread is a keyword *)
  builtin_va_list: bool; (* whether __builtin_va_list is builtin (gccism) *)
  alignof_aligned: int;   (* Alignment of anything with the "aligned" attribute *)
  stdc_ver: int;
  size_type: string;
  wchar_type: string;
}

type model = {
  typeinfo: (basictyp, basictypinfo) Hashtbl.t;
  misc: modelmisc;
  gcc_ver: compilerver;
  clang_ver: compilerver option;
}

val allBasicTyps : basictyp list

val model_to_yojson : model -> Yojson.Safe.t
val model_of_yojson : Yojson.Safe.t -> model Ppx_deriving_yojson_runtime.error_or

val theModel : model ref

val sizeOf : basictyp -> int
val alignOf : basictyp -> int
val typeExists : basictyp -> bool

val initModelFromMacroDefs : (string, string) H.t -> unit

type modelsrc = 
| MMacroDefs (* model detected from macro defintions in preprocessor output via -Wp,-dD *)
| MFixed of model (* model specified by the user via CIL_MACHINE environment variable *)

val modelSource : modelsrc ref

val gcc10x64Model : model
