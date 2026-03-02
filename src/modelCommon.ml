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
[@@deriving enumerate]


let allBasicTyps: basictyp list = all_of_basictyp

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
  typeinfo: (basictyp, basictypinfo) H.t;
  misc: modelmisc;
  gcc_ver: compilerver;
  clang_ver: compilerver option;
}
