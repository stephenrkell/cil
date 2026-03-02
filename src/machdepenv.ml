open ModelCommon
module R = Str
module L = List
module H = Hashtbl

let preparse (s:string) : (string, string list) H.t =
  let specTable = H.create 32 in
  let commaRegexp = R.regexp "," in
  let spaceRegexp = R.regexp "[ \t]+" in
  let specRegexp = R.regexp "^\\([a-zA-Z_0-9]+\\)[ \t]*=\\(.*\\)$" in
  let specs = R.split spaceRegexp s in
  let addSpec spec =
    if R.string_match specRegexp spec 0 then begin
      let name = R.matched_group 1 spec in
      let value = R.matched_group 2 spec in
      H.add specTable name (R.split commaRegexp value)
    end
    else
      raise (Failure ("invalid specification string " ^ spec))
  in
  L.iter addSpec specs;
  specTable

let errorWrap name f =
  try
    f name
  with Not_found -> raise (Failure (name ^ " not specified"))
  | _ -> raise (Failure ("invalid format for " ^ name))

let getNthString n specTable name =
  let l = H.find specTable name in
  L.nth l n

let getNthInt n specTable name =
  errorWrap name (fun name -> int_of_string (getNthString n specTable name))

let getNthBool n specTable name =
  errorWrap name (fun name -> bool_of_string (getNthString n specTable name))

let getBool = getNthBool 0
let getInt = getNthInt 0
let getSizeof = getNthInt 0
let getAlignof = getNthInt 1

let respace = Str.global_replace (Str.regexp "_") " "

let modelParse (s:string) : model =
  let entries =
    try
      preparse s
    with Failure msg -> raise (Failure msg)
    | _ -> raise (Failure "invalid machine specification") in
  let typeinfo = H.create 16 in
  let addTypeInfo (t: basictyp) (spec: string) =
    H.add typeinfo t {
      sizeof = getSizeof entries spec;
      alignof = getAlignof entries spec;
    }
  in
    addTypeInfo Short "short";
    addTypeInfo Int "int";
    addTypeInfo Bool "bool";
    addTypeInfo Long "long";
    addTypeInfo LongLong "long_long";
    addTypeInfo Ptr "pointer";
    addTypeInfo Float "float";
    addTypeInfo Double "double";
    addTypeInfo LongDouble "long_double";
    addTypeInfo Float16 "float16";
    addTypeInfo Float32x "float32x";
    addTypeInfo Float64x "float64x";
    addTypeInfo Float128 "float128";
    addTypeInfo Bf16 "__bf16";
    addTypeInfo Void "void";
    addTypeInfo Fun "fun";

    H.add typeinfo Str {
      sizeof = 0;
      alignof = getInt entries "alignof_string";
    };

    if getBool entries "have_float16" then
      addTypeInfo Float16 "float16";

    {
      typeinfo;
      misc = {
        char_is_unsigned = not (getBool entries "char_signed");
        little_endian = not (getBool entries "big_endian");
        thread_is_keyword = getBool entries "__thread_is_keyword";
        builtin_va_list = getBool entries "__builtin_va_list";
        alignof_aligned = getInt entries "max_alignment";
        stdc_ver = 0; (* TODO *)
        size_type = respace (getNthString 0 entries "size_t");
        wchar_type = respace (getNthString 0 entries "wchar_t");
      };
      gcc_ver = {
        major = 0;
        minor = 0;
        patch = 0;
      }; (* TODO *)
      clang_ver = None; (* TODO *)
    }
