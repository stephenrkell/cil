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
[@@deriving enumerate, show { with_path = false }, yojson]

let basictyp_of_string (s: string): basictyp Ppx_deriving_yojson_runtime.error_or =
  basictyp_of_yojson (`List [ `String s ])

let basictyp_to_string (t: basictyp): string =
  match basictyp_to_yojson t with
  | `List [ `String s ] -> s
  | _ -> failwith "Unexpected yojson format for basictyp"

type basictypemeta = {
  c_type: string option;
  optional: bool;
}

let metaOfBasicType (t: basictyp): basictypemeta =
  match t with
  | Short -> {c_type = Some "short"; optional = false}
  | Int -> {c_type = Some "int"; optional = false}
  | Bool -> {c_type = Some "_Bool"; optional = false}
  | Long -> {c_type = Some "long"; optional = false}
  | LongLong -> {c_type = Some "long long"; optional = false}
  | Ptr -> {c_type = None; optional = false}
  | Float -> {c_type = Some "float"; optional = false}
  | Double -> {c_type = Some "double"; optional = false}
  | LongDouble -> {c_type = Some "long double"; optional = false}
  | Float16 -> {c_type = Some "_Float16"; optional = true}
  | Float32 -> {c_type = Some "_Float32"; optional = true}
  | Float64 -> {c_type = Some "_Float64"; optional = true}
  | Float128 -> {c_type = Some "_Float128"; optional = true}
  | Float32x -> {c_type = Some "_Float32x"; optional = true}
  | Float64x -> {c_type = Some "_Float64x"; optional = true}
  | Bf16 -> {c_type = Some "__bf16"; optional = true}
  | Void -> {c_type = Some "void"; optional = false}
  | Fun -> {c_type = None; optional = false}
  | Str -> {c_type = None; optional = false}

let nameOfBasicType (t: basictyp): string = 
  show_basictyp t |> String.lowercase_ascii

let allBasicTyps: basictyp list = all_of_basictyp

type basictypinfo = {
  sizeof: int;
  alignof: int;
} [@@deriving show { with_path = false }, yojson]

type compilerver = {
  major: int;
  minor: int;
  patch: int;
} [@@deriving show { with_path = false }, yojson]

type modelmisc = {
  char_is_unsigned: bool; (* Whether "char" is unsigned *)
  little_endian: bool; (* whether the machine is little endian *)
  thread_is_keyword: bool; (* whether __thread is a keyword *)
  builtin_va_list: bool; (* whether __builtin_va_list is builtin (gccism) *)
  alignof_aligned: int;   (* Alignment of anything with the "aligned" attribute *)
  stdc_ver: int;
  size_type: string;
  wchar_type: string;
} [@@deriving show { with_path = false }, yojson]

let hashtbl_to_yojson (k_to_string: 'a -> string) (v_to_yojson: 'b -> Yojson.Safe.t) (h: ('a, 'b) H.t): Yojson.Safe.t =
  let lst = H.fold (fun k v acc -> (k_to_string k, v_to_yojson v) :: acc) h [] in
  `Assoc lst

let rec collect (x: ('a, 'b) result list): ('a list, 'b) result = match x with
  | [] -> Ok []
  | Ok x :: xs -> begin
      match collect xs with
      | Ok ys -> Ok (x :: ys)
      | Error e -> Error e
  end
  | Error e :: xs -> Error e 

let hashtbl_of_yojson (k_of_string: string -> 'a Ppx_deriving_yojson_runtime.error_or) (v_of_yojson: Yojson.Safe.t -> 'b Ppx_deriving_yojson_runtime.error_or) (json: Yojson.Safe.t): ('a, 'b) H.t Ppx_deriving_yojson_runtime.error_or =
  match json with
  | `Assoc lst -> begin
    match lst |> List.map (fun (k, v) ->
      match (k_of_string k, v_of_yojson v) with
      | (Ok k, Ok v) -> Ok (k, v)
      | (Error e, _) -> Error e
      | (_, Error e) -> Error e
    ) |> collect with 
    Ok lst -> Ok (lst |> List.to_seq |> H.of_seq)
  | Error e -> Error e
    end
  | _ -> Error "Expected an object for hashtbl"

type model = {
  typeinfo: (basictyp, basictypinfo) H.t [@to_yojson hashtbl_to_yojson basictyp_to_string basictypinfo_to_yojson] [@of_yojson hashtbl_of_yojson basictyp_of_string basictypinfo_of_yojson];
  misc: modelmisc;
  gcc_ver: compilerver;
  clang_ver: compilerver option;
} [@@deriving yojson]
