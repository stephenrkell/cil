module H = Hashtbl
open ModelCommon

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
