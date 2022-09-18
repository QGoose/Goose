type prog = {
  version : nnint * nnint;
  body : stmt list;
}

and stmt =
  | Qreg of id * nnint
  | Creg of id * nnint
  | Gate of id * id list * id list * gop list
  | Qop of qop
  | If of id * nnint * qop
  | Opaque of id * id list * id list
  | Barrier of arg list

and gop =
  | G_uop of uop
  | G_barrier of id list

and qop =
  | Q_uop of uop
  | Q_measure of arg * arg
  | Q_reset of arg

and uop =
  | U of expr list * arg
  | CX of arg * arg
  | App of id * expr list * arg list

and id = Id of string

and nnint = Nnint of int

and arg =
  | A_id of id * nnint option

and expr =
  | E_cst of float
  | E_int of nnint
  | E_Pi
  | E_id of id
  | E_bop of binaryop * expr * expr
  | E_uop of unaryop * expr

and binaryop =
  | ADD
  | MUL
  | SUB
  | DIV
  | POW

and unaryop =
  | SIN
  | COS
  | TAN
  | EXP
  | LN
  | SQRT