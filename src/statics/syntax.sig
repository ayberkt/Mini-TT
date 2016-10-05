signature SYNTAX = sig

  type name

  exception EmptyEnv
  exception NoApplicationRule

  datatype exp =
      ELAM of patt * exp
    | ESET
    | EPI of patt * exp * exp
    | ESIGMA of patt * exp * exp
    | EONE
    | EUNIT
    | EPAIR of exp * exp
    | ECON of name * exp
    | ESUM of branch
    | EFUN of branch
    | EFST of exp
    | ESND of exp
    | EAPP of exp * exp
    | EVAR of name
    | EVOID
    | EDEC of decl * exp
  and decl =
      DEF  of patt * exp * exp
    | DREC of patt * exp * exp
  and patt =
      PPAIR of patt * patt
    | PUNIT
    | PVAR of name
  and branch = ENV of (name * exp) list

  datatype value =
      LAM of clos
    | PAIR of value * value
    | CON of name * value
    | UNIT
    | SET
    | PI of value * clos
    | SIGMA of value * clos
    | ONE
    | FUN of sclos
    | SUM of sclos
    | NT of neut
  (* Function closures. *)
  and clos =
      CL of patt * exp * rho
    | CLCMP of clos * name
  and rho =
      RNIL
    | UPVAR of rho * patt * value
    | UPDEC of rho * decl
  and neut =
      GEN of int
    | APP of neut * value
    | FST of neut
    | SND of neut
    | NTFUN of sclos * neut
  withtype sclos = branch * rho

  val ** : clos * value -> value

  val makeClos : patt -> exp -> rho -> clos

  val clCmp : clos -> name -> clos

  val get : name -> branch -> exp

  val vfst : value -> value
  val vsnd : value -> value

  val inPat : name -> patt -> bool

end
