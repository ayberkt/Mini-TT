structure Syntax : SYNTAX =
struct
  type name = string

  fun @@ (f, x) = f x
  infixr 1 @@

  exception Todo

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
    | NT of neu
  (* Function closures. *)
  and clos =
      CL of patt * exp * rho
    | CLCMP of clos * name
  and rho =
      RNIL
    | UPVAR of rho * patt * value
    | UPDEC of rho * decl
  and neu =
      GEN of int
    | APP of neu * value
    | FST of neu
    | SND of neu
    | NTFUN of sclos * neu
  and sclos = SCL of branch * rho

  fun ** (CL (p, e, rho), v) = raise Todo
    | ** (CLCMP (clos, s), v) = raise Todo
  infix **

  fun makeClos p e r = CL (p, e, r)

  fun clCmp g c = CLCMP (g, c)

  fun get s (ENV []) = raise EmptyEnv
    | get s (ENV ((s1, u)::us)) =
        if s = s1 then u else get s (ENV us)

  val vfst =
    fn PAIR (u1, _) => u1
     | (NT k)       => NT (FST k)
     | _            => raise Fail "fst of non-pair"

  val vsnd =
    fn PAIR (_, u2) => u2
     | NT k         => NT (SND k)
     | _            => raise Fail "snd of non-pair"

  (* Check if a given name `x` occurs in a pattern. *)
  fun inPat x (PVAR y) = x = y
    | inPat x (PPAIR (p1, p2)) = inPat x p1 orelse inPat x p2
    | inPat _ PUNIT = false

  fun patProj x y z = raise Todo


end
