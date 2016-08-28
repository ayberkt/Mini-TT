structure Syntax =
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

  fun op** (CL (p, e, rho), v) = raise Todo
    | op** (CLCMP (clos, s), v) = raise Todo
  infix **

  fun makeClos (p : patt) (e : exp) (r : rho) : clos =
    CL (p, e, r)

  fun clCmp (g : clos) (c : name) = CLCMP (g, c)

  fun get (s : name) (ENV []) = raise EmptyEnv
    | get (s : name) (ENV ((s1, u)::us)) =
        if s = s1 then u else get s (ENV us)

  fun eval x y = raise Todo

  infix $$
  fun op$$ ((v1, v2) : value * value) : value =
    case (v1, v2) of
        (LAM f, v) => f ** v
      | (FUN (SCL (ces, rho)), CON (c, v)) =>
          (eval (get c ces) rho) $$ v
      | (FUN (SCL s), NT k) => NT @@ NTFUN (SCL s, k)
      | (NT k, m) => NT @@ APP (k, m)
      | (_, _) => raise NoApplicationRule

  val vfst : value -> value =
    fn PAIR (u1, _) => u1
     | (NT k)       => NT (FST k)
     | _            => raise Fail "fst of non-pair"

  val  vsnd : value -> value =
    fn PAIR (_, u2) => u2
     | NT k         => NT (SND k)
     | _            => raise Fail "snd of non-pair"

end
