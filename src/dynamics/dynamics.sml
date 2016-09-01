structure Dynamics =
struct

  open Syntax
  infix 5 **

  fun @@ (f, x) = f x
  infixr 1 @@

  exception Todo

  fun patProj x y z = raise Todo

  infix 5 $$
  fun eval e0 rho =
    case e0 of
        ESET               => SET
      | EDEC   (d, e)      => eval e @@ UPDEC (rho, d)
      | ELAM   (p, e)      => LAM @@ makeClos p e rho
      | EAPP   (e1, e2)    => (eval e1 rho) $$ (eval e2 rho)
      | EPAIR  (e1, e2)    => PAIR (eval e1 rho, eval e2 rho)
      | ECON   (c, e1)     => CON (c, eval e1 rho)
      | EPI    (p, a, b)   => PI (eval a rho, makeClos p b rho)
      | ESIGMA (p, a, b)   => SIGMA (eval a rho, makeClos p b rho)
      | EONE               => ONE
      | EUNIT              => UNIT
      | EFST e             => vfst @@ eval e rho
      | ESND e             => vsnd @@ eval e rho
      | EVAR x             => getRho rho x
      | ESUM cas           => SUM @@ SCL (cas, rho)
      | EFUN ces           => FUN @@ SCL (ces, rho)
      | _                  => raise Fail "Something went wrong in eval!"
  and getRho (UPVAR (r, p, v)) x =
        if inPat x p then patProj p x v else getRho r x
    | getRho (UPDEC (r, DEF (p, _, e))) x =
        if inPat x p then patProj p x (eval e r) else getRho r x
    | getRho (UPDEC (r, DREC (p, q, e))) x =
        if inPat x p
        then patProj p x @@ eval e @@ UPDEC (r, (DREC (p, q, e)))
        else getRho r x
    | getRho RNIL _ = raise Fail "getRho"
  and op$$ ((v1, v2) : value * value) : value =
    case (v1, v2) of
        (LAM f, v) => f ** v
      | (FUN (SCL (ces, rho)), CON (c, v)) =>
          (eval (get c ces) rho) $$ v
      | (FUN (SCL s), NT k) => NT @@ NTFUN (SCL s, k)
      | (NT k, m) => NT @@ APP (k, m)
      | (_, _) => raise NoApplicationRule

  fun lRho RNIL = 0
    | lRho (UPVAR (rho, _, _)) = lRho rho + 1
    | lRho (UPDEC (rho, _))    = lRho rho

end
