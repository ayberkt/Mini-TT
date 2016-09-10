structure Readback : READBACK =
struct
  open NormalForms
  open Syntax
  infixr 5 **

  exception Todo

  exception ReadbackError

  fun @@ (f, x) = f x
  infixr 1 @@

  fun meet x y = (x, y)

  (* This should probably be in some other module. *)
  fun genValue (x : int) : value = raise Todo

  fun rbValue (k : int) (v0 : value) =
    case v0 of
      LAM f       => NLAM (k, rbValue (k + 1) @@ f ** (genValue k))
    | PAIR (u, v) => NPAIR (rbValue k u, rbValue k v)
    | CON (c, v)  => NCON (c, rbValue k v)
    | UNIT => NUNIT
    | SET  => NSET
    | ONE  => NONE
    | PI (t, g) => NPI (rbValue k t, k, rbValue (k+1) @@ g ** genValue k)
    | SIGMA (t, g) => NSIGMA (rbValue k t, k, rbValue (k+1) @@ g ** genValue k)
    | FUN (SCL (s, rho)) => NFUN @@ NSCL @@ meet s (rbRho k rho)
    | SUM (SCL (s, rho)) => NSUM @@ NSCL @@ meet s (rbRho k rho)
    | _                 => raise Todo
  and rbNeut i k0 =
    case k0 of
      GEN j                    => NGEN j
    | APP (k, m)               => NAPP @@ meet (rbNeut i k) (rbValue i m)
    | FST k                    => NFST (rbNeut i k)
    | SND k                    => NSND (rbNeut i k)
    | NTFUN (SCL (s, rho), k)  => NNTFUN (NSCL (s, rbRho i rho), rbNeut i k)
  and rbRho _ RNIL = NRNIL
    | rbRho i (UPVAR (rho, p, v)) = NUPVAR @@ (rbRho i rho, p, rbValue i v)
    | rbRho i (UPDEC (rho, d))    = NUPDEC @@ meet (rbRho i rho) d

end
