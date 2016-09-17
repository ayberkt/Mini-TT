structure Typechecker : TYPECHECKER =
struct
  open TypeEnv
  open Syntax
  open Result
  infixr 2 >>=
  open Dynamics
  open Readback
  exception Todo

  val success = return ()
  val succeed = fn _ => success

  fun check k rho gma e0 t0 = raise Todo

  fun checkT k rho gma ESET = success
    | checkT k rho gma (EPI (p, a, b))  =
        checkT k rho gma a                  >>= (fn _    =>
        upG gma p (eval a rho) (genValue k) >>= (fn gma1 =>
        checkT (k+1) (UPVAR (rho, p, genValue k)) gma1 b))
    | checkT k rho gma (ESIGMA (p, a, b)) =
        checkT k rho gma a                  >>= (fn _ =>
        upG gma p (eval a rho) (genValue k) >>= (fn gma1 =>
        checkT (k+1) (UPVAR (rho, p, genValue k)) gma1 b))
    | checkT k rho gma e =
        check k rho gma e ESET >>= succeed

  fun checkI k rho gma e0 = raise Todo

  (* `d` is a correct declaration and extends gma to gma' *)
  fun checkD k rho gma d = raise Todo
end
