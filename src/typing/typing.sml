structure Typechecker : TYPECHECKER =
struct
  open TypeEnv
  open Syntax
  open Result
  infixr 2 >>=
  open Dynamics
  open Readback
  exception Todo

  exception TypeError of string

  val success = return ()
  val succeed = fn _ => success

  fun check k rho gma e0 t0 = raise Todo

  fun validType k rho gma ESET = true
    | validType k rho gma (EPI (p, a, b))  =
        let
          val a_type = validType k rho gma a
          val k_val = genValue k
        in
          case upG gma p (eval a rho) k_val of
            SOME gma' =>
              a_type andalso validType (k+1) (UPVAR (rho, p, k_val)) gma' b
          | NONE => false
        end
    | validType k rho gma (ESIGMA (p, a, b)) =
        let
          val a_type = validType k rho gma a
          val k_val = genValue k
        in
          case upG gma p (eval a rho) k_val of
            SOME gma' =>
              a_type andalso validType (k+1) (UPVAR (rho, p, k_val)) gma' b
          | NONE => false
        end
    | validType k rho gma e = check k rho gma e ESET

  fun checkI k rho gma e0 = raise Todo

  (* `d` is a correct declaration and extends gma to gma' *)
  fun checkD k rho gma d = raise Todo
end
