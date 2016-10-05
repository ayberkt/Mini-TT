structure Typechecker : TYPECHECKER =
struct
  open TypeEnv
  open Syntax
  infixr 2 >>=
  open Dynamics
  open Readback
  exception Todo

  exception TypeError of string

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

  local
    fun extPiG (PI (t, g)) = (t, g)
      | extPiG u = raise Fail "problem in extPiG"
    fun extSigmaG (SIGMA (t, g)) = (t, g)
      | extSigmaG u = raise Fail ("problem in extSigG")
  in
    fun checkI k rho gma e0 =
      case e0 of
        EVAR x => lookupG x gma
      | EAPP (e1, e2) => raise Todo
  end

  (* `d` is a correct declaration and extends gma to gma' *)
  fun checkD k rho gma =
    fn DEF (p, e1, e2) =>
      let
        val e1_type = validType k rho gma e1
        val t = eval e1 rho
      in
        case check k rho gma e2 t of
          SOME t => upG gma p t (genValue k)
        | NONE => NONE
      end
     | DREC (p, e1, e2) =>
        let
          val e1_type = validType k rho gma e1
        in
          raise Todo
        end

end
