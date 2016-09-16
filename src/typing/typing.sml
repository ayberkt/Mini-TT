structure Typechecker : TYPECHECKER =
struct
  open TypeEnv
  exception Todo

  fun checkT k rho gma e0 = raise Todo

  fun check k rho gma e0 t0 = raise Todo

  fun checkI k rho gma e0 = raise Todo

  (* `d` is a correct declaration and extends gma to gma' *)
  fun checkD k rho gma d = raise Todo
end
