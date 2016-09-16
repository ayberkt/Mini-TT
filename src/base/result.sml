structure Result : MONAD =
struct
  datatype 'a result = SUCCESS of 'a | FAIL of S.name

  fun >>= (SUCCESS x, k) = k x
    | >>= (FAIL s, _) = FAIL s

  val return = fn x => SUCCESS x
  val fail = fn x => FAIL x
end
