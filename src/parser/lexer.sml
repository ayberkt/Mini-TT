structure ExpLrVals =
  ExpLrValsFun(structure Token = LrParser.Token)

structure Lexer =
  ExpLexFun(structure Tokens = ExpLrVals.Tokens)
