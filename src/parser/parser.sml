structure Parser =
  Join(structure LrParser = LrParser
       structure ParserData = ExpLrVals.ParserData
       structure Lex = Lexer)
