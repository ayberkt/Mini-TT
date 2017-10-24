structure Token = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
fun getstring s =
   let
    val n = size s
   in
     substring (s, 1, n-2)
   end

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)

exception Illegal_character of pos

%%
%header (functor ExpLexFun(structure Tokens: Exp_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
any = [@a-zA-Z0-9];

ws = [\ \t];

%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
"λ"      => (Tokens.LAMBDA(!pos, !pos));
"Π"      => (Tokens.PI(!pos, !pos));
"Σ"      => (Tokens.SIGMA(!pos, !pos));
"|"      => (Tokens.BAR(!pos, !pos));
"."      => (Tokens.DOT(!pos, !pos));
":"      => (Tokens.COLON(!pos, !pos));
"rec"    => (Tokens.REC(!pos, !pos));
"="      => (Tokens.EQUAL(!pos, !pos));
"→"      => (Tokens.ARROW(!pos, !pos));
"⟶"    => (Tokens.ARROW(!pos, !pos));
"0"      => (Tokens.ZERO(!pos, !pos));
"1"      => (Tokens.ONE(!pos, !pos));
"2"      => (Tokens.TWO(!pos, !pos));
"_"      => (Tokens.UNDERSCORE(!pos, !pos));
"×"      => (Tokens.TIMES(!pos, !pos));
"fun"    => (Tokens.TIMES(!pos, !pos));
{alpha}{any}* => (Tokens.IDENT(yytext, !pos, !pos));
