type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val numComment = ref 0
val stringToken = ref ""

fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in if !numComment <> 0 then (ErrorMsg.error pos "unclosed comment") else (); Tokens.EOF(pos,pos) end

%%
%header (functor TigerLexFun (structure Tokens: Tiger_TOKENS));

letter=[a-zA-Z];
digit=[0-9];
quote=[\"];
ascii={digit}{3};
escapechar=[nt\"\\]|{ascii};
whitespace=[\t\ ]+;
newline=[\n\r]+;
formatchar={whitespace}|{newline};

%s COMMENT STRING ESCAPE FORMAT;

%%
<INITIAL,COMMENT>{newline}     => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL,COMMENT>{whitespace}  => (continue());

<INITIAL>"type"         => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"var"          => (Tokens.VAR(yypos,yypos+3));
<INITIAL>"break"        => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>"of"           => (Tokens.OF(yypos,yypos+2));
<INITIAL>"end"          => (Tokens.END(yypos,yypos+3));
<INITIAL>"in"           => (Tokens.IN(yypos,yypos+2));
<INITIAL>"nil"          => (Tokens.NIL(yypos,yypos+3));
<INITIAL>"let"          => (Tokens.LET(yypos,yypos+3));
<INITIAL>"do"           => (Tokens.DO(yypos,yypos+2));
<INITIAL>"to"           => (Tokens.TO(yypos,yypos+2));
<INITIAL>"for"          => (Tokens.FOR(yypos,yypos+3));
<INITIAL>"while"        => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>"else"         => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>"if"           => (Tokens.IF(yypos,yypos+2));
<INITIAL>"then"         => (Tokens.THEN(yypos,yypos+4));
<INITIAL>"array"        => (Tokens.ARRAY(yypos,yypos+5));

<INITIAL>":="           => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>"|"            => (Tokens.OR(yypos,yypos+1));
<INITIAL>"&"            => (Tokens.AND(yypos,yypos+1));
<INITIAL>">="           => (Tokens.GE(yypos,yypos+2));
<INITIAL>">"            => (Tokens.GT(yypos,yypos+2));
<INITIAL>"<="           => (Tokens.LE(yypos,yypos+2));
<INITIAL>"<"            => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<>"           => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"="            => (Tokens.EQ(yypos,yypos+1));

<INITIAL>"/"            => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"*"            => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"+"            => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"            => (Tokens.MINUS(yypos,yypos+1));
            
<INITIAL>"{"            => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"            => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"["            => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"            => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"("            => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"            => (Tokens.RPAREN(yypos,yypos+1));
                    
<INITIAL>";"            => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>","            => (Tokens.COMMA(yypos,yypos+1));

        

<INITIAL>{digit}+       => (Tokens.INT(valOf(Int.fromString yytext),yypos,yypos+size yytext));

<INITIAL>{letter}({letter}|{digit}|"_")* => (Tokens.ID(yytext,yypos,yypos+size(yytext)));

<INITIAL,COMMENT>"/*"   => ( numComment := !numComment+1; YYBEGIN COMMENT; continue());
<COMMENT>"*/"           => (numComment := !numComment-1;
                            if (!numComment=0)
                            then (YYBEGIN INITIAL; continue())
                            else (continue()));                   
<COMMENT>.              => (continue());

<INITIAL>. => (ErrorMsg.error yypos ("illegal character:" ^ "|" ^ yytext ^ "|"); continue());
