structure Absyn = 
struct

type pos = int   and   symbol = Symbol.symbol

datatype var = SimpleVar of symbol * pos

and exp = VarExp of var
        | NilExp
        | IntExp of int
        | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
        | SeqExp of (exp * pos) list
        | AssignExp of {var: string, exp: exp, pos: pos}
        | IfExp of {test: exp, then': exp, else': exp, pos: pos}
        | WhileExp of {test: exp, body: exp, pos: pos}
	    | ForExp of {var: symbol, 
		     lo: exp, hi: exp, body: exp, pos: pos}
        | LetExp of {decs: exp, body: exp, pos: pos}
        | ArrayExp of {typ: symbol, size: exp, init: exp, pos: pos}
       

		|  VarDec of {name: symbol,
		     typ: (symbol * pos) option,
		     init: exp,
		     pos: pos}
        | TypeDec of {name: symbol,ty: symbol * pos , pos: pos}

        | NameTy of symbol * pos



and oper = PlusOp | MinusOp | TimesOp | DivideOp
         | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

     
end
        
