

  
fun convert_expression(Absyn.IfExp{test: Absyn.exp, then': Absyn.exp, else': Absyn.exp, pos: Absyn.pos}) = String.concat(["if",convert_expression(test),"then",convert_expression(then'),"else",convert_expression(else')]) 


| convert_expression (Absyn.OpExp{left: Absyn.exp, oper: Absyn.oper, right: Absyn.exp, pos: Absyn.pos}) = let val q = oper in 
							 if q = Absyn.PlusOp then String.concat([convert_expression(left),"+",convert_expression(right)]) 
							else  if q = Absyn.MinusOp then String.concat([convert_expression(left),"-",convert_expression(right)])
							else  if q = Absyn.TimesOp then String.concat([convert_expression(left),"*",convert_expression(right)]) 
							else  if q = Absyn.DivideOp then String.concat([convert_expression(left),"/",convert_expression(right)]) 
							else  if q = Absyn.EqOp then String.concat([convert_expression(left),"==",convert_expression(right)]) 
							else  if q = Absyn.NeqOp then String.concat([convert_expression(left),"!=",convert_expression(right)]) 
							else  if q = Absyn.LtOp then String.concat([convert_expression(left),"<",convert_expression(right)]) 
							else  if q = Absyn.LeOp then String.concat([convert_expression(left),"<=",convert_expression(right)]) 
							else  if q = Absyn.GtOp then String.concat([convert_expression(left),">",convert_expression(right)]) 
							else  if q = Absyn.GeOp then String.concat([convert_expression(left),">=",convert_expression(right)]) 
							else String.concat([convert_expression(left),"##",convert_expression(right)])	end

| convert_expression (Absyn.IntExp(a))	     = Int.toString(a)






 
