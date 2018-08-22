structure Types =
struct

  type unique = unit ref

  datatype ty = 
          | NIL
          | INT
          | ARRAY of ty * unique
	  | NAME of Symbol.symbol * ty option ref
	  | UNIT

end

