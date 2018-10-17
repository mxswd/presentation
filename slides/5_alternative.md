Quoted Initialiser
Unlike macros, these can write other files. The position matches up to initialisers in C.
---

return type      quoted string   arguments
∨                ∨               ∨
int res = @|c_hs|"*build* %s %s"|+docDir+, +filen+|;
            ∧              ∧  
            transformer    types


1. can not modify the AST outside of it
2. the return type defined
3. all arguments evaluated trivially
