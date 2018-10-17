Quoted Initialiser
A new kind of C Initialiser. QQ can do lots of things, we want to limit to only some, simpler.
---

Need to add this to C

int res = @|c_hs|"*build* %s %s"|+docDir+, +filen+|;

data Initializer = ExpInitializer Exp !SrcLoc
                 | &QuotedInitializer& Id String [Id] !SrcLoc
                 | CompoundInitializer ...



- quoter_name         |  Id
- quoter_expression   |  String
- quoter_args         |  [Id]
