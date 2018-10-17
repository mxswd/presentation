Example Int4 Transformer
A simple example, producing the integer constant 4. Regardless of quoter string or arguments.
---

When we write:
    int &x& = @|int4|""||;
We want it to compile to:
    int &x& = 4;
mkInt4 :: (IO (),
      () -> String -> [Id] -> SrcLoc -> IO ((), Initializer),
      () -> IO ())
mkInt4 = (return (), mkInt4', const . return $ ())

mkInt4' :: () -> String -> [Id] -> SrcLoc
        -> IO ((), Initializer)
mkInt4' _ expression vars l = return ((), [&cinit&|4|])
                                           ∧ language-c-quote
      (ExpInitializer (Const (IntConst "4" Signed 4 l) l) l)