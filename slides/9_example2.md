c_hs Transformer
Lets make one for C FFI generation. 1. Write module header, pass file handle around.
2. Run language c quote and template haskell at each quoter occurrence. 3. Close file.
---

&initHs& = do
  h <- openFile "HSAux.hs" WriteMode
  hPutStrLn h "module HSAux where\n"
  return h
&closeHs& = hClose

&emit& :: (FormatString a) => Handle -> SrcLoc -> FCall a
     -> IO (Handle, C.Initializer)
&emit& h loc (FCall fname ffi_name args) = do
    let carg_list = map snd args
        cexpr = C.*FnCall* (C.Var (C.Id ffi_name loc) loc)
                      (map (flip C.Var loc) carg_list) loc
    hs_expr <- *runQ* $ mkHsExpr ffi_name fname args

    hPutStrLn h $ pprint hs_expr
    return (h, [cinit|$cexpr|])