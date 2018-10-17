FFI
Lets look at how we write an FFI to do this. We define and export this function. We pack some marshalling in (green). This is function we want to call (magenta).
The only bits we care about are the arguments and which function to use. We don't care about the rest. We want to automate this away.
---

= C Code =
char docDir[128], filen[128];
int res = &compileFile&(+docDir+, +filen+);

= Haskell Code =
foreign export ccall &compileFile& :: CString
                                 -> CString
                                 -> IO CInt

&compileFile& :: CString -> CString -> IO CInt
&compileFile& freeDir fname = do
  fname'   <- &peekCString& fname
  freeDir' <- &peekCString& freeDir
  *build* freeDir' fname'
