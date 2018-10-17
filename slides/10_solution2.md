Code Generation
Walkthrough.
---

int res = @|c_hs|"*build* %s %s"|+docDir+, +filen+|;

- We generate foreign exports:

    foreign export ccall &compileFile& :: CString ...

- We generate marshalling:
    &compileFile& freeDir fname = do
      fname'   <- &peekCString& fname
      freeDir' <- &peekCString& freeDir
      *build* freeDir' fname'

- The type of the exported FFI function is checked by CC
    hs_ffi_1(docDir, filen);

- All generated as needed