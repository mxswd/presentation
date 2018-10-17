cpppp
What happens when I want to use a new function? More work.
Instead. I want this quasi quoter like thing in C.
Except we don't need anti quoting.
---

= C Code =
char docDir[128], filen[128];
int res = @|c_hs|"*build* %s %s"|+docDir+, +filen+|;
