@@@ metaprogram
@@@ extension cs
@@@ include myother.mp
@@@ require ostruct
@@@ inject_tokens @@= =@@

@@> for iter in 0..10
@@>     if iter % 2 == 0 then
    // This \n is 'a' test @@=iter=@@ of injects
    // This is a test of nothing special
    // This is a test @@=iter=@@ of two injects @@=iter=@@
@@>     end
@@> end

\@@ testing @@=ddeee=@@

@@+ def HelperMethodX (x)
@@+     HelperMethodY (x)
@@+     puts "HelperX"
@@+ end