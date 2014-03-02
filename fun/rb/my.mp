@@@ metaprogram
@@@ extension cs
@@@ include myother.mp
@@@ require ostruct
@@@ inject_tokens @@= =@@

@@> for (var iter = 0; iter < 10; ++iter ) {
@@>     if (iter % 2 == 0) {
    // This \n is 'a' test @@=iter=@@ of injects
    // This is a test of nothing special
    // This is a test @@=iter=@@ of two injects @@=iter=@@
@@>     }
@@> }

@@+ class HelperClass
@@+ {
@@+     public string Name;
@@+ }