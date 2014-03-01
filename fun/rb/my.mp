@@@ metaprogram
@@@ extension cs
@@@ include myother.mp
@@@ require ostruct
@@@ inject_token begin='@@=' end='=@@'

@@> for (var iter = 0; iter < 10; ++iter ) {
@@>     if (iter % 2 == 0) {
    // This is a test @@=iter=@@
@@>     }
@@> }

@@+ class HelperClass
@@+ {
@@+     public string Name;
@@+ }