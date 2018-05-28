@@@ metaprogram
@@@ extension cs
// Copyright (c) Mårten Rånge. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Flazor.Formlets
{
  using System;

  public static partial class Formlet
  {
@@> for overload in 1...$overloads

    public static Formlet<U> Map<
@@> for i in 0..overload
      T@@=i=@@,
@@> end
      U>(
@@> for i in 0..overload
        Formlet<T@@=i=@@> t@@=i=@@,
@@> end
        Func<
@@> for i in 0..overload
              T@@=i=@@,
@@> end
              U> m) =>
      (context, failureContext, state) =>
        {
          var t = t0
@@> for i in 1..overload
            .AndAlso(t@@=i=@@)
@@> end
            ;

          var tr = t(context, failureContext, state);

@@> deconstruct = "tv0"
@@> for i in 1..overload
@@>   deconstruct = "(#{deconstruct}, tv#{i})"
@@> end
          var @@=deconstruct=@@ = tr.Value;

          return tr.WithValue(
              m(
@@> prepend = "  "
@@> for i in 0..overload
                @@=prepend=@@tv@@=i=@@
@@>   prepend = ", "
@@> end
                ));
        };
@@> end
  }
}
