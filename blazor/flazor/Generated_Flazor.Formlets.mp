@@@ metaprogram
@@@ extension cs
// Copyright (c) Mårten Rånge. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Flazor.Formlets
{
  using System;

  public static partial class Formlet
  {
@@> for overload in 3...5

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
@@> for i in 0..overload
          var tr@@=i=@@ = t@@=i=@@(context, failureContext, state);
@@> end
          return Result(
              m(
@@> prepend = "  "
@@> for i in 0..overload
                @@=prepend=@@tr@@=i=@@.Value
@@>   prepend = ", "
@@> end
              )
            , FormletFailureState.Join(tr0.FailureState, tr1.FailureState)
            , FormletVisualState.Join(tr0.VisualState, tr1.VisualState)
            , FormletState.Join(tr0.State, tr1.State)
            );
        };
@@> end
  }
}
