// Copyright (c) Mårten Rånge. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Flazor.Formlets
{
  using System;

  public static partial class Formlet
  {

    public static Formlet<U> Map<
                                  T0,
                                  T1,
                                  T2,
                                  T3,
                                  U>(
        Formlet<T0> t0,
        Formlet<T1> t1,
        Formlet<T2> t2,
        Formlet<T3> t3,
        Func<
              T0,
              T1,
              T2,
              T3,
              U> m) =>
      (context, failureContext, state) =>
        {
          var tr0 = t0(context, failureContext, state);
          var tr1 = t1(context, failureContext, state);
          var tr2 = t2(context, failureContext, state);
          var tr3 = t3(context, failureContext, state);
          return Result(
              m(
                  tr0.Value
                , tr1.Value
                , tr2.Value
                , tr3.Value
              )
            , FormletFailureState.Join(tr0.FailureState, tr1.FailureState)
            , FormletVisualState.Join(tr0.VisualState, tr1.VisualState)
            , FormletState.Join(tr0.State, tr1.State)
            );
        };

    public static Formlet<U> Map<
                                  T0,
                                  T1,
                                  T2,
                                  T3,
                                  T4,
                                  U>(
        Formlet<T0> t0,
        Formlet<T1> t1,
        Formlet<T2> t2,
        Formlet<T3> t3,
        Formlet<T4> t4,
        Func<
              T0,
              T1,
              T2,
              T3,
              T4,
              U> m) =>
      (context, failureContext, state) =>
        {
          var tr0 = t0(context, failureContext, state);
          var tr1 = t1(context, failureContext, state);
          var tr2 = t2(context, failureContext, state);
          var tr3 = t3(context, failureContext, state);
          var tr4 = t4(context, failureContext, state);
          return Result(
              m(
                  tr0.Value
                , tr1.Value
                , tr2.Value
                , tr3.Value
                , tr4.Value
              )
            , FormletFailureState.Join(tr0.FailureState, tr1.FailureState)
            , FormletVisualState.Join(tr0.VisualState, tr1.VisualState)
            , FormletState.Join(tr0.State, tr1.State)
            );
        };
  }
}