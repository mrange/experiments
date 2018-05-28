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
      U>(
        Formlet<T0> t0,
        Formlet<T1> t1,
        Func<
              T0,
              T1,
              U> m) =>
      (context, failureContext, state) =>
        {
          var t = t0
            .AndAlso(t1)
            ;

          var tr = t(context, failureContext, state);

          var (tv0, tv1) = tr.Value;

          return tr.WithValue(
              m(
                  tv0
                , tv1
                ));
        };

    public static Formlet<U> Map<
      T0,
      T1,
      T2,
      U>(
        Formlet<T0> t0,
        Formlet<T1> t1,
        Formlet<T2> t2,
        Func<
              T0,
              T1,
              T2,
              U> m) =>
      (context, failureContext, state) =>
        {
          var t = t0
            .AndAlso(t1)
            .AndAlso(t2)
            ;

          var tr = t(context, failureContext, state);

          var ((tv0, tv1), tv2) = tr.Value;

          return tr.WithValue(
              m(
                  tv0
                , tv1
                , tv2
                ));
        };

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
          var t = t0
            .AndAlso(t1)
            .AndAlso(t2)
            .AndAlso(t3)
            ;

          var tr = t(context, failureContext, state);

          var (((tv0, tv1), tv2), tv3) = tr.Value;

          return tr.WithValue(
              m(
                  tv0
                , tv1
                , tv2
                , tv3
                ));
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
          var t = t0
            .AndAlso(t1)
            .AndAlso(t2)
            .AndAlso(t3)
            .AndAlso(t4)
            ;

          var tr = t(context, failureContext, state);

          var ((((tv0, tv1), tv2), tv3), tv4) = tr.Value;

          return tr.WithValue(
              m(
                  tv0
                , tv1
                , tv2
                , tv3
                , tv4
                ));
        };

    public static Formlet<U> Map<
      T0,
      T1,
      T2,
      T3,
      T4,
      T5,
      U>(
        Formlet<T0> t0,
        Formlet<T1> t1,
        Formlet<T2> t2,
        Formlet<T3> t3,
        Formlet<T4> t4,
        Formlet<T5> t5,
        Func<
              T0,
              T1,
              T2,
              T3,
              T4,
              T5,
              U> m) =>
      (context, failureContext, state) =>
        {
          var t = t0
            .AndAlso(t1)
            .AndAlso(t2)
            .AndAlso(t3)
            .AndAlso(t4)
            .AndAlso(t5)
            ;

          var tr = t(context, failureContext, state);

          var (((((tv0, tv1), tv2), tv3), tv4), tv5) = tr.Value;

          return tr.WithValue(
              m(
                  tv0
                , tv1
                , tv2
                , tv3
                , tv4
                , tv5
                ));
        };

    public static Formlet<U> Map<
      T0,
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      U>(
        Formlet<T0> t0,
        Formlet<T1> t1,
        Formlet<T2> t2,
        Formlet<T3> t3,
        Formlet<T4> t4,
        Formlet<T5> t5,
        Formlet<T6> t6,
        Func<
              T0,
              T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              U> m) =>
      (context, failureContext, state) =>
        {
          var t = t0
            .AndAlso(t1)
            .AndAlso(t2)
            .AndAlso(t3)
            .AndAlso(t4)
            .AndAlso(t5)
            .AndAlso(t6)
            ;

          var tr = t(context, failureContext, state);

          var ((((((tv0, tv1), tv2), tv3), tv4), tv5), tv6) = tr.Value;

          return tr.WithValue(
              m(
                  tv0
                , tv1
                , tv2
                , tv3
                , tv4
                , tv5
                , tv6
                ));
        };

    public static Formlet<U> Map<
      T0,
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      U>(
        Formlet<T0> t0,
        Formlet<T1> t1,
        Formlet<T2> t2,
        Formlet<T3> t3,
        Formlet<T4> t4,
        Formlet<T5> t5,
        Formlet<T6> t6,
        Formlet<T7> t7,
        Func<
              T0,
              T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              U> m) =>
      (context, failureContext, state) =>
        {
          var t = t0
            .AndAlso(t1)
            .AndAlso(t2)
            .AndAlso(t3)
            .AndAlso(t4)
            .AndAlso(t5)
            .AndAlso(t6)
            .AndAlso(t7)
            ;

          var tr = t(context, failureContext, state);

          var (((((((tv0, tv1), tv2), tv3), tv4), tv5), tv6), tv7) = tr.Value;

          return tr.WithValue(
              m(
                  tv0
                , tv1
                , tv2
                , tv3
                , tv4
                , tv5
                , tv6
                , tv7
                ));
        };

    public static Formlet<U> Map<
      T0,
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      U>(
        Formlet<T0> t0,
        Formlet<T1> t1,
        Formlet<T2> t2,
        Formlet<T3> t3,
        Formlet<T4> t4,
        Formlet<T5> t5,
        Formlet<T6> t6,
        Formlet<T7> t7,
        Formlet<T8> t8,
        Func<
              T0,
              T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              T8,
              U> m) =>
      (context, failureContext, state) =>
        {
          var t = t0
            .AndAlso(t1)
            .AndAlso(t2)
            .AndAlso(t3)
            .AndAlso(t4)
            .AndAlso(t5)
            .AndAlso(t6)
            .AndAlso(t7)
            .AndAlso(t8)
            ;

          var tr = t(context, failureContext, state);

          var ((((((((tv0, tv1), tv2), tv3), tv4), tv5), tv6), tv7), tv8) = tr.Value;

          return tr.WithValue(
              m(
                  tv0
                , tv1
                , tv2
                , tv3
                , tv4
                , tv5
                , tv6
                , tv7
                , tv8
                ));
        };

    public static Formlet<U> Map<
      T0,
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      U>(
        Formlet<T0> t0,
        Formlet<T1> t1,
        Formlet<T2> t2,
        Formlet<T3> t3,
        Formlet<T4> t4,
        Formlet<T5> t5,
        Formlet<T6> t6,
        Formlet<T7> t7,
        Formlet<T8> t8,
        Formlet<T9> t9,
        Func<
              T0,
              T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              T8,
              T9,
              U> m) =>
      (context, failureContext, state) =>
        {
          var t = t0
            .AndAlso(t1)
            .AndAlso(t2)
            .AndAlso(t3)
            .AndAlso(t4)
            .AndAlso(t5)
            .AndAlso(t6)
            .AndAlso(t7)
            .AndAlso(t8)
            .AndAlso(t9)
            ;

          var tr = t(context, failureContext, state);

          var (((((((((tv0, tv1), tv2), tv3), tv4), tv5), tv6), tv7), tv8), tv9) = tr.Value;

          return tr.WithValue(
              m(
                  tv0
                , tv1
                , tv2
                , tv3
                , tv4
                , tv5
                , tv6
                , tv7
                , tv8
                , tv9
                ));
        };
  }
}