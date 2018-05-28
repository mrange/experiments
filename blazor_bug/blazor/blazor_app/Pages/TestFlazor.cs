namespace blazor_app.Pages
{
  using System;

  public static class Reproduce
  {
    public static Func<(T left, U right)> AndAlso<T, U>(this Func<T> t, Func<U> u)
    {
      return () => (t(), u());
    }

    public static Func<U> Map<T0, T1, T2, T3, U>(
        Func<T0> t0,
        Func<T1> t1,
        Func<T2> t2,
        Func<T3> t3,
        Func<T0, T1, T2, T3, U> m) =>
      () =>
        {
          var t = t0.AndAlso(t1).AndAlso(t2).AndAlso(t3);

          var (((tv0, tv1), tv2), tv3) = t();

          return m(tv0, tv1, tv2, tv3);
        };

    public static string TestStrings()
    {
      var r = Map(() => "1", () => "2", () => "3", () => "4", (x, y, z, w) => (x, y, z, w))();
      return r.ToString();
    }

    public static string TestInts()
    {
      var r = Map(() => 1, () => 2, () => 3, () => 4, (x, y, z, w) => (x, y, z, w))();
      return r.ToString();
    }

    public static string TestMixed()
    {
      var r = Map(() => "1", () => "2", () => 3, () => 4, (x, y, z, w) => (x, y, z, w))();
      return r.ToString();
    }

  }
}
