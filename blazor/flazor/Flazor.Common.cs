// Copyright (c) Mårten Rånge. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Flazor
{
  using System;
  using System.Text;

  public static partial class Common
  {
    public const int InitialSize = 16;
  }

  public sealed partial class Unit
  {
    Unit()
    {
    }

    public static readonly Unit Value = new Unit();

    public override string ToString() => "()";
  }

  public partial struct Maybe<T>
  {
    readonly bool m_hasValue;
    readonly T m_value;

    public Maybe(T value)
    {
      m_hasValue = true;
      m_value = value;
    }

    public bool HasValue => m_hasValue;

    public T Value => m_hasValue ? m_value : throw new Exception("Maybe holds no value");

    public override string ToString() => m_hasValue ? $"(Just {m_value})" : "(Nothing)";
  }

  public static partial class Maybe
  {
    public static Maybe<T> Just<T>(T v) => new Maybe<T>(v);

    public static Maybe<T> Nothing<T>() => new Maybe<T>();
  }

  public sealed partial class ImmutableList<T>
  {
    readonly T m_head;
    readonly ImmutableList<T> m_tail;

    ImmutableList(T head, ImmutableList<T> tail)
    {
      m_head = head;
      m_tail = tail;
    }

    public bool IsEmpty => m_tail == null;

    public static ImmutableList<T> Empty = new ImmutableList<T>(default(T), null);

    public T Head => !IsEmpty ? m_head : throw new Exception("List is empty");
    public ImmutableList<T> Tail => !IsEmpty ? m_tail : throw new Exception("List is empty");

    public ImmutableList<T> Cons(T head) => new ImmutableList<T>(head, this);

    public override string ToString()
    {
      var sb = new StringBuilder(Common.InitialSize);

      sb.Append('[');

      var list = this;
      var first = true;
      while (!list.IsEmpty)
      {
        if (!first)
        {
          sb.Append(", ");
        }

        first = false;

        sb.Append(list.Head);

        list = list.Tail;
      }

      sb.Append(']');

      return sb.ToString();
    }

  }

  public static partial class ImmutableList
  {
    public static ImmutableList<T> Empty<T>() => ImmutableList<T>.Empty;
    public static ImmutableList<T> Cons<T>(T head, ImmutableList<T> tail) => tail.Cons(head);
    public static ImmutableList<T> Singleton<T>(T value) => Empty<T>().Cons(value);
  }

}
