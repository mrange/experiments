// Copyright (c) Mårten Rånge. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Flazor.Formlets
{
  using Microsoft.AspNetCore.Blazor;
  using Microsoft.AspNetCore.Blazor.RenderTree;
  using System;
  using System.Text.RegularExpressions;
  using System.Text;

  using FailureContext  = ImmutableList<string>;
  using Attributes      = ImmutableList<Attribute>;
  using System.Collections.Generic;

  public enum NotifyType
  {
    Submit,
    Reset ,
    Change,
  }

  public sealed class FormletContext
  {
    readonly RenderTreeBuilder   m_builder ;
    readonly Action<NotifyType>  m_notify  ;

    readonly Stack<List<string>> m_stack    = new Stack<List<string>>(Common.InitialSize);
    readonly Stack<List<string>> m_reused   = new Stack<List<string>>(Common.InitialSize);
    readonly StringBuilder       m_classes  = new StringBuilder(Common.InitialSize);

    int seq = 1000;

    public FormletContext(RenderTreeBuilder builder, Action<NotifyType> notify)
    {
      m_builder = builder ;
      m_notify  = notify  ;
    }

    public void AddAttribute(string key, string value)
    {
      var list = m_stack.Peek();
      list.Add(key);
      list.Add(value);
    }

    public void CloseAttributes(Attributes attributes)
    {
      Console.WriteLine($"(CloseAttributes, {attributes})");
      while (!attributes.IsEmpty)
      {
        var attribute = attributes.Head;
        attribute.AddAttributes(this);
        attributes = attributes.Tail;
      }

      var list = m_stack.Peek();

      if (list.Count > 0)
      {
        m_classes.Clear();

        for (var iter = 0; iter < list.Count; iter += 2)
        {
          var key = list[iter];
          var value = iter + 1 < list.Count ? list[iter + 1] : "";

          // Special handling for multiple classes
          if (key.Equals("class", StringComparison.Ordinal))
          {
            if (m_classes.Length > 1)
            {
              m_classes.Append(' ');
            }
            m_classes.Append(value);
          }
          else
          {
            Console.WriteLine($"(Attribute, {seq}, {key}, {value})");
            m_builder.AddAttribute(seq++, key, value);
          }
        }

        if (m_classes.Length > 0)
        { 
          Console.WriteLine($"(Attribute, {seq},  class, {m_classes})");
          m_builder.AddAttribute(seq++, "class", m_classes.ToString());
        }

        m_classes.Clear();
      }
    }

    public void AddContent(string content)
    {
      m_builder.AddContent(seq++, content);
    }

    public void CloseElement()
    {
      var list = m_stack.Pop();
      list.Clear();
      m_reused.Push(list);
      m_builder.CloseElement();
    }

    public void OpenElement(string tag)
    {
      m_builder.OpenElement(seq++, tag);

      if (m_reused.Count > 0)
      {
        var list = m_reused.Pop();
        list.Clear();
        m_stack.Push(list);
      }
      else
      {
        var list = new List<string>(Common.InitialSize);
        m_stack.Push(list);
      }
    }

    public void OpenValueElement(string tag, FormletState.Input input)
    {
      OpenElement(tag);

      UIEventHandler handler = args =>
      {
        var a = args as UIChangeEventArgs;
        if (a != null)
        {
          var v = a.Value as string ?? "";
          input.Value = v;
          m_notify(NotifyType.Change);
        }
      };

      m_builder.AddAttribute(seq++, "onchange", handler);
    }

    public override string ToString() => "(FormletContext)";
  }

  public abstract class Attribute
  {
    public sealed class Single : Attribute
    {
      public readonly string Key   ;
      public readonly string Value ;

      public Single(string key, string value)
      {
        Key   = key   ;
        Value = value ;
      }

      public override void AddAttributes(FormletContext context)
      {
        context.AddAttribute(Key, Value);
      }

      public override string ToString() => $"(Single, {Key}, {Value})";
    }

    public sealed class Many : Attribute
    {
      public readonly string[] KeyValues;

      public Many(string[] keyValues)
      {
        KeyValues = keyValues;
      }

      public override void AddAttributes(FormletContext context)
      {
        for (var iter = 0; iter < KeyValues.Length; iter += 2)
        {
          var key = KeyValues[iter];
          var value = iter + 1 < KeyValues.Length ? KeyValues[iter + 1] : "";

          context.AddAttribute(key, value);
        }
      }

      public override string ToString() {
        var sb = new StringBuilder(Common.InitialSize);
        sb.Append("(Many");
        for (var iter = 0; iter < KeyValues.Length; iter += 2)
        {
          var key = KeyValues[iter];
          var value = iter + 1 < KeyValues.Length ? KeyValues[iter + 1] : "";
          sb.Append(", ");
          sb.Append(key);
          sb.Append(':');
          sb.Append(value);
        }
        sb.Append(')');
        return sb.ToString();
      }
    }

    public abstract void AddAttributes(FormletContext context);
  }

  public abstract class FormletFailureState
  {
    public sealed class Empty : FormletFailureState
    {
      public static readonly FormletFailureState Value = new Empty();

      public override string ToString() => "(Empty)";
    }

    public sealed class Failure : FormletFailureState
    {
      public readonly FailureContext  FailureContext;
      public readonly string          Message       ;

      public Failure(FailureContext failureContext, string message)
      {
        FailureContext  = failureContext;
        Message         = message       ;
      }

      public override string ToString() => $"(Failure, {FailureContext}, {Message})";
    }

    public sealed class Fork : FormletFailureState
    {
      public readonly FormletFailureState Left  ;
      public readonly FormletFailureState Right ;

      public Fork(FormletFailureState left, FormletFailureState right)
      {
        Left  = left  ;
        Right = right ;
      }

      public override string ToString() => $"(Fork, {Left}, {Right})";
    }

    public static FormletFailureState Join(FormletFailureState left, FormletFailureState right)
    {
      if (left is Empty)
      {
        return right;
      } 
      else if (right is Empty)
      {
        return left;
      }
      else
      {
        return new Fork(left, right);
      }

    }
  }

  public sealed class Tag
  {
    public readonly string Name;

    public Tag(string name)
    {
      Name = name;
    }

    public override string ToString() => $"(Tag, {Name})";
  }

  public abstract class FormletState
  {
    public sealed class Empty : FormletState
    {
      public static readonly FormletState Value = new Empty();

      public override string ToString() => "(Empty)";
    }

    public sealed class Input : FormletState
    {
      public readonly Tag Tag   ;
      public string       Value ;  // Intentionally mutable

      public Input(Tag tag, string value)
      {
        Tag     = tag   ;
        Value   = value ;
      }

      public override string ToString() => $"(Input, {Tag}, {Value})";
    }

    public sealed class Debug : FormletState
    {
      public readonly string       Name  ;
      public readonly FormletState State ;

      public Debug(string name, FormletState state)
      {
        Name  = name  ;
        State = state ;
      }

      public override string ToString() => $"(Debug, {Name}, {State})";
    }

    public sealed class Named : FormletState
    {
      public readonly string        Name  ;
      public readonly FormletState  State ;

      public Named(string name, FormletState state)
      {
        Name  = name  ;
        State = state ;
      }

      public override string ToString() => $"(Named, {Name}, {State})";
    }

    public sealed class Fork : FormletState
    {
      public readonly FormletState Left ;
      public readonly FormletState Right;

      public Fork(FormletState left, FormletState right)
      {
        Left  = left  ;
        Right = right ;
      }

      public override string ToString() => $"(Fork, {Left}, {Right})";
    }

    public static FormletState Join(FormletState left, FormletState right) =>
      new Fork(left, right);

  }

  public struct FormletResult<T>
  {
    public readonly T                   Value       ;
    public readonly FormletFailureState FailureState;
    public readonly FormletState        State       ;

    public FormletResult(T value, FormletFailureState failureState, FormletState state)
    {
      Value         = value       ;
      FailureState  = failureState;
      State         = state       ;
    }

    public override string ToString() => $"(FormletResult, {Value}, {FailureState}, {State})";
  }

  public delegate FormletResult<T> Formlet<T>(
        FormletContext  context
      , FailureContext  failureContext
      , Attributes      attributes
      , FormletState    state
    );

  public static class Formlet
  {
    public static FormletResult<T> Result<T>(T value, FormletFailureState failureState, FormletState state) =>
      new FormletResult<T>(value, failureState, state);

    public static FormletResult<T> Success<T>(T value, FormletState state) =>
      new FormletResult<T>(value, FormletFailureState.Empty.Value, state);

    public static Formlet<T> Value<T>(T v) => (context, failureContext, attributes, state) => 
      Success(v, FormletState.Empty.Value);

    public static Formlet<T> FailWith<T>(T v, string message) => 
      (context, failureContext, attributes, state) => Result(v, new FormletFailureState.Failure (failureContext, message), FormletState.Empty.Value);

    public static Formlet<U> Bind<T, U>(this Formlet<T> t, Func<T, Formlet<U>> uf) =>
      (context, failureContext, attributes, state) => 
        {
          var fork = state as FormletState.Fork;
          // TODO: Is this struct tuple?
          var (ts, us) = fork != null
            ? (fork.Left, fork.Right)
            : (FormletState.Empty.Value, FormletState.Empty.Value)
            ;

          var tr = t(context, failureContext, attributes, ts);
          var u  = uf(tr.Value);
          var ur = u(context, failureContext, attributes, us);

          return Result(ur.Value, FormletFailureState.Join(tr.FailureState, ur.FailureState), FormletState.Join(tr.State, ur.State));
        };

    public static Formlet<U> Map<T, U>(this Formlet<T> t, Func<T, U> m) =>
      (context, failureContext, attributes, state) =>
        {
          var tr = t(context, failureContext, attributes, state);
          return Result(m(tr.Value), tr.FailureState, tr.State);
        };

    public static Formlet<(T left, U right)> AndAlso<T, U>(this Formlet<T> t, Formlet<U> u) =>
      (context, failureContext, attributes, state) =>
        {
          var fork = state as FormletState.Fork;
          // TODO: Is this struct tuple?
          var (ts, us) = fork != null
            ? (fork.Left, fork.Right)
            : (FormletState.Empty.Value, FormletState.Empty.Value)
            ;

          var tr = t(context, failureContext, attributes, ts);
          var ur = u(context, failureContext, attributes, us);

          return Result((tr.Value, ur.Value), FormletFailureState.Join(tr.FailureState, ur.FailureState), FormletState.Join(tr.State, ur.State));
        };

    public static Formlet<T> KeepLeft<T, U>(this Formlet<T> t, Formlet<U> u) =>
      (context, failureContext, attributes, state) =>
      {
        var fork = state as FormletState.Fork;
        // TODO: Is this struct tuple?
        var (ts, us) = fork != null
          ? (fork.Left, fork.Right)
          : (FormletState.Empty.Value, FormletState.Empty.Value)
          ;

        var tr = t(context, failureContext, attributes, ts);
        var ur = u(context, failureContext, attributes, us);

        return Result(tr.Value, FormletFailureState.Join(tr.FailureState, ur.FailureState), FormletState.Join(tr.State, ur.State));
      };

    public static Formlet<U> KeepRight<T, U>(this Formlet<T> t, Formlet<U> u) =>
      (context, failureContext, attributes, state) =>
      {
        var fork = state as FormletState.Fork;
        // TODO: Is this struct tuple?
        var (ts, us) = fork != null
          ? (fork.Left, fork.Right)
          : (FormletState.Empty.Value, FormletState.Empty.Value)
          ;

        var tr = t(context, failureContext, attributes, ts);
        var ur = u(context, failureContext, attributes, us);

        return Result(ur.Value, FormletFailureState.Join(tr.FailureState, ur.FailureState), FormletState.Join(tr.State, ur.State));
      };

    public static Formlet<T> Unpack<T>(this Formlet<Formlet<T>> t) =>
      (context, failureContext, attributes, state) =>
        {
          var fork = state as FormletState.Fork;
          // TODO: Is this struct tuple?
          var (ts, us) = fork != null
            ? (fork.Left, fork.Right)
            : (FormletState.Empty.Value, FormletState.Empty.Value)
            ;

          var tr = t(context, failureContext, attributes, ts);
          var u  = tr.Value;
          var ur = u(context, failureContext, attributes, us);

          return Result(ur.Value, FormletFailureState.Join(tr.FailureState, ur.FailureState), FormletState.Join(tr.State, ur.State));
        };

    public static Formlet<T> Debug<T>(this Formlet<T> t, string name) =>
      (context, failureContext, attributes, state) =>
        {
          var debug = state as FormletState.Debug;
          // TODO: Is this struct tuple?
          var ts = debug != null
            ? debug.State
            : FormletState.Empty.Value
            ;

          var tr = t(context, failureContext, attributes, ts);

          return Result(tr.Value, tr.FailureState, new FormletState.Debug(name, tr.State));
        };

    public static Formlet<T> Named<T>(this Formlet<T> t, string name) =>
      (context, failureContext, attributes, state) =>
        {
          var named = state as FormletState.Named;
          // TODO: Is this struct tuple?
          var ts = named != null && named.Name == name
            ? named.State
            : FormletState.Empty.Value
            ;

          var tr = t(context, failureContext, attributes, ts);

          return Result(tr.Value, tr.FailureState, new FormletState.Named(name, tr.State));
        };

    public static Formlet<T> Validate<T>(this Formlet<T> t, Func<T, Maybe<string>> validator) =>
      (context, failureContext, attributes, state) =>
        {
          var tr = t(context, failureContext, attributes, state);
          var v  = validator(tr.Value);
          return v.HasValue
            ? Result(tr.Value, FormletFailureState.Join(tr.FailureState, new FormletFailureState.Failure(failureContext, v.Value)), tr.State)
            : tr
            ;
        };

    public static readonly Func<string, Maybe<string>> ValidatorNotEmpty = 
      s => string.IsNullOrEmpty(s) ? Maybe.Just("Must not be empty") : Maybe.Nothing<string>();

    public static Formlet<string> ValidateNotEmpty(this Formlet<string> t) =>
      t.Validate(ValidatorNotEmpty);

    public static Func<string, Maybe<string>> ValidatorRegex(Regex regex, string message) =>
      s => regex.IsMatch(s) ? Maybe.Nothing<string>() : Maybe.Just(message);

    public static Formlet<string> ValidateRegex(this Formlet<string> t, Regex regex, string message) =>
      t.Validate(ValidatorRegex(regex, message));

    public static Formlet<T> WithAttribute<T>(this Formlet<T> t, string key, string value) =>
      (context, failureContext, attributes, state) =>
        t(context, failureContext, attributes.Cons(new Attribute.Single(key, value)), state);

    public static Formlet<T> WithAttributes<T>(this Formlet<T> t, params string[] keyValues) =>
      (context, failureContext, attributes, state) =>
        t(context, failureContext, attributes.Cons(new Attribute.Many(keyValues)), state);

    public static Formlet<T> WithClass<T>(this Formlet<T> t, string @class) =>
      (context, failureContext, attributes, state) =>
        t(context, failureContext, attributes.Cons(new Attribute.Single("class", @class)), state);

    public static Formlet<T> WithId<T>(this Formlet<T> t, string id) =>
      (context, failureContext, attributes, state) =>
        t(context, failureContext, attributes.Cons(new Attribute.Single("id", id)), state);

    public static Formlet<T> WithLabel<T>(this Formlet<T> t, string @for, string label) =>
      (context, failureContext, attributes, state) =>
        {
          var ffc = failureContext.Cons(label);

          context.OpenElement("label");
          context.AddAttribute("for", @for);
          context.CloseAttributes(Attributes.Empty);
          context.AddContent(label);
          context.CloseElement();

          return t(context, ffc, attributes, state);
        };

    public static FormletResult<T> BuildUp<T>(this Formlet<T> t, RenderTreeBuilder builder, Action<NotifyType> notify, FormletState state)
    { 
      var context         = new FormletContext(builder, notify);
      var failureContext  = FailureContext.Empty;
      var attributes      = Attributes.Empty;
      return t(context, failureContext, attributes, state);
    }

    public static Formlet<T> NestWithTag<T>(this Formlet<T> t, string tag) =>
      (context, failureContext, attributes, state) =>
        {
          context.OpenElement(tag);
          context.CloseAttributes(attributes);
          var tr = t(context, failureContext, Attributes.Empty, state);
          context.CloseElement();
          return tr;
        };

    public static Formlet<Unit> Tag(string tag, string content = null) =>
      (context, failureContext, attributes, state) =>
        {
          context.OpenElement(tag);
          context.CloseAttributes(attributes);
          if (content != null)
          {
            context.AddContent(content);
          }
          context.CloseElement();
          return Success(Unit.Value, FormletState.Empty.Value);
        };
  }

  public static class Tags
  {
    static readonly Tag inputTag = new Tag("input");

    public static Formlet<Unit> Br<T>() => 
      Formlet.Tag("br");

    public static Formlet<T> Div<T>(Formlet<T> t) =>
      t.NestWithTag("div");

    public static Formlet<T> Span<T>(Formlet<T> t) =>
      t.NestWithTag("span");

    public static Formlet<T> P<T>(Formlet<T> t) =>
      t.NestWithTag("p");

    public static Formlet<string> Input(string placeholder, string initial) =>
      (context, failureContext, attributes, state) =>
        {
          var input = state as FormletState.Input;

          var ts = input != null && inputTag == input.Tag
            ? input
            : new FormletState.Input(inputTag, initial)
            ;

          context.OpenValueElement("input", ts);
          context.AddAttribute("placeholder", placeholder);
          context.AddAttribute("type", "text");
          context.CloseAttributes(attributes);
          context.CloseElement();

          return Formlet.Success(ts.Value, ts);
        };
  }

  namespace Bootstrap
  {
    public static class Formlet
    {
      public static Formlet<T> WithValidation<T>(this Formlet<T> t, string tag) =>
        (context, failureContext, attributes, state) =>
        {
          context.OpenElement(tag);
          var tr = t(context, failureContext, attributes, state);
          var value = tr.FailureState is FormletFailureState.Empty
            ? "is-valid"
            : "is-invalid"
            ;
          context.AddAttribute("class", value);
          context.CloseAttributes(Attributes.Empty);
          context.CloseElement();
          return tr;
        };
    }
  }
}
