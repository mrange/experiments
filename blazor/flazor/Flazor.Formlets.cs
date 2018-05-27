// Copyright (c) Mårten Rånge. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Flazor.Formlets
{
  using Microsoft.AspNetCore.Blazor;
  using Microsoft.AspNetCore.Blazor.RenderTree;
  using System;
  using System.Text.RegularExpressions;
  using System.Text;

  public enum NotifyType
  {
    Submit,
    Reset ,
    Change,
  }

  public sealed partial class FormletContext
  {
    public override string ToString() => "(FormletContext)";
  }

  public abstract partial class FormletVisualState
  {
    public sealed partial class Empty : FormletVisualState
    {
      public static readonly FormletVisualState Value = new Empty();

      public override void BuildUp(Action<NotifyType> notify, RenderTreeBuilder builder, ref int seq, ImmutableList<string> contents, ImmutableList<string[]> attributes)
      {
      }

      public override string ToString() => "(Empty)";
    }

    public sealed partial class Fork : FormletVisualState
    {
      public readonly FormletVisualState Left ;
      public readonly FormletVisualState Right;

      public Fork(FormletVisualState left, FormletVisualState right)
      {
        Left  = left  ;
        Right = right ;
      }

      public override void BuildUp(Action<NotifyType> notify, RenderTreeBuilder builder, ref int seq, ImmutableList<string> contents, ImmutableList<string[]> attributes)
      {
        Left.BuildUp(notify, builder, ref seq, contents, attributes);
        Right.BuildUp(notify, builder, ref seq, contents, attributes);
      }

      public override string ToString() => $"(Fork, {Left}, {Right})";
    }

    public sealed partial class WithAttributes : FormletVisualState
    {
      public readonly FormletVisualState  VisualState ;
      public readonly string[]            Attributes  ;

      public WithAttributes(FormletVisualState visualState, params string[] attributes)
      {
        VisualState = visualState;
        Attributes  = attributes ;
      }

      public override void BuildUp(Action<NotifyType> notify, RenderTreeBuilder builder, ref int seq, ImmutableList<string> contents, ImmutableList<string[]> attributes)
      {
        VisualState.BuildUp(notify, builder, ref seq, contents, attributes.Cons(Attributes));
      }

      public override string ToString()
      {
        var sb = new StringBuilder(Common.InitialSize);
        sb.Append("(WithAttributes, ");
        sb.Append(VisualState);
        for (var iter = 0; iter < Attributes.Length; iter += 2)
        {
          var key = Attributes[iter];
          var value = iter + 1 < Attributes.Length ? Attributes[iter + 1] : "";
          sb.Append(", ");
          sb.Append(key);
          sb.Append(':');
          sb.Append(value);
        }
        sb.Append(')');
        return sb.ToString();
      }
    }

    public sealed partial class WithContent : FormletVisualState
    {
      public readonly FormletVisualState  VisualState ;
      public readonly string              Content     ;

      public WithContent(FormletVisualState visualState, string content)
      {
        VisualState = visualState ;
        Content     = content     ;
      }

      public override void BuildUp(Action<NotifyType> notify, RenderTreeBuilder builder, ref int seq, ImmutableList<string> contents, ImmutableList<string[]> attributes)
      {
        VisualState.BuildUp(notify, builder, ref seq, contents.Cons(Content), attributes);
      }

      public override string ToString() => $"(WithContent, {VisualState}, {Content})";
    }

    [ThreadStatic]
    readonly static StringBuilder classes = new StringBuilder(Common.InitialSize);

    static void AddAttributes(RenderTreeBuilder builder, ref int seq, ImmutableList<string[]> attributes)
    {
      var clss = classes;
      var attrs = attributes;

      clss.Clear();

      while (!attrs.IsEmpty)
      {
        var list = attrs.Head;

        for (var iter = 0; iter < list.Length; iter += 2)
        {
          var key = list[iter];
          var value = iter + 1 < list.Length ? list[iter + 1] : "";

          // Special handling for multiple classes
          if (key.Equals("class", StringComparison.Ordinal))
          {
            if (clss.Length > 1)
            {
              clss.Append(' ');
            }
            clss.Append(value);
          }
          else
          {
            builder.AddAttribute(seq++, key, value);
          }
        }

        attrs = attrs.Tail;
      }

      if (clss.Length > 0)
      {
        builder.AddAttribute(seq++, "class", clss.ToString());
      }

      clss.Clear();
    }

    static void AddContents(RenderTreeBuilder builder, ref int seq, ImmutableList<string> contents)
    {
      if(!contents.IsEmpty)
      {
        builder.AddContent(seq++, contents.Head);
        AddContents(builder, ref seq, contents.Tail);
      }
    }

    static void OpenElement(RenderTreeBuilder builder, ref int seq, string tag, ImmutableList<string> contents, ImmutableList<string[]> attributes)
    {
      builder.OpenElement(seq++, tag);

      AddAttributes(builder, ref seq, attributes);
      AddContents(builder, ref seq, contents);
    }

    public sealed partial class Element : FormletVisualState
    {
      public readonly FormletVisualState  VisualState ;
      public readonly string              Tag         ;

      public Element(FormletVisualState visualState, string tag)
      {
        VisualState = visualState ;
        Tag         = tag         ;
      }

      public override void BuildUp(Action<NotifyType> notify, RenderTreeBuilder builder, ref int seq, ImmutableList<string> contents, ImmutableList<string[]> attributes)
      {
        OpenElement(builder, ref seq, Tag, contents, attributes);

        VisualState.BuildUp(notify, builder, ref seq, ImmutableList.Empty<string>(), ImmutableList.Empty<string[]>());

        builder.CloseElement();
      }

      public override string ToString() => $"(Element, {VisualState}, {Tag})";
    }

    public sealed partial class InputElement : FormletVisualState
    {
      public readonly string              Tag  ;
      public readonly FormletState.Input  Input;

      public InputElement(string tag, FormletState.Input input)
      {
        Tag   = tag   ;
        Input = input ;
      }

      public override void BuildUp(Action<NotifyType> notify, RenderTreeBuilder builder, ref int seq, ImmutableList<string> contents, ImmutableList<string[]> attributes)
      {
        OpenElement(builder, ref seq, Tag, contents, attributes);

        UIEventHandler handler = args =>
        {
          var a = args as UIChangeEventArgs;
          if (a != null)
          {
            var v = a.Value as string ?? "";
            Input.Value = v;
            notify(NotifyType.Change);
          }
        };

        builder.AddAttribute(seq++, "onchange", handler);

        builder.CloseElement();
      }

      public override string ToString() => $"(InputElement, {Tag}, {Input})";
    }

    public static FormletVisualState Join(FormletVisualState left, FormletVisualState right) =>
      new Fork(left, right);


    public abstract void BuildUp(Action<NotifyType> notify, RenderTreeBuilder builder, ref int seq, ImmutableList<string> contents, ImmutableList<string[]> attributes);
  }

  public abstract partial class FormletFailureState
  {
    public sealed partial class Empty : FormletFailureState
    {
      public static readonly FormletFailureState Value = new Empty();

      public override string ToString() => "(Empty)";
    }

    public sealed partial class Failure : FormletFailureState
    {
      public readonly ImmutableList<string> FailureContext;
      public readonly string                Message       ;

      public Failure(ImmutableList<string> failureContext, string message)
      {
        FailureContext  = failureContext;
        Message         = message       ;
      }

      public override string ToString() => $"(Failure, {FailureContext}, {Message})";
    }

    public sealed partial class Fork : FormletFailureState
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

  public sealed partial class Tag
  {
    public readonly string Name;

    public Tag(string name)
    {
      Name = name;
    }

    public override string ToString() => $"(Tag, {Name})";
  }

  public abstract partial class FormletState
  {
    public sealed partial class Empty : FormletState
    {
      public static readonly FormletState Value = new Empty();

      public override string ToString() => "(Empty)";
    }

    public sealed partial class Input : FormletState
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

    public sealed partial class Debug : FormletState
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

    public sealed partial class Named : FormletState
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

    public sealed partial class Fork : FormletState
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

  public partial struct FormletResult<T>
  {
    public readonly T                   Value       ;
    public readonly FormletFailureState FailureState;
    public readonly FormletVisualState  VisualState ;
    public readonly FormletState        State       ;

    public FormletResult(
        T                   value
      , FormletFailureState failureState
      , FormletVisualState  visualState
      , FormletState        state
      )
    {
      Value         = value       ;
      FailureState  = failureState;
      VisualState   = visualState ;
      State         = state       ;
    }

    public FormletResult<T> WithValue(T value)
    {
      return new FormletResult<T>(value, FailureState, VisualState, State);
    }

    public FormletResult<T> WithFailureState(FormletFailureState failureState)
    {
      return new FormletResult<T>(Value, failureState, VisualState, State);
    }

    public FormletResult<T> WithVisualState(FormletVisualState visualState)
    {
      return new FormletResult<T>(Value, FailureState, visualState, State);
    }

    public FormletResult<T> WithState(FormletState state)
    {
      return new FormletResult<T>(Value, FailureState, VisualState, state);
    }

    public override string ToString() => $"(FormletResult, {Value}, {FailureState}, {VisualState}, {State})";
  }

  public delegate FormletResult<T> Formlet<T>(
        FormletContext        context
      , ImmutableList<string> failureContext
      , FormletState          state
    );

  public static partial class Formlet
  {
    public static FormletResult<T> Result<T>(
        T                   value
      , FormletFailureState failureState
      , FormletVisualState  visualState
      , FormletState        state
      ) =>
      new FormletResult<T>(value, failureState, visualState, state);

    public static FormletResult<T> Success<T>(
        T                   value
      , FormletVisualState  visualState
      , FormletState        state
      ) =>
      new FormletResult<T>(value, FormletFailureState.Empty.Value, visualState, state);

    public static Formlet<T> Value<T>(T v) => (context, failureContext, state) =>
      Success(v, FormletVisualState.Empty.Value, FormletState.Empty.Value);

    public static Formlet<T> FailWith<T>(T v, string message) =>
      (context, failureContext, state) =>
        Result(
            v
          , new FormletFailureState.Failure (failureContext, message)
          , FormletVisualState.Empty.Value
          , FormletState.Empty.Value
          );

    public static Formlet<U> Bind<T, U>(this Formlet<T> t, Func<T, Formlet<U>> uf) =>
      (context, failureContext, state) =>
        {
          var fork = state as FormletState.Fork;
          // TODO: Is this struct tuple?
          var (ts, us) = fork != null
            ? (fork.Left, fork.Right)
            : (FormletState.Empty.Value, FormletState.Empty.Value)
            ;

          var tr = t(context, failureContext, ts);
          var u  = uf(tr.Value);
          var ur = u(context, failureContext, us);

          return Result(
              ur.Value
            , FormletFailureState.Join(tr.FailureState, ur.FailureState)
            , FormletVisualState.Join(tr.VisualState, ur.VisualState)
            , FormletState.Join(tr.State, ur.State)
            );
        };

    public static Formlet<U> Map<T, U>(this Formlet<T> t, Func<T, U> m) =>
      (context, failureContext, state) =>
        {
          var tr = t(context, failureContext, state);
          return Result(m(tr.Value), tr.FailureState, tr.VisualState, tr.State);
        };

    public static Formlet<U> Map<T0, T1, U>(Formlet<T0> t0, Formlet<T1> t1, Func<T0, T1, U> m) =>
      (context, failureContext, state) =>
        {
          var tr0 = t0(context, failureContext, state);
          var tr1 = t1(context, failureContext, state);
          return Result(
              m(tr0.Value, tr1.Value)
            , FormletFailureState.Join(tr0.FailureState, tr1.FailureState)
            , FormletVisualState.Join(tr0.VisualState, tr1.VisualState)
            , FormletState.Join(tr0.State, tr1.State)
            );
        };

    public static Formlet<U> Map<T0, T1, T2, U>(Formlet<T0> t0, Formlet<T1> t1, Formlet<T2> t2, Func<T0, T1, T2, U> m) =>
      (context, failureContext, state) =>
        {
          var tr0 = t0(context, failureContext, state);
          var tr1 = t1(context, failureContext, state);
          var tr2 = t2(context, failureContext, state);
          return Result(
              m(tr0.Value, tr1.Value, tr2.Value)
            , FormletFailureState.Join( FormletFailureState.Join(tr0.FailureState, tr1.FailureState), tr2.FailureState)
            , FormletVisualState.Join(FormletVisualState.Join(tr0.VisualState, tr1.VisualState), tr2.VisualState)
            , FormletState.Join(FormletState.Join(tr0.State, tr1.State), tr2.State)
            );
        };

    public static Formlet<(T left, U right)> AndAlso<T, U>(this Formlet<T> t, Formlet<U> u) =>
      (context, failureContext, state) =>
        {
          var fork = state as FormletState.Fork;
          // TODO: Is this struct tuple?
          var (ts, us) = fork != null
            ? (fork.Left, fork.Right)
            : (FormletState.Empty.Value, FormletState.Empty.Value)
            ;

          var tr = t(context, failureContext, ts);
          var ur = u(context, failureContext, us);

          return Result(
              (tr.Value, ur.Value)
            , FormletFailureState.Join(tr.FailureState, ur.FailureState)
            , FormletVisualState.Join(tr.VisualState, ur.VisualState)
            , FormletState.Join(tr.State, ur.State)
            );
        };

    public static Formlet<T> KeepLeft<T, U>(this Formlet<T> t, Formlet<U> u) =>
      (context, failureContext, state) =>
      {
        var fork = state as FormletState.Fork;
        // TODO: Is this struct tuple?
        var (ts, us) = fork != null
          ? (fork.Left, fork.Right)
          : (FormletState.Empty.Value, FormletState.Empty.Value)
          ;

        var tr = t(context, failureContext, ts);
        var ur = u(context, failureContext, us);

        return Result(
            tr.Value
          , FormletFailureState.Join(tr.FailureState, ur.FailureState)
          , FormletVisualState.Join(tr.VisualState, ur.VisualState)
          , FormletState.Join(tr.State, ur.State)
          );
      };

    public static Formlet<U> KeepRight<T, U>(this Formlet<T> t, Formlet<U> u) =>
      (context, failureContext, state) =>
      {
        var fork = state as FormletState.Fork;
        // TODO: Is this struct tuple?
        var (ts, us) = fork != null
          ? (fork.Left, fork.Right)
          : (FormletState.Empty.Value, FormletState.Empty.Value)
          ;

        var tr = t(context, failureContext, ts);
        var ur = u(context, failureContext, us);

        return Result(
            ur.Value
          , FormletFailureState.Join(tr.FailureState, ur.FailureState)
          , FormletVisualState.Join(tr.VisualState, ur.VisualState)
          , FormletState.Join(tr.State, ur.State)
          );
      };

    public static Formlet<T> Unpack<T>(this Formlet<Formlet<T>> t) =>
      (context, failureContext, state) =>
        {
          var fork = state as FormletState.Fork;
          // TODO: Is this struct tuple?
          var (ts, us) = fork != null
            ? (fork.Left, fork.Right)
            : (FormletState.Empty.Value, FormletState.Empty.Value)
            ;

          var tr = t(context, failureContext, ts);
          var u  = tr.Value;
          var ur = u(context, failureContext, us);

          return Result(
              ur.Value
            , FormletFailureState.Join(tr.FailureState, ur.FailureState)
            , FormletVisualState.Join(tr.VisualState, ur.VisualState)
            , FormletState.Join(tr.State, ur.State));
        };

    public static Formlet<T> Debug<T>(this Formlet<T> t, string name) =>
      (context, failureContext, state) =>
        {
          var debug = state as FormletState.Debug;
          // TODO: Is this struct tuple?
          var ts = debug != null
            ? debug.State
            : FormletState.Empty.Value
            ;

          var tr = t(context, failureContext, ts);

          return tr.WithState(new FormletState.Debug(name, tr.State));
        };

    public static Formlet<T> Named<T>(this Formlet<T> t, string name) =>
      (context, failureContext, state) =>
        {
          var named = state as FormletState.Named;
          // TODO: Is this struct tuple?
          var ts = named != null && named.Name == name
            ? named.State
            : FormletState.Empty.Value
            ;

          var tr = t(context, failureContext, ts);

          return tr.WithState(new FormletState.Named(name, tr.State));
        };

    public static Formlet<T> Validate<T>(this Formlet<T> t, Func<T, Maybe<string>> validator) =>
      (context, failureContext, state) =>
        {
          var tr = t(context, failureContext, state);
          var v  = validator(tr.Value);
          return v.HasValue
            ? tr.WithFailureState(FormletFailureState.Join(tr.FailureState, new FormletFailureState.Failure(failureContext, v.Value)))
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

    public static Formlet<T> WithAttributes<T>(this Formlet<T> t, params string[] keyValues) =>
      (context, failureContext, state) =>
        {
          var tr = t(context, failureContext, state);
          return tr.WithVisualState(new FormletVisualState.WithAttributes(tr.VisualState, keyValues));
        };

    public static Formlet<T> WithClass<T>(this Formlet<T> t, string @class) =>
      t.WithAttributes("class", @class);

    public static Formlet<T> WithId<T>(this Formlet<T> t, string id) =>
      t.WithAttributes("id", id);

    public static Formlet<T> WithLabel<T>(this Formlet<T> t, string @for, string label) =>
      (context, failureContext, state) =>
        {
          var ffc = failureContext.Cons(label);

          var visualState =
            new FormletVisualState.WithContent(
              new FormletVisualState.WithAttributes(
                  new FormletVisualState.Element(FormletVisualState.Empty.Value, "label")
                , "for", @for
                ),
                label
              );

          var tr = t(context, ffc, state);
          return tr.WithVisualState(FormletVisualState.Join(visualState, tr.VisualState));
        };

    public static FormletResult<T> BuildUp<T>(this Formlet<T> t, RenderTreeBuilder builder, Action<NotifyType> notify, FormletState state)
    {
      var context         = new FormletContext();
      var failureContext  = ImmutableList<string>.Empty;
      var result          = t(context, failureContext, state);
      var seq             = 1000;

      result.VisualState.BuildUp(notify, builder, ref seq, ImmutableList.Empty<string>(), ImmutableList.Empty<string[]>());

      Console.WriteLine($"(BuildUp, {result})");

      return result;
    }

    public static Formlet<T> NestWithTag<T>(this Formlet<T> t, string tag) =>
      (context, failureContext, state) =>
        {
          var tr = t(context, failureContext, state);
          return tr.WithVisualState(new FormletVisualState.Element(tr.VisualState, tag));
        };

    public static Formlet<Unit> Tag(string tag, string content = null) =>
      (context, failureContext, state) =>
        {
          var element = new FormletVisualState.Element(FormletVisualState.Empty.Value, tag);
          var visualState = content != null
            ? (FormletVisualState)new FormletVisualState.WithContent(element, content)
            : element
            ;
          return Success(Unit.Value, visualState, FormletState.Empty.Value);
        };
  }

  public static partial class Tags
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
      (context, failureContext, state) =>
        {
          var input = state as FormletState.Input;

          var ts = input != null && inputTag == input.Tag
            ? input
            : new FormletState.Input(inputTag, initial)
            ;

          var visualState =
            new FormletVisualState.WithAttributes(
                new FormletVisualState.InputElement("input", ts)
              , "placeholder", placeholder
              , "type", "text"
              );

          return Formlet.Success(ts.Value, visualState, ts);
        };
  }

  namespace Bootstrap
  {
    public static partial class Formlet
    {
      public static Formlet<T> WithValidation<T>(this Formlet<T> t) =>
        (context, failureContext, state) =>
        {
          var tr = t(context, failureContext, state);
          Console.WriteLine($"(WithValidation, {tr.FailureState})");
          var value = tr.FailureState is FormletFailureState.Empty
            ? "is-valid"
            : "is-invalid"
            ;
          return tr.WithVisualState(new FormletVisualState.WithAttributes(tr.VisualState, "class", value));
        };
    }
  }
}
