namespace blazor_app.Galactus.Formlet
{
  using Microsoft.AspNetCore.Blazor;
  using Microsoft.AspNetCore.Blazor.RenderTree;
  using System;
  using System.Text.RegularExpressions;

  using FailureContext  = ImmutableList<string>;
  using Attributes      = ImmutableList<(string key, string value)>;

  public sealed class Unit
  {
    public static readonly Unit Value = new Unit ();
  }

  public struct Maybe<T>
  {
    readonly bool m_hasValue;
    readonly T    m_value   ;

    public Maybe(T value)
    {
      m_hasValue = true ;
      m_value = value   ;
    }

    public bool HasValue  => m_hasValue ;
    public T    Value
    {
      get
      {
        return m_hasValue ? m_value : throw new Exception("Maybe holds no value");
      }
    }
  }

  public static class Maybe
  {
    public static Maybe<T> Just<T>(T v)
    {
      return new Maybe<T>(v);
    }

    public static Maybe<T> Nothing<T>()
    {
      return new Maybe<T>();
    }
  }


  public sealed class ImmutableList<T>
  {
    readonly T                m_head;
    readonly ImmutableList<T> m_tail;

    public ImmutableList(T head, ImmutableList<T> tail)
    {
      m_head = head;
      m_tail = tail;
    }

    public bool IsEmpty => m_tail == null;

    public static ImmutableList<T> Empty = new ImmutableList<T>(default(T), null);

    public T Head => !IsEmpty ? m_head : throw new Exception("List is empty");
    public ImmutableList<T> Tail => !IsEmpty ? m_tail : throw new Exception("List is empty");

    public ImmutableList<T> Cons(T head) => new ImmutableList<T>(head, this);
  }

  public static class ImmutableList
  {
    public static ImmutableList<T> Cons<T>(T head, ImmutableList<T> tail) => tail.Cons(head);
  }

  public enum NotifyType
  {
    Submit,
    Reset ,
    Change,
  }

  public sealed class FormletContext
  {
    public readonly RenderTreeBuilder   Builder ;
    public readonly Action<NotifyType>  Notify  ;

    int seq = 0;

    public FormletContext(RenderTreeBuilder builder, Action<NotifyType> notify)
    {
      Builder = builder ;
      Notify  = notify  ;
    }

    public void AddAttribute(string key, string value)
    {
      Builder.AddAttribute(seq++, key, value);
    }

    public void AddContent(string content)
    {
      Builder.AddContent(seq++, content);
    }

    public void CloseElement()
    {
      Builder.CloseElement();
    }

    public void OpenElement(string tag, ImmutableList<(string key, string value)> attributes)
    {
      Builder.OpenElement(seq++, tag);
      while (!attributes.IsEmpty)
      {
        var attribute = attributes.Head;
        Builder.AddAttribute(seq++, attribute.Item1, attribute.Item2);
        attributes = attributes.Tail;
      }
    }

    public void OpenValueElement(string tag, Attributes attributes, FormletState.Input input)
    {
      OpenElement(tag, attributes);

      UIEventHandler handler = args =>
      {
        var a = args as UIChangeEventArgs;
        if (a != null)
        {
          var v = a.Value as string ?? "";
          input.Value = v;
          Notify(NotifyType.Change);
        }
      };

      Builder.AddAttribute(seq++, "onchange", handler);
    }
  }

  public abstract class FormletFailureState
  {
    public sealed class Empty : FormletFailureState
    {
      public static readonly FormletFailureState Value = new Empty();
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

    public static Formlet<string> ValidateNotEmpty(this Formlet<string> t) =>
      t.Validate(s => string.IsNullOrEmpty(s) ? Maybe.Just("Must not be empty") : Maybe.Nothing<string>());

    public static Formlet<string> ValidateRegex(this Formlet<string> t, Regex regex, string message) =>
      t.Validate(s => regex.IsMatch(s) ? Maybe.Nothing<string>() : Maybe.Just(message));

    public static Formlet<T> WithAttribute<T>(this Formlet<T> t, string key, string value) =>
      (context, failureContext, attributes, state) =>
        t(context, failureContext, attributes.Cons((key, value)), state);

    public static Formlet<T> WithLabel<T>(this Formlet<T> t, string @for, string label) =>
      (context, failureContext, attributes, state) =>
        {
          context.OpenElement("label", Attributes.Empty);
          context.AddAttribute("for", @for);
          context.AddContent(label);
          context.CloseElement();

          return t(context, failureContext, attributes, state);
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
          context.OpenElement(tag, attributes);
          var tr = t(context, failureContext, Attributes.Empty, state);
          context.CloseElement();
          return tr;
        };

    public static Formlet<Unit> Tag(string tag, string content = null) =>
      (context, failureContext, attributes, state) =>
        {
          context.OpenElement(tag, attributes);
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

          var ts = state != null && inputTag == input.Tag
            ? input
            : new FormletState.Input(inputTag, initial)
            ;

          context.OpenValueElement("input", attributes, ts);
          context.AddAttribute("placeholder", placeholder);
          context.AddAttribute("type", "text");
          context.CloseElement();

          return Formlet.Success(ts.Value, ts);
        };
  }
}
