  using Microsoft.AspNetCore.Blazor;
using Microsoft.AspNetCore.Blazor.RenderTree;
using System;

namespace blazor_app.Galactus
{
  public sealed class Unit
  {
    Unit()
    {
    }

    public static readonly Unit Value = new Unit();
  }


  public class BuildUpContext
  {
    readonly RenderTreeBuilder m_builder;
    int seq = 0;

    public BuildUpContext(RenderTreeBuilder builder)
    {
      m_builder = builder;
    }

    public Unit AddReceiver(string name, Action<String> receiver)
    {
      Console.WriteLine("AddReceiver(0)");
      if (receiver == null)
      {
        return Unit.Value;
      }

      UIEventHandler handler = args =>
        { 
          var a = args as UIChangeEventArgs;
          if (a != null)
          {
            var v = a.Value as string ?? "";
            receiver(v);
          }
        };

      Console.WriteLine($"AddReceiver - {name}");
      m_builder.AddAttribute(seq++, name, handler);
      return Unit.Value;
    }

    public Unit AddReceiver(string name, Action<Unit> receiver)
    {
      if (receiver == null)
      {
        return Unit.Value;
      }

      UIEventHandler handler = args =>
      {
        receiver(Unit.Value);
      };

      m_builder.AddAttribute(seq++, name, handler);
      return Unit.Value;
    }

    public Unit AddAttribute(string name, string value)
    {
      m_builder.AddAttribute(seq++, name, value);
      return Unit.Value;
    }

    public Unit AddAttribute(string name, bool value)
    {
      m_builder.AddAttribute(seq++, name, value);
      return Unit.Value;
    }

    public Unit AddContent(string value)
    {
      m_builder.AddContent(seq++, value);
      return Unit.Value;
    }

    public Unit OpenElement(string tag)
    {
      m_builder.OpenElement(seq++, tag);
      return Unit.Value;
    }

    public Unit CloseElement()
    {
      m_builder.CloseElement();
      return Unit.Value;
    }
  }

  public interface IView
  {
    Unit BuildUp(BuildUpContext ctx);
  }

  public interface IView<TMessage> : IView
  {
  }

  public interface IAttribute
  {
    string Name { get; }
  }

  public interface IAttribute<in TElement, T> : IAttribute
  {

  }

  public sealed class Attribute<TElement, T> : IAttribute<TElement, T>
  {
    readonly string m_name;

    public Attribute(string name)
    {
      m_name = name ?? "__NONAME__";
    }

    public string Name => m_name;
  }

  public interface IEvent
  {
    string Name { get; }
  }

  public interface IEvent<in TElement, T> : IEvent
  {

  }

  public sealed class Event<TElement, T> : IEvent<TElement, T>
  {
    readonly string m_name;

    public Event(string name)
    {
      m_name = name ?? "__NONAME__";
    }

    public string Name => m_name;
  }

  public interface IValue
  {
    Unit BuildUp(BuildUpContext ctx);
  }

  public interface IValue<TMessage, in TElement> : IValue
  {
  }

  public interface ISetValue<TMessage, in TElement, T> : IValue<TMessage, TElement>
  {
    IAttribute<TElement, T> Attribute { get; }
    T Value { get; }
  }

  public sealed class SetStringValue<TMessage, TElement> : ISetValue<TMessage, TElement, string>
  {
    readonly IAttribute<TElement, string> m_attribute;
    readonly string m_value;

    public SetStringValue(IAttribute<TElement, string> property, string value)
    {
      m_attribute = property; // TODO: Check null
      m_value = value;
    }

    public IAttribute<TElement, string> Attribute => m_attribute;

    public string Value => m_value;

    public Unit BuildUp(BuildUpContext ctx)
    {
      ctx.AddAttribute(m_attribute.Name, m_value);
      return Unit.Value;
    }
  }

  public sealed class SetBoolValue<TMessage, TElement> : ISetValue<TMessage, TElement, bool>
  {
    readonly IAttribute<TElement, bool> m_attribute;
    readonly bool m_value;

    public SetBoolValue(IAttribute<TElement, bool> property, bool value)
    {
      m_attribute = property; // TODO: Check null
      m_value = value;
    }

    public IAttribute<TElement, bool> Attribute => m_attribute;

    public bool Value => m_value;

    public Unit BuildUp(BuildUpContext ctx)
    {
      ctx.AddAttribute(m_attribute.Name, m_value);
      return Unit.Value;
    }
  }

  public interface IReceiveValue<TMessage, in TElement, T> : IValue<TMessage, TElement>
  {
    IEvent<TElement, T> Event { get; }
    Action<T> Receiver { get; }
  }


  public sealed class ReceiveStringValue<TMessage, TElement> : IReceiveValue<TMessage, TElement, string>
  {
    readonly IEvent<TElement, string> m_event;
    readonly Action<string> m_receiver;

    public ReceiveStringValue(IEvent<TElement, string> @event, Action<string> receiver)
    {
      m_event = @event; // TODO: Check null
      m_receiver = receiver ?? (v => { });
    }

    public IEvent<TElement, string> Event => m_event;

    public Action<string> Receiver => m_receiver;

    public Unit BuildUp(BuildUpContext ctx)
    {
      ctx.AddReceiver(m_event.Name, m_receiver);
      return Unit.Value;
    }
  }

  public sealed class ReceiveUnitValue<TMessage, TElement> : IReceiveValue<TMessage, TElement, Unit>
  {
    readonly IEvent<TElement, Unit> m_event;
    readonly Action<Unit> m_receiver;

    public ReceiveUnitValue(IEvent<TElement, Unit> @event, Action<Unit> receiver)
    {
      m_event = @event; // TODO: Check null
      m_receiver = receiver ?? (v => { });
    }

    public IEvent<TElement, Unit> Event => m_event;

    public Action<Unit> Receiver => m_receiver;

    public Unit BuildUp(BuildUpContext ctx)
    {
      ctx.AddReceiver(m_event.Name, m_receiver);
      return Unit.Value;
    }
  }

  public sealed class View<TMessage, TElement> : IView<TMessage>
  {
    readonly string m_tag;
    readonly IValue<TMessage, TElement>[] m_values;
    readonly IView<TMessage>[] m_children;

    public View(string tag, IValue<TMessage, TElement>[] values, IView<TMessage>[] children)
    {
      m_tag = tag ?? "div";
      m_values = values ?? new IValue<TMessage, TElement>[0];
      m_children = children ?? new IView<TMessage>[0];
    }

    public Unit BuildUp(BuildUpContext ctx)
    {
      ctx.OpenElement(m_tag);

      foreach (var value in m_values)
      {
        value.BuildUp(ctx);
      }

      foreach (var child in m_children)
      {
        child.BuildUp(ctx);
      }

      ctx.CloseElement();

      return Unit.Value;
    }
  }

  public sealed class GroupView<TMessage> : IView<TMessage>
  {
    readonly IView<TMessage>[] m_views;

    public GroupView(IView<TMessage>[] views)
    {
      m_views = views ?? new IView<TMessage>[0];
    }

    public Unit BuildUp(BuildUpContext ctx)
    {
      foreach (var view in m_views)
      {
        view.BuildUp(ctx);
      }

      return Unit.Value;
    }
  }

  public sealed class TextView<TMessage> : IView<TMessage>
  {
    readonly string m_text;

    public TextView(string text)
    {
      m_text = text ?? "";
    }

    public Unit BuildUp(BuildUpContext ctx)
    {
      ctx.AddContent(m_text);
      return Unit.Value;
    }
  }

  public delegate IView<TMessage> ContainerView<TMessage>(params IView<TMessage>[] children);

  public static partial class Views<TMessage>
  {
    static ISetValue<TMessage, TElement, string> Set_string<TElement>(IAttribute<TElement, string> attribute, string v) => new SetStringValue<TMessage, TElement>(attribute, v);
    static ISetValue<TMessage, TElement, bool> Set_bool<TElement>(IAttribute<TElement, bool> attribute, bool v) => new SetBoolValue<TMessage, TElement>(attribute, v);

    static IReceiveValue<TMessage, TElement, string> Receive_string<TElement>(IEvent<TElement, string> @event, Action<string> r) => new ReceiveStringValue<TMessage, TElement>(@event, r);
    static IReceiveValue<TMessage, TElement, Unit> Receive_Unit<TElement>(IEvent<TElement, Unit> @event, Action<Unit> r) => new ReceiveUnitValue<TMessage, TElement>(@event, r);

    static IView<TMessage> Create_View<TElement>(string tag, IValue<TMessage, TElement>[] values) => new View<TMessage, TElement>(tag, values, null);
    static ContainerView<TMessage> Create_ContainerView<TElement>(string tag, IValue<TMessage, TElement>[] values) => children => new View<TMessage, TElement>(tag, values, children);

    public static IView<TMessage> Text(string v) => new TextView<TMessage>(v);
    public static IView<TMessage> Group(params IView<TMessage>[] views) => new GroupView<TMessage>(views);
  }

  public static class Extensions
  {
    public static RenderFragment CreateRenderFragment<TMessage>(this IView<TMessage> view)
    {
      return builder =>
      {
        var context = new BuildUpContext(builder);
        view.BuildUp(context);
      };
    }
  }
}
