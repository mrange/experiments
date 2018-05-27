// Copyright (c) Mårten Rånge. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Flazor.Elmish
{
  using System;

  public static partial class Hierarchy
  {
    public partial class Node : object
    {
    }

    public partial class Br : Node
    {
    }

    public partial class ContentNode : Node
    {
    }

    public partial class Button : ContentNode
    {
    }

    public partial class Div : ContentNode
    {
    }

    public partial class Img : Node
    {
    }

    public partial class H1 : ContentNode
    {
    }

    public partial class H2 : ContentNode
    {
    }

    public partial class H3 : ContentNode
    {
    }

    public partial class H4 : ContentNode
    {
    }

    public partial class H5 : ContentNode
    {
    }

    public partial class Hr : Node
    {
    }

    public partial class Input : Node
    {
    }

    public partial class P : ContentNode
    {
    }

    public partial class Span : ContentNode
    {
    }

  }

  public static partial class Attributes
  {
    public partial class Node : object
    {
      public static readonly IAttribute<Hierarchy.Node, string> AccessKey = new Attribute<Hierarchy.Node, string>("accesskey");
      public static readonly IAttribute<Hierarchy.Node, string> Class = new Attribute<Hierarchy.Node, string>("class");
      public static readonly IAttribute<Hierarchy.Node, bool> ContentEditable = new Attribute<Hierarchy.Node, bool>("contenteditable");
      public static readonly IAttribute<Hierarchy.Node, string> ContextMenu = new Attribute<Hierarchy.Node, string>("contextmenu");
      public static readonly IAttribute<Hierarchy.Node, string> Dir = new Attribute<Hierarchy.Node, string>("dir");
      public static readonly IAttribute<Hierarchy.Node, bool> Draggable = new Attribute<Hierarchy.Node, bool>("draggable");
      public static readonly IAttribute<Hierarchy.Node, bool> DropZone = new Attribute<Hierarchy.Node, bool>("dropzone");
      public static readonly IAttribute<Hierarchy.Node, bool> Hidden = new Attribute<Hierarchy.Node, bool>("hidden");
      public static readonly IAttribute<Hierarchy.Node, string> Id = new Attribute<Hierarchy.Node, string>("id");
      public static readonly IAttribute<Hierarchy.Node, string> Lang = new Attribute<Hierarchy.Node, string>("lang");
      public static readonly IAttribute<Hierarchy.Node, bool> SpellCheck = new Attribute<Hierarchy.Node, bool>("spellcheck");
      public static readonly IAttribute<Hierarchy.Node, string> Style = new Attribute<Hierarchy.Node, string>("style");
      public static readonly IAttribute<Hierarchy.Node, string> TabIndex = new Attribute<Hierarchy.Node, string>("tabindex");
      public static readonly IAttribute<Hierarchy.Node, string> Title = new Attribute<Hierarchy.Node, string>("title");
      public static readonly IAttribute<Hierarchy.Node, bool> Translate = new Attribute<Hierarchy.Node, bool>("translate");
    }

    public partial class Br : Node
    {
    }

    public partial class ContentNode : Node
    {
    }

    public partial class Button : ContentNode
    {
    }

    public partial class Div : ContentNode
    {
    }

    public partial class Img : Node
    {
      public static readonly IAttribute<Hierarchy.Img, string> Alt = new Attribute<Hierarchy.Img, string>("alt");
      public static readonly IAttribute<Hierarchy.Img, string> CrossOrigin = new Attribute<Hierarchy.Img, string>("crossorigin");
      public static readonly IAttribute<Hierarchy.Img, bool> IsMap = new Attribute<Hierarchy.Img, bool>("ismap");
      public static readonly IAttribute<Hierarchy.Img, string> LongDesc = new Attribute<Hierarchy.Img, string>("longdesc");
      public static readonly IAttribute<Hierarchy.Img, string> Sizes = new Attribute<Hierarchy.Img, string>("sizes");
      public static readonly IAttribute<Hierarchy.Img, string> Src = new Attribute<Hierarchy.Img, string>("src");
      public static readonly IAttribute<Hierarchy.Img, string> SrcSet = new Attribute<Hierarchy.Img, string>("srcset");
      public static readonly IAttribute<Hierarchy.Img, string> UseMap = new Attribute<Hierarchy.Img, string>("usemap");
      public static readonly IAttribute<Hierarchy.Img, string> Vspace = new Attribute<Hierarchy.Img, string>("vspace");
      public static readonly IAttribute<Hierarchy.Img, string> Width = new Attribute<Hierarchy.Img, string>("width");
    }

    public partial class H1 : ContentNode
    {
    }

    public partial class H2 : ContentNode
    {
    }

    public partial class H3 : ContentNode
    {
    }

    public partial class H4 : ContentNode
    {
    }

    public partial class H5 : ContentNode
    {
    }

    public partial class Hr : Node
    {
    }

    public partial class Input : Node
    {
      public static readonly IAttribute<Hierarchy.Input, string> Placeholder = new Attribute<Hierarchy.Input, string>("placeholder");
      public static readonly IAttribute<Hierarchy.Input, string> Value = new Attribute<Hierarchy.Input, string>("value");
    }

    public partial class P : ContentNode
    {
    }

    public partial class Span : ContentNode
    {
    }

  }

  public static partial class Events
  {
    public partial class Node : object
    {
      public static readonly IEvent<Hierarchy.Node, Unit> OnBlur = new Event<Hierarchy.Node, Unit>("onblur");
      public static readonly IEvent<Hierarchy.Node, string> OnChange = new Event<Hierarchy.Node, string>("onchange");
      public static readonly IEvent<Hierarchy.Node, Unit> OnContextMenu = new Event<Hierarchy.Node, Unit>("oncontextmenu");
      public static readonly IEvent<Hierarchy.Node, Unit> OnFocus = new Event<Hierarchy.Node, Unit>("onfocus");
      public static readonly IEvent<Hierarchy.Node, Unit> OnInput = new Event<Hierarchy.Node, Unit>("oninput");
      public static readonly IEvent<Hierarchy.Node, Unit> OnInvalid = new Event<Hierarchy.Node, Unit>("oninvalid");
      public static readonly IEvent<Hierarchy.Node, Unit> OnReset = new Event<Hierarchy.Node, Unit>("onreset");
      public static readonly IEvent<Hierarchy.Node, Unit> OnSearch = new Event<Hierarchy.Node, Unit>("onsearch");
      public static readonly IEvent<Hierarchy.Node, Unit> OnSelect = new Event<Hierarchy.Node, Unit>("onselect");
      public static readonly IEvent<Hierarchy.Node, Unit> OnSubmit = new Event<Hierarchy.Node, Unit>("onsubmit");
    }

    public partial class Br : Node
    {
    }

    public partial class ContentNode : Node
    {
    }

    public partial class Button : ContentNode
    {
    }

    public partial class Div : ContentNode
    {
    }

    public partial class Img : Node
    {
    }

    public partial class H1 : ContentNode
    {
    }

    public partial class H2 : ContentNode
    {
    }

    public partial class H3 : ContentNode
    {
    }

    public partial class H4 : ContentNode
    {
    }

    public partial class H5 : ContentNode
    {
    }

    public partial class Hr : Node
    {
    }

    public partial class Input : Node
    {
    }

    public partial class P : ContentNode
    {
    }

    public partial class Span : ContentNode
    {
    }

  }

  public static partial class Views<TMessage>
  {
    // Node
    public static ISetValue<TMessage, Hierarchy.Node, string> AccessKey(string v) => Set_string(Attributes.Node.AccessKey, v);
    public static ISetValue<TMessage, Hierarchy.Node, string> Class(string v) => Set_string(Attributes.Node.Class, v);
    public static ISetValue<TMessage, Hierarchy.Node, bool> ContentEditable(bool v) => Set_bool(Attributes.Node.ContentEditable, v);
    public static ISetValue<TMessage, Hierarchy.Node, string> ContextMenu(string v) => Set_string(Attributes.Node.ContextMenu, v);
    public static ISetValue<TMessage, Hierarchy.Node, string> Dir(string v) => Set_string(Attributes.Node.Dir, v);
    public static ISetValue<TMessage, Hierarchy.Node, bool> Draggable(bool v) => Set_bool(Attributes.Node.Draggable, v);
    public static ISetValue<TMessage, Hierarchy.Node, bool> DropZone(bool v) => Set_bool(Attributes.Node.DropZone, v);
    public static ISetValue<TMessage, Hierarchy.Node, bool> Hidden(bool v) => Set_bool(Attributes.Node.Hidden, v);
    public static ISetValue<TMessage, Hierarchy.Node, string> Id(string v) => Set_string(Attributes.Node.Id, v);
    public static ISetValue<TMessage, Hierarchy.Node, string> Lang(string v) => Set_string(Attributes.Node.Lang, v);
    public static ISetValue<TMessage, Hierarchy.Node, bool> SpellCheck(bool v) => Set_bool(Attributes.Node.SpellCheck, v);
    public static ISetValue<TMessage, Hierarchy.Node, string> Style(string v) => Set_string(Attributes.Node.Style, v);
    public static ISetValue<TMessage, Hierarchy.Node, string> TabIndex(string v) => Set_string(Attributes.Node.TabIndex, v);
    public static ISetValue<TMessage, Hierarchy.Node, string> Title(string v) => Set_string(Attributes.Node.Title, v);
    public static ISetValue<TMessage, Hierarchy.Node, bool> Translate(bool v) => Set_bool(Attributes.Node.Translate, v);
    public static IReceiveValue<TMessage, Hierarchy.Node, Unit> OnBlur(Action<Unit> r) => Receive_Unit(Events.Node.OnBlur, r);
    public static IReceiveValue<TMessage, Hierarchy.Node, string> OnChange(Action<string> r) => Receive_string(Events.Node.OnChange, r);
    public static IReceiveValue<TMessage, Hierarchy.Node, Unit> OnContextMenu(Action<Unit> r) => Receive_Unit(Events.Node.OnContextMenu, r);
    public static IReceiveValue<TMessage, Hierarchy.Node, Unit> OnFocus(Action<Unit> r) => Receive_Unit(Events.Node.OnFocus, r);
    public static IReceiveValue<TMessage, Hierarchy.Node, Unit> OnInput(Action<Unit> r) => Receive_Unit(Events.Node.OnInput, r);
    public static IReceiveValue<TMessage, Hierarchy.Node, Unit> OnInvalid(Action<Unit> r) => Receive_Unit(Events.Node.OnInvalid, r);
    public static IReceiveValue<TMessage, Hierarchy.Node, Unit> OnReset(Action<Unit> r) => Receive_Unit(Events.Node.OnReset, r);
    public static IReceiveValue<TMessage, Hierarchy.Node, Unit> OnSearch(Action<Unit> r) => Receive_Unit(Events.Node.OnSearch, r);
    public static IReceiveValue<TMessage, Hierarchy.Node, Unit> OnSelect(Action<Unit> r) => Receive_Unit(Events.Node.OnSelect, r);
    public static IReceiveValue<TMessage, Hierarchy.Node, Unit> OnSubmit(Action<Unit> r) => Receive_Unit(Events.Node.OnSubmit, r);

    // Br
    public static IView<TMessage> Br(params IValue<TMessage, Hierarchy.Br>[] values) => Create_View<Hierarchy.Br>("br", values);

    // ContentNode

    // Button
    public static ContainerView<TMessage> Button(params IValue<TMessage, Hierarchy.Button>[] values) => Create_ContainerView<Hierarchy.Button>("button", values);

    // Div
    public static ContainerView<TMessage> Div(params IValue<TMessage, Hierarchy.Div>[] values) => Create_ContainerView<Hierarchy.Div>("div", values);

    // Img
    public static ISetValue<TMessage, Hierarchy.Img, string> Alt(string v) => Set_string(Attributes.Img.Alt, v);
    public static ISetValue<TMessage, Hierarchy.Img, string> CrossOrigin(string v) => Set_string(Attributes.Img.CrossOrigin, v);
    public static ISetValue<TMessage, Hierarchy.Img, bool> IsMap(bool v) => Set_bool(Attributes.Img.IsMap, v);
    public static ISetValue<TMessage, Hierarchy.Img, string> LongDesc(string v) => Set_string(Attributes.Img.LongDesc, v);
    public static ISetValue<TMessage, Hierarchy.Img, string> Sizes(string v) => Set_string(Attributes.Img.Sizes, v);
    public static ISetValue<TMessage, Hierarchy.Img, string> Src(string v) => Set_string(Attributes.Img.Src, v);
    public static ISetValue<TMessage, Hierarchy.Img, string> SrcSet(string v) => Set_string(Attributes.Img.SrcSet, v);
    public static ISetValue<TMessage, Hierarchy.Img, string> UseMap(string v) => Set_string(Attributes.Img.UseMap, v);
    public static ISetValue<TMessage, Hierarchy.Img, string> Vspace(string v) => Set_string(Attributes.Img.Vspace, v);
    public static ISetValue<TMessage, Hierarchy.Img, string> Width(string v) => Set_string(Attributes.Img.Width, v);
    public static IView<TMessage> Img(params IValue<TMessage, Hierarchy.Img>[] values) => Create_View<Hierarchy.Img>("img", values);

    // H1
    public static ContainerView<TMessage> H1(params IValue<TMessage, Hierarchy.H1>[] values) => Create_ContainerView<Hierarchy.H1>("h1", values);

    // H2
    public static ContainerView<TMessage> H2(params IValue<TMessage, Hierarchy.H2>[] values) => Create_ContainerView<Hierarchy.H2>("h2", values);

    // H3
    public static ContainerView<TMessage> H3(params IValue<TMessage, Hierarchy.H3>[] values) => Create_ContainerView<Hierarchy.H3>("h3", values);

    // H4
    public static ContainerView<TMessage> H4(params IValue<TMessage, Hierarchy.H4>[] values) => Create_ContainerView<Hierarchy.H4>("h4", values);

    // H5
    public static ContainerView<TMessage> H5(params IValue<TMessage, Hierarchy.H5>[] values) => Create_ContainerView<Hierarchy.H5>("h5", values);

    // Hr
    public static IView<TMessage> Hr(params IValue<TMessage, Hierarchy.Hr>[] values) => Create_View<Hierarchy.Hr>("hr", values);

    // Input
    public static ISetValue<TMessage, Hierarchy.Input, string> Placeholder(string v) => Set_string(Attributes.Input.Placeholder, v);
    public static ISetValue<TMessage, Hierarchy.Input, string> Value(string v) => Set_string(Attributes.Input.Value, v);
    public static IView<TMessage> Input(params IValue<TMessage, Hierarchy.Input>[] values) => Create_View<Hierarchy.Input>("input", values);

    // P
    public static ContainerView<TMessage> P(params IValue<TMessage, Hierarchy.P>[] values) => Create_ContainerView<Hierarchy.P>("p", values);

    // Span
    public static ContainerView<TMessage> Span(params IValue<TMessage, Hierarchy.Span>[] values) => Create_ContainerView<Hierarchy.Span>("span", values);

  }
}

