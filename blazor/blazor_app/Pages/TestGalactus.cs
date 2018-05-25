namespace blazor_app.Pages
{
  using Galactus;
  using static global::Galactus.Formlet.Core;

  using System;
  using static Galactus.Views<Message>;
  using Microsoft.FSharp.Core;
  using Microsoft.AspNetCore.Blazor.RenderTree;
    using Microsoft.AspNetCore.Blazor;

    public class Message
  {

  }

  public static class Test
  {
    public static readonly IView<Message> MiniView        = Div(Class("Test"))();
    public static readonly IView<Message> View            = Create();
    public static readonly IView<Message> CalculatorView  = CreateCalculator(new CalculatorModel());

    public static IView<Message> Create()
    {
      IView<Message> Label(string txt) => Div(Class("my-label"))(Text(txt));
      IView<Message> Paragraph(string txt) => P(Class("my-paragraph"))(Text(txt));
      IView<Message> Chapter(string label, string txt) => Group(Label(label), Paragraph(txt));
      return
        Div
          ()
          ( Chapter("Hello", "There!")
          , Chapter("Hello", $"Again!")
          , Input(Class("my-input"), OnChange(v => Console.WriteLine($"OnChange(1): {v}")))
          , Input(Class("my-input"), OnChange(v => Console.WriteLine($"OnChange(2): {v}")))
          );
    }

    public class CalculatorModel
    {
      public string FirstNumber   = "123";
      public string SecondNumber  = "456";
      public string ResultNumber  = "";
    }

    public static IView<Message> CreateCalculator(CalculatorModel model)
    {
      IView<Message> FormInput(string label, string value) => Group
            ( Div(Class("row"))
              (Div(Class("col-sm-3"))(P()(Text(label)))
              , Div(Class("col-sm-4"))(Input(Placeholder($"Enter {label}"), Value(value)))
              )
            , Br()
            );
      IView<Message> FormButton(string label) => Div(Class("col-sm-2"))
            (Button(Class("btn"))(Text(label)));

      return
        Group
        ( H1()(Text("Basic Calculator Demo Using Blazor"))
        , Hr()
        , Div()
          ( FormInput("First number" , model.FirstNumber)
          , FormInput("Second number", model.SecondNumber)
          , FormInput("Result"       , model.ResultNumber)
          , Div(Class("row"))
            ( FormButton("Add (+)")
            , FormButton("Subtract (-)")
            , FormButton("Multiply (*)")
            , FormButton("Divide (/)")
            )
          )
        );
    }
  }

  public static class Test2
  {
    public class Builder : FormletRenderTreeBuilder
    {
      readonly RenderTreeBuilder m_builder;

      int seq = 0;

      public Builder(RenderTreeBuilder builder)
      {
        m_builder = builder;
      }

      public void AddAttribute(string key, string value)
      {
        m_builder.AddAttribute(seq++, key, value);
      }

      public void AddContent(string content)
      {
        m_builder.AddContent(seq++, content);
      }

      public void CloseElement()
      {
        m_builder.CloseElement();
      }

      public void OpenElement(string tag, FormletElementDecorators feds)
      {
        m_builder.OpenElement(seq++, tag);
        var list = feds.Item;
        while (!list.IsEmpty)
        {
          var fed = list.Head;
          m_builder.AddAttribute(seq++, fed.Item1, fed.Item2);
          list = list.Tail;
        }
      }

      public void OpenValueElement(string tag, FormletElementDecorators feds, FSharpRef<string> rval)
      {
        OpenElement(tag, feds);

        UIEventHandler handler = args =>
        {
          var a = args as UIChangeEventArgs;
          if (a != null)
          {
            var v = a.Value as string ?? "";
            rval.Value = v;
          }
        };

        m_builder.AddAttribute(seq++, "onchange", handler);
      }
    }

    public static RenderFragment Create<T>(Formlet<T> formlet)
    {
      return b => {
        var builder = new Builder(b);
        Action<NotifyType> notify = nt =>
        {
          Console.WriteLine($"Notify: {nt}");
        };
        var tr = global::Galactus.Formlet.Core.Formlet.buildUp(builder, notify, formlet, FormletTree.Empty);
        Console.WriteLine($"FormletResult: {tr}");
      };
    }

  }

}
