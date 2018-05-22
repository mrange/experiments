namespace blazor_app.Pages
{
  using Galactus;
  using System;
  using static Galactus.Views<Message>;

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
          , Chapter("Hello", "Again!")
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
}
