namespace blazor_app.Pages
{

  using System;
  using static Flazor.Elmish.Views<Message>;
  using Microsoft.AspNetCore.Blazor;
  using Flazor.Elmish;
  using Flazor.Formlets;
  using Flazor.Formlets.Bootstrap;

  using static Flazor.Formlets.Tags;

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

  public class Address
  {
    public readonly string CarryOver  ;
    public readonly string Line1      ;
    public readonly string Line2      ;
    public readonly string Line3      ;
    public readonly string Zip        ;
    public readonly string City       ;
    public readonly string County     ;
    public readonly string Country    ;

    public Address(
        string carryOver
      , string line1
      , string line2
      , string line3
      , string zip
      , string city
      , string county
      , string country
      )
    {
      CarryOver = carryOver ;
      Line1     = line1     ;
      Line2     = line2     ;
      Line3     = line3     ;
      Zip       = zip       ;
      City      = city      ;
      County    = county    ;
      Country   = country   ;
    }

    public override string ToString()
    {
      return $"(Address, {CarryOver}, {Line1}, {Line2}, {Line3}, {Zip}, {City}, {County}, {Country})";
    }
  }

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

  }

  public static class Test2
  {

    public static RenderFragment Create()
    {
      Action<NotifyType> notify = nt =>
      {
        Console.WriteLine($"Notify: {nt}");
      };

      Formlet<string> LabeledInput(string id, string label, FormletValidator<string> validator) => 
        Input(label, "").Validate(validator).WithValidation().WithClass("form-control").WithId(id).WithLabel(id, label);

      Formlet<string> NotEmpty(string id, string label) => LabeledInput(id, label, Formlet.ValidatorNotEmpty);
      Formlet<string> Any(string id, string label) => LabeledInput(id, label, Formlet.ValidatorNop<string>());

      var ts = FormletState.Empty.Value;

      /*
      var t = Formlet.Map(
          Any       ("id-co"      , "C/O"     )
        , NotEmpty  ("id-line1"   , "Line 1"  )
        , Any       ("id-line2"   , "Line 2"  ) 
        , Any       ("id-line3"   , "Line 3"  )
        , NotEmpty  ("id-zip"     , "Zip"     )
        , NotEmpty  ("id-city"    , "City"    )
        , Any       ("id-county"  , "County"  )
        , NotEmpty  ("id-country" , "Country" )
        , (co, l1, l2, l3, z, c1, c2, c3) => new Address(co, l1, l2, l3, z, c1, c2, c3)
        );
      */

      var fr = Reproduce.Map(() => "1", () => "2", () => "3", () => "4", (x, y, z, w) => (x, y, z, w))();

      Console.WriteLine($"FR: {fr}");

      var t = Formlet.Map(
          Any("id-hello", "Hello")
        , Any("id-there", "There")
        , Any("id-gg", "GG")
//        , Any("id-hh", "HH")
        , (x, y, z) => (x, y, z)
        );
        /*
      var t = 
        Any("id-hello", "Hello")
        .AndAlso(Any("id-there", "There"))
        .AndAlso(Any("id-gg", "GG"))
//        .AndAlso(Any("id-hh", "HH"))
        .Map(v => (v.left.left, v.left.right, v.right))
        ;
        */

      return b => {
        var tr = t.BuildUp(b, notify, ts);

        ts = tr.State;

        Console.WriteLine($"Value: {tr.Value}");
        Console.WriteLine($"FailureState: {tr.FailureState}");
        Console.WriteLine($"VisualState: {tr.VisualState}");
        Console.WriteLine($"State: {tr.State}");
      };
    }

  }

}
