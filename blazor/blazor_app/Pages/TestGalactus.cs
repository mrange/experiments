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
    public static readonly IView<Message> MiniView = Div(Class("Test"))();
    public static readonly IView<Message> View = Create();

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
          , Input(Class("my-input"), OnChange(v => Console.WriteLine($"OnChange: {v}")))
          );
    }
  }
}
