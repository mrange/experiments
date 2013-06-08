package RockPaperScissor;

public interface Player extends AutoCloseable {
	Play play ();
	
	void outcome (Outcome outcome);

	String getName ();
}