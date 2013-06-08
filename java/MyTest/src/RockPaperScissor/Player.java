package RockPaperScissor;

public interface Player extends AutoCloseable {
	Play play ();
	
	void outcome (Play otherPlay, Outcome outcome);

	String getName ();
}