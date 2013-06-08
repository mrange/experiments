package RockPaperScissor;

public class RandomPlayerFactory implements PlayerFactory {

	@Override
	public Player create() {
		return new RandomPlayer();
	}
	
}