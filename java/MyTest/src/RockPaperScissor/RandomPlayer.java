package RockPaperScissor;

import java.util.Random;

public class RandomPlayer implements Player {

	Random random = new Random();
	
	@Override
	public void close() throws Exception {
	}

	@Override
	public Play play() {
		int play = random.nextInt (3);
		
		return Game.IntToPlay (play);
	}

	@Override
	public void outcome(Play otherPlay, Outcome outcome) {
	}

	@Override
	public String getName() {
		return "Mr. Random";
	}
	
}