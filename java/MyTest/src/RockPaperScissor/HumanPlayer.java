package RockPaperScissor;

import java.util.Scanner;

public class HumanPlayer implements Player {

	final String name;
	final Scanner scanner;
	
	public HumanPlayer(String nm, Scanner sc) {
		name = nm;
		scanner = sc;
	}

	@Override
	public void close() throws Exception {
		scanner.close();
	}

	@Override
	public Play play() {
		
		System.out.println("Do your play (0 - Rock, 1 - Paper, 2 - Scissor");
		int play = scanner.nextInt();

		return Game.IntToPlay(play);
	}

	@Override
	public void outcome(Outcome outcome) {
	}

	@Override
	public String getName() {
		return name;
	}
	
}