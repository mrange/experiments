package RockPaperScissor;

import java.util.Scanner;

public class HumanPlayerFactory implements PlayerFactory {

	String name;
	
	public HumanPlayerFactory (String nm) {
		name = nm;
	}
	
	@Override
	public Player create() {
		return new HumanPlayer(name, new Scanner(System.in));
	}
	
}