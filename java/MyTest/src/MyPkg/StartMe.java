package MyPkg;

import RockPaperScissor.*;

public class StartMe {
	
	
	
	public static void main(String[] args) {
		Game game = new Game();
		
		try {
			
			PlayerFactory red = new HumanPlayerFactory("M�rten");
			PlayerFactory blue = new RandomPlayerFactory(); 
			
			game.determineWinner(
					3, 
					red,
					blue
					);
		}
		catch (Exception exc) {
			System.out.printf("Exception: %0", exc);
		}
		
		
		
		
	}

}
