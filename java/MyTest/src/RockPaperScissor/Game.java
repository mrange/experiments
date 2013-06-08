package RockPaperScissor;


public class Game {

	static final Outcome[][] outcomes = new Outcome[][] {
		new Outcome[] {Outcome.NoOneWins	, Outcome.RedWins 	,Outcome.BlueWins	}, 
		new Outcome[] {Outcome.BlueWins		, Outcome.NoOneWins	,Outcome.RedWins	}, 
		new Outcome[] {Outcome.RedWins 		, Outcome.BlueWins	,Outcome.NoOneWins	}, 
	};

	static Play IntToPlay (int play) {
		switch (play) {
		case 0:
			return Play.Rock;
		case 1: 
			return Play.Paper;
		case 2:
			return Play.Scissor;
		default:
			return Play.Rock;
		}
		
	}
	
	static int PlayToInt (Play play) {
		switch (play)  {
		case Rock:
			return 0;
		case Paper:
			return 1;
		case Scissor:
			return 2;
		default:
			return -1;
		}
		
	}
	
	public static Outcome DetermineOutcome (Play redPlay, Play bluePlay) {
		return outcomes[PlayToInt(bluePlay)][PlayToInt(redPlay)];
	}
 
	public Outcome determineWinner (int rounds, PlayerFactory red, PlayerFactory blue) throws Exception {
		int redScore 	= 0;
		int blueScore 	= 0;

		try (	Player redPlayer = red.create();
				Player bluePlayer = blue.create();
				) {
			
			System.out.printf("The game is Rock, Paper, Scissor - best out of %1$d rounds takes the game\n", rounds);
			
			System.out.printf("In the red corner: %1$s\n", redPlayer.getName());
			System.out.printf("In the blue corner: %1$s\n", bluePlayer.getName());
			
			
			for (int round = 0; round < rounds; ++round) {
				System.out.printf("Round %1$d - Ready, set .... Rock!\n", round + 1);
				
				Play redPlay = redPlayer.play();
				Play bluePlay = bluePlayer.play();
				
				System.out.printf("Red plays %1$s\n", redPlay);
				System.out.printf("Blue plays %1$s\n", bluePlay);
				
				Outcome outcome = DetermineOutcome(redPlay, bluePlay);
				
				switch (outcome) {
				case BlueWins:
					System.out.println("Blue player took the round");
					++blueScore;
					break;
				case RedWins:
					System.out.println("Red player took the round");
					++redScore;
					break;
				default:
					System.out.println("It's a tie");
					break;
				}
				
				bluePlayer.outcome(redPlay, outcome);
				redPlayer.outcome(bluePlay, outcome);
				
			}
		}
		

		if (redScore < blueScore) {
			System.out.printf("Blue player wins! Score %1$d - %2$d\n", redScore, blueScore);
			return Outcome.BlueWins;
		} else if (redScore > blueScore) {
			System.out.printf("Red player wins! Score %1$d - %2$d\n", redScore, blueScore);
			return Outcome.RedWins;
		} else {
			System.out.printf("It's a tie! Score %1$d - %2$d\n", redScore, blueScore);
			return Outcome.NoOneWins;
		}
		
	}
	
}
