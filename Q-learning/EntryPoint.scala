import rl.qLearning.main.QLearning
import rl.qlearning.cliffWalking.CliffWalker
import rl.qlearning.cliffWalking.State

@main def start() = 
    val cliffWalker = new CliffWalker(State(0, 11), 0.1)
    new QLearning(cliffWalker, 400, 0.1).execute()