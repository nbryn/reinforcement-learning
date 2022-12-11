package rl.qLearning

@main def run() = 
    val cliffWalker = new CliffWalker((0, 11), 0.2, true)
    new QLearning(cliffWalker, 400, 0.1).execute