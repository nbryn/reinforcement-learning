package rl.qLearning.main

import rl.qlearning.random.RNG
import rl.qlearning.cliffWalking.CliffWalker

type QTable[State, Action] = Map[State, Map[Action, Double]]
object QTable:
    def apply[State, Action](): QTable[State, Action] = Map()

type Policy[State, Action] = Map[State, Action]
object Policy:
    def apply[State, Action](): Policy[State, Action] = Map()

opaque type Reward = Int

type Parameter[State, Action] = (State, QTable[State, Action], RNG)

trait Agent[State, Action]:
    def initialize(): (State, QTable[State, Action])
    def stateTransition(state: State, table: QTable[State, Action], random: Boolean): (State, QTable[State, Action])
    def isTerminalState(state: State): Boolean
    def printPolicy(state: State, qTable: QTable[State, Action], policy: Policy[State, Action]): Unit

class QLearning[State, Action](agent: Agent[State, Action], numberOfEpisodes: Int, epsilon: Double) {
    def execute() =
        val (startState, initialQTable) = agent.initialize()
        val qTable = trainAgent((initialQTable, new RNG.Simple(30)), startState, 0)
        agent.printPolicy(startState, qTable, Policy())

    private def trainAgent(params: (QTable[State, Action], RNG), state: State, episodeCount: Int): QTable[State, Action] =
        if episodeCount == numberOfEpisodes then params(0)
        else trainAgent(episode(state, params(0), params(1)), state, episodeCount + 1)

    private def episode(params: Parameter[State, Action]): (QTable[State, Action], RNG) =
        if agent.isTerminalState(params(0)) then (params(1), params(2))
        else episode(epoch(params))

    private def epoch(params: Parameter[State, Action]) =
        val (double, newRng) = RNG.double(params(2))
        val (newState, updatedQTable) = agent.stateTransition(params(0), params(1), double < epsilon)

        (newState, updatedQTable, newRng)
}

