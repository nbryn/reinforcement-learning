package rl.qLearning

import rl.qlearning.random.RNG

type StateActionRNG[State, Action] = (State, QTable[State, Action], RNG)

type QTable[State, Action] = Map[State, Map[Action, Double]]

type Policy[State, Action] = Map[State, Action]

trait Agent[State, Action]:
    def initialize: (State, QTable[State, Action])
    def stateTransition(state: State, table: QTable[State, Action], random: Boolean): (State, QTable[State, Action])
    def isTerminalState(state: State): Boolean
    def extractPolicy(qTable: QTable[State, Action]): Policy[State, Action]
    def followPolicy(policy: Policy[State, Action]): State

class QLearning[State, Action](agent: Agent[State, Action], numberOfEpisodes: Int, epsilon: Double) {
    def execute = agent.extractPolicy(trainAgent(agent.initialize))

    private def trainAgent(params: (State, QTable[State, Action])): QTable[State, Action] =
        def go(params: (QTable[State, Action], RNG), state: State, episodeCount: Int): QTable[State, Action] =
            if episodeCount == numberOfEpisodes then params(0)
            else go(episode(state, params(0), params(1)), state, episodeCount + 1)
        
        go((params(1), new RNG.Simple(30)), params(0), 0)

    private def episode(params: StateActionRNG[State, Action]): (QTable[State, Action], RNG) =
        if agent.isTerminalState(params(0)) then (params(1), params(2))
        else episode(epoch(params))

    private def epoch(params: StateActionRNG[State, Action]) =
        val (double, newRng) = RNG.double(params(2))
        val (newState, updatedQTable) = agent.stateTransition(params(0), params(1), double < epsilon)

        (newState, updatedQTable, newRng)
}

