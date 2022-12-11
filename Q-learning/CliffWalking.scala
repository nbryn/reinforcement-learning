package rl.qLearning

import scala.math.max
import scala.util.Random

enum Action:
    case Up, Down, Left, Right


val cliff = List((0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 9), (0, 10))
type State = (Int, Int)
extension(state: State)
    def reward = if cliff.contains(state) then -100 else -1

    def nextState(action: Action): State =
        action match
            case Action.Up    => (Math.min(3, state._1+1), state._2)
            case Action.Down  => (Math.max(0, state._1-1), state._2)
            case Action.Left  => (state._1, Math.max(0, state._2-1))
            case Action.Right => (state._1, Math.min(11, state._2+1))

   
type CliffTable = QTable[State, Action]
extension(table: CliffTable)
    def bestAction(state: State) = table(state).maxBy(_._2)._1

    def bestQVal(state: State) = table(state).maxBy(_._2)._2

    def update(state: State, action: Action, value: Double) = table + (state -> (table(state) + (action -> value)))

object CliffTable:
    def apply() = Map[State, Map[Action, Double]]()

type CliffPolicy = Policy[State, Action]

class CliffWalker(terminalState: State, learningRate: Double, printPolicy: Boolean = false) extends Agent[State, Action]:
    val initialState = (0, 0)
    def initialize: (State, CliffTable) = (initialState, initializeQTable(12, 12).asInstanceOf[CliffTable])
    
    def isTerminalState(state: State) = state == terminalState || cliff.contains(state)

    def stateTransition(state: State, qTable: CliffTable, random: Boolean): (State, QTable[State, Action]) =
        updateQTable(state, qTable, if random then randomAction else qTable.bestAction(state)) 
    
    def extractPolicy(qTable: CliffTable) =
        if printPolicy then println("Road to goal state:")
        def go(state: State, policy: CliffPolicy): CliffPolicy =
            if printPolicy then println(state)
            if state == terminalState then policy
            else
                val action = qTable.bestAction(state)
                go(state.nextState(action), policy + (state -> action))
        
        go(initialState, Map())

    def followPolicy(policy: CliffPolicy) =
        def go(state: State): State =
            if state == terminalState then state
            else go(state.nextState(policy(state)))

        go(initialState)

    private def updateQTable(state: State, qTable: CliffTable, action: Action): (State, QTable[State, Action]) =
        val nextState = state.nextState(action)
        val currentQVal = qTable(state)(action)
        val updatedQvalue = currentQVal + learningRate*(nextState.reward + qTable.bestQVal(nextState) - currentQVal)

        (nextState, qTable.update(state, action, updatedQvalue))
    
    private def randomAction =
        new Random().nextInt(4) match
            case 0 => Action.Up
            case 1 => Action.Down
            case 2 => Action.Left
            case 3 => Action.Right

    private def initializeQTable(rows: Int, columns: Int) =
        var table = CliffTable()
        for (r <- 0 to rows) {
            for (c <- 0 to columns) {
                table = table + ((r, c) -> Map(Action.Up -> 0.0, Action.Down -> 0.0, Action.Left -> 0.0, Action.Right -> 0.0))
            }
        } 
        
        table

