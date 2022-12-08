package rl.qlearning.cliffWalking

import scala.math.max
import scala.util.Random
import rl.qLearning.main.Agent
import rl.qLearning.main.QTable
import rl.qLearning.main.Policy

opaque type State = (Int, Int)
object State:
    def apply(x: Int, y: Int): State = (x, y)

type CliffTable = QTable[State, Action]
object CliffTable:
    def apply() = QTable[State, Action]()

type CliffPolicy = Map[State, Action]

enum Action:
    case Up, Down, Left, Right

def initializeQTable(rows: Int, columns: Int): CliffTable =
    var table = CliffTable()
    for (r <- 0 to rows) {
        for (c <- 0 to columns) {
            table = table + ((r, c) -> Map(Action.Up -> 0.0, Action.Down -> 0.0, Action.Left -> 0.0, Action.Right -> 0.0))
        }
    } 
    
    table

def getNewState(state: State, action: Action): State =
    action match
        case Action.Up    => (Math.min(3, state._1+1), state._2)
        case Action.Down  => (Math.max(0, state._1-1), state._2)
        case Action.Left  => (state._1, Math.max(0, state._2-1))
        case Action.Right => (state._1, Math.min(11, state._2+1))

def randomAction(state: State) =
    new Random().nextInt(4) match
        case 0 => Action.Up
        case 1 => Action.Down
        case 2 => Action.Left
        case 3 => Action.Right

val cliff = List((0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 9), (0, 10))
def getReward(state: State) = if cliff.contains(state) then -100 else -1

def getBestAction(state: State, qTable: CliffTable) = qTable(state).maxBy(_._2)._1

def getBestActionVal(state: State, qTable: CliffTable) = qTable(state).maxBy(_._2)._2

class CliffWalker(terminalState: State, learningRate: Double) extends Agent[State, Action]: 
    def initialize(): (State, CliffTable) = (State(0, 0), initializeQTable(12, 12))
    
    def isTerminalState(state: State) = state == terminalState || cliff.contains(state)

    def stateTransition(state: State, qTable: CliffTable, random: Boolean) =
        updateQTable(state, qTable, if random then randomAction(state) else getBestAction(state, qTable)) 

    def printPolicy(state: State, qTable: CliffTable, policy: CliffPolicy) =
        println(state)
        if state == terminalState then ()
        else
            val action = getBestAction(state, qTable)
            printPolicy(getNewState(state, action), qTable, policy.updated(state, action))
    
    private def updateQTable(state: State, qTable: CliffTable, action: Action) =
        val newState = getNewState(state, action)
        val qValNextState = getBestActionVal(newState, qTable)
        val currentQVal = qTable(state)(action)
        val updatedQvalue = currentQVal + learningRate*(getReward(newState) + qValNextState - currentQVal)

        (newState, qTable.updated(state, qTable(state).updated(action, updatedQvalue)))