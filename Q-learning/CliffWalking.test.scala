//> using target { scope "test" }
//> using lib "org.scalacheck::scalacheck:1.16.0"
//> using lib "org.scalactic::scalactic:3.2.14"

package rl.qLearning

import org.scalacheck.{Arbitrary, Prop, Test, Gen}
import org.scalacheck.Prop.*
import org.scalactic.Equality

object CliffWalkingSpec extends org.scalacheck.Properties("CliffWalking"):
    val stateGen = for {
        n <- Gen.choose(1, 2)
        m <- Gen.choose(1, 10)
    } yield (n, m)

    property("finds the correct terminal state") =
        forAll (stateGen, Gen.choose(0.2, 0.5), Gen.choose(0.1, 0.4), Gen.choose(300, 750)) { 
            (terminalState, learningRate, epsilon, numberOfEpisodes) =>
                val cliffWalker = new CliffWalker(terminalState, learningRate)
                val policy = new QLearning(cliffWalker, numberOfEpisodes, epsilon).execute
                
                cliffWalker.followPolicy(policy) == terminalState
        }

    property("calling state.nextState results in a state transition") =
        forAll (stateGen, Gen.oneOf(Action.Up, Action.Down, Action.Left, Action.Right)) { 
            (currentState, action) => currentState.nextState(action) != currentState
        }

    property("stateTransition returns the correct next state") = 
        given Arbitrary[Double] = Arbitrary { Gen.choose(0, 0.9) }
        given Arbitrary[State] = Arbitrary { stateGen }
        forAll { (currentState: State, up: Double, down: Double, left: Double, right: Double) =>
            val cliffWalker = new CliffWalker((0, 11), 0.2)
            val (initialState, qTable) = cliffWalker.initialize
            qTable + (currentState -> Map(Action.Up -> up, Action.Down -> down, Action.Left -> left, Action.Right -> right))
            val (nextState, _) = cliffWalker.stateTransition(currentState, qTable, false)

            currentState.nextState(qTable.bestAction(currentState)) == nextState
        }

    property("isTerminalState returns true when state is terminal") = 
        forAll (stateGen) { terminalState => 
            new CliffWalker(terminalState, 0.2).isTerminalState(terminalState) == true
        }