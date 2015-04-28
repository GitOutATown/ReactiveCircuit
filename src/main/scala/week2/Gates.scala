package week2

abstract class Gates extends Simulation {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  // Generic construction, no params
  class Wire {

    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal = sigVal

    /*
     * Each time setSignal is called on an output Wire it causes all of 
     * the Actions registered for that output Wire to evaluate the state 
     * of their inputs and potentially call further setSignal transformations
     * on a cascading series of of actions. Actions propagate potentially 
     * from multiple inputs to multiple outputs.
     */
    def setSignal(s: Boolean) =
      if (s != sigVal) {
        sigVal = s
        actions foreach (_())
      }

    /* An Action is just a stripped down parameterless Unit function.
     * This pattern, rather than taking any paramaters or returning anything,
     * calls the function afterDelay, passing time and behavior (in the 
     * form of a "block"). By using afterDelay in this way, the action 
     * performs the signal transformation by drawing signals from the Gate's 
     * input Wires, performing a signal transformation according to the 
     * Gate's characteristic nature, and setting the transformed signal on
     * the output Wire.
     */
    def addAction(a: Action) = {
      actions = a :: actions
      a()
    }
  } // end Wire
  
  /////// GATES ///////
  
  /*
   * A Gate provides an interface to its input and output Wires, and an 
   * Action provides the Gate's behavior in respect to the Wires and the 
   * signals that are propagated through them. The Gate then binds the 
   * action to the input Wire(s). Actions perform signal transformation and 
   * propagation involving getSignal from inputs and setSignal for output. 
   */

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        // block
        output setSignal !inputSig
      }
    }
    input addAction invertAction
  }

  def andGate(in1: Wire, in2: Wire, output: Wire) = {
    def andAction() = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(AndGateDelay) {
        // block
        output setSignal (in1Sig & in2Sig)
      }
    }
    in1 addAction andAction
    in2 addAction andAction
  }

  /** Design orGate analogously to andGate */
  def orGateAlt(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction() = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(OrGateDelay) {
        // block
        output setSignal (in1Sig | in2Sig)
      }
    }
    in1 addAction orAction
    in2 addAction orAction
  }
  
  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      // block
      println(name + " " + currentTime + " new-value = " + wire.getSignal)
    }
    wire addAction probeAction
  }
  
  /** Design orGate in terms of andGate, inverter */
  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    val notIn1, notIn2, notOut = new Wire
    inverter(in1, notIn1)
    inverter(in2, notIn2)
    andGate(notIn1, notIn2, notOut)
    inverter(notOut, output)
  }
}
