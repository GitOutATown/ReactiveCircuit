package week2

object test2 {

  def main(args: Array[String]): Unit = {
	    
	  object sim extends Circuits with Parameters
	  import sim._
	  
	  // Wire constructor takes no params.
	  /* Note that Wire and Event are the only classes being instantiated 
	   * in the simulation application. */
	  val in1, in2, sum, carry = new Wire             
	
	  /* Ciruits (eg. halfAdder) compose gates (Circuits extends the Gates
	   * class.). Gates compose Wires/Actions. The Wire definition is nested 
	   * inside the Gates abstract definition (which itself is never instantiated.
	   * what are referred to as gates are actually several defs 
	   * (i.e. inverter, andGate, orGate) that each define action functions
	   * which are bound to input Wires. Eventually actions are consumed and
	   * queues by Event objects according to time priority.
	   */
	  halfAdder(in1, in2, sum, carry)
	  probe("sum", sum)                               
	  probe("carry", carry) // What does carry represent? What is it?                        
	  
	  /* Manual state inputs (changes) and runs (loop activations) */
	  
	  in1 setSignal true
	  run()
	  
	  in2 setSignal true
	  run()
	  
	  in1 setSignal false
	  run()
  }

}