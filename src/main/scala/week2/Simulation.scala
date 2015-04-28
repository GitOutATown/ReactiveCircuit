package week2

// Root of class hierarchy
abstract class Simulation {

  /* TODO: To get a clearer understanding of the constraints and 
   * characteristics of this pattern, perform some experiments and 
   * diagnostics on this type. */ 
  type Action = () => Unit
  
  // And Event is an Action that takes place at a specific point in time.
  case class Event(time: Int, action: Action)

  /* TODO: What is the purpose of curtime?
   * How is curtime being updated/incremented? */ 
  private var curtime = 0 
  def currentTime: Int = curtime

  // And agenda is the list of events that make up the simulation.
  private var agenda: List[Event] = List()

  // 'insert' orders events by time. Called by 'afterDelay'. Recursive.
  /* TODO: So would this constitute a priority queue? 
   * Review PriorityQue assignment to ascertain connection, comparison,
   * contrast, relevance. 
   * TODO: Is agenda passed by name or reference? How is it being updated? */ 
  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest if first.time <= item.time => first :: insert(rest, item)
    case _ => item :: ag
  }

  /* Inserts an event into the agenda. Called from Gate actions 
   * and used by run to initialize simulation.
   * Purpose is to queue actions according to time (priority).
   * TODO: Need to develop a better understanding of time mechanism.
   * block is a "call by name" parameter.
   */
  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => {
      /* block manifests action behavior: i.e. signal transformation and 
       * propagation.
       * TODO: Need to clarify understanding of how the block parameter's 
       * Unit function construction (with call-by-name) works. 
       */
      block
    })
    /* priority queue of Event objects with action behavior bound to Wires 
    	 * according to Circuit and Gate composition. 
    	 */
    agenda = insert(agenda, item) 
  }

  def run() {
    afterDelay(0) {
      // block
      println("*** simulation started, time = " + currentTime + " ***")
    } 
    loop()
  }

  /* Agenda is composed by ... */
  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }
}
