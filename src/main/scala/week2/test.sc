package week2

object test {

  object sim extends Circuits with Parameters
  import sim._
  
  val in1, in2, sum, carry = new Wire             //> in1  : week2.test.sim.Wire = week2.Gates$Wire@291aa152
                                                  //| in2  : week2.test.sim.Wire = week2.Gates$Wire@43f3bb61
                                                  //| sum  : week2.test.sim.Wire = week2.Gates$Wire@36930021
                                                  //| carry  : week2.test.sim.Wire = week2.Gates$Wire@6ced9284

  halfAdder(in1, in2, sum, carry)
  probe("sum", sum)                               //> sum 0 new-value = false
  probe("carry", carry)                           //> carry 0 new-value = false
  
  in1 setSignal true
  run()                                           //> *** simulation started, time = 0 ***
                                                  //| sum 5 new-value = true
                                                  //| sum 10 new-value = false
                                                  //| sum 10 new-value = true
          
  in2 setSignal true
  run()                                           //> *** simulation started, time = 10 ***
                                                  //| carry 13 new-value = true
                                                  //| sum 18 new-value = false
  in1 setSignal false
  run()                                           //> *** simulation started, time = 18 ***
                                                  //| carry 21 new-value = false
                                                  //| sum 26 new-value = true
  
  '''                                             //> res0: Char('\'') = '
}
/*






*/