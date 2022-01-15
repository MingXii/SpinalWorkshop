package workshop.prime

import spinal.core._
import spinal.lib._


object Prime{
  //Pure scala function which return true when the number is prime
  def apply(n : Int) =  ! ((2 until n-1) exists (n % _ == 0))

  //Should return True when the number is prime.
  def apply(n : UInt) : Bool = {
    //TODO
    (0 until 1<<widthOf(n)).filter(i => Prime(i)).map(j => j===n).orR
  }
}


