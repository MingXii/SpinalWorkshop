package workshop.function

import spinal.core._
import spinal.lib._


case class FunctionUnit() extends Component{
  val io = new Bundle{
    val cmd    = slave Flow(Bits(8 bits))
    val valueA = out Bits(8 bits)
    val valueB = out Bits(32 bits)
    val valueC = out Bits(48 bits)
  }

  def patternDetector(str : String) = new Area{
    val hit = False
    // TODO
    val cnt = Counter(str.length)
    when (io.cmd.valid){
      when ((0 until str.length).map(i => cnt === i && io.cmd.payload === str(i)).orR) {
        cnt.increment()
      }.otherwise{
        cnt.clear()
      }
       hit := cnt ===  str.length-1
      //hit := cnt.willOverflow
    }

  }

  def valueLoader(start : Bool,that : Data)= new Area{
    require(widthOf(that) % widthOf(io.cmd.payload) == 0) //You can make the assumption that the 'that' width is always an mulitple of 8
    // TODO
    val bytecnt = widthOf(that) / widthOf(io.cmd.payload)
    val hit = Reg(False) setWhen(start)
    val cnt = Counter(bytecnt)
    val data = Reg(Bits(widthOf(that) bits))
    when (hit && io.cmd.valid) {
      data.subdivideIn(bytecnt slices)(cnt) := io.cmd.payload
      hit.clearWhen(cnt.willOverflowIfInc)
      cnt.increment()
    }
    that := data
  }

  val setA    = patternDetector("setValueA")
  val loadA   = valueLoader(setA.hit,io.valueA)

  val setB    = patternDetector("setValueB")
  val loadB   = valueLoader(setB.hit,io.valueB)

  val setC    = patternDetector("setValueC")
  val loadC   = valueLoader(setC.hit,io.valueC)
}

