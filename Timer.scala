package workshop.timer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory


case class Timer(width : Int) extends Component {
  val io = new Bundle {
    val tick = in Bool
    val clear = in Bool
    val limit = in UInt (width bits)

    val full  = out Bool
    val value = out UInt (width bits)
  }

  //TODO phase 1
  val cnt = Reg(UInt(width bits))
    when(io.clear) {
    cnt := 0
  }.elsewhen(cnt === io.limit) {
    cnt := cnt
  }.elsewhen(io.tick) {
    cnt := cnt + 1
  }.otherwise{
    cnt := cnt
  }

  io.full := cnt === io.limit
  io.value := cnt

  def driveFrom(busCtrl : BusSlaveFactory,baseAddress : BigInt)(ticks : Seq[Bool],clears : Seq[Bool]) = new Area {
    //TODO phase 2
    val ticksEnalbe = busCtrl.createReadAndWrite(dataType = Bits(ticks.length bits),address = baseAddress + 0,bitOffset = 0) init(0)
    val clearsEnable = busCtrl.createReadAndWrite(dataType = Bits(clears.length bits),address = baseAddress + 0,bitOffset = 16) init(0)

    io.tick := (ticksEnalbe & ticks.asBits()).orR
    io.clear := (clearsEnable & clears.asBits()).orR | busCtrl.isWriting(baseAddress+4) | busCtrl.isWriting(baseAddress+8)

    busCtrl.driveAndRead(io.limit,baseAddress+4,0)
    busCtrl.read(io.value,baseAddress+8,0)

  }
}