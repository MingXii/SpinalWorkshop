package workshop.pwm

import org.scalatest.FunSuite
import spinal.core._
import spinal.lib._

//APB configuration class (generic/parameter)
case class ApbConfig(addressWidth : Int,
                     dataWidth    : Int,
                     selWidth     : Int)

//APB interface definition
case class Apb(config: ApbConfig) extends Bundle with IMasterSlave {
  //TODO define APB signals
  val PSEL = Bits(config.selWidth bits)
  val PENABLE = Bool()
  val PWRITE = Bool()
  val PADDR = UInt(config.addressWidth bits)
  val PWDATA = Bits(config.dataWidth bits)
  val PRDATA = Bits(config.dataWidth bits)
  val PREADY = Bool()

  override def asMaster(): Unit = {
    //TODO define direction of each signal in a master mode
    out(PADDR,PSEL,PENABLE,PWRITE,PWDATA)
    in(PREADY,PRDATA)
  }
}

case class ApbPwm(apbConfig: ApbConfig,timerWidth : Int) extends Component{
  require(apbConfig.dataWidth == 32)
  require(apbConfig.selWidth == 1)

  val io = new Bundle{
    val apb = slave(Apb(apbConfig)) //TODO
    val pwm = out Bool()//TODO
  }

  val logic = new Area {
    //TODO define the PWM logic
    val enable = Reg(Bool()) init(False)
    val dutyCycle = Reg(UInt(timerWidth bits)) init(0)
    val timer = Reg(UInt(timerWidth bits)) init(0)
    val output = Reg(Bool()) init(False)
    when(enable) {
      timer := timer + 1
    }
    when(timer < dutyCycle) {
      output := True
    }.otherwise{
      output := False
    }

    io.pwm := output
  }

  val control = new Area {
    //TODO define the APB slave logic that will make PWM's registers writable/readable
      io.apb.PREADY := True
      io.apb.PRDATA := 0  //初始化
      switch(io.apb.PADDR) {
        is(0) {
          io.apb.PRDATA(0) := logic.enable
          when(io.apb.PWRITE && io.apb.PENABLE && io.apb.PSEL(0)) {
            logic.enable := io.apb.PWDATA(0)
          }
        }
        is(4) {
          io.apb.PRDATA := logic.dutyCycle.asBits.resized
          when(io.apb.PWRITE && io.apb.PENABLE && io.apb.PSEL(0)) {
            logic.dutyCycle := io.apb.PWDATA.asUInt.resized
          }
        }
       default {
          logic.enable := logic.enable           //reg
          logic.dutyCycle := logic.dutyCycle
      }
    }
  }
}