package workshop.uart

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class UartRxGenerics( preSamplingSize: Int = 1,
                           samplingSize: Int = 5,
                           postSamplingSize: Int = 2){

  val rxdSamplePerBit = preSamplingSize + samplingSize + postSamplingSize
  require(isPow2(rxdSamplePerBit))

  if ((samplingSize % 2) == 0)
    SpinalWarning(s"It's not nice to have a even samplingSize value at ${ScalaLocated.short} (because of the majority vote)")
}

case class UartCtrlRx(generics : UartRxGenerics) extends Component{
  import generics._  //Allow to directly use generics attribute without generics. prefix
  val io = new Bundle{
    val rxd  = in Bool
    val samplingTick = in Bool
    val read = master Flow(Bits(8 bits))
  }

  // Implement the rxd sampling with a majority vote over samplingSize bits
  // Provide a new sampler.value each time sampler.tick is high
  val sampler = new Area {
    val samples = History(
      that  = io.rxd,
      range = 2 until 2+samplingSize,
      when  = io.samplingTick,
      init  = True
    )
    val value       = RegNext(MajorityVote(samples)) init(True)
    val tick        = RegNext(io.samplingTick) init(False)
  }

  // Provide a bitTimer.tick each rxSamplePerBit
  // reset() can be called to recenter the counter over a start bit.
  val bitTimer = new Area {
    val counter  = Reg(UInt(log2Up(rxdSamplePerBit) bit))
    val recenter = False
    val tick = False
    when(sampler.tick) {
      counter := counter - 1
      when(counter === 0) {
        tick := True
      }
    }
    when(recenter){
      counter := preSamplingSize + (samplingSize - 1) / 2 - 1
    }
  }

  // Provide bitCounter.value that count up each bitTimer.tick, Used by the state machine to count data bits and stop bits
  // reset() can be called to reset it to zero
  val bitCounter = new Area {
    val value = Reg(UInt(3 bits))
    val clear = False

    when(bitTimer.tick) {
      value := value + 1
    }
    when(clear){
      value := 0
    }
  }

  // Statemachine that use all precedent area
  val stateMachine = new Area {
    //TODO state machine

/*    val buffer = Reg(Bits(8 bits))
    io.read.payload := buffer
    io.read.valid := False*/

    val fsm = new StateMachine {
      val buffer = Reg(Bits(8 bits))
      io.read.payload := buffer
      io.read.valid := False
      val IDLE : State = new State with EntryPoint {
        whenIsActive{
          buffer := 0
          when (sampler.tick && !sampler.value) {
            bitTimer.recenter := True
            goto(START)
          }
        }
      }
      val START : State = new State {
        whenIsActive{
          when (bitTimer.tick) {
            bitCounter.clear := True
            goto(DATA)
          }
        }
      }
      val DATA : State = new State {
        whenIsActive{
          when(bitTimer.tick ) {
              buffer(bitCounter.value) := sampler.value
           when(bitCounter.value === 7){
              goto(STOP)
            }
          }
        }
      }
      val STOP : State = new State {
        whenIsActive{
          when(bitTimer.tick) {
            goto (IDLE)
            io.read.valid := True
          }
        }
      }
    }
  }
}
