/*
package workshop.waveplayer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory

//Class used to parametrized the WavePlayer instanciation
case class WavePlayerGenerics(                                //定义参数
                               sampleWidth : Int,
                               sampleCountLog2 : Int,
                               phaseWidth : Int,
                               filterCoefWidth : Int
                             ){
  def Sample = UInt(sampleWidth bits)
  def sampleCount = 1 << sampleCountLog2
}

//Area of logic which implement a phase counter, a rom sampler.
case class WavePlayer(generics : WavePlayerGenerics) extends Area{
  import generics._
  assert(phaseWidth >= sampleCountLog2) //断言

  val phase = new Area{
    val run = Bool  //Driven later by a WavePlayerMapper (see WavePlayerMapper implementation)
    val rate = UInt(phaseWidth bits) //Driven later by a WavePlayerMapper

    //TODO phase
    val value =Reg(UInt(phaseWidth bits)) init(0)
    when(run) {
      value := value +rate
    }
  }

  val sampler = new Area{
    //TODO Rom definition with a sinus + it sampling
    val romSamples = for(sampleId <- 0 until sampleCount) yield{
      val sin = Math.sin(2.0*Math.PI*sampleId/sampleCount)
      val normalizedSin =(0.5*sin +0.5) * (Math.pow(2.0,sampleWidth)-1)  //Math.pow 取幂
      BigInt(normalizedSin.toLong)
    }
    val rom = Mem(Sample, sampleCount) initBigInt(romSamples)  //rom初始化把romSamples送入
    val sample = rom.readAsync(phase.value >> (phaseWidth - sampleCountLog2))  //右移  phase.value/(phaseWidth/sampleCountLog2)
  }

  val filter = new Area{
    val bypass = Bool //Driven later by a WavePlayerMapper
    val coef = UInt(filterCoefWidth bits) //Driven later by a WavePlayerMapper
    //val value = Sample //Output value of the filter Area
    //TODO first order filter + bypass logic
    val accumulator = Reg(UInt(sampleWidth + filterCoefWidth bits)) init(0)
    accumulator := accumulator - (accumulator*coef >> filterCoefWidth) + sampler.sample * coef
    val filtredSampler = accumulator >> filterCoefWidth
    val value = bypass ? sampler.sample | filtredSampler
  }
}

//Area capable to map a WavePlayer on a BusSlaveFactory
class WavePlayerMapper(bus : BusSlaveFactory, wavePlayer : WavePlayer) extends Area{
  bus.driveAndRead(wavePlayer.phase.run,  address = 0x00) init(False)
  //TODO phase.rate, phase.value, filter.bypass, filter.coef mapping
  bus.drive(wavePlayer.phase.rate,address = 0x04)
  bus.read(wavePlayer.phase.value,address = 0x08)
  bus.driveAndRead(wavePlayer.filter.bypass,address = 0x10) init(True)
  bus.drive(wavePlayer.filter.coef,address = 0x14) init(0)
}
*/
package workshop.waveplayer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory

//Class used to parametrized the WavePlayer instanciation
case class WavePlayerGenerics(
                               sampleWidth : Int,
                               sampleCountLog2 : Int,
                               phaseWidth : Int,
                               filterCoefWidth : Int
                             ){
  def Sample = UInt(sampleWidth bits)
  def sampleCount = 1 << sampleCountLog2
}

//Area of logic which implement a phase counter, a rom sampler.
case class WavePlayer(generics : WavePlayerGenerics) extends Area{
  import generics._
  assert(phaseWidth >= sampleCountLog2)

  val phase = new Area{
    val run = Bool  //Driven later by a WavePlayerMapper (see WavePlayerMapper implementation)
    val rate = UInt(phaseWidth bits) //Driven later by a WavePlayerMapper

    //TODO phase
    val value = Reg(rate) init(0)
    when (run) {
      value := value +rate
    }

  }

  val sampler = new Area{
    //TODO Rom definition with a sinus + it sampling
    val romValue = for (i <- 0 until sampleCount) yield {
      val sin = Math.sin(Math.PI * 2.0 * i / sampleCount) //各采样点的sin值
      BigInt (((sin+1)/2.0 * ((1 << sampleWidth )-1)).toLong)//将sin归一化至0~1的区间并乘以幅值
    }
    val rom = Mem(Sample,sampleCount) initBigInt(romValue)
    val sample = rom.readAsync(phase.value >> (phaseWidth - sampleCountLog2))//若phasewidth>sampleCpuntLog2时，保证取值位宽与rom地址位宽一致，对phase.value截断
  }

  val filter = new Area{
    val bypass = Bool //Driven later by a WavePlayerMapper
    val coef = UInt(filterCoefWidth bits) //Driven later by a WavePlayerMapper
    val value = Sample //Output value of the filter Area
    //TODO first order filter + bypass logic
    val fil = Reg(Sample) init(0)
    fil := fil - ((fil * coef) >> filterCoefWidth) + ((sampler.sample*coef)>>filterCoefWidth)
    value := bypass ? sampler.sample | fil
  }
}

//Area capable to map a WavePlayer on a BusSlaveFactory
class WavePlayerMapper(bus : BusSlaveFactory, wavePlayer : WavePlayer) extends Area{
  bus.driveAndRead(wavePlayer.phase.run,  address = 0x00) init(False)
  //TODO phase.rate, phase.value, filter.bypass, filter.coef mapping
  bus.drive(wavePlayer.phase.rate, address = 0x04) init(0)
  bus.read(wavePlayer.phase.value, address = 0x08) init(0)
  bus.driveAndRead(wavePlayer.filter.bypass, address = 0x10) init(True)
  bus.drive(wavePlayer.filter.coef, address = 0x14) init(0)
}
