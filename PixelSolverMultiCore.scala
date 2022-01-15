package workshop.mandelbrot

import spinal.core._
import spinal.lib._


case class Dispatcher[T <: Data](dataType : T,outputsCount : Int) extends Component{
  val io = new Bundle {
    val input = slave Stream(dataType)
    val outputs = Vec(master Stream(dataType),outputsCount)
  }
  // TODO
  val cnt = Counter(io.outputs.length)
  for(output <- io.outputs){
    output.payload := io.input.payload
    output.valid := False
    }

  io.outputs(cnt).valid := io.input.valid
  io.input.ready := io.outputs(cnt).ready

  when(io.input.valid && io.input.ready) {
    cnt.increment()
  }
}

// TODO Define the Arbiter component (similar to the Dispatcher)
case class Arbiter[T <: Data](dataType : T,inputsCount : Int) extends Component{
  val io = new Bundle {
    val inputs = Vec(slave Stream(dataType),inputsCount)
    val output = master Stream(dataType)
  }
  // TODO
  val cnt = Counter(io.inputs.length)

  for(in <- io.inputs) {
    in.ready := False
  }
  io.output.valid := io.inputs(cnt.value).valid
  io.output.payload := io.inputs(cnt.value).payload
  io.inputs(cnt.value).ready := io.output.ready

  when(io.output.valid && io.output.ready) {
    cnt.increment()
  }
}
case class PixelSolverMultiCore(g : PixelSolverGenerics,coreCount : Int) extends Component {
  val io = new Bundle {
    val cmd = slave Stream (PixelTask(g))
    val rsp = master Stream (PixelResult(g))
  }
  //TODO instantiate all components
  val dispatcher = Dispatcher(PixelTask(g),coreCount)
  val arbiter = Arbiter(PixelResult(g),coreCount)
  val solver = List.fill(coreCount)(PixelSolver(g))
  //TODO interconnect all that stuff
  io.cmd >> dispatcher.io.input
  for(i <- 0 until coreCount) {
    solver(i).io.cmd << dispatcher.io.outputs(i)
    arbiter.io.inputs(i) << solver(i).io.rsp
  }
  io.rsp << arbiter.io.output
}

