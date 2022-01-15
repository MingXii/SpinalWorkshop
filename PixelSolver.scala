package workshop.mandelbrot

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, StateParallelFsm, State, StateMachine}

case class PixelSolverGenerics(fixAmplitude : Int,
                               fixResolution : Int,
                               iterationLimit : Int){
  val iterationWidth = log2Up(iterationLimit+1)
  def iterationType = UInt(iterationWidth bits)
  def fixType = SFix(
    peak = fixAmplitude exp,
    resolution = fixResolution exp
  )//定点小数的表示方法，peek代表自然部分位宽，resolution代表小数部分位宽
}

case class PixelTask(g : PixelSolverGenerics) extends Bundle{
  val x,y = g.fixType
}

case class PixelResult(g : PixelSolverGenerics) extends Bundle{
  val iteration = g.iterationType
}

case class PixelSolver(g : PixelSolverGenerics) extends Component {
  val io = new Bundle {
    val cmd = slave Stream (PixelTask(g))
    val rsp = master Stream (PixelResult(g))
  }
  //TODO implement the mandelbrot algorithm

  import g._

  val x, y = Reg(fixType) init (0)
  val iteration = Reg(iterationType) init (0)

  val xx = x * x
  val yy = y * y
  val xy = x * y

  io.cmd.ready := False
  io.rsp.valid := False
  io.rsp.iteration := iteration

  val x0, y0 = Reg(fixType) init (0)
 val fsm = new StateMachine {
   val idle: State = new State with EntryPoint {
     whenIsActive {
       when(io.cmd.valid) {
         io.cmd.ready := True
         x := 0
         y := 0
         iteration :=0
         x0 := io.cmd.x
         y0 := io.cmd.y
         goto(iter)
       }
     }
   }
   val iter: State = new State {
     whenIsActive {
       x := (xx - yy + x0).truncated
       y := (((xy) << 1) + y0).truncated
       iteration := iteration + 1
       when(xx + yy >= 4.0 || iteration === iterationLimit) {
         io.rsp.valid := True
         x := x
         y := y
         iteration := iteration
         when(io.rsp.ready){
           goto (idle)
         }
       }
     }
   }
 }
}


