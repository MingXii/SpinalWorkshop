package workshop.mandelbrot

import spinal.core._
import spinal.lib._

case class PixelSolverGenerics(fixAmplitude : Int,
                               fixResolution : Int,
                               iterationLimit : Int){
  val iterationWidth = log2Up(iterationLimit+1)
  def iterationType = UInt(iterationWidth bits)
  def fixType = SFix(
    peak = fixAmplitude exp,
    resolution = fixResolution exp
  )
}

case class PixelTask(g : PixelSolverGenerics) extends Bundle{
  val x,y = g.fixType
}

case class PixelResult(g : PixelSolverGenerics) extends Bundle{
  val iteration = g.iterationType
}



case class PixelSolver(g : PixelSolverGenerics) extends Component{
  val io = new Bundle{
    val cmd = slave  Stream(PixelTask(g))
    val rsp = master Stream(PixelResult(g))
  }

  import g._

  val idWidth = 3
  class Context extends Bundle{
    val id        = UInt(idWidth bits)
    val x0,y0     = fixType
    val iteration = UInt(iterationWidth bits)
    val done      = Bool
  }

  case class InserterContext() extends Context{
    val x,y = fixType
  }

  case class MulStageContext() extends Context{
    val xx,yy,xy = fixType
  }

  case class AddStageContext() extends Context{
    val x,y = fixType
  }

  case class RouterContext() extends Context{
    val x,y = fixType
  }


  val inserter = new Area{
    val freeId = Counter(1 << idWidth,inc = io.cmd.fire)
    //val input = slave Stream(InserterContext())
    val loop = slave Stream(RouterContext())
    val output = master Stream(InserterContext())

    when(loop.valid) {
      output.valid := loop.valid
      output.payload.assignSomeByName(loop.payload)
      loop.ready := output.ready
    }.otherwise{
      output.valid := io.cmd.valid
      output.id := freeId
      output.x0 := io.cmd.x
      output.y0 := io.cmd.y
      output.x := 0.0
      output.y := 0.0
      output.iteration := 0
      output.done := False
    }
    io.cmd.ready := !loop.valid && output.ready
  }

  val mulStage = new Area{
    val input = inserter.output.stage()
    //val input = slave Stream(InserterContext())
    val output = master Stream(MulStageContext())
    input := inserter.output.m2sPipe()

    output.valid := input.valid
    input.ready := output.ready
    output.xx := (input.x * input.x).truncated
    output.yy := (input.y * input.y).truncated
    output.xy := (input.x * input.y).truncated
    output.payload.assignSomeByName(input.payload)
  }


  val addStage = new Area{
    val input = mulStage.output.stage().stage()
    //val input = slave Stream(MulStageContext())
    val output = master Stream(AddStageContext())
    input := mulStage.output.m2sPipe().m2sPipe()
    output.valid := input.valid
    input.ready := output.ready
    output.payload.assignSomeByName(input.payload)
    output.x := (input.xx - input.yy + input.x0).truncated
    output.y := (((input.xy) << 1) + input.y0).truncated
    output.done.allowOverride
    output.iteration.allowOverride
    output.done := input.done || input.xx + input.yy >= 4.0 || input.iteration === iterationLimit
    output.iteration := input.iteration + (!output.done).asUInt
  }


  val router = new Area {
    val wantedId = Counter(1 << idWidth, inc = io.rsp.fire)
    val input = addStage.output.stage()
    //val input = slave Stream(AddStageContext())
    //val loop = master Stream(RouterContext())
    input := addStage.output.m2sPipe()
    input.ready := io.rsp.ready || inserter.loop.ready
    io.rsp.valid := input.valid && input.done && wantedId === input.id
    io.rsp.iteration := input.iteration

    inserter.loop.valid := input.valid && (!(input.done && wantedId === input.id) || !io.rsp.ready)
    inserter.loop.payload.assignSomeByName(input.payload)
  }

/*  inserter.loop << router.loop
  mulStage.input <-< inserter.output
  addStage.input <-< mulStage.output
  router.input <-< addStage.output*/



  /*  mulStage.input << inserter.output.stage()
    addStage.input << mulStage.output.stage().stage()
    router.input << addStage.output.stage()*/
}

