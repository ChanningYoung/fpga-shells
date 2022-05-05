package ictfreedom.device

import chisel3._
import Chisel.{INPUT, UInt}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem.SlavePortParams
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange}
import freechips.rocketchip.regmapper.{RegField, HasRegMap}
import freechips.rocketchip.tile.HasTileParameters

/** IO ports for Zynq adapter module */
trait ZynqAdapterCoreBundle extends Bundle {
  implicit val p: Parameters

  val sys_reset = Output(Bool())
  val pc = Input(UInt(width = 32))//UInt(INPUT, width = vaddrBitsExtended)
}
/** Map system reset signal to 0x43c00000 */
trait ZynqAdapterCoreModule extends HasRegMap
{
  implicit val p: Parameters
  val io: ZynqAdapterCoreBundle

  val sys_reset = RegInit(true.B)
  io.sys_reset := sys_reset

  regmap(
    0x00 -> Seq(RegField(1, sys_reset)),
    0x04 -> Seq(RegField.r( 32, io.pc)))
}

/** Adds Zynq adapter module to AXI$ register router module */
class ZynqAdapterCore(address: BigInt, beatBytes: Int)(implicit p: Parameters)
  extends AXI4RegisterRouter(
    address, beatBytes = 4, concurrency = 1)(
      new AXI4RegBundle((), _)    with ZynqAdapterCoreBundle)(
      new AXI4RegModule((), _, _) with ZynqAdapterCoreModule)

/** Actually generates the corresponding IO in the concrete Module */
class ZynqAdapter(address: BigInt, config: SlavePortParams)(implicit p: Parameters)
    extends LazyModule {

  val node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "Zynq Adapter",
      id = IdRange(0, 1 << config.idBits))))))

  val core = LazyModule(new ZynqAdapterCore(address, config.beatBytes))
  core.node := AXI4Fragmenter() := node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val sys_reset = Output(Bool())
 val pc = Input(UInt(width = 32))
    })
    val axi = IO(Flipped(node.out(0)._1.cloneType))
    node.out(0)._1 <> axi

    val coreIO = core.module.io
    io.sys_reset := coreIO.sys_reset
    coreIO.pc := io.pc
  }
}
