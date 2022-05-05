package ictfreedom.device

import Chisel.Module
import chisel3.core._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, BufferParams, LazyModule, LazyModuleImp, SimpleBus, TransferSizes}
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem.{BaseSubsystem, ExtBus, SimAXIMem}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class GPIOParams(address: BigInt, beatBytes: Int, concurrency: Int)

class GPIOBundle extends Bundle {
  val btns = Input(UInt(4.W))
  val sws = Input(UInt(2.W))
}

trait GPIORegBundle extends Bundle {
  implicit val p: Parameters

  val bundle = new GPIOBundle
}

trait GPIORegModule extends HasRegMap {
  implicit val p: Parameters
  val io: GPIORegBundle

  regmap(
    0x00 -> Seq(RegField.r(4, io.bundle.btns)),
    0x04 -> Seq(RegField.r(2, io.bundle.sws)))
}

class GPIOReg(c: GPIOParams)(implicit p: Parameters)
  extends TLRegisterRouter(
    c.address, "gpio", Seq("pynq,gpio"),
    beatBytes = c.beatBytes, concurrency = c.concurrency)(
    new TLRegBundle((), _)    with GPIORegBundle)(
    new TLRegModule((), _, _) with GPIORegModule)

trait HasGPIOPort { this: BaseSubsystem =>
  implicit val p: Parameters

  private val address = BigInt(0xE0300000L)
  private val portName = "gpio"

  val gpioreg = LazyModule(new GPIOReg(
    GPIOParams(address=address, beatBytes = sbus.beatBytes, concurrency=1))(p))

  sbus.toVariableWidthSlave(Some(portName), buffer = BufferParams.none) { gpioreg.node }
}

trait HasGPIOPortModuleImp extends LazyModuleImp {
  implicit val p: Parameters
  val outer: HasGPIOPort
  val gpio = IO(new GPIOBundle)
  gpio <> outer.gpioreg.module.io.bundle
}
