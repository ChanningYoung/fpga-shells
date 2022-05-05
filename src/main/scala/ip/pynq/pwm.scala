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

case class HCPFParams(address: BigInt, beatBytes: Int, buffNum: Int, tableEntryNum: Int, tableEntryBits: Int)

trait HCPFTLBundle extends Bundle {
    val out = Output(UInt(32.W))
}

trait HCPFTLModule  extends HasRegMap {
    val io: HCPFTLBundle
    implicit val p: Parameters
    def params: HCPFParams
    val read_offset1 = RegInit(0.U(32.W))
    regmap(
        0x00 -> Seq(
            RegField.w(32, read_offset1))
    )
    io.out := read_offset1
}

class PWMTL(c: HCPFParams)(implicit p: Parameters)
  extends TLRegisterRouter(
      c.address, "hcpf", Seq("zxhero,hcpf"),
      beatBytes = c.beatBytes, concurrency = 0)(
      new TLRegBundle(c, _) with HCPFTLBundle)(
      new TLRegModule(c, _, _) with HCPFTLModule)

trait HasPeripheryHCPF { this: BaseSubsystem =>
    implicit val p: Parameters

    private val address = 0x2000
    private val portName = "hcpf"

    val hcpf = LazyModule(new PWMTL(
        HCPFParams(address=address, beatBytes = sbus.beatBytes, buffNum=1024, tableEntryNum=64, tableEntryBits=56))(p))

    sbus.toVariableWidthSlave(Some(portName),buffer = BufferParams.none) { hcpf.node }
}

trait HasPeripheryHCPFModuleImp extends LazyModuleImp {
    implicit val p: Parameters
    val outer: HasPeripheryHCPF
    val hcpftest = IO(Output(UInt(32.W)))
    hcpftest := outer.hcpf.module.io.out
}
