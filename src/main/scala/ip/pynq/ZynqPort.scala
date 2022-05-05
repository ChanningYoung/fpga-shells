// See LICENSE.SiFive for license details.

package ictfreedom.device

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.util._
import freechips.rocketchip.subsystem._

/** Specifies the size and width of external memory ports */
case class ZynqMasterPortParams(
  base: BigInt,
  size: BigInt,
  tlMapBase: Option[BigInt] = None,
  beatBytes: Int,
  idBits: Int,
  maxXferBytes: Int = 256,
  executable: Boolean = true)

case object ZynqExtMem extends Field[Seq[ZynqMasterPortParams]](Seq())
case object ZynqExtBus extends Field[Seq[ZynqMasterPortParams]](Seq())

///// The following traits add ports to the sytem, in some cases converting to different interconnect standards

/** Adds a port to the system intended to master an AXI4 DRAM controller. */
trait CanHaveZynqMasterAXI4MemPort { this: BaseSubsystem =>
  private val portName = s"zynqmem_axi4"
  private val device = new MemoryDevice

  val zynqmemAXI4Node = p(ZynqExtMem).zipWithIndex.map { case (params, i) =>
    val memAXI4Node = AXI4SlaveNode(Seq({
      val base = AddressSet(params.base, params.size-1)

      AXI4SlavePortParameters(
        slaves = Seq(AXI4SlaveParameters(
          address       = Seq(base),
          resources     = device.reg,
          regionType    = RegionType.UNCACHED, // cacheable
          executable    = true,
          supportsWrite = TransferSizes(1, cacheBlockBytes),
          supportsRead  = TransferSizes(1, cacheBlockBytes),
          interleavedId = Some(0))), // slave does not interleave read responses
        beatBytes = params.beatBytes)
    }))
    
    val mapFn: (AddressSet => BigInt) =
      x => x.base - params.base + params.tlMapBase.getOrElse(params.base)

    memAXI4Node := mbus.toDRAMController(Some(portName)) {
      (AXI4UserYanker()
        := AXI4IdIndexer(params.idBits)
        := TLToAXI4()
        := TLMap(mapFn))
    }

    memAXI4Node
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait CanHaveZynqMasterAXI4MemPortModuleImp extends LazyModuleImp {
  val outer: CanHaveZynqMasterAXI4MemPort

  val zynqmem_axi4 = outer.zynqmemAXI4Node.map(x => IO(HeterogeneousBag.fromNode(x.in)))
  (zynqmem_axi4 zip outer.zynqmemAXI4Node) foreach { case (io, node) =>
    (io zip node.in).foreach { case (io, (bundle, _)) => io <> bundle }
  }

  // NOTE: No simulation support yet
}

/** Adds a AXI4 port to the system intended to master an MMIO device bus */
trait CanHaveZynqMasterAXI4MMIOPort { this: BaseSubsystem =>
  private val zynqmmioPortParams = p(ZynqExtBus)
  private val portName = "zynqmmio_axi4"
  private val device = new SimpleBus(portName.kebab, Nil)

  val zynqmmioAXI4Node =  zynqmmioPortParams.map { params =>
    val node = AXI4SlaveNode(Seq(
      AXI4SlavePortParameters(
        slaves = Seq(AXI4SlaveParameters(
          address       = AddressSet.misaligned(params.base, params.size),
          resources     = device.ranges,
          executable    = params.executable,
          supportsWrite = TransferSizes(1, params.maxXferBytes),
          supportsRead  = TransferSizes(1, params.maxXferBytes))),
        beatBytes = params.beatBytes)))

    val mapFn: (AddressSet => BigInt) =
      x => x.base - params.base + params.tlMapBase.getOrElse(params.base)

    node := sbus.toFixedWidthPort(Some(portName)) {
      (AXI4Buffer()
        := AXI4UserYanker()
        := AXI4Deinterleaver(sbus.blockBytes)
        := AXI4IdIndexer(params.idBits)
        := TLToAXI4()
        := TLMap(mapFn))
    }

    node
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait CanHaveZynqMasterAXI4MMIOPortModuleImp extends LazyModuleImp {
  val outer: CanHaveZynqMasterAXI4MMIOPort
  val zynqmmio_axi4 = outer.zynqmmioAXI4Node.map(x => IO(HeterogeneousBag.fromNode(x.in)))

  (zynqmmio_axi4 zip outer.zynqmmioAXI4Node) foreach { case (io, node) =>
    (io zip node.in).foreach { case (io, (bundle, _)) => io <> bundle }
  }

  // NOTE: No simulation support yet
}
