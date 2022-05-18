package ictfreedom.device

import Chisel.{Module, log2Ceil}
import chisel3.fromBooleanToLiteral
import chisel3.core.{when}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.{BaseSubsystem, ExtBus}
import freechips.rocketchip.tilelink.TLToAXI4
import freechips.rocketchip.util._
import scala.collection.immutable.ListMap

/** Adds a AXI4 port to the system bus intended to master system level control regs in PS */

trait HasSLCRPort { this: BaseSubsystem =>
  private val slcrparams = p(ExtBus)
  private val portName = "slcr"
  private val device = new SimpleBus(portName.kebab, Seq("xlnx,zynq-slcr", "syscon", "simple-mfd")){
    override def describe(resources: ResourceBindings): Description = {
      def optDef(x: String, seq: Seq[ResourceValue]) = if (seq.isEmpty) None else Some(x -> seq)
      def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))

      val name = describeName(portName, resources)  // the generated device name in device tree
      val compat = optDef("compatible", Seq("xlnx,zynq-slcr", "syscon", "simple-mfd").map(ResourceString(_))) // describe the list of compatiable devices
      val reg = resources.map.filterKeys(DiplomacyUtils.regFilter)
      val (named, bulk) = reg.partition { case (k, v) => DiplomacyUtils.regName(k).isDefined }
      // We need to be sure that each named reg has exactly one AddressRange associated to it
      named.foreach {
        case (k, Seq(Binding(_, value: ResourceAddress))) =>
          val ranges = AddressRange.fromSets(value.address)
          require (ranges.size == 1, s"DTS device $name has $k = $ranges, must be a single range!")
        case (k, seq) =>
          require (false, s"DTS device $name has $k = $seq, must be a single ResourceAddress!")
      }
      val names = optDef("reg-names", named.map(x => ResourceString(DiplomacyUtils.regName(x._1).get)).toList) // names of the named address space
      val regs = optDef("reg", (named ++ bulk).flatMap(_._2.map(_.value)).toList) // address ranges of all spaces (named and bulk)
      val extra = Map(
        "#address-cells"   -> ofInt(1),
        "#size-cells"      -> ofInt(1),
        "ranges"           -> Nil,
        "u-boot,dm-pre-reloc" -> Nil
      )

      Description(name, ListMap() ++ compat ++ names ++ regs ++ extra)
    }
  }

  val clkc = new SimpleDevice("clkc", Seq("xlnx,ps7-clkc")){
    override def parent = Some(device)
    override def describe(resources: ResourceBindings): Description = {
      def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))

      val extra = Map(
        "u-boot,dm-pre-reloc" -> Nil,
        "#clock-cells"  -> Seq(ResourceInt(BigInt(1))),
        "fclk-enable"   ->  Seq(ResourceInt(BigInt(0xf))),
        "clock-output-names"    ->  Seq("armpll", "ddrpll", "iopll", "cpu_6or4x",
          "cpu_3or2x", "cpu_2x", "cpu_1x", "ddr2x", "ddr3x",
          "dci", "lqspi", "smc", "pcap", "gem0", "gem1",
          "fclk0", "fclk1", "fclk2", "fclk3", "can0", "can1",
          "sdio0", "sdio1", "uart0", "uart1", "spi0", "spi1",
          "dma", "usb0_aper", "usb1_aper", "gem0_aper",
          "gem1_aper", "sdio0_aper", "sdio1_aper",
          "spi0_aper", "spi1_aper", "can0_aper", "can1_aper",
          "i2c0_aper", "i2c1_aper", "uart0_aper", "uart1_aper",
          "gpio_aper", "lqspi_aper", "smc_aper", "swdt",
          "dbg_trc", "dbg_apb").map(ResourceString(_)),
        "regs"     ->  Seq(ResourceInt(BigInt(0x100)),ResourceInt(BigInt(0x100)))
      )

      Description("clkc", ListMap()  ++ extra)
    }
  }

  ResourceBinding {
    Resource(device, "reg").bind(ResourceAddress(
      AddressSet.misaligned(x"f8000000", 0xc00),
      ResourcePermissions(true, true, false, false, false)))
    Resource(clkc, "dummy").bind(ResourceInt(0))
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasSLCRPortModuleImp extends LazyModuleImp
{
}
