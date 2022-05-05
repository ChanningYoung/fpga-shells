package ictfreedom.device

import Chisel.{Bool, Module}
import chisel3.core.Input
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.{BaseSubsystem, ExtBus, SimAXIMem}
import freechips.rocketchip.tilelink.TLToAXI4
import freechips.rocketchip.util._
import freechips.rocketchip.interrupts._
import scala.collection.immutable.ListMap

/** Adds a port to the system bus intended to master an AXI4 mac periphery. */
trait HasMAC { this: BaseSubsystem with HasSLCRPort =>
  private val macParamsOpt = p(ExtBus)
  private val portName = "ethernet"
  private val device = new SimpleDevice(portName.kebab, Seq("sifive, mac")){
    override def describe(resources: ResourceBindings): Description = {
      def optDef(x: String, seq: Seq[ResourceValue]) = if (seq.isEmpty) None else Some(x -> seq)
      def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))

      val name = describeName(portName, resources)  // the generated device name in device tree
      val int = describeInterrupts(resources)      // interrupt description
      val clocks = describeClocks(resources)
      val compat = optDef("compatible", Seq("cdns,zynq-gem", "cdns,gem").map(ResourceString(_))) // describe the list of compatiable devices
      val reg = resources.map.filterKeys(regFilter)
      val (named, bulk) = reg.partition { case (k, v) => regName(k).isDefined }
      // We need to be sure that each named reg has exactly one AddressRange associated to it
      named.foreach {
        case (k, Seq(Binding(_, value: ResourceAddress))) =>
          val ranges = AddressRange.fromSets(value.address)
          require (ranges.size == 1, s"DTS device $name has $k = $ranges, must be a single range!")
        case (k, seq) =>
          require (false, s"DTS device $name has $k = $seq, must be a single ResourceAddress!")
      }
      val names = optDef("reg-names", named.map(x => ResourceString(regName(x._1).get)).toList) // names of the named address space
      val regs = optDef("reg", (named ++ bulk).flatMap(_._2.map(_.value)).toList) // address ranges of all spaces (named and bulk)
      val status = optDef("status", Seq("okay").map(ResourceString(_)))
      val extra = Map(
        "clocks"            -> Seq(ResourceReference(clkc.label), ResourceInt(BigInt(30)),ResourceReference(clkc.label), ResourceInt(BigInt(13))),
        "clock-names"       -> Seq(ResourceString("pclk"), ResourceString("hclk"), ResourceString("tx_clk")),
        "#address-cells"    -> ofInt(1),
        "#size-cells"       -> ofInt(0),
        "phy-mode"          ->  Seq(ResourceString("rgmii-id")),
        "phy-handle"        ->  Seq(ResourceReference(label + "_phy"))
      )

      Description(name,ListMap() ++ compat ++ int ++ clocks ++ names ++ regs ++ extra)
    }
  }
    
  val phy = new SimpleDevice("phy", Seq("xlnx,ps7-clkc")){
    override val label = device.label + "_phy"
    override def parent = Some(device)
    override def describe(resources: ResourceBindings): Description = {
      def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))

      val extra = Map(
        "device_type"   ->  Seq(ResourceString("ethernet-phy")),
        "reg"     ->  Seq(ResourceInt(BigInt(1)))
      )

      Description("phy", ListMap()  ++ extra)
    }
  }

  ResourceBinding {
    Resource(device, "reg").bind(ResourceAddress(
      AddressSet.misaligned(x"e000b000", 0x1000),
      ResourcePermissions(true, true, false, false, false)))
    Resource(phy, "dummy").bind(ResourceInt(0))
  }

  val macInterrupts = IntSourceNode(IntSourcePortSimple(num = 1, resources = device.int))

  ibus.fromSync := macInterrupts
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasMACPortModuleImp extends LazyModuleImp
{
  val outer: HasMAC
  val mac_int = IO(Input(Bool()))
  outer.macInterrupts.out.map(_._1).flatten.foreach { case o => o := mac_int }
}
