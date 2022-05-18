package ictfreedom.device

import Chisel.{Module, log2Ceil}
import chisel3.core.{Input,Bool}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.{BaseSubsystem, ExtBus}
import freechips.rocketchip.tilelink.TLToAXI4
import freechips.rocketchip.util._
import freechips.rocketchip.interrupts._
import scala.collection.immutable.ListMap

/** Adds a AXI4 port to the system intended to master an AXI4 uart periphery */
trait HasUSBPort { this: BaseSubsystem with HasSLCRPort =>
  private val uartparams = p(ExtBus)
  private val portName = "usb"
  private val device = new SimpleDevice(portName.kebab, Seq("sifive, usb")){
    override def describe(resources: ResourceBindings): Description = {
      def optDef(x: String, seq: Seq[ResourceValue]) = if (seq.isEmpty) None else Some(x -> seq)
      def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))

      val compat = optDef("compatible", Seq("xlnx,zynq-usb-2.20a", "chipidea,usb2").map(ResourceString(_))) // describe the list of compatiable devices
      val name = describeName(portName, resources)  // the generated device name in device tree
      val int = describeInterrupts(resources)      // interrupt description
      val clocks = describeClocks(resources)
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
        "clocks"            -> Seq(ResourceReference(clkc.label), ResourceInt(BigInt(28))),
        "status"            -> Seq(ResourceString("okay")),
        "phy_type"          -> Seq(ResourceString("ulpi"))
      )

      Description(name,ListMap() ++ compat ++ int ++ clocks ++ names ++ regs ++ extra)
    }

  }

  ResourceBinding {
    Resource(device, "reg").bind(ResourceAddress(
      AddressSet.misaligned(x"e0002000", 0x1000),
      ResourcePermissions(true, true, false, false, false)))}

  val usbInterrupts = IntSourceNode(IntSourcePortSimple(num = 1, resources = device.int))

  ibus.fromSync := usbInterrupts
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasUSBPortModuleImp extends LazyModuleImp {
  val outer: HasUSBPort
  val usb_int = IO(Input(Bool()))
  outer.usbInterrupts.out.map(_._1).flatten.foreach { case o => o := usb_int }
}
