/*
  Copyright (c) 2014, Elliot Stirling
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
  * Neither the name of the copyright holder nor the
    names of its contributors may be used to endorse or promote products
    derived from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
    DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
    ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.package hardware
*/

import hardware.{Bus, Simulation, Wire}

import org.junit.Test
import org.junit.Assert._

/**
 * Created by elliot on 12/02/14.
 */
class SimulationTest {

  @Test def wireTest(): Unit = {

    // basic expectations
    val wire = new Wire
    assertEquals(false, wire.getSignal) // initial value false
    wire.setSignal(signal = true)       // can be set to true
    assertEquals(true, wire.getSignal)  // reports as true

    // simple action closure
    var detectedValue = true
    def handleChange(): Unit = {
      detectedValue = wire.getSignal
    }
    wire.addAction( handleChange )

    // test the action mechanism
    assertEquals(true, detectedValue)
    wire.setSignal(signal = false)
    assertEquals(false, detectedValue)
  }

  @Test def busTest(): Unit = {

    // basic expectations
    val bus = new Bus(2)
    assertEquals(0, bus.value().toInt)
    bus(0)(signal = true)
    assertEquals(1, bus.value().toInt)

    bus(1)(signal = true)
    assertEquals(3, bus.value().toInt)
  }

  def oneToOneTest(sim: Simulation, input: Wire, output: Wire, table: List[(Boolean, Boolean)]) {
    table.foreach{ case (i, o) =>
      input.setSignal(i)
      sim.run()
      assertEquals(o, output.getSignal)
    }
  }

  def twoToOneTest(sim: Simulation, input1: Wire, input2: Wire, output: Wire, table: List[(Boolean, Boolean, Boolean)]) {
    table.foreach{ case (i1, i2, o) =>
      input1.setSignal(i1)
      input2.setSignal(i2)
      sim.run()
      assertEquals(o, output.getSignal)
    }
  }

  def oneToTwoTest(sim: Simulation, input: Wire, output1: Wire, output2: Wire, table: List[(Boolean, Boolean, Boolean)]) {
    table.foreach{ case (i, o1, o2) =>
      input.setSignal(i)
      sim.run()
      assertEquals((i, o1, o2), (input.getSignal, output1.getSignal, output2.getSignal))
    }
  }

  def threeToOneTest(sim: Simulation, input1: Wire, input2: Wire, input3: Wire, output: Wire, table: List[(Boolean, Boolean, Boolean, Boolean)]) {
    table.foreach{ case (i1, i2, i3, o) =>
      input1.setSignal(i1)
      input2.setSignal(i2)
      input3.setSignal(i3)
      sim.run()
      assertEquals((i1, i2, i3, o), (input1.getSignal, input2.getSignal, input3.getSignal, output.getSignal))
    }
  }

  def twoToTwoTest(sim: Simulation, input1: Wire, input2: Wire, output1: Wire, output2: Wire, table: List[(Boolean, Boolean, Boolean, Boolean)]) {
    table.foreach{ case (i1, i2, o1, o2) =>
      input1.setSignal(i1)
      input2.setSignal(i2)
      sim.run()
      assertEquals(o1, output1.getSignal)
      assertEquals(o2, output2.getSignal)
    }
  }

  def threeToTwoTest(sim: Simulation, input1: Wire, input2: Wire, input3: Wire, output1: Wire, output2: Wire, table: List[(Boolean, Boolean, Boolean, Boolean, Boolean)]) {
    table.foreach{ case (i1, i2, i3, o1, o2) =>
      input1.setSignal(i1)
      input2.setSignal(i2)
      input3.setSignal(i3)
      sim.run()
      assertEquals(o1, output1.getSignal)
      assertEquals(o2, output2.getSignal)
    }
  }

  def fourToTwoTest(sim: Simulation, input1: Wire, input2: Wire, input3: Wire, input4: Wire, output1: Wire, output2: Wire, table: List[(Boolean, Boolean, Boolean, Boolean, Boolean, Boolean)]) {
    table.foreach{ case (i1, i2, i3, i4, o1, o2) =>
      input1.setSignal(i1)
      input2.setSignal(i2)
      input3.setSignal(i3)
      input4.setSignal(i4)
      sim.run()
      assertEquals(o1, output1.getSignal)
      assertEquals(o2, output2.getSignal)
    }
  }

  def twoToBusTest(sim: Simulation, input1: Wire, input2: Wire, table1: (Boolean, Boolean), output: Bus, table2: List[Boolean]) {
    assertEquals(output.wires.length, table2.length)

    input1.setSignal(table1._1)
    input2.setSignal(table1._2)
    sim.run()

    assertEquals(table2, output.wires.map( _.getSignal ))
  }

  def threeToBusTest(sim: Simulation, input1: Wire, input2: Wire, input3: Wire, table1: (Boolean, Boolean, Boolean), output: Bus, table2: List[Boolean]) {
    assertEquals(output.wires.length, table2.length)

    input1.setSignal(table1._1)
    input2.setSignal(table1._2)
    input3.setSignal(table1._3)
    sim.run()

    assertEquals(table2, output.wires.map( _.getSignal ))
  }

  def threePlusBusToOneTest(sim: Simulation, input: Bus, input1: Wire, input2: Wire, input3: Wire, output: Wire, table1: List[Boolean], table2: (Boolean, Boolean, Boolean), expected: Boolean) {
    assertEquals(input.wires.length, table1.length)
    input.wires.zip( table1 ).foreach { case (wire, value) => wire.setSignal(value) }
    input1.setSignal(table2._1)
    input2.setSignal(table2._2)
    input3.setSignal(table2._3)
    sim.run()

    assertEquals(expected, output.getSignal)
  }


  def busOneToBusTest(sim: Simulation, input: Bus, input1: Wire, output: Bus, table: List[(List[Boolean], Boolean, List[Boolean])]): Unit = {

    table.foreach { case (inputTable, a, outputTable) =>
      assertEquals(input.width, inputTable.length)
      assertEquals(output.width, outputTable.length)

      input.wires.zip( inputTable ).foreach { case (wire, value) => wire.setSignal(value) }
      input1.setSignal(a)
      sim.run()
      assertEquals((inputTable, a, outputTable), (input.wires.map( _.getSignal ), input1.getSignal, output.wires.map( _.getSignal ) ) )
    }
  }

  def busToOneTest(sim: Simulation, input: Bus, output: Wire, table: List[(List[Boolean], Boolean)]): Unit = {
    table.foreach { case (inputTable, o) =>
      assertEquals(input.width, inputTable.length)

      input.wires.zip( inputTable ).foreach { case (wire, value) => wire.setSignal(value) }
      sim.run()
      assertEquals((inputTable, o), (input.wires.map( _.getSignal ), output.getSignal))
    }
  }


  def busBusToOneTest(sim: Simulation, input1: Bus, input2: Bus, output: Wire, table: List[(List[Boolean], List[Boolean], Boolean)]): Unit = {
    table.foreach { case (input1Table, input2Table, o) =>
      assertEquals(input1.width, input1Table.length)
      assertEquals(input2.width, input2Table.length)

      input1.wires.zip( input1Table ).foreach { case (wire, value) => wire.setSignal(value) }
      input2.wires.zip( input2Table ).foreach { case (wire, value) => wire.setSignal(value) }
      sim.run()
      assertEquals((input1Table, input2Table, o), (input1.wires.map( _.getSignal ), input2.wires.map( _.getSignal ), output.getSignal))
    }
  }

  @Test def inverterTest(): Unit = {
    val input, output = new Wire

    val sim = new Simulation { }

    // connect the inverter
    sim.inverter(input, output)

    oneToOneTest(sim, input, output, List(
      (false, true),
      (true, false)
    ))
  }

  @Test def nAndGateTest(): Unit = {
    val input1, input2, output = new Wire

    val sim = new Simulation { }

    // connect the nAnd gate
    sim.nAndGate(input1, input2, output)

    twoToOneTest(sim, input1, input2, output, List(
      (false, false, true),
      (false, true, true),
      (true, false, true),
      (true, true, false)
    ))
  }

  @Test def nAndGate3Test(): Unit = {
    val input1, input2, input3, output = new Wire

    val sim = new Simulation { }

    // connect the nAnd gate
    sim.nAndGate(input1, input2, input3, output)

    threeToOneTest(sim, input1, input2, input3, output, List(
      (false, false, false, true),
      (false, true, false, true),
      (true, false, false, true),
      (true, true, false, true),
      (false, false, true, true),
      (false, true, true, true),
      (true, false, true, true),
      (true, true, true, false)
    ))
  }

  @Test def andGateTest(): Unit = {
    val input1, input2, output = new Wire

    val sim = new Simulation { }

    // connect the and gate
    sim.andGate(input1, input2, output)

    twoToOneTest(sim, input1, input2, output, List(
      (false, false, false),
      (false, true, false),
      (true, false, false),
      (true, true, true)
    ))
  }

  @Test def orGateTest(): Unit = {
    val input1, input2, output = new Wire

    val sim = new Simulation { }

    // connect the or gate
    sim.orGate(input1, input2, output)

    twoToOneTest(sim, input1, input2, output, List(
      (false, false, false),
      (false, true, true),
      (true, false, true),
      (true, true, true)
    ))
  }

  @Test def orBusGateTest(): Unit = {
    val input = new Bus(4)
    val output = new Wire

    val sim = new Simulation { }

    // connect the or gate
    sim.orGate(input, output)

    busToOneTest(sim, input, output, List(
      (List(false, false, false, false), false),
      (List(true, false, false, false), true),
      (List(false, true, false, false), true),
      (List(true, true, false, false), true),
      (List(false, false, true, false), true),
      (List(true, false, true, false), true),
      (List(false, true, true, false), true),
      (List(true, true, true, false), true),
      (List(false, false, false, true), true),
      (List(true, false, false, true), true),
      (List(false, true, false, true), true),
      (List(true, true, false, true), true),
      (List(false, false, true, true), true),
      (List(true, false, true, true), true),
      (List(false, true, true, true), true),
      (List(true, true, true, true), true)
    ))
  }

  @Test def xOrGateTest(): Unit = {
    val input1, input2, output = new Wire

    val sim = new Simulation { }

    // connect the xor gate
    sim.xOrGate(input1, input2, output)

    twoToOneTest(sim, input1, input2, output, List(
      (false, false, false),
      (false, true, true),
      (true, false, true),
      (true, true, false)
    ))
  }

  @Test def halfAdderTest(): Unit = {
    val input1, input2, sum, carry = new Wire

    val sim = new Simulation { }

    // connect the half adder
    sim.halfAdder(input1, input2, sum, carry)

    twoToTwoTest(sim, input1, input2, sum, carry, List(
      (false, false, false, false),
      (false, true, true, false),
      (true, false, true, false),
      (true, true, false, true)
    ))
  }

  @Test def fullAdderTest(): Unit = {
    val input1, input2, carryIn, sum, carryOut = new Wire

    val sim = new Simulation { }

    // connect the full adder
    sim.fullAdder(input1, input2, carryIn, sum, carryOut)

    threeToTwoTest(sim, input1, input2, carryIn, sum, carryOut, List(
      (false, false, false, false, false),
      (false, true, false, true, false),
      (true, false, false, true, false),
      (true, true, false, false, true),
      (false, false, true, true, false),
      (false, true, true, false, true),
      (true, false, true, false, true),
      (true, true, true, true, true)
    ))
  }

  @Test def dFlipFlopTest(): Unit = {
    val data, clock, nPreset, nClear, q, nq = new Wire

    val sim = new Simulation { }

    // connect the flip flop
    sim.dFlipFlop(data, clock, nPreset, nClear, q, nq)
    sim.run()

    fourToTwoTest(sim, data, clock, nPreset, nClear, q, nq, List(

      // clear
      (false, false, true, false, false, true),

      // initial state
      (false, false, true, true, false, true),

      // clock, no data, output 0
      (false, false, true, true, false, true),
      (false, true, true, true, false, true),
      (false, false, true, true, false, true),

      // clock, data, output 0 -> 1
      (true, false, true, true, false, true),
      (true, true, true, true, true, false),
      (true, false, true, true, true, false),

      // clock, no data, output 1 -> 0
      (false, false, true, true, true, false),
      (false, true, true, true, false, true),
      (false, false, true, true, false, true),

      // clock, no data, preset, output 0 -> 1
      (false, false, true, true, false, true),
      (false, false, false, true, true, false),
      (false, true, false, true, true, true),   // proven to be accurate
      (false, false, false, true, true, false),

      // clock, no data, clear, output 1 -> 0
      (false, false, true, true, true, false),
      (false, false, true, false, false, true),
      (false, true, true, false, false, true),
      (false, false, true, false, false, true)
    ))
  }


  @Test def registerSISOTest(): Unit = {
    val data, clock, nClear, output = new Wire

    val sim = new Simulation { }

    // connect the register
    sim.registerSISO(data, clock, nClear, output, 4)
    sim.run()

    threeToOneTest(sim, data, clock, nClear, output, List(

      // clear
      (false, false, false, false),

      // clock 4 bits on
      (true, false, true, false),
      (true, true, true, false), // write
      (true, false, true, false),
      (true, true, true, false), // write
      (true, false, true, false),
      (true, true, true, false), // write
      (true, false, true, false),
      (true, true, true, true),  // write

      // clock 4 bits off
      (false, false, true, true), // read
      (false, true, true, true),
      (false, false, true, true), // read
      (false, true, true, true),
      (false, false, true, true), // read
      (false, true, true, true),
      (false, false, true, true), // read
      (false, true, true, false)
    ))
  }

  @Test def registerSIPOTest(): Unit = {
    val data, nClear, clock = new Wire
    val output = new Bus(4)

    val sim = new Simulation { }

    sim.registerSIPO(data, clock, nClear, output)

    // clear
    threeToBusTest(sim, data, clock, nClear, (false, false, false), output, List(false, false, false, false))

    // no-op
    threeToBusTest(sim, data, clock, nClear, (false, true, true),  output, List(false, false, false, false))
    threeToBusTest(sim, data, clock, nClear, (false, false, true),  output, List(false, false, false, false))

    // clock in 4 bits
    threeToBusTest(sim, data, clock, nClear, (true, false, true),  output, List(false, false, false, false))
    threeToBusTest(sim, data, clock, nClear, (true, true, true),  output, List(true, false, false, false))
    threeToBusTest(sim, data, clock, nClear, (true, false, true),  output, List(true, false, false, false))
    threeToBusTest(sim, data, clock, nClear, (true, true, true),  output, List(true, true, false, false))
    threeToBusTest(sim, data, clock, nClear, (true, false, true),  output, List(true, true, false, false))
    threeToBusTest(sim, data, clock, nClear, (true, true, true),  output, List(true, true, true, false))
    threeToBusTest(sim, data, clock, nClear, (true, false, true),  output, List(true, true, true, false))
    threeToBusTest(sim, data, clock, nClear, (true, true, true),  output, List(true, true, true, true))

    // clear
    threeToBusTest(sim, data, clock, nClear, (false, false, false), output, List(false, false, false, false))
  }


  @Test def registerPISOTest(): Unit = {
    val data = new Bus(4)
    val shift, clock, nClear, output = new Wire

    val sim = new Simulation { }

    sim.registerPISO(data, shift, clock, nClear, output)

    // clear
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (false, false, false), expected = false)

    // no-op
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (false, false, true), expected = false)
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (false, true, true), expected = false)
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (false, false, true), expected = false)

    // set 4 bits
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(true, true, true, true), (false, false, true), expected = false)
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(true, true, true, true), (false, true, true), expected = true)
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (false, false, true), expected = true)

    // shift 4 bits off
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (true, false, true), expected = true) // read
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (true, true, true), expected = true) // shift
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (true, false, true), expected = true) // read
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (true, true, true), expected = true) // shift
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (true, false, true), expected = true) // read
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (true, true, true), expected = true) // shift
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (true, false, true), expected = true) // read
    threePlusBusToOneTest(sim, data, shift, clock, nClear, output, List(false, false, false, false), (true, true, true), expected = false) // shift
  }

  @Test def oneToTwoDecoderTest(): Unit = {
    val input, enable,  output1, output2 = new Wire

    val sim = new Simulation { }

    // connect the decoder
    sim.oneToTwoDecoder(input, enable, output1, output2)

    twoToTwoTest(sim, input, enable, output1, output2, List(
      (false, false, false, false),
      (true, false, false, false),
      (false, true, true, false),
      (true, true, false, true)
    ))
  }

  @Test def twoToFourDecoderTest(): Unit = {
    val input = new Bus(2)
    val output = new Bus(4)
    val enable = new Wire

    val sim = new Simulation { }

    // connect the decoder
    sim.decoder(input, enable, output)

    busOneToBusTest(sim, input, enable, output, List(
      (List(false, false), false, List(false, false, false, false)),
      (List(true, false), false, List(false, false, false, false)),
      (List(false, true), false, List(false, false, false, false)),
      (List(true, true), false, List(false, false, false, false)),
      (List(false, false), true, List(true, false, false, false)),
      (List(true, false), true, List(false, true, false, false)),
      (List(false, true), true, List(false, false, true, false)),
      (List(true, true), true, List(false, false, false, true))
    ))
  }

  @Test def fourToSixteenDecoderTest(): Unit = {
    val input = new Bus(4)
    val output = new Bus(16)
    val enable = new Wire

    val sim = new Simulation { }

    // connect the decoder
    sim.decoder(input, enable, output)

    busOneToBusTest(sim, input, enable, output, List(
      (List(false, false, false, false), true, List(true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, false, false, false), true, List(false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, true, false, false), true, List(false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, true, false, false), true, List(false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, false, true, false), true, List(false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, false, true, false), true, List(false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false)),
      (List(false, true, true, false), true, List(false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false)),
      (List(true, true, true, false), true, List(false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false)),
      (List(false, false, false, true), true, List(false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false)),
      (List(true, false, false, true), true, List(false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false)),
      (List(false, true, false, true), true, List(false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false)),
      (List(true, true, false, true), true, List(false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false)),
      (List(false, false, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false)),
      (List(true, false, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false)),
      (List(false, true, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false)),
      (List(true, true, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true))
    ))
  }

  @Test def fiveToThirtyTwoDecoderTest(): Unit = {
    val input = new Bus(5)
    val output = new Bus(32)
    val enable = new Wire

    val sim = new Simulation { }

    // connect the decoder
    sim.decoder(input, enable, output)

    busOneToBusTest(sim, input, enable, output, List(
      (List(false, false, false, false, false), true, List(true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, false, false, false, false), true, List(false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, true, false, false, false), true, List(false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, true, false, false, false), true, List(false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, false, true, false, false), true, List(false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, false, true, false, false), true, List(false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, true, true, false, false), true, List(false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, true, true, false, false), true, List(false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, false, false, true, false), true, List(false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, false, false, true, false), true, List(false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, true, false, true, false), true, List(false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, true, false, true, false), true, List(false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, false, true, true, false), true, List(false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, false, true, true, false), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, true, true, true, false), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, true, true, true, false), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, false, false, false, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, false, false, false, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, true, false, false, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, true, false, false, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false)),
      (List(false, false, true, false, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false)),
      (List(true, false, true, false, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false)),
      (List(false, true, true, false, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false)),
      (List(true, true, true, false, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false)),
      (List(false, false, false, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false)),
      (List(true, false, false, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false)),
      (List(false, true, false, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false)),
      (List(true, true, false, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false)),
      (List(false, false, true, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false)),
      (List(true, false, true, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false)),
      (List(false, true, true, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false)),
      (List(true, true, true, true, true), true, List(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true))
    ))
  }

  @Test def multiplexerTwoTest(): Unit = {
    val input = new Bus(2)
    val select = new Bus(1)
    val output = new Wire

    val sim = new Simulation { }

    sim.multiplexer(input, select, output)

    busBusToOneTest(sim, input, select, output, List(
      (List(false, false), List(false), false),
      (List(true, false), List(false), true),
      (List(false, true), List(false), false),
      (List(true, true), List(false), true),
      (List(false, false), List(true), false),
      (List(true, false), List(true), false),
      (List(false, true), List(true), true),
      (List(true, true), List(true), true)
    ))
  }

  @Test def multiplexerThreeTest(): Unit = {
    val input = new Bus(3)
    val select = new Bus(2)
    val output = new Wire

    val sim = new Simulation { }

    sim.multiplexer(input, select, output)

    busBusToOneTest(sim, input, select, output, List(

      // first input selected
      (List(false, false, false), List(false, false), false),
      (List(true, false, false), List(false, false), true),
      (List(false, true, false), List(false, false), false),
      (List(true, true, false), List(false, false), true),
      (List(false, false, true), List(false, false), false),
      (List(true, false, true), List(false, false), true),
      (List(false, true, true), List(false, false), false),
      (List(true, true, true), List(false, false), true),

      // second input selected
      (List(false, false, false), List(true, false), false),
      (List(true, false, false), List(true, false), false),
      (List(false, true, false), List(true, false), true),
      (List(true, true, false), List(true, false), true),
      (List(false, false, true), List(true, false), false),
      (List(true, false, true), List(true, false), false),
      (List(false, true, true), List(true, false), true),
      (List(true, true, true), List(true, false), true),

      // third input selected
      (List(false, false, false), List(false, true), false),
      (List(true, false, false), List(false, true), false),
      (List(false, true, false), List(false, true), false),
      (List(true, true, false), List(false, true), false),
      (List(false, false, true), List(false, true), true),
      (List(true, false, true), List(false, true), true),
      (List(false, true, true), List(false, true), true),
      (List(true, true, true), List(false, true), true),

      // invalid input selected
      (List(false, false, false), List(true, true), false),
      (List(true, false, false), List(true, true), false),
      (List(false, true, false), List(true, true), false),
      (List(true, true, false), List(true, true), false),
      (List(false, false, true), List(true, true), false),
      (List(true, false, true), List(true, true), false),
      (List(false, true, true), List(true, true), false),
      (List(true, true, true), List(true, true), false)
    ))
  }

  @Test def eightBitALUTest(): Unit = {
    val a = new Bus(8)
    val b = new Bus(8)
    val s = new Bus(5)
    val r = new Bus(8)

    val sim = new Simulation { }

    val sub, zero = new Wire
    sim.ALU(a, b, s, sub, r, zero)
    sim.run()

    // setup basic values
    a(3)(signal = true)
    a(1)(signal = true)
    b(1)(signal = true)
    assertEquals(10, a.value().toInt)
    assertEquals(2, b.value().toInt)
    assertEquals(0, r.value().toInt)
    assertEquals(true, zero())

    // and
    sim.run()
    assertEquals(2, r.value().toInt)
    assertEquals(false, zero())

    // or
    s(0)(signal = true)
    sim.run()
    assertEquals(10, r.value().toInt)
    assertEquals(false, zero())

    // xor
    s(0)(signal = false)
    s(1)(signal = true)
    sim.run()
    assertEquals(8, r.value().toInt)
    assertEquals(false, zero())

    // nor
    s(0)(signal = true)
    s(1)(signal = true)
    sim.run()
    assertEquals(245, r.value().toInt)
    assertEquals(false, zero())

    // add
    s(0)(signal = false)
    s(1)(signal = false)
    s(2)(signal = true)
    sim.run()
    assertEquals(12, r.value().toInt)
    assertEquals(false, zero())

    // sub
    sub(signal = true)
    sim.run()
    assertEquals(8, r.value().toInt)
    assertEquals(false, zero())

    // sub to zero for equality
    b(3)(signal = true)
    sim.run()
    assertEquals(0, r.value().toInt)
    assertEquals(true, zero())

    // sub for less than
    s(0)(signal = true)
    sim.run()
    assertEquals(0, r.value().toInt)
    assertEquals(true, zero())

    b(3)(signal = false)
    sim.run()
    assertEquals(0, r.value().toInt)
    assertEquals(true, zero())

    a(3)(signal = false)
    b(3)(signal = true)
    sim.run()
    assertEquals(1, r.value().toInt)
    assertEquals(false, zero())
  }
}