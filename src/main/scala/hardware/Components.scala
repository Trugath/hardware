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

package hardware

import scala.annotation.tailrec

/**
 * Created by Elliot on 01/09/2015.
 */
object Components {

  def logicDiode(input: Wire, output: Wire)(implicit sim: Simulation) {
    val gateVariance = sim.getGateVariance
    def passAction() {
      val inputSig = input.getSignal
      sim.afterDelay(sim.GateDelay + gateVariance) { output setSignal inputSig }
    }
    input addAction passAction
  }

  def nAndGate(a1: Wire, a2: Wire, a3: Wire, output: Wire)(implicit sim: Simulation): Unit = {
    val gateVariance = sim.getGateVariance
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      val a3Sig = a3.getSignal
      sim.afterDelay(sim.GateDelay + gateVariance) { output setSignal !(a1Sig & a2Sig & a3Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
    a3 addAction andAction
  }

  def nAndGate(a1: Wire, a2: Wire, output: Wire)(implicit sim: Simulation): Unit = nAndGate(a1, a2, Wire.trueWire, output)

  def inverter(input: Wire, output: Wire)(implicit sim: Simulation): Unit = nAndGate(input, input, output)

  def andGate(a1: Wire, a2: Wire, output: Wire)(implicit sim: Simulation): Unit = {
    val i1: Wire = new Wire
    nAndGate(a1, a2, i1)
    inverter(i1, output)
  }

  def andGate(a1: Wire, a2: Wire, a3: Wire, output: Wire)(implicit sim: Simulation): Unit = {
    val i1: Wire = new Wire
    andGate(a1, a2, i1)
    andGate(i1, a3, output)
  }

  def andGate(a1: Wire, a2: Wire, a3: Wire, a4: Wire, output: Wire)(implicit sim: Simulation): Unit = {
    val i1, i2 = new Wire
    andGate(a1, a2, i1)
    andGate(a3, a4, i2)
    andGate(i1, i2, output)
  }

  def orGate(a1: Wire, a2: Wire, output: Wire)(implicit sim: Simulation): Unit = {
    val i1, i2 = new Wire
    inverter(a1, i1)
    inverter(a2, i2)
    nAndGate(i1, i2, output)
  }

  def orGate(a1: Wire, a2: Wire, a3: Wire, output: Wire)(implicit sim: Simulation): Unit = {
    val i1, i2, i3 = new Wire
    inverter(a1, i1)
    inverter(a2, i2)
    inverter(a3, i3)
    nAndGate(i1, i2, i3, output)
  }

  /**
   * Meant to create a minimal or gate tree
   */
  def orGate(a: Bus, output: Wire)(implicit sim: Simulation): Unit = {
    @tailrec def go(a: List[Wire]): Unit = a match {
      case i1 :: i2 :: Nil =>
        orGate( i1, i2, output )
      case i1 :: i2 :: tail =>
        val c1 = new Wire
        orGate( i1, i2, c1 )
        go( tail :+ c1 )
      case head :: Nil =>
        logicDiode( head, output )
      case Nil =>
    }
    go(a.wires)
  }

  def xOrGate(a1: Wire, a2: Wire, output: Wire)(implicit sim: Simulation): Unit = {
    val i1, i2, i3 = new Wire
    nAndGate(a1, a2, i1)
    nAndGate(a1, i1, i2)
    nAndGate(a2, i1, i3)
    nAndGate(i2, i3, output)
  }

  def nOrGate(a1: Wire, a2: Wire, output: Wire)(implicit sim: Simulation): Unit = {
    val i1 = new Wire
    orGate(a1, a2, i1)
    inverter(i1, output)
  }

  def nOrGate(a1: Wire, a2: Wire, a3: Wire, output: Wire)(implicit sim: Simulation): Unit = {
    val i1 = new Wire
    orGate(a1, a2, a3, i1)
    inverter(i1, output)
  }

  def nOrGate(a: Bus, output: Wire)(implicit sim: Simulation): Unit = {
    val i1 = new Wire
    orGate(a, i1)
    inverter(i1, output)
  }

  def halfAdder(a: Wire, b: Wire, sum: Wire, carry: Wire)(implicit sim: Simulation): Unit = {
    val d = new Wire
    val e = new Wire
    orGate(a, b, d)
    andGate(a, b, carry)
    inverter(carry, e)
    andGate(d, e, sum)
  }

  def fullAdder(a: Wire, b: Wire, carryIn: Wire, sum: Wire, carryOut: Wire)(implicit sim: Simulation): Unit = {
    val s = new Wire
    val c1 = new Wire
    val c2 = new Wire
    halfAdder(a, carryIn, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, carryOut)
  }

  // classical rising edge d-type flip flop
  def dFlipFlop(d: Wire, clk: Wire, nPreset: Wire, nClear: Wire, q: Wire, nq: Wire)(implicit sim: Simulation): Unit = {
    val c1 = new Wire
    val c2 = new Wire
    val c3 = new Wire
    val c4 = new Wire

    nAndGate(c3, c1, c4)
    nAndGate(c4, clk, c1)
    nAndGate(c1, clk, c3, c2)
    nAndGate(c2, d, c3)

    nAndGate(c1, nPreset, nq, q)
    nAndGate(c2, nClear, q, nq)
  }


  def dFlipFlop(d: Wire, clk: Wire, nClear: Wire, q: Wire, nq: Wire)(implicit sim: Simulation): Unit = dFlipFlop(d, clk, Wire.trueWire, nClear, q, nq)

  // serial in serial out
  def registerSISO(d: Wire, clk: Wire, nClear: Wire, output: Wire, size: Int)(implicit sim: Simulation): Unit = {
    @tailrec def attachFlipFlop(d: Wire, o: Wire, left: Int): Unit = {
      if( left <= 0 ) {
        dFlipFlop(d, clk, nClear, output, new Wire)
      } else {
        dFlipFlop(d, clk, nClear, o, new Wire)
        attachFlipFlop(o, new Wire, left - 1)
      }
    }
    attachFlipFlop(d, new Wire, size - 1)
  }

  // serial in parallel out
  def registerSIPO(d: Wire, clk: Wire, nClear: Wire, output: Bus)(implicit sim: Simulation): Unit = {
    @tailrec def attachFlipFlop(data: Wire, bundle: List[Wire]): Unit = bundle match {
      case head :: tail =>  dFlipFlop(data, clk, nClear, head, new Wire)
        attachFlipFlop(head, tail)
      case Nil          =>
    }
    attachFlipFlop(d, output.wires)
  }

  // parallel in serial out
  def registerPISO(d: Bus, shift: Wire, clk: Wire, nClear: Wire, output: Wire)(implicit sim: Simulation): Unit = {
    val c1 = new Wire
    inverter(shift, c1)

    @tailrec def attachGates(data: Wire, bundle: List[Wire]): Unit = bundle match {
      case head :: tail =>
        val d1, d2, d3, d4 = new Wire
        dFlipFlop(data, clk, nClear, d1, new Wire)
        nAndGate(head, c1, d2)
        nAndGate(shift, d1, d3)
        nAndGate(d2, d3, d4)
        attachGates(d4, tail)
      case Nil          =>
        dFlipFlop(data, clk, nClear, output, new Wire)
    }

    attachGates(d.wires.head, d.wires.tail)
  }

  /*
   * line decoder (1-bit to 2 wire)
   */
  def oneToTwoDecoder(input: Wire, en: Wire, output1: Wire, output2: Wire)(implicit sim: Simulation): Unit = {
    val c1 = new Wire
    inverter(input, c1)
    andGate(c1, en, output1)
    andGate(input, en, output2)
  }

  /*
   * line decoder (3-bit to 8 wire)
   */
  def threeToEightDecoder(input: Bus, en: Wire, output: Bus)(implicit sim: Simulation): Unit = {
    assert(input.width == 3)
    assert(output.width == 8)

    val (a,b,c) = (input(0), input(1), input(2))
    val na, nb, nc = new Wire

    inverter(a, na)
    inverter(b, nb)
    inverter(c, nc)

    andGate(na, nb, nc, en, output(0))
    andGate(a, nb, nc, en, output(1))
    andGate(na, b, nc, en, output(2))
    andGate(a, b, nc, en, output(3))
    andGate(na, nb, c, en, output(4))
    andGate(a, nb, c, en, output(5))
    andGate(na, b, c, en, output(6))
    andGate(a, b, c, en, output(7))
  }

  /*
   * arbitrary size line decoder
   */
  def decoder(input: Bus, en: Wire, output: Bus)(implicit sim: Simulation): Unit = {

    // must be correct input output widths
    assert(output.width == scala.math.pow(2, input.width).toInt)

    if(input.width == 1 && output.width == 2) {
      oneToTwoDecoder(input(0), en, output(0), output(1))
    } else if(input.width == 2 && output.width == 4) {
      val c1, c2 = new Wire
      oneToTwoDecoder(input(1), en, c1, c2)
      oneToTwoDecoder(input(0), c1, output(0), output(1))
      oneToTwoDecoder(input(0), c2, output(2), output(3))
    } else if(input.width == 3 && output.width == 8) {
      threeToEightDecoder(input, en, output)
    } else if(input.width == 4 && output.width == 16) {
      val i1 = Bus(4)
      decoder(input.divide(2)(1), en, i1)
      decoder(input.divide(2)(0), i1(0), output.divide(4)(0))
      decoder(input.divide(2)(0), i1(1), output.divide(4)(1))
      decoder(input.divide(2)(0), i1(2), output.divide(4)(2))
      decoder(input.divide(2)(0), i1(3), output.divide(4)(3))
    } else if(input.width == 5 && output.width == 32) {
      val i1 = Bus(4)
      decoder(input.split(3, 5), en, i1)
      threeToEightDecoder(input.split(0, 3), i1(0), output.divide(4)(0))
      threeToEightDecoder(input.split(0, 3), i1(1), output.divide(4)(1))
      threeToEightDecoder(input.split(0, 3), i1(2), output.divide(4)(2))
      threeToEightDecoder(input.split(0, 3), i1(3), output.divide(4)(3))
    } else {
      val i1 = Bus(8)
      val splitStart = input.width - 3
      val splitEnd = input.width
      threeToEightDecoder(input.split(splitStart, splitEnd), en, i1)
      decoder(input.split(0, splitStart), i1(0), output.divide(8)(0))
      decoder(input.split(0, splitStart), i1(1), output.divide(8)(1))
      decoder(input.split(0, splitStart), i1(2), output.divide(8)(2))
      decoder(input.split(0, splitStart), i1(3), output.divide(8)(3))
      decoder(input.split(0, splitStart), i1(4), output.divide(8)(4))
      decoder(input.split(0, splitStart), i1(5), output.divide(8)(5))
      decoder(input.split(0, splitStart), i1(6), output.divide(8)(6))
      decoder(input.split(0, splitStart), i1(7), output.divide(8)(7))
    }
  }

  /**
   * data line multiplexer. chooses an input line and routes it to the output based on the select value
   */
  def multiplexer(d: Bus, select: Bus, output: Wire)(implicit sim: Simulation): Unit = {

    // select bus must be capable of choosing all of the input bus lines
    assert(log2(d.width) <= select.width)

    // use a decoder on the select bus to retrieve the individual signals
    val c1 = Bus( scala.math.pow(2, select.width).toInt )
    decoder(select, Wire.trueWire, c1)

    // ensure we have enough select lines
    assert(d.width <= c1.width)

    // wire up select and input lines to and gates
    val c2 = Bus(d.width)
    d.wires.zip( c1.wires ).zip( c2.wires ).foreach { case ((i1, i2), o) => andGate(i1, i2, o) }

    // gather output, only the selected input can effect the output
    orGate(c2, output)
  }

  def oneBitALU(a: Wire, b: Wire, select: Bus, sub: Wire, cin: Wire, less: Wire, cout: Wire, set: Wire, result: Wire)(implicit sim: Simulation): Unit = {
    // bus should have minimum of 3 bits
    assert(select.width >= 3)

    // internal result wiring
    val i1 = Bus(6)

    // and
    andGate(a, b, i1(0))

    // or
    orGate(a, b, i1(1))

    // xor
    xOrGate(a, b, i1(2))

    // nor
    nOrGate(a, b, i1(3))

    // full adder with sub logic
    val c1 = new Wire
    xOrGate(sub, b, c1)
    fullAdder(a, c1, cin, i1(4), cout)

    // attach set output
    logicDiode(i1(4), set)

    // less
    logicDiode(less, i1(5))

    // mux the results
    multiplexer(i1, select, result)
  }

  def ALU(a: Bus, b: Bus, select: Bus, sub: Wire, result: Bus, zero: Wire )(implicit sim: Simulation): Unit = {

    // works on busses the same width
    assert(b.width == a.width)
    assert(result.width == b.width)

    // bus should have minimum of 3 bits
    assert(select.width >= 3)

    @tailrec def attachOneBitALU(a: List[Wire], b: List[Wire], select: Bus, sub: Wire, cin: Wire, set: Wire, result: List[Wire]): Unit = a match {
      case head :: Nil  =>
        oneBitALU(a.head, b.head, select, sub, cin, Wire.falseWire, Wire.falseWire, set, result.head)
      case head :: tail =>
        val cout = new Wire
        oneBitALU(a.head, b.head, select, sub, cin, Wire.falseWire, cout, Wire.falseWire, result.head)
        attachOneBitALU(a.tail, b.tail, select, sub, cout, set, result.tail)
      case Nil =>
    }
    val set, cout = new Wire
    oneBitALU(a.wires.head, b.wires.head, select, sub, sub, set, cout, Wire.falseWire, result.wires.head)
    attachOneBitALU(a.wires.tail, b.wires.tail, select, sub, cout, set, result.wires.tail)

    // zero line
    nOrGate(result, zero)
  }
}
