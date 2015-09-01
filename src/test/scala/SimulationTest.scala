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

import hardware.{Components, Bus, Simulation, Wire}

import org.junit.Test
import org.junit.Assert._

/**
 * Created by elliot on 12/02/14.
 */
class SimulationTest {

  import Components._
  import Util._

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
    val bus = Bus(2)
    assertEquals(0, bus.value().toInt)
    bus(0)(signal = true)
    assertEquals(1, bus.value().toInt)

    bus(1)(signal = true)
    assertEquals(3, bus.value().toInt)
  }

  @Test def inverterTest(): Unit = {
    val input, output = new Wire

    implicit val sim = new Simulation { }

    // connect the inverter
    inverter(input, output)

    oneToOneTest(sim, input, output, List(
      (false, true),
      (true, false)
    ))
  }

}