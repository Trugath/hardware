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

/**
 * Created by Elliot on 01/09/2015.
 */
object Z80 {

  import Components._

  // http://righto.com/files/z80-alu-schematic.pdf
  def ALUCore(op1: Wire, op2: Wire, RSV: Bus, cin: Wire, cout: Wire, result: Wire)(implicit sim: Simulation): Unit = {

    val i1, i2, i3 = new Wire
    val internalCarry = new Wire

    orGate(op1, op2, i1)
    andGate(op1, op2, i2)

    andGate(cin, i1, i3)
    nOrGate(i3, i2, RSV(1), internalCarry)

    nOrGate(RSV(0), internalCarry, cout)

    val i4, i5, i6, i7, i8 = new Wire

    andGate(cin, op2, op1, i4)
    orGate(cin, op2, op1, i5)
    orGate(RSV(2), internalCarry, i6)
    andGate(i5, i6, i7)
    nOrGate(i4, i7, i8)

    inverter(i8, result)
  }

  def ALU4bit(op1: Bus, op2: Bus, RSV: Bus, cin: Wire, cout: Wire, result: Bus)(implicit sim: Simulation): Unit = {
    assert(op1.width == 4)
    assert(op2.width == 4)
    assert(RSV.width == 3)
    assert(result.width == 4)

    val i1, i2, i3 = new Wire
    ALUCore(op1(0), op2(0), RSV, cin, i1,   result(0))
    ALUCore(op1(1), op2(1), RSV, i1,  i2,   result(1))
    ALUCore(op1(2), op2(2), RSV, i2,  i3,   result(2))
    ALUCore(op1(3), op2(3), RSV, i2,  cout, result(3))
  }
}
