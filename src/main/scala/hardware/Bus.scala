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

object Bus {
  def apply(width: Int): Bus = {
    Bus(width, List.fill(width)(new Wire))
  }
}

case class Bus(width: Int, wires: List[Wire]) {

  def apply(wire: Int): Wire = wires(wire)

  /**
   * Returns a specified portion of the bus wires
   * (0,4) would return wires (0,1,2,3)
   * @param start first wire to return
   * @param end last wire to return + 1
   * @return new bus object
   */
  def split(start: Int, end: Int): Bus = {
    new Bus(end - start, wires.slice(start, end))
  }

  /**
   * Divides the bus into a list of smaller busses
   */
  def divide(count: Int): List[Bus] = {
    val newWidth = width / count

    @tailrec def go(index: Int, acc: List[Bus]): List[Bus] = {
      if(index >= count) {
        acc
      } else {
        val start = newWidth * index
        val end = newWidth * (index + 1)
        go(index + 1, split(start, end) :: acc)
      }
    }
    go(0, Nil).reverse
  }

  def value(): BigInt = {

    @tailrec def read(bit: Int, bundle: List[Wire], result: BigInt): BigInt = bundle match {
      case head :: tail => read(bit+1, tail, if( head() ) { result.setBit(bit) } else { result })
      case Nil          => result
    }

    read(0, wires, BigInt(0))
  }

  override def toString: String = {

    @tailrec def go(wires: List[Wire], string: String): String = wires match {
      case head :: Nil  => (if(head.getSignal) {"1"} else {"0"}) + string
      case head :: tail => go(tail, (if(head.getSignal) {",1"} else {",0"}) + string )
      case Nil          => string
    }

    "Bus(" + width + ",(" + go(wires, "),") + value() + ")"
  }
}
