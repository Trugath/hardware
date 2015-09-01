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
import scala.util.Random

abstract class Simulation {

  case class WorkItem(time: Long, action: Action)

  private type Agenda = List[WorkItem]
  private var agenda: Agenda = List()
  private var currentTime = 0L

  // add a small amount of variance
  private val random: Random = new Random()

  private def insert(ag: Agenda, item: WorkItem): Agenda = {
    if (ag.isEmpty || item.time < ag.head.time)
      item :: ag
    else
      ag.head :: insert(ag.tail, item)
  }

  // delay lower bounds
  val GateDelay = 1000

  def getGateVariance: Int = random.nextInt(10)

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = WorkItem(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def next(): Unit = {
    agenda match {
      case WorkItem(time, action) :: rest =>
        agenda = rest
        currentTime = time
        action()
      case List() =>
    }
  }

  // run the simulation till its stable
  def run(): Long = {
    val startTime = currentTime
    while (agenda.nonEmpty)
      next()
    currentTime - startTime
  }

  def probe(name: String, wire: Wire): Unit = {
    wire addAction { () =>
      println(name + " " + currentTime + " new_value = " + wire.getSignal)
    }
  }
}