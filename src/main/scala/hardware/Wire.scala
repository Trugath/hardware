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

object Wire {
  val trueWire = new Wire {
    override def getSignal: Boolean = true
    override def setSignal(signal: Boolean): Unit = {}
    override def addAction(a: Action): Unit = {}
  }

  val falseWire = new Wire {
    override def getSignal: Boolean = false
    override def setSignal(signal: Boolean): Unit = {}
    override def addAction(a: Action): Unit = {}
  }
}

class Wire {
  private var sigVal: Boolean = false
  private var actions: List[Action] = List()

  def apply(): Boolean = getSignal
  def apply(signal: Boolean) = setSignal(signal)

  def getSignal: Boolean = sigVal

  def setSignal(signal: Boolean): Unit = {
    if (signal != sigVal) {
      sigVal = signal
      actions.foreach(action => action())
    }
  }

  def addAction(a: Action): Unit = {
    actions = a :: actions; a()
  }
}

