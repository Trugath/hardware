package hardware

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