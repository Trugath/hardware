package hardware

import scala.annotation.tailrec

class Bus(val width: Int, val wires: List[Wire]) {

  def this(width: Int) {
    this(width, List.fill(width)(new Wire))
  }

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
