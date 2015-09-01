import hardware.{Bus, Wire, Simulation}
import org.junit.Assert._

/**
 * Created by Elliot on 01/09/2015.
 */
object Util {
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

  def sixToTwoTest(sim: Simulation, input1: Wire, input2: Wire, input3: Wire, input4: Wire, input5: Wire, input6: Wire, output1: Wire, output2: Wire, table: List[(Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean)]) {
    table.foreach{ case (i1, i2, i3, i4, i5, i6, o1, o2) =>
      input1.setSignal(i1)
      input2.setSignal(i2)
      input3.setSignal(i3)
      input4.setSignal(i4)
      input5.setSignal(i5)
      input6.setSignal(i6)
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
}
