import hardware.{Bus, Simulation, Wire, Components}
import org.junit.Assert._
import org.junit.Test

/**
 * Created by Elliot on 01/09/2015.
 */
class ComponentTest {
  import Components._
  import Util._


  @Test def nAndGateTest(): Unit = {
    val input1, input2, output = new Wire

    implicit val sim = new Simulation { }

    // connect the nAnd gate
    nAndGate(input1, input2, output)

    twoToOneTest(sim, input1, input2, output, List(
      (false, false, true),
      (false, true, true),
      (true, false, true),
      (true, true, false)
    ))
  }

  @Test def nAndGate3Test(): Unit = {
    val input1, input2, input3, output = new Wire

    implicit val sim = new Simulation { }

    // connect the nAnd gate
    nAndGate(input1, input2, input3, output)

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

    implicit val sim = new Simulation { }

    // connect the and gate
    andGate(input1, input2, output)

    twoToOneTest(sim, input1, input2, output, List(
      (false, false, false),
      (false, true, false),
      (true, false, false),
      (true, true, true)
    ))
  }

  @Test def orGateTest(): Unit = {
    val input1, input2, output = new Wire

    implicit val sim = new Simulation { }

    // connect the or gate
    orGate(input1, input2, output)

    twoToOneTest(sim, input1, input2, output, List(
      (false, false, false),
      (false, true, true),
      (true, false, true),
      (true, true, true)
    ))
  }

  @Test def orBusGateTest(): Unit = {
    val input = Bus(4)
    val output = new Wire

    implicit val sim = new Simulation { }

    // connect the or gate
    orGate(input, output)

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

    implicit val sim = new Simulation { }

    // connect the xor gate
    xOrGate(input1, input2, output)

    twoToOneTest(sim, input1, input2, output, List(
      (false, false, false),
      (false, true, true),
      (true, false, true),
      (true, true, false)
    ))
  }

  @Test def halfAdderTest(): Unit = {
    val input1, input2, sum, carry = new Wire

    implicit val sim = new Simulation { }

    // connect the half adder
    halfAdder(input1, input2, sum, carry)

    twoToTwoTest(sim, input1, input2, sum, carry, List(
      (false, false, false, false),
      (false, true, true, false),
      (true, false, true, false),
      (true, true, false, true)
    ))
  }

  @Test def fullAdderTest(): Unit = {
    val input1, input2, carryIn, sum, carryOut = new Wire

    implicit val sim = new Simulation { }

    // connect the full adder
    fullAdder(input1, input2, carryIn, sum, carryOut)

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

    implicit val sim = new Simulation { }

    // connect the flip flop
    dFlipFlop(data, clock, nPreset, nClear, q, nq)
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

    implicit val sim = new Simulation { }

    // connect the register
    registerSISO(data, clock, nClear, output, 4)
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
    val output = Bus(4)

    implicit val sim = new Simulation { }

    registerSIPO(data, clock, nClear, output)

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
    val data = Bus(4)
    val shift, clock, nClear, output = new Wire

    implicit val sim = new Simulation { }

    registerPISO(data, shift, clock, nClear, output)

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

    implicit val sim = new Simulation { }

    // connect the decoder
    oneToTwoDecoder(input, enable, output1, output2)

    twoToTwoTest(sim, input, enable, output1, output2, List(
      (false, false, false, false),
      (true, false, false, false),
      (false, true, true, false),
      (true, true, false, true)
    ))
  }

  @Test def twoToFourDecoderTest(): Unit = {
    val input = Bus(2)
    val output = Bus(4)
    val enable = new Wire

    implicit val sim = new Simulation { }

    // connect the decoder
    decoder(input, enable, output)

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
    val input = Bus(4)
    val output = Bus(16)
    val enable = new Wire

    implicit val sim = new Simulation { }

    // connect the decoder
    decoder(input, enable, output)

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
    val input = Bus(5)
    val output = Bus(32)
    val enable = new Wire

    implicit val sim = new Simulation { }

    // connect the decoder
    decoder(input, enable, output)

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
    val input = Bus(2)
    val select = Bus(1)
    val output = new Wire

    implicit val sim = new Simulation { }

    multiplexer(input, select, output)

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
    val input = Bus(3)
    val select = Bus(2)
    val output = new Wire

    implicit val sim = new Simulation { }

    multiplexer(input, select, output)

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
    val a = Bus(8)
    val b = Bus(8)
    val s = Bus(5)
    val r = Bus(8)

    implicit val sim = new Simulation { }

    val sub, zero = new Wire
    ALU(a, b, s, sub, r, zero)
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
