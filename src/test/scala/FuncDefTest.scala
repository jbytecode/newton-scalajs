import funcdef.Quadratic
import funcdef.CrossTerms
import funcdef.Ackley

class FunctionDefinitions extends munit.FunSuite {

  val eps: Double = 1e-6

  test("Quadratic function"):
    val x = Array(math.Pi, math.exp(1))
    val obtained = Quadratic(x)
    val expected = 0.0
    assertEquals(obtained, expected)
  
  test("Cross Terms function"):
    val x = Array(math.Pi, math.exp(1))
    val obtained = CrossTerms(x)
    val expected = 4.269867111336783
    assert(
      math.abs(obtained - expected) < eps,
      s"Expected $expected but got $obtained"
    )
  
  test("Ackley function"): 
    val x = Array(0.0, 0.0)
    val obtained = Ackley(x)
    val expected = 0.0
    assert(
      math.abs(obtained - expected) < eps,
      s"Expected $expected but got $obtained"
    )
}
