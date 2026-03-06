import diff.hessian
import funcdef.Quadratic

class HessianTests extends munit.FunSuite {
    val eps: Double = 1e-6
    
    test("Hessian of Quadratic"):
        val x = Array(math.Pi, math.exp(1))
        val hess = hessian(Quadratic, x)
        val expected = Array(Array(2.0, 0.0), Array(0.0, 2.0))
        assert(
            math.abs(hess(0)(0) - expected(0)(0)) < eps &&
            math.abs(hess(0)(1) - expected(0)(1)) < eps &&
            math.abs(hess(1)(0) - expected(1)(0)) < eps &&
            math.abs(hess(1)(1) - expected(1)(1)) < eps,
            s"Expected ${expected.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")} but got ${hess.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")}"
        )
    
}