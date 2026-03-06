import diff.inverse


class MatrixTests extends munit.FunSuite {
    val eps: Double = 1e-6
    
    test("Singular"):
        val a = Array(Array(1.0, 2.0), Array(2.0, 4.0))
        val invA = inverse(a)
        val result = invA match
            case None => "Correctly identified as singular"
            case Some(_) => fail("Expected matrix to be singular but got an inverse")
        assertEquals(result, "Correctly identified as singular")

    test("Inverse 1"):
        val a = Array(Array(2.0, 0.0), Array(0.0, 2.0))
        val invA = inverse(a)
        val result = invA match
            case None => fail("Expected inverse to exist but got None")
            case Some(inv) => "Inverse computed successfully"
            
        assertEquals(result, "Inverse computed successfully")
        val expected = Array(Array(0.5, 0.0), Array(0.0, 0.5))
        val obtained = invA.get
        assert(
            math.abs(obtained(0)(0) - expected(0)(0)) < eps &&
            math.abs(obtained(0)(1) - expected(0)(1)) < eps &&
            math.abs(obtained(1)(0) - expected(1)(0)) < eps &&
            math.abs(obtained(1)(1) - expected(1)(1)) < eps,
            s"Expected ${expected.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")} but got ${obtained.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")}"
        )
    
}