package funcdef

trait TwoVarFuncDef:
    def apply(x: Array[Double]): Double 
    def name: String
    def toLaTeX: String

object Quadratic extends TwoVarFuncDef:
    def apply(x: Array[Double]): Double = 
        math.pow(x(0) - math.Pi, 2) + math.pow(x(1) - math.exp(1), 2)
    def name: String = "Quadratic"
    def toLaTeX: String = "f(x) = (x_1 - \\pi)^2 + (x_2 - e)^2"


object CrossTerms extends TwoVarFuncDef:
    def apply(x: Array[Double]): Double = 
        math.pow(x(0) - math.Pi, 2) + math.pow(x(1) - math.exp(1), 2) + 0.5 * x(0) * x(1)
    def name: String = "Cross Terms"
    def toLaTeX: String = "f(x) = (x_1 - \\pi)^2 + (x_2 - e)^2 + 0.5 x_1 x_2"


object Ackley extends TwoVarFuncDef:
    def apply(x: Array[Double]): Double = 
        -20 * math.exp(-0.2 * math.sqrt(0.5 * (math.pow(x(0), 2) + math.pow(x(1), 2)))) - 
        math.exp(0.5 * (math.cos(2 * math.Pi * x(0)) + math.cos(2 * math.Pi * x(1)))) + 
        math.E + 20
    def name: String = "Ackley"
    def toLaTeX: String = "f(x) = -20 e^{-0.2 \\sqrt{0.5 (x_1^2 + x_2^2)}} - e^{0.5 (\\cos(2 \\pi x_1) + \\cos(2 \\pi x_2))} + e + 20"


val functions: List[TwoVarFuncDef] = List(Quadratic, CrossTerms, Ackley)