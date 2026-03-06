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
    math.pow(x(0) - math.Pi, 2) + math.pow(x(1) - math.exp(1), 2) + 0.5 * x(
      0
    ) * x(1)
  def name: String = "Cross Terms"
  def toLaTeX: String = "f(x) = (x_1 - \\pi)^2 + (x_2 - e)^2 + 0.5 x_1 x_2"

object Ackley extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    -20 * math.exp(
      -0.2 * math.sqrt(0.5 * (math.pow(x(0), 2) + math.pow(x(1), 2)))
    ) -
      math.exp(
        0.5 * (math.cos(2 * math.Pi * x(0)) + math.cos(2 * math.Pi * x(1)))
      ) +
      math.E + 20
  def name: String = "Ackley"
  def toLaTeX: String =
    "f(x) = -20 e^{-0.2 \\sqrt{0.5 (x_1^2 + x_2^2)}} - e^{0.5 (\\cos(2 \\pi x_1) + \\cos(2 \\pi x_2))} + e + 20"

object Rastrigin extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    20 + math.pow(x(0), 2) + math.pow(x(1), 2) - 10 * (math.cos(
      2 * math.Pi * x(0)
    ) + math.cos(2 * math.Pi * x(1)))
  def name: String = "Rastrigin"
  def toLaTeX: String =
    "f(x) = 20 + x_1^2 + x_2^2 - 10 (\\cos(2 \\pi x_1) + \\cos(2 \\pi x_2))"

object Sphere extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    math.pow(x(0), 2) + math.pow(x(1), 2)
  def name: String = "Sphere"
  def toLaTeX: String = "f(x) = x_1^2 + x_2^2"

object Bohacevsky extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    math.pow(x(0), 2) + 2 * math.pow(x(1), 2) - 0.3 * math.cos(
      3 * math.Pi * x(0)
    ) - 0.4 * math.cos(4 * math.Pi * x(1)) + 0.7
  def name: String = "Bohacevsky"
  def toLaTeX: String =
    "f(x) = x_1^2 + 2 x_2^2 - 0.3 \\cos(3 \\pi x_1) - 0.4 \\cos(4 \\pi x_2) + 0.7"

object Rosenbrock extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    math.pow(1 - x(0), 2) + 100 * math.pow(x(1) - math.pow(x(0), 2), 2)
  def name: String = "Rosenbrock"
  def toLaTeX: String = "f(x) = (1 - x_1)^2 + 100 (x_2 - x_1^2)^2"

object Easom extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    -math.cos(x(0)) * math.cos(x(1)) * math.exp(
      -math.pow(x(0) - math.Pi, 2) - math.pow(x(1) - math.Pi, 2)
    )
  def name: String = "Easom"
  def toLaTeX: String =
    "f(x) = -\\cos(x_1) \\cos(x_2) e^{-((x_1 - \\pi)^2 + (x_2 - \\pi)^2)}"

object EggHolder extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    -(x(1) + 47) * math.sin(math.sqrt(math.abs(x(1) + x(0) / 2 + 47))) - x(
      0
    ) * math.sin(math.sqrt(math.abs(x(0) - (x(1) + 47))))
  def name: String = "Egg Holder"
  def toLaTeX: String =
    "f(x) = -(x_2 + 47) \\sin\\left(\\sqrt{\\left|x_2 + \\frac{x_1}{2} + 47\\right|}\\right) - x_1 \\sin\\left(\\sqrt{\\left|x_1 - (x_2 + 47)\\right|}\\right)"

object Griewank extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    1 + (math.pow(x(0), 2) + math.pow(x(1), 2)) / 4000 - math.cos(x(0)) * math
      .cos(x(1) / math.sqrt(2))
  def name: String = "Griewank"
  def toLaTeX: String =
    "f(x) = 1 + \\frac{x_1^2 + x_2^2}{4000} - \\cos(x_1) \\cos\\left(\\frac{x_2}{\\sqrt{2}}\\right)"

object Levy extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    math.pow(math.sin(3 * math.Pi * x(0)), 2)
      + math
        .pow(x(0) - 1, 2) * (1 + math.pow(math.sin(3 * math.Pi * x(0) + 1), 2))
      + math.pow(x(1) - 1, 2) * (1 + math.pow(math.sin(2 * math.Pi * x(1)), 2))
  def name: String = "Levy"
  def toLaTeX: String =
    "f(x) = \\sin^2(3 \\pi x_1) + (x_1 - 1)^2 (1 + \\sin^2(3 \\pi x_1 + 1)) + (x_2 - 1)^2 (1 + \\sin^2(2 \\pi x_2))"

object Michalewicz extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    -math.sin(x(0)) * math.pow(math.sin(x(0) * x(0) / math.Pi), 20) - math.sin(
      x(1)
    ) * math.pow(math.sin(2 * x(1) * x(1) / math.Pi), 20)
  def name: String = "Michalewicz"
  def toLaTeX: String =
    "f(x) = -\\sin(x_1) \\sin^{20}\\left(\\frac{x_1^2}{\\pi}\\right) - \\sin(x_2) \\sin^{20}\\left(\\frac{2 x_2^2}{\\pi}\\right)"

object HolderTable extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    -math.abs(
      math.sin(x(0)) * math.cos(x(1)) * math.exp(
        math.abs(1 - math.sqrt(x(0) * x(0) + x(1) * x(1)) / math.Pi)
      )
    )
  def name: String = "Holder Table"
  def toLaTeX: String =
    "f(x) = -\\left| \\sin(x_1) \\cos(x_2) e^{\\left| 1 - \\frac{\\sqrt{x_1^2 + x_2^2}}{\\pi} \\right|} \\right|"

object ThreeHumpCamel extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    2 * math.pow(x(0), 2) - 1.05 * math.pow(x(0), 4) + math.pow(
      x(0),
      6
    ) / 6 + math.pow(x(1), 2)
  def name: String = "Three Hump Camel"
  def toLaTeX: String =
    "f(x) = 2 x_1^2 - 1.05 x_1^4 + \\frac{x_1^6}{6} + x_2^2"

object Zakharov extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    math.pow(x(0), 2) + math.pow(x(1), 2) + math.pow(0.5 * x(0) + 0.5 * x(1), 2)
  def name: String = "Zakharov"
  def toLaTeX: String =
    "f(x) = x_1^2 + x_2^2 + \\left(\\frac{x_1 + x_2}{2}\\right)^2"

object DropWave extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    -math.exp(-2 * math.sqrt(math.pow(x(0), 2) + math.pow(x(1), 2))) * math.cos(
      4 * math.sqrt(math.pow(x(0), 2) + math.pow(x(1), 2))
    )
  def name: String = "Drop Wave"
  def toLaTeX: String =
    "f(x) = -e^{-2 \\sqrt{x_1^2 + x_2^2}} \\cos\\left(4 \\sqrt{x_1^2 + x_2^2}\\right)"

object Shubert extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    (1 to 5).map(i => i * math.cos((i + 1) * x(0) + i)).sum * (1 to 5)
      .map(i => i * math.cos((i + 1) * x(1) + i))
      .sum
  def name: String = "Shubert"
  def toLaTeX: String =
    "f(x) = \\left(\\sum_{i=1}^5 i \\cos((i + 1) x_1 + i)\\right) \\left(\\sum_{i=1}^5 i \\cos((i + 1) x_2 + i)\\right)"

object Branin extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    math.pow(x(0), 2) + math.pow(x(1), 2) + math.cos(18 * x(0)) + math.cos(
      18 * x(1)
    )
  def name: String = "Branin"
  def toLaTeX: String =
    "f(x) = x_1^2 + x_2^2 + \\cos(18 x_1) + \\cos(18 x_2)"

object Colville extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    math.pow(x(0), 2) + math.pow(x(1), 2) + math.cos(10 * x(0)) + math.cos(
      10 * x(1)
    )
  def name: String = "Colville"
  def toLaTeX: String =
    "f(x) = x_1^2 + x_2^2 + \\cos(10 x_1) + \\cos(10 x_2)"

object Cosines extends TwoVarFuncDef:
  def apply(x: Array[Double]): Double =
    math.cos(x(0)) + math.cos(x(1))
  def name: String = "Cosines"
  def toLaTeX: String =
    "f(x) = \\cos(x_1) + \\cos(x_2)"

val functions: List[TwoVarFuncDef] =
  List(
    Quadratic,
    CrossTerms,
    Ackley,
    Rastrigin,
    Sphere,
    Bohacevsky,
    Rosenbrock,
    Easom,
    EggHolder,
    Griewank,
    Levy,
    Michalewicz,
    HolderTable,
    ThreeHumpCamel,
    Zakharov,
    DropWave,
    Shubert,
    Branin,
    Colville
  )
