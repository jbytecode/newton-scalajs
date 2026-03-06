import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js
import scala.scalajs.js.annotation._
import funcdef._
import diff._
import newton._

@js.native
@JSGlobal
object MathJax extends js.Object {
  def typeset(): Unit = js.native
}


var selectedFunction = functions.head
val selectFunction = document.getElementById("selectFunction").asInstanceOf[dom.html.Select]
val divFunctionDefinition = document.getElementById("divFunctionDefinition").asInstanceOf[dom.html.Div]
val inputx1 = document.getElementById("x1").asInstanceOf[dom.html.Input]
val inputx2 = document.getElementById("x2").asInstanceOf[dom.html.Input]
val buttonSolve = document.getElementById("buttonSolve").asInstanceOf[dom.html.Button]
val divSteps = document.getElementById("divSteps").asInstanceOf[dom.html.Div]


def initComponents(): Unit = 
  funcdef.functions.foreach { f =>
    val option = document.createElement("option").asInstanceOf[dom.html.Option]
    option.value = f.name
    option.textContent = f.name
    selectFunction.appendChild(option)
  }

def registerEvents(): Unit = 
  selectFunction.oninput = { _ =>
    val selectedName = selectFunction.value
    selectedFunction = funcdef.functions.find(_.name == selectedName).getOrElse(functions.head)
    divFunctionDefinition.innerHTML = "<p>" + "$$" + selectedFunction.toLaTeX + "$$</p>"
    MathJax.typeset()
  }
  buttonSolve.onclick = { _ =>
    solve()
  }

def solve(): Unit = 
  val x1 = inputx1.value.toDoubleOption.getOrElse(0.0)
  val x2 = inputx2.value.toDoubleOption.getOrElse(0.0)
  val initialPoint = Array(x1, x2)
  divSteps.innerHTML = ""
  var currentPoint = initialPoint
  def helper(i: Int): Unit =
    val grad = diff.gradient(selectedFunction, currentPoint)
    val hess = diff.hessian(selectedFunction, currentPoint)
    diff.inverse(hess) match 
      case Some(invHess) =>
        val step = diff.matVecMul(invHess, grad)
        val nextpoint = diff.vecSub(currentPoint, step)
        divSteps.innerHTML += stepToLaTeX(currentPoint, i + 1, grad, hess, nextpoint)
        currentPoint = nextpoint
      case None =>
        divSteps.innerHTML += s"<p>Step ${i + 1}: Hessian is singular, cannot proceed.</p>"
    if i < 20 && !hasconverged(selectedFunction, currentPoint) then helper(i + 1)
  helper(0)
  MathJax.typeset()

def formatNumber(num: Double): String = 
  if num.isWhole then num.toInt.toString else f"$num%.5f"
  
def stepToLaTeX(x: Array[Double], step: Int, gradient: Vect, hessian: Matrix, nextpoint: Array[Double]): String = 
  val buf = new StringBuilder
  buf.append(s"Step $step: ")
  buf.append("$$\n")
  buf.append(s"x = \\begin{bmatrix} ${formatNumber(x(0))} \\\\ ${formatNumber(x(1))} \\end{bmatrix}")
  buf.append("-")
  // Append inverse of the Hessian matrix in latex format
  buf.append("\\begin{bmatrix} ")
  buf.append(s"${formatNumber(hessian(0)(0))} & ${formatNumber(hessian(0)(1))} \\\\ ")
  buf.append(s"${formatNumber(hessian(1)(0))} & ${formatNumber(hessian(1)(1))} ")
  buf.append("\\end{bmatrix}^{-1}")
  buf.append("\\cdot ")
  buf.append(s"\\begin{bmatrix} ${formatNumber(gradient(0))} \\\\ ${formatNumber(gradient(1))} \\end{bmatrix}")
  buf.append(" = ")
  buf.append(s"\\begin{bmatrix} ${formatNumber(nextpoint(0))} \\\\ ${formatNumber(nextpoint(1))} \\end{bmatrix}")
  buf.append("\n$$\n")
  buf.append("<br>\n")
  buf.append("$$\n")
  buf.append(s"f(x) = ${formatNumber(selectedFunction(x))}\n")
  buf.append("\n$$\n")
  buf.toString()

@main
def main(): Unit =
    initComponents()
    registerEvents()
    divFunctionDefinition.innerHTML = "<p>" + "$$" + selectedFunction.toLaTeX + "$$</p>"
    MathJax.typeset()
