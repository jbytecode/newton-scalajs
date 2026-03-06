package newton

import funcdef._
import diff.{Vect, Matrix, gradient, hessian, inverse, matVecMul, vecSub}

def newtonstep(f: TwoVarFuncDef, x: Vect): Option[Vect] = 
    val grad = gradient(f, x)
    val hess = hessian(f, x)
    val hessInvOpt = inverse(hess)
    hessInvOpt match 
        case Some(hessInv) => Some(vecSub(x, matVecMul(hessInv, grad)))
        case None => None


def hasconverged(f: TwoVarFuncDef, x: Vect, tol: Double = 1e-7): Boolean = 
    val grad = gradient(f, x)
    math.sqrt(grad(0) * grad(0) + grad(1) * grad(1)) < tol

