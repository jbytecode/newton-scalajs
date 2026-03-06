package diff

import funcdef.TwoVarFuncDef

type Vector = Array[Double]
type Matrix = Array[Array[Double]]

def gradient(f: TwoVarFuncDef, x: Array[Double], h: Double = 1e-6): Array[Double] = 
    val dirx1 = Array(x(0) + h, x(1))
    val dirx2 = Array(x(0), x(1) + h)
    val fval = f(x)
    val df1 = (f(dirx1) - fval) / h
    val df2 = (f(dirx2) - fval) / h
    Array(df1, df2)

def det(a: Matrix): Double = 
    a(0)(0) * a(1)(1) - a(0)(1) * a(1)(0)

def inverse(a: Matrix): Option[Matrix] =
    val d = det(a)
    if d == 0 then None
    else Some(Array(
        Array(a(1)(1) / d, -a(0)(1) / d),
        Array(-a(1)(0) / d, a(0)(0) / d)
    ))

def matVecMul(a: Matrix, v: Vector): Vector = 
    Array(
        a(0)(0) * v(0) + a(0)(1) * v(1),
        a(1)(0) * v(0) + a(1)(1) * v(1)
    )

def vecSub(v1: Vector, v2: Vector): Vector = 
    Array(v1(0) - v2(0), v1(1) - v2(1))

def hessian(f: TwoVarFuncDef, x: Array[Double], h: Double = 1e-6): Matrix = 
    val dirx1 = Array(x(0) + h, x(1))
    val dirx2 = Array(x(0), x(1) + h)
    val dirx12 = Array(x(0) + h, x(1) + h)
    val fval = f(x)
    val d2f11 = (f(dirx1) - 2 * fval + f(Array(x(0) - h, x(1)))) / (h * h)
    val d2f22 = (f(dirx2) - 2 * fval + f(Array(x(0), x(1) - h))) / (h * h)
    val d2f12 = (f(dirx12) - f(dirx1) - f(dirx2) + fval) / (h * h)
    Array(
        Array(d2f11, d2f12),
        Array(d2f12, d2f22)
    )