package popl

object hw04 extends js.util.JsApp:
  import js.ast._
  import js._
  import Bop._, Uop._

  /*
   * CSCI-UA.0480-055: Homework 4
   */

  /*
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your solution will _not_ be graded if it does not compile!!
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   *
   */

  /* JakartaScript */
  
  // Value environments
  type Env = Map[String, Val]
  // Empty environment
  def emp: Env = Map()
  // env[x |-> v]
  def extend(env: Env, x: String, v: Val): Env = env + (x -> v)
  
  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   */

  def toNum(v: Val): Double =
    v match
      case Num(n) => n
      case Bool(b) => ???
      case Undefined => ???
      case Str(s) => ???

  
  def toBool(v: Val): Boolean =
    v match
      case Num(n) => ???
      case Bool(b) => b
      case Undefined => ???
      case Str(s) => ???

  
  def toStr(v: Val): String =
    v match
      case Num(n) => ???
      case Bool(b) => ???
      case Undefined => ???
      case Str(s) => s

  
  def eval(env: Env, e: Expr): Val =
    /* Some helper functions for convenience. */
    def eToNum(e: Expr): Double = toNum(eval(env, e))
    def eToBool(e: Expr): Boolean = toBool(eval(env, e))
    def eToVal(e: Expr): Val = eval(env, e)
    
    e match
      /* Base Cases */
      case v: Val => v
      
      case Var(x) => ???
      
      /* Inductive Cases */
      case Print(e) => println(eToVal(e).prettyVal); Undefined

      /* Replace this entire catch-all case with your implementations of the remaining recursive cases */
      case _ => ???
  
  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = eval(emp, e)

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Val = eval(emp, parse.fromString(s))


  /* Interface to run your interpreter from the command line.  You can ignore the code below. */ 
  
  def processFile(file: java.io.File): Unit =
    if debug then
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    
    val expr = handle(fail()) {
      parse.fromFile(file)
    }
      
    if debug then
      println("Parsed expression:")
      println(expr)

    
    handle(()) {
      val v = eval(expr)
      println(v.prettyVal)
    }

end hw04