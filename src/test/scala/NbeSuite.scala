import munit.FunSuite
import scala.collection.mutable.WeakHashMap

class NbeSuite extends FunSuite:
  test("evaluate-type") {
    given Env = WeakHashMap.empty
    assertEquals(
      eval(Exp.Typ),
      Val.Typ
    )
  }

  test("evaluate-abs") {
    given Env = WeakHashMap.empty
    val x     = Sym()
    assertEquals(
      eval(Exp.Abs(x, Exp.Var(x))),
      Val.Abs(Set.empty, x, Exp.Var(x))
    )
  }

  test("evaluate-app") {
    given Env = WeakHashMap.empty
    val x     = Sym()
    assertEquals(
      eval(Exp.App(Exp.Abs(x, Exp.Var(x)), Exp.Typ)),
      Val.Typ
    )
  }
