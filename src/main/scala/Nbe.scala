import scala.collection.mutable.WeakHashMap

final class Thunk[A](_a: => A):
  private lazy val a: A = _a

  def get: A = a

final case class Sym private (id: Int)

object Sym:
  private var id: Int = 0

  def apply(): Sym =
    id += 1
    new Sym(id)

type Vars = Set[Sym]

enum Exp:
  case Var(key: Sym)
  case Abs(key: Sym, body: Exp)
  case App(operator: Exp, operand: Exp)
  case Fun(key: Sym, domain: Exp, codomain: Exp)
  case Typ
  case Ann(annotated: Exp, annotation: Exp)

enum Val:
  case Var(key: Sym)
  case Abs(variables: Vars, key: Sym, body: Exp)
  case App(operator: Val, operand: Thunk[Val])
  case Fun(variables: Vars, key: Sym, domain: Val, codomain: Exp)
  case Typ

type Env = WeakHashMap[Sym, Thunk[Val]]

// TODO: free variables caching?
def freeVars(using env: Env): Exp => Vars =
  case Exp.Var(key)                   => if env.contains(key) then Set.empty else Set(key)
  case Exp.Abs(key, body)             => freeVars(body) - key
  case Exp.App(operator, operand)     => freeVars(operator) ++ freeVars(operand)
  case Exp.Fun(key, domain, codomain) => freeVars(domain) ++ freeVars(codomain) - key
  case Exp.Typ                        => Set.empty
  case Exp.Ann(annotated, annotation) => freeVars(annotated) ++ freeVars(annotation)

def call(using env: Env)(key: Sym, v: Thunk[Val], e: Exp): Val =
  env(key) = v
  eval(e)

def eval(using env: Env): Exp => Val =
  case Exp.Var(key)                       => env(key).get
  case e @ Exp.Abs(key, body)             => Val.Abs(freeVars(e), key, body)
  case Exp.App(operator, operand)         =>
    eval(operator) match
      case Val.Abs(_, key, body) => call(key, Thunk(eval(operand)), body)
      case operator              => Val.App(operator, Thunk(eval(operand)))
  case e @ Exp.Fun(key, domain, codomain) => Val.Fun(freeVars(e), key, eval(domain), codomain)
  case Exp.Typ                            => Val.Typ
  case Exp.Ann(annotated, _)              => eval(annotated)
