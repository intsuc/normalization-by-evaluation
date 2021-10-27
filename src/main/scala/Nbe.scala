import scala.collection.mutable.WeakHashMap

final class Sym private (id: Int)

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
  case App(operator: Val, operand: Val)
  case Fun(variables: Vars, key: Sym, domain: Val, codomain: Exp)
  case Typ

type Env = WeakHashMap[Sym, Val]

def freeVars(using env: Env): Exp => Vars =
  case Exp.Var(key)                   => if env.contains(key) then Set.empty else Set(key)
  case Exp.Abs(key, body)             => freeVars(body) - key
  case Exp.App(operator, operand)     => freeVars(operator) ++ freeVars(operand)
  case Exp.Fun(key, domain, codomain) => freeVars(domain) ++ freeVars(codomain) - key
  case Exp.Typ                        => Set.empty
  case Exp.Ann(annotated, _)          => freeVars(annotated)

def call(using env: Env)(key: Sym, v: Val, e: Exp): Val =
  env(key) = v
  eval(e)

def eval(using env: Env): Exp => Val =
  case Exp.Var(key)                         => env(key)
  case Exp @ Exp.Abs(key, body)             => Val.Abs(freeVars(Exp), key, body)
  case Exp.App(operator, operand)           =>
    eval(operator) match
      case Val.Abs(_, key, body) => call(key, eval(operand), body)
      case operator              => Val.App(operator, eval(operand))
  case Exp @ Exp.Fun(key, domain, codomain) => Val.Fun(freeVars(Exp), key, eval(domain), codomain)
  case Exp.Typ                              => Val.Typ
  case Exp.Ann(annotated, _)                => eval(annotated)