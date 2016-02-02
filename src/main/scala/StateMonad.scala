trait StateMonad[S, A] {
  def map[B](f: A => B): StateMonad[S, B] = StateMonad { s =>
    val (res, a) = run(s)
    (res, f(a))
  }

  def flatMap[B](f: A => StateMonad[S, B]): StateMonad[S, B] = StateMonad { s =>
    val (res, a) = run(s)
    (f(a).run(s))
  }

  def run(s: S): (S, A)
}

object StateMonad {
  def apply[S, A](f: S => (S, A)): StateMonad[S, A] = new StateMonad[S, A] {
    def run(s: S): (S, A) = f(s)
  }
}

case class MyHolder(lt: List[Int])

object Bar extends App {

  val holder: MyHolder = MyHolder(List[Int]())

  def add(i: Int): (MyHolder, Int) = {
    val newHolder = holder.copy(lt = i :: holder.lt)
    (newHolder, i)
  }

  def multiply(x: Int, y: Int) = x * y

  val multiplyCurried = (multiply _).curried
  println(multiply(4, 5))
  val multiplyCurriedFour=multiplyCurried(4)
  println(multiplyCurriedFour(2))

  println(add(1))
  println(holder)
  println(add(3))
  println(holder)

}