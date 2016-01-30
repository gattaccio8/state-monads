trait State[S, A] {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (res, a) = run(s)
    (res, f(a))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (res, a) = run(s)
    (f(a).run(s))
  }

  def run(s: S): (S, A)
}

object State {
  def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
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

  println(add(1))
  println(holder)
  println(add(3))
  println(holder)

}