
trait State[S, A] {
  val run: S => (S, A)

  def apply(s: S): (S, A) =
    run(s)

  def eval(s: S): A =
    apply(s)._2

  def map[B](f: A => B): State[S, B] = State { s: S =>
    val (s1, a) = run(s)
    (s1, f(a))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s: S =>
    val (s1, a) = run(s)
    f(a)(s1)
  }
}

object State {

  def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
    final val run = f
  }

  def state[S, A](a: A): State[S, A] = State { s: S => (s, a) }
  def get[S]: State[S, S] = State { s: S => (s, s) }
  def gets[S, A](f: S => A): State[S, A] = State { s: S => (s, f(s)) }
  def modify[S](f: S => S): State[S, Unit] = State { s: S => (f(s), ()) }
}

object NotFib extends App {
  val n = 2
  val foo: State[Map[Int, String], Option[String]] = for {
    x <- State.gets[Map[Int, String], Option[String]] { m => m get n }
    _ <- State.modify[Map[Int, String]] { _ + (n -> n.toString) }

    y <- State.gets[Map[Int, String], Option[String]] { m => m get n }
    _ = println(y)
    _ <- State.modify[Map[Int, String]] { _ + (n * 2 -> (n * 2).toString) }

    z <- State.gets[Map[Int, String], Option[String]] { m => m get (n * 2) }
  } yield z

  val bar = foo(Map.empty)
  println(bar)
}
