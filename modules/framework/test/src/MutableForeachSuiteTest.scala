import cats.effect.concurrent.Ref
import cats.effect.{IO, Resource}
import weaver._

object MutableForeachSuiteTest extends SimpleIOSuite{
  private val foreachSuite: MutableFResourceSuite[IO] = new IOForEachSuite{
    override type Res = Ref[IO, Int]
    override def foreachResource: Resource[IO, Res] = Resource.liftF(Ref.of(0))

    for(i <- 0 to 5){
      test(s"$i gets a fresh version of Ref resource") { r =>
        r.getAndUpdate(_ + 1).map(i => expect(i == 0))
      }
    }
  }
  private val forAllSuite: MutableFResourceSuite[IO] = new IOSuite{
    override type Res = Ref[IO, Int]
    override def sharedResource: Resource[IO, Ref[IO, Int]] = Resource.liftF(Ref.of(0))
    override def maxParallelism: Int = 1

    for(i <- 0 to 5){
      test(s"$i reuses shared Ref resource") { r =>
        r.getAndUpdate(_ + 1).map(j => expect(i == j))
      }
    }
  }

  override def spec(args: List[String]): fs2.Stream[IO, Event] =
    foreachSuite.spec(args) ++ forAllSuite.spec(args)
}

