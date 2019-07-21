package tempus
package timeSeries
package storage

import implicits._
import tempus.timeSeries.storage.InfluxTSStorage.{Codec, fromFuture}
import tempus.testUtil.{instantEST, tsOf}
import cats.effect.{Async, IO}
import cats.implicits._
import com.paulgoldbaum.influxdbclient.InfluxDB
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.{BeforeAndAfter, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global

class InfluxTSStorageSuite extends AnyFunSuiteLike with Matchers with BeforeAndAfter {
  type F[A] = IO[A]
  type TS[A] = ListTimeSeries[A]
  type K = String
  val F = Async[IO]
  val testDBName = "test"

  def withDBClient[A](f: InfluxDB => F[A]): F[A] = {
    InfluxTSStorage
      .influxDBClient[F]("localhost")
      .bracket(f)(cl => F.delay(cl.close))
  }

  def storageF[A: Codec](implicit cl: InfluxDB): F[TSStorage[K, A, TS, F]] =
    InfluxTSStorage[K, A, TS, F](testDBName, "test")

  test("integration smoke") {
    withDBClient { implicit cl =>
      InfluxTSStorage[K, String, TS, F](testDBName, "test")
    }.unsafeRunSync()
  }

  test("get the latest timestamp") {
    val last = withDBClient { implicit cl =>
      for {
        storage <- storageF[String]
        last <- storage.last("k2")
      } yield last

    }.unsafeRunSync()

    last should be(empty)
  }

  test("store a series of string") {
    val last = withDBClient { implicit cl =>
      for {
        storage <- storageF[String]
        _ <- storage.upsert("k",
                            _ =>
                              F.pure(
                                tsOf(("a", instantEST(day = 1)),
                                     ("b", instantEST(day = 2)))
                            ))

        l <- storage.last("k")
      } yield l

    }.unsafeRunSync()

    last shouldBe Some(("b", instantEST(day = 2)))
  }

  test("update a series of string") {
    val result = withDBClient { implicit cl =>
      for {
        storage <- storageF[String]
        _ <- storage.upsert("k",
                            _ =>
                              F.pure(
                                tsOf(("a", instantEST(day = 1)),
                                     ("b", instantEST(day = 2)))
                            ))

        _ <- storage.upsert(
          "k",
          i =>
            F.pure(
              tsOf(("b", instantEST(day = 2)),
                   ("c", instantEST(day = 3)),
                   ("c", instantEST(day = 4))).after(i.get.minusSeconds(1))
          ))

        l <- storage.query("k")
      } yield l

    }.unsafeRunSync()

    result shouldBe tsOf(("a", instantEST(day = 1)),
                         ("b", instantEST(day = 2)),
                         ("c", instantEST(day = 3)),
                         ("c", instantEST(day = 4)))
  }

  test("query a series of string") {
    val ts = tsOf(("a", instantEST(day = 1)), ("b", instantEST(day = 2)))

    val retrieved = withDBClient { implicit cl =>
      for {
        storage <- storageF[String]
        _ <- storage.upsert("k", _ => F.pure(ts))
        l <- storage.query("k", None, None)
      } yield l

    }.unsafeRunSync()

    retrieved shouldBe ts
  }

  test("query with start") {
    val ts = tsOf(("a", instantEST(day = 1)),
                  ("b", instantEST(day = 2)),
                  ("c", instantEST(day = 3)))

    val retrieved = withDBClient { implicit cl =>
      for {
        storage <- storageF[String]
        _ <- storage.upsert("k", _ => F.pure(ts))
        l <- storage.query("k", Some(instantEST(day = 2)), None)
      } yield l

    }.unsafeRunSync()

    retrieved shouldBe tsOf(("b", instantEST(day = 2)), ("c", instantEST(day = 3)))
  }

  test("query with end") {
    val ts = tsOf(("a", instantEST(day = 1)),
                  ("b", instantEST(day = 2)),
                  ("c", instantEST(day = 3)))

    val retrieved = withDBClient { implicit cl =>
      for {
        storage <- storageF[String]
        _ <- storage.upsert("k", _ => F.pure(ts))
        l <- storage.query("k", None, Some(instantEST(day = 2)))
      } yield l

    }.unsafeRunSync()

    retrieved shouldBe tsOf(("a", instantEST(day = 1)), ("b", instantEST(day = 2)))
  }

  test("query with start and end") {
    val ts = tsOf(("a", instantEST(day = 1)),
                  ("b", instantEST(day = 2)),
                  ("c", instantEST(day = 3)))

    val retrieved = withDBClient { implicit cl =>
      for {
        storage <- storageF[String]
        _ <- storage.upsert("k", _ => F.pure(ts))
        l <- storage.query("k", Some(instantEST(day = 2)), Some(instantEST(day = 2)))
      } yield l

    }.unsafeRunSync()

    retrieved shouldBe tsOf(("b", instantEST(day = 2)))
  }

  test("drop a series of string") {
    val ts = tsOf(("a", instantEST(day = 1)), ("b", instantEST(day = 2)))

    val retrieved = withDBClient { implicit cl =>
      for {
        storage <- storageF[String]
        _ <- storage.upsert("k", _ => F.pure(ts))
        _ <- storage.remove("k")
        l <- storage.query("k", None, None)
      } yield l

    }.unsafeRunSync()

    retrieved.list should be(empty)
  }

  after(withDBClient { cl =>
    fromFuture[Unit, F](cl.selectDatabase(testDBName).drop().void)
  }.unsafeRunSync())

}
