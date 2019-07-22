package tempus
package timeSeries
package storage

import java.time.Instant

import InfluxTSStorage.Codec
import Codec.{DecodeError, IncorrectDecodeType}
import tempus.timeSeries.storage.InfluxTSStorage.{DecodeErrors, fromFuture}
import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.{Async, IO, LiftIO, Sync}
import cats.implicits._
import com.paulgoldbaum.influxdbclient.Parameter.Precision
import com.paulgoldbaum.influxdbclient._
import implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NoStackTrace

class InfluxTSStorage[K, A, TS[_], F[_]](db: Database, measure: String)(
    implicit
    TS: TimeSeries[TS],
    K: TagEncoder[K],
    A: Codec[A],
    F: Async[F],
    ex: ExecutionContext)
    extends TSStorage[K, A, TS, F] {

  def upsert(k: K, f: Option[Instant] => F[TS[A]]): F[Unit] =
    for {
      lastOne <- last(k)
      lastTime = lastOne.map(_._2)
      ts <- f(lastTime)
      points = lastTime.fold(ts)(ts.after).toListWithTime.map {
        case (a, i) =>
          Point(measure,
                tags = K.toTags(k).map(p => Tag(p._1, p._2)).toList,
                fields = A.encode(a),
                timestamp = i.toEpochMilli) //todo: make precision configurable
      }
      _ <- fromFuture(db.bulkWrite(points, precision = Precision.MILLISECONDS))
    } yield ()

  def last(k: K): F[Option[(A, Instant)]] =
    q(s"SELECT LAST(*) ${fromWhere(k)}").flatMap { s =>
      s.headOption
        .flatMap(_.records.headOption)
        .traverse(decode(_, "last_"))
    }

  def query(k: K, start: Option[Instant], end: Option[Instant]): F[TS[A]] = {
    val startCrit = start.fold("")(s => s"""AND "time" >= '$s'""")
    val endCrit = end.fold("")(e => s"""AND "time" <= '$e'""")
    q(s"SELECT * ${fromWhere(k)} $startCrit $endCrit").flatMap { s =>
      s.headOption.fold(F.pure(TS.empty[A])) { s =>
        s.records.traverse(decode(_)).map(TS.fromOrdered)
      }
    }
  }

  def remove(k: K): F[Unit] =
    q(s"DROP SERIES ${fromWhere(k)}").void

  private def fromWhere(k: K): String = s"from $measure where ${keyCrit(k)}"

  private def keyCrit(k: K): String =
    K.toTags(k).map { case (kk, v) => s""""$kk" = '$v'""" }.mkString(" AND ")

  private def decode(r: Record, prefix: String = ""): F[(A, Instant)] =
    A.decode(r, prefix)
      .leftMap(DecodeErrors(_))
      .toEither
      .liftTo[F]
      .product(
        Try(Instant.parse(r("time").asInstanceOf[String])).liftTo[F]
      )

  private def q(query: String): F[List[Series]] =
    fromFuture {
      db.query(query)
    }.map(_.series)
}

object InfluxTSStorage {

  trait Codec[A] {
    def encode(a: A): List[Field]
    def decode(r: Record, prefix: String = ""): ValidatedNel[DecodeError, A]
  }

  object Codec extends CodecInstances {
    trait DecodeError extends RuntimeException with NoStackTrace
    case class IncorrectDecodeType(override val getMessage: String) extends DecodeError

  }

  sealed abstract class CodecInstances {

    implicit val stringCodec: Codec[String] = new Codec[String] {
      def encode(a: String): List[Field] = List(StringField("value", a))

      def decode(r: Record, prefix: String = ""): ValidatedNel[DecodeError, String] = {
        r(prefix + "value") match {
          case s: String => s.validNel[DecodeError]
          case _         => IncorrectDecodeType("expecting String type").invalidNel
        }
      }
    }

    implicit val doubleCodec: Codec[Double] = new Codec[Double] {
      def encode(a: Double): List[Field] = List(DoubleField("value", a))

      def decode(r: Record, prefix: String = ""): ValidatedNel[DecodeError, Double] = {
        r(prefix + "value") match {
          case s: Double => s.validNel[DecodeError]
          case _         => IncorrectDecodeType("expecting Double type").invalidNel
        }
      }
    }

    implicit val longCodec: Codec[Long] = new Codec[Long] {
      def encode(a: Long): List[Field] = List(LongField("value", a))

      def decode(r: Record, prefix: String = ""): ValidatedNel[DecodeError, Long] = {
        r(prefix + "value") match {
          case s: Long => s.validNel[DecodeError]
          case _       => IncorrectDecodeType("expecting Long type").invalidNel
        }
      }
    }
  }

  case class DecodeErrors(errors: NonEmptyList[DecodeError])
      extends RuntimeException
      with NoStackTrace

  implicit def fromFuture[T, F[_]: LiftIO](fa: => Future[T])(implicit ec: ExecutionContext): F[T] = {
    implicit val es = IO.contextShift(ec)
    IO.fromFuture(IO(fa)).to[F]
  }


  def apply[K, A, TS[_], F[_]](dbName: String, measure: String)(
      implicit
      TS: TimeSeries[TS],
      K: TagEncoder[K],
      A: Codec[A],
      F: Async[F],
      influxDB: InfluxDB,
      ex: ExecutionContext): F[TSStorage[K, A, TS, F]] = {
    val db = influxDB.selectDatabase(dbName)
    for {
      exists <- fromFuture(db.exists())
      _ <- if (exists) F.unit else fromFuture(db.create())
    } yield new InfluxTSStorage(db, measure)
  }

  def influxDBClient[F[_]](host: String, port: Int = 8086)(
      implicit F: Sync[F],
      ex: ExecutionContext): F[InfluxDB] =
    F.delay {
      InfluxDB.connect(host, port)
    }
}

trait TagEncoder[K] {
  def toTags(k: K): Map[String, String]
}

object TagEncoder {

  implicit val identityTagEncoder: TagEncoder[Map[String, String]] =
    new TagEncoder[Map[String, String]] {
      def toTags(m: Map[String, String]): Map[String, String] = m
    }

  implicit val stringTagEncoder: TagEncoder[String] =
    new TagEncoder[String] {
      def toTags(s: String): Map[String, String] = Map("singleKey" -> s)
    }
}
