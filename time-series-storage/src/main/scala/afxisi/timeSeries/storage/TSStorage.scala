package afxisi.timeSeries.storage

import java.time.Instant

/**
  * a key-value time series storage for a single type of series
  */
trait TSStorage[K, A, TS[_], F[_]] {
  def upsert(k: K, f: Option[Instant] => F[TS[A]]): F[Unit]
  def query(k: K, start: Option[Instant] = None, end: Option[Instant] = None): F[TS[A]]
  def remove(k: K): F[Unit]
  def last(k: K): F[Option[(A, Instant)]]
}
