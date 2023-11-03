import cats.effect.{IO, IOApp}
import cats.effect.{IO, IOApp, Sync}
import cats.effect.kernel.Ref
import cats.syntax.all._
import cats.effect.std.Console
import java.util.Timer
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.collection.immutable.IntMap

import scala.concurrent.duration.FiniteDuration

object CacheImpl{
  import CacheDataModel._
  /**
   * A type [A] describe the whole Cache object.
   * Cache contains meta information (like:
   * 1. Set[DependObjectName] - Unique set of objects on which current cache elements (CacheEntity) depend.
   *    F.e. table names from database.
   * 2. HeartbeatCounter -
   * 3. cacheSize: Int - count of CacheEntity elements in cache, to eliminate using .size each time.
   * 4. Aggregate counters of operations on Cache: save, get, remove (removed by time, removed by dependent objects)
   * 5. Map[DependObjectName,CountOfChanges] - list of objects and count of changes this object in external system.
   * )
   * and cache elements.
   */
  class RefCache[F[_],A <: CacheEntity[_],B](ref: Ref[F,Cache[B]])(implicit F: Sync[F]) {

    def get(key: Int): F[Option[_]] =
      ref.get.map(_.cacheEntities.get(key))

    /**
     * Save one element CacheEntity into Cache.
     */
    def save(entity: CacheEntity[B]): F[Unit] =
      ref.get.flatMap{cache =>
        ref.set{
          cache.copy(
            setDependObjects = cache.setDependObjects ++ entity.setDependObjects,
            cacheEntities = cache.cacheEntities.updated(entity.hashCode(), entity)
          )
        }
      }

    def size(): F[Int] =
      ref.get.map(_.cacheEntities.size)

    private def remove(key: Int): F[Unit] =
      ref.get.flatMap{cache =>
        ref.set{
          cache.copy(
            setDependObjects = cache.setDependObjects,
            cacheEntities = cache.cacheEntities -key)
        }
      }

    def update(key: Int, entity: CacheEntity[B]): F[Unit] =
      for {
        _ <- remove(key)
        _ <- save(entity)
      } yield ()

    def saveEntitiesInCache(entities: List[CacheEntity[B]]): F[Unit] ={
      import cats.implicits._
      entities.map(save).sequence.map(_ => ())
    }

    /*
    private def maintenance(seq: Set[DependObjectName]): IO[Unit] =
      ???
      */

    private def logChangedObjects(seq: Set[DependObjectName]): IO[Unit] =
      seq.foreach(Console[IO].println).pure[IO]

    /**
     * Repeat function f with delay and if it returns non empty Set
     * make some maintenance actions (F.e. clean Cache from some elements - CacheEntity).
     */
    def checkDependentObjectChanges(delay: FiniteDuration, f: IO[Unit] => IO[Set[DependObjectName]]): IO[Unit] =
      for {
        _ <- IO.sleep(delay)
        seq <- f(().pure[IO])
        //todo: remove output
        _ <- Console[IO].println(s"checkDependentObjectChanges seq.size = ${seq.size}")
        _ <- logChangedObjects(seq).whenA(seq.nonEmpty)
        //_ <- maintenance(seq).whenA(seq.nonEmpty)
        _ <- checkDependentObjectChanges(delay, f)
      } yield ()

  }
}