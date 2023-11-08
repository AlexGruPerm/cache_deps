import CacheDataModel.CacheEntity
import cats.effect.IO.sleep
import cats.effect.{IO, Sync}
import cats.effect.kernel.Ref
import cats.syntax.all._
//import Common.currentTime
import Common.currTimeMcSec

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

    def getDepends: F[SetDependObjectName] =
      ref.get.map(_.depends)

    private def saveMetaForGet(key: Int): F[Unit] =
      for {
        ct <- currTimeMcSec
        cache <- ref.get
        entitiesMeta = cache.entitiesMeta.get(key)
        updatedEntitiesMeta = entitiesMeta.foldLeft(cache.entitiesMeta) {
            case (cacheEntitiesMeta, entityMetaToSave) =>
              cacheEntitiesMeta.updated(key,
                entityMetaToSave.copy(
                  counterGet = entityMetaToSave.counterGet + 1,
                  tsLru = ct.length))
          }
        _ <- ref.set(cache.copy(entitiesMeta = updatedEntitiesMeta))
      } yield ()

    def get(key: Int): F[Option[CacheEntity[B]]] =
      for {
       ce <- ref.get.map(cache => cache.entities.get(key))
        _ <- saveMetaForGet(key).whenA(ce.isDefined)
      } yield ce

    def getMeta(key: Int): F[Option[CacheEntityMeta]] =
      ref.get.map(_.entitiesMeta.get(key))

    def save(entities: Seq[CacheEntity[B]]): F[Unit] =
      for {
        ct <- currTimeMcSec
        cache <- ref.get
        updatedEntities = entities.foldLeft(cache.entities) {
          case (cacheEntities, entityToSave) =>
            cacheEntities.updated(entityToSave.hashCode(), entityToSave)
        }
        updatedEntitiesMeta = entities.foldLeft(cache.entitiesMeta) {
          case (cacheEntitiesMeta, entityToSave) =>
            cacheEntitiesMeta.updated(entityToSave.hashCode(),
              CacheEntityMeta(ct.length, ct.length))
        }
        _ <- ref.set(cache.copy(entities= updatedEntities,entitiesMeta = updatedEntitiesMeta))
      } yield ()

    def size(): F[Int] =
      ref.get.map(_.entities.size)

    def remove(key: Int): F[Unit] =
      for {
        cache <- ref.get
        _ <- ref.set(cache.copy(entities = cache.entities - key))
      } yield ()

    /**
     * Receive set of changed object from "external system" and remove related CacheEntity
     * from Cache.
    */
    private def maintenance(set: SetDependObjectName): F[Unit] =
      for {
        cache <- ref.get
        entities = cache.entities
        newEntities = entities.filterNot(cacheEntity => set.exists(ch => cacheEntity._2.depends.contains(ch)))
        _ <- ref.set(cache.copy(entities = newEntities))
      } yield ()


    /**
     * Return true if in the Cache.depends exist at least one element from set (list of changed objects)
    */
    private def dependsExists(set: SetDependObjectName): F[Boolean] =
      for {
        dependencies <- getDepends
        isExist = set.exists(ch => dependencies.contains(ch))
      } yield isExist

    /**
     * Repeat function f with delay and if it returns non empty Set
     * make some maintenance actions (F.e. clean Cache from some elements - CacheEntity).
     */
    def dependsChanged(delay: FiniteDuration, f: IO[Unit] => F[SetDependObjectName]): F[Unit] =
      for {
        _ <- sleep(delay).pure[F]
        seq <- f(().pure[IO])
        depExists <- dependsExists(seq)
        _ <- maintenance(seq).whenA(depExists)
        _ <- dependsChanged(delay, f)
      } yield ()

  }
}