import scala.collection.immutable.IntMap
import common._

object CacheDataModel {
  type TimeStamp = Long
  type DependObjectName = String
  type SetDependObjectName = Set[DependObjectName]

  /**
   * data: T - abstract data, we don't care about structure.
   * depends: - Set of strings, names of objects this Entity depends of.
   * getCount - number of Cache.get calls.
  */
  case class CacheEntity[T](data: T, depends: SetDependObjectName)

  /**
   * Pair case class for CacheEntity,
   * contains meta information about CacheEntity.
  */
  case class CacheEntityMeta(tsCreate: TimeStamp = currentTime,
                             tsLru: TimeStamp = currentTime,
                             counterGet: Int = 0){
    def getLifetime: TimeStamp = currentTime - tsCreate
    def getLruTime: TimeStamp = currentTime - tsLru
  }

  case class Cache[T](entities: IntMap[CacheEntity[T]] = IntMap.empty,
                      entitiesMeta: IntMap[CacheEntityMeta] = IntMap.empty){

    val depends: SetDependObjectName =
      entities.values.foldLeft(Set.empty[DependObjectName]) {
        case (r, c) => r ++ c.depends
      }

  }

}