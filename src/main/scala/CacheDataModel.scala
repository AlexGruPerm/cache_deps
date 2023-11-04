import scala.collection.immutable.IntMap

object CacheDataModel {
  type DependObjectName = String
  type SetDependObjectName = Set[DependObjectName]

  /**
   * data: T - abstract data, we don't care about structure.
   * depends: - Set of strings, names of objects this Entity depends of.
   * getCount - number of Cache.get calls.
  */
  case class CacheEntity[T](data: T, depends: SetDependObjectName, getCount: Int = 0)

  case class Cache[T](entities: IntMap[CacheEntity[T]] = IntMap.empty){

    val depends: SetDependObjectName =
      entities.values.foldLeft(Set.empty[DependObjectName]) {
        case (r, c) => r ++ c.depends
      }

  }

}