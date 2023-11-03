import scala.collection.immutable.IntMap

object CacheDataModel {
  type DependObjectName = String
  case class CacheEntity[T](data: T, setDependObjects: Set[DependObjectName])

  case class Cache[T](setDependObjects: Set[DependObjectName] = Set.empty,
                      cacheEntities: IntMap[CacheEntity[T]] = IntMap.empty)

}