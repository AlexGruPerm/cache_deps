import scala.collection.immutable.IntMap

object CacheDataModel {
  type DependObjectName = String
  type SetDependObjectName = Set[DependObjectName]

  case class CacheEntity[T](data: T, depends: SetDependObjectName)

  case class Cache[T](entities: IntMap[CacheEntity[T]] = IntMap.empty){

    val depends: SetDependObjectName =
      entities.values.foldLeft(Set.empty[DependObjectName]) {
        case (r, c) => r ++ c.depends
      }

  }

}