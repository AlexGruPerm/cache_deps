package com.cd

import scala.collection.immutable.IntMap
import scala.language.implicitConversions

object CacheDataModel {
  type TimeStamp = Long
  type DependObjectName = String
  type SetDependObjectName = Set[DependObjectName]

  /**
   * data: T - abstract data, we don't care about structure.
   * depends: - Set of strings, names of objects this Entity depends of.
   * getCount - number of Cache.get calls.
  */
  case class CacheEntity[T](data: T, depends: SetDependObjectName){
    implicit def toList: Seq[CacheEntity[T]] = Seq(this)
  }

  /**
   * Pair case class for CacheEntity,
   * contains meta information about CacheEntity.
  */
  case class CacheEntityMeta(tsCreate: TimeStamp,
                             tsLru: TimeStamp,
                             counterGet: Int = 0)

  /**
   * History of depends changes. For statistic and monitoring,
   * which object, how many times changed and what the last time of change.
  */
  case class HistDepChanges(changeCount: Int, tsLastChange: TimeStamp)

  case class Cache[T](entities: IntMap[CacheEntity[T]] = IntMap.empty,
                      entitiesMeta: IntMap[CacheEntityMeta] = IntMap.empty,
                      histDepChanges: Map[DependObjectName,HistDepChanges] = Map.empty
                     ){

    /**
     * List of objects on which cache elements depend now, online.
    */
    val depends: SetDependObjectName =
      entities.values.foldLeft(Set.empty[DependObjectName]) {
        case (r, c) => r ++ c.depends
      }

  }

}