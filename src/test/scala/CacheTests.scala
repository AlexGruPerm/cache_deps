import com.cd.CacheDataModel.{_}
import com.cd.CacheImpl.RefCache
import com.cd.Common.currTimeMcSec
import cats.effect.IO
import cats.effect.IO.sleep
import munit.CatsEffectSuite
import cats.syntax.all._
import cats.effect.kernel.Ref
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

class CacheTests extends CatsEffectSuite {

  case class User(id: Int, name: String, age: Int)

  private val users = List(
    CacheEntity(User(1, "John", 34), Set("t_sys", "t_session")),
    CacheEntity(User(2, "Mark", 40), Set("t_users", "t_privs")),
    CacheEntity(User(3, "Anna", 22), Set.empty)
  )

  def createAndGetCache[A] :IO[RefCache[IO, CacheEntity[A],A]] =
  for {
     ref <- Ref[IO].of(Cache[A]())
     cache = new RefCache[IO, CacheEntity[A], A](ref)
  } yield cache

  test("0) CE Clock check.") {
    (for {
      ct <- currTimeMcSec[IO]
    } yield (ct.length > 1000000000000000L,ct.unit)).map(s => assertEquals(s, (true,TimeUnit.MICROSECONDS)))
  }

  test("1) New Cache is empty.") {
    (for {
      cache <- createAndGetCache[User]
      cacheSize <- cache.size()
    } yield cacheSize).map(s => assertEquals(s, 0))
  }

  test("2) 3 elements added to Cache.") {
    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(users)
      cacheSize <- cache.size()
    } yield cacheSize).map(s => assertEquals(s, 3))
  }

  test("3) 3 elements added to Cache and removed. Check size = 0.") {
    import cats.implicits._
    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(users)
      cacheSize1 <- cache.size()
      _ <- cache.remove(users.head.hashCode())
      cacheSize2 <- cache.size()
      _ <- users.map(user => cache.remove(user.hashCode())).sequence.map(_ => ())
      cacheSize3 <- cache.size()
    } yield (cacheSize1,cacheSize2,cacheSize3)).map(s => assertEquals(s, (3,2,0)))
  }

  test("4) get method in empty Cache.") {
    val john = CacheEntity(User(1, "John", 34), Set("t_sys", "t_session"))
    val keyJohn = john.hashCode()
    (for {
      cache <- createAndGetCache[User]
      userJohn <- cache.get(keyJohn)
    } yield userJohn).map(s => assertEquals(s, None))
  }

  test("5) Cache with 3 elements. We check CacheEntity exists.") {
    val john = CacheEntity(User(1, "John", 34),Set("t_sys","t_session"))
    val keyJohn = john.hashCode()
    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(users)
      userJohn <- cache.get(keyJohn)
    } yield userJohn).map(s => assertEquals(s, Some(john)))
  }

  test("6) Cache with 3 elements. We check CacheEntity not exists .") {
    val john = CacheEntity(User(1, "John", 34), Set("t_sys", "t_session"))
    val keyJohn = john.hashCode()
    val failJohn = CacheEntity(User(10, "John", 44), Set("t_sys"))
    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(users)
      userJohn <- cache.get(keyJohn)
    } yield userJohn).map(s => assertNotEquals(s, Some(failJohn)))
  }

  test("7) Cache depends contains all depends from entities.") {
     val userList = List(
      CacheEntity(User(1, "John", 34), Set("t_sys", "t_session")),
      CacheEntity(User(2, "Mark", 40), Set("t_users", "t_privs")),
      CacheEntity(User(3, "Anna", 22), Set.empty)
    )
    val usersDeps: SetDependObjectName = Set("t_sys","t_session","t_users","t_privs")
    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(userList)
      cacheDepends <- cache.getDepends
    } yield cacheDepends).map(deps => assertEquals(deps, usersDeps))
  }

  test("8) Cache dependsChanged and maintenance, after 5 seconds Cache must be empty.") {
    val usersLocal = List(
      CacheEntity(User(1, "John", 34), Set("t_sys", "t_session")),
      CacheEntity(User(2, "Mark", 40), Set("t_users", "t_sys")),
      CacheEntity(User(3, "Anna", 22), Set("t_roles"))
    )

    val funcChangeChecker:  IO[Unit] => IO[SetDependObjectName] = _ => Set("t_sys").pure[IO]
    val funcChangeChecker2: IO[Unit] => IO[SetDependObjectName] = _ => Set("t_roles").pure[IO]

    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(usersLocal)
      cacheSizeBefore <- cache.size()
      _ <- cache.dependsChanged(1.seconds,funcChangeChecker).foreverM.start
      _ <- IO.sleep(2.seconds)
      cacheSizeAfter <- cache.size()
      _ <- cache.dependsChanged(1.seconds,funcChangeChecker2).foreverM.start
      _ <- IO.sleep(2.seconds)
      cacheSizeAfter2 <- cache.size()
    } yield (cacheSizeBefore,cacheSizeAfter,cacheSizeAfter2)).map(res => assertEquals(res, (3,1,0)))
  }

  test("9) Single save into Cache with 3 elements. Size=4") {
    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(users)
      _ <- cache.save(List(CacheEntity(User(10, "John", 64), Set("t_sys", "t_session"))))
      cacheSize <- cache.size()
    } yield cacheSize).map(s => assertEquals(s, 4))
  }

  test("10) Single save into Cache with 3 elements. And get this CacheEntity.") {
    val user = CacheEntity(User(10, "John", 64),Set("t_sys", "t_session"))
    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(users)
      _ <- cache.save(List(user))
      cacheSize <- cache.size()
      element <- cache.get(user.hashCode())
    } yield (cacheSize,element)).map(s => assertEquals(s, (4,Some(user))))
  }

  test("11) counterGet = 0 and then 1 for newly added element with first getting.") {
    val user = CacheEntity(User(10, "John", 64), Set("t_sys", "t_session"))
    val keyUser = user.hashCode()
    (for {
      cache <- createAndGetCache[User]
      cacheSizeEmpty <- cache.size()
      _ <- cache.save(List(user))
      cacheSize <- cache.size()
      userFromCache <- cache.get(keyUser)
      userMetaFromCache <- cache.getMeta(keyUser)
    } yield (cacheSizeEmpty, cacheSize, userMetaFromCache.map(_.counterGet), userFromCache)).map{ s =>
      assertEquals(s, (0, 1, Some(1), Some(user)))
    }
  }

  test("12) counterGet = 5 for repeating cache.get(keyUser).") {
    val user = CacheEntity(User(10, "John", 64), Set("t_sys", "t_session"))
    val keyUser = user.hashCode()
    (for {
      cache <- createAndGetCache[User]
      cacheSizeEmpty <- cache.size()
      _ <- cache.save(List(user))
      cacheSize <- cache.size()
      _ <- cache.get(keyUser)
      _ <- cache.get(keyUser)
      _ <- cache.get(keyUser)
      _ <- cache.get(keyUser)
      userFromCache <- cache.get(keyUser)
      userMetaFromCache <- cache.getMeta(keyUser)
    } yield (cacheSizeEmpty, cacheSize, userMetaFromCache.map(_.counterGet), userFromCache)).map{ s =>
      assertEquals(s, (0, 1, Some(5), Some(user)))
    }
  }

  test("13) 2 Objects in Cache. Different counterGet(s). Cache size. Emtpty.  ") {
    val user1 = CacheEntity(User(10, "John", 64), Set("t_sys"))
    val keyUser1 = user1.hashCode()

    val user2 = CacheEntity(User(20, "Mary", 25), Set("t_session"))
    val keyUser2 = user2.hashCode()

    val funcChangeChecker:  IO[Unit] => IO[SetDependObjectName] = _ => Set("t_sys","t_session").pure[IO]

    case class TestCheck(
                          initCacheSize: Int,
                          cacheSizeUser1Added: Int,
                          cacheSizeUser2Added: Int,
                          counterGetUser1_2gets: Option[Int],
                          counterGetUser2_4gets: Option[Int],
                          endCacheSize: Int
                        )

    (for {
      cache <- createAndGetCache[User]
      cacheSizeEmpty <- cache.size()
      _ <- cache.save(List(user1))
      cacheSizeUser1Added <- cache.size()
      _ <- cache.save(List(user2))
      cacheSizeUser2Added <- cache.size()
      _ <- cache.get(keyUser1)
      _ <- cache.get(keyUser1)
        _ <- cache.get(keyUser2)
        _ <- cache.get(keyUser2)
        _ <- cache.get(keyUser2)
        _ <- cache.get(keyUser2)
      counterGetUser1_2gets <- cache.getMeta(keyUser1)
      counterGetUser2_4gets <- cache.getMeta(keyUser2)

      _ <- cache.dependsChanged(1.seconds, funcChangeChecker).foreverM.start
      _ <- IO.sleep(2.seconds)
      endCacheSize <- cache.size()

    } yield TestCheck(
                        cacheSizeEmpty,
                        cacheSizeUser1Added,
                        cacheSizeUser2Added,
                        counterGetUser1_2gets.map(_.counterGet),
                        counterGetUser2_4gets.map(_.counterGet),
                        endCacheSize
                      )).map{ s =>
      assertEquals(s, TestCheck(0,1,2, Some(2),Some(4), 0))
    }
  }

  test("14) tsCreate not changed in meta and tsLru changing.  ") {
    val user1 = CacheEntity(User(10, "John", 64), Set("t_sys"))
    val keyUser1 = user1.hashCode()

    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(List(user1))
      m1 <- cache.getMeta(keyUser1)
      _ <- cache.get(keyUser1)
      _ <- cache.get(keyUser1)
      _ <- cache.get(keyUser1)
      _ <- cache.get(keyUser1)
      m2 <- cache.getMeta(keyUser1)
      _ <- cache.get(keyUser1)
      _ <- cache.get(keyUser1)
      _ <- cache.get(keyUser1)
      _ <- cache.get(keyUser1)
      m3 <- cache.getMeta(keyUser1)
    } yield (m3.map(_.counterGet),
             m1.map(_.tsCreate) == m2.map(_.tsCreate),
             m2.map(_.tsCreate) == m3.map(_.tsCreate),
             m1.map(_.tsLru) < m2.map(_.tsLru),
             m2.map(_.tsLru) < m3.map(_.tsLru)
    )
      ).map { s =>
      assertEquals(s, (Some(8),true,true,true,true))
    }
  }

  test("15) None tsCreateTime on empty Cache") {
    val user = CacheEntity(User(1, "John", 34), Set("t_sys", "t_session"))
    val key = user.hashCode()
    (for {
      cache <- createAndGetCache[User]
      m <- cache.getMeta(key)
      size <- cache.size()
    } yield (size, m.map(_.tsCreate))
      ).map(s =>
      assertEquals(s,(0, None)))
  }

  test("16) get entity lifetime > 1 sec., > 3 sec.") {
    val user = CacheEntity(User(1, "John", 34), Set("t_sys", "t_session"))
    val key = user.hashCode()
    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(List(user))
      mBefore <- cache.getMeta(key)
      _ <- sleep(1.seconds)
      _ <- cache.get(key)
      mAfter1Sec <- cache.getMeta(key)
       _ <- sleep(3.seconds)
      _ <- cache.get(key)
      mAfter3Sec <- cache.getMeta(key)
    } yield (
        mAfter1Sec.flatMap(c => mBefore.map(a => (c.tsLru - a.tsCreate) > 1000000L && (c.tsLru - a.tsCreate) < 1200000L)),
        mAfter3Sec.flatMap(c => mAfter1Sec.map(a => (c.tsLru - a.tsLru) > 3000000L && (c.tsLru - a.tsLru) < 3200000L )))
      ).map(s => assertEquals(s, (Some(true),Some(true))))
  }

  test("17) Cache dependsChanged works forever without cancellation. Look test 18 also.") {
    val users = List(
      CacheEntity(User(1, "John", 34), Set("t_sys", "t_session")),
      CacheEntity(User(2, "Mark", 40), Set("t_users", "t_sys")),
      CacheEntity(User(3, "Anna", 22), Set("t_sys", "t_roles"))
    )

    val f: IO[Unit] => IO[SetDependObjectName] = _ => Set("t_sys","t_session","t_users","t_roles").pure[IO]

    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(users)
      cacheSize1 <- cache.size()
      _ <- cache.dependsChanged(2.seconds, f).foreverM.start
      cacheSize2 <- cache.size()
      _ <- IO.sleep(3.seconds)
      cacheSize3 <- cache.size()
    } yield (cacheSize1, cacheSize2, cacheSize3 )).map{res =>
      assertEquals(res, (3,3,0))
    }
  }

  test("18) Cache dependsChanged cancellation. Look test 17 also.") {
    val users = List(
      CacheEntity(User(1, "John", 34), Set("t_sys", "t_session")),
      CacheEntity(User(2, "Mark", 40), Set("t_users", "t_sys")),
      CacheEntity(User(3, "Anna", 22), Set("t_sys", "t_roles"))
    )

    val f: IO[Unit] => IO[SetDependObjectName] = _ => Set("t_sys", "t_session", "t_users", "t_roles").pure[IO]

    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(users)
      cacheSize1 <- cache.size()
      fiber <- cache.dependsChanged(2.seconds, f).foreverM.start
      cacheSize2 <- cache.size()
      _ <- IO.sleep(3.seconds)
      cacheSize3 <- cache.size()
      _ <- fiber.cancel
      _ <- cache.save(users)
      cacheSize4 <- cache.size()
      _ <- IO.sleep(2.seconds)
      cacheSize5 <- cache.size()
    } yield (cacheSize1, cacheSize2, cacheSize3, cacheSize4, cacheSize5)).map { res =>
      assertEquals(res, (3,3,0, 3,3 ))
    }
  }

  test("19.1) Count of total objects in histDepChanges, without cache.dependsChanged") {
    val user1 = CacheEntity(User(1, "John", 34), Set("t_sys", "t_session","t_users","t_roles"))
    val user2 = CacheEntity(User(2, "Mary", 44), Set("t_sys", "t_session","t_users","t_roles","t_users_roles","t_oiv"))
    val user3 = CacheEntity(User(3, "Alex", 54), Set("t_area","t_city","t_sys", "t_session"))

    (for {
      cache <- createAndGetCache[User]
      h0 <- cache.getHistDepends
      _ <- cache.save(user1.toList)
      cacheDepends1 <- cache.getDepends
      h1 <- cache.getHistDepends
      _ <- cache.save(user2.toList)
      h2 <- cache.getHistDepends
      _ <- cache.save(user3.toList)
      h3 <- cache.getHistDepends
      _ <- List(user1,user2,user3).map(user => cache.remove(user.hashCode())).sequence.map(_ => ())
      cacheDepends2 <- cache.getDepends
      h4 <- cache.getHistDepends
    } yield (cacheDepends1, cacheDepends1.size, h0.size, h1.size, h2.size, h3.size, cacheDepends2.size, h4.size
    )).map { res =>
      assertEquals(res, (
        Set("t_sys", "t_session","t_users","t_roles"), 4, 0, 4, 6, 8, 0, 8
      ))
    }
  }

  test("19.2) Count of total objects in histDepChanges, with cache.dependsChanged") {
    val funcChange1: IO[Unit] => IO[SetDependObjectName] = _ => Set("t_sys", "t_session").pure[IO]
    val funcChange2: IO[Unit] => IO[SetDependObjectName] = _ => Set(                      "t_users").pure[IO]
    val funcChange3: IO[Unit] => IO[SetDependObjectName] = _ => Set(         "t_session","xxx","yyy").pure[IO]

    val user1 = CacheEntity(User(1, "John", 34), Set("t_sys", "t_session", "t_users", "t_roles"))

    (for {
      cache <- createAndGetCache[User]
      h0 <- cache.getHistDepends
      _ <- cache.save(user1.toList)
      cacheDepends1 <- cache.getDepends
      h1 <- cache.getHistDepends
      fiber1 <- cache.dependsChanged(1.seconds, funcChange1).foreverM.start
      //at this moment user1 removed from Cache(by "t_sys", "t_session") and this 2 deps. updated in histDepChanges
      _ <- IO.sleep(2.seconds) *> fiber1.cancel
      cacheDepends2 <- cache.getDepends //0
      h2 <- cache.getHistDepends //4

      fiber2 <- cache.dependsChanged(1.seconds, funcChange2).foreverM.start
      _ <- IO.sleep(2.seconds) *> fiber2.cancel
      h3 <- cache.getHistDepends

      fiber3 <- cache.dependsChanged(1.seconds, funcChange3).foreverM.start
      _ <- IO.sleep(2.seconds) *> fiber3.cancel
      h4 <- cache.getHistDepends

      cacheDepends3 <- cache.getDepends
      h5 <- cache.getHistDepends

    } yield (h0.size,
             cacheDepends1,
             cacheDepends1.size /*1 user in Cache has 4 depends*/,
             h1.size,
             cacheDepends2.size/*user removed from cache*/,
             h2.size,
             h1.get("t_sys").map(_.tsLastChange) < h2.get("t_sys").map(_.tsLastChange),
             h1.get("t_sys").map(_.changeCount) < h2.get("t_sys").map(_.changeCount),
             h3.get("t_users").map(_.changeCount) , h3.get("t_sys").map(_.changeCount),
             (h4.get("t_session").map(_.changeCount) > h4.get("t_users").map(_.changeCount) &&
               h4.get("t_session").map(_.tsLastChange) > h4.get("t_users").map(_.tsLastChange)
               ),
             cacheDepends3.size,
             h5.size,
            h4.get("t_session").map(_.changeCount),
            h4.get("xxx").map(_.changeCount)
    )).map { res =>
      assertEquals(res, (
              0, // empty cache 0 elements in history deps.
              Set("t_sys", "t_session", "t_users", "t_roles"),
              4, // 4 depends. look Set
              4, // this 4 elements in
              0, // cacheDepends2.size
              4, // h2.size
              true,
              true,
              Some(1),Some(1),
              true,
              0, // empty cache
              6,
              Some(2),
              Some(1)
      ))
    }
  }

  test("19.4) changeCount of one element from 1 added entity in cache") {
    val user1 = CacheEntity(User(1, "John", 34), Set("t_sys"))
    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(user1.toList)
      h1 <- cache.getHistDepends
    } yield (
      h1.size,
      h1.get("t_sys").map(_.changeCount)
    )).map { res =>
      assertEquals(
        res, (1,Some(0))
      )
    }
  }

  test("19.5) changeCount of one element from 1 added entity in cache") {
    val user1 = CacheEntity(User(1, "John", 34), Set("t_sys"))
    val funcChange1: IO[Unit] => IO[SetDependObjectName] = _ => Set("t_sys").pure[IO]
    (for {
      cache <- createAndGetCache[User]
      _ <- cache.save(user1.toList)
      fiber <- cache.dependsChanged(1.seconds, funcChange1).foreverM.start
      _ <- IO.sleep(3.seconds) *> fiber.cancel
      h1 <- cache.getHistDepends
    } yield
      h1.get("t_sys").map(_.changeCount)
    ).map { res =>
      assertEquals(res, Some(2))
    }
  }

  test("19.6) tsLastChange of one object in histDepChanges") {
    val funcChange1: IO[Unit] => IO[SetDependObjectName] = _ => Set("t_sys","t_users","t_roles").pure[IO]
    val user1 = CacheEntity(User(1, "John", 34), Set("t_sys", "t_session"))
    (for {
      cache <- createAndGetCache[User]
      h0 <- cache.getHistDepends
      _ <- cache.save(user1.toList)
      h1 <- cache.getHistDepends
      fiber1 <- cache.dependsChanged(1.seconds, funcChange1).foreverM.start
      _ <- IO.sleep(3.seconds) *> fiber1.cancel
      h2 <- cache.getHistDepends
    } yield (
      h0.get("t_sys").map(_.changeCount),
      h0.size,
      h1.size, h1.get("t_sys").map(_.changeCount), // 2 elements in hist from user1, no counter!
      h2.size, // 2 elements in hist from user1 and 2 additionally from funcChange1
      h2.get("t_sys").map(_.changeCount)
    )).map { res =>
      assertEquals(
        res, (None, 0, 2, Some(0), 4, Some(2))
      )
    }
  }


}
