import CacheDataModel.{Cache, CacheEntity, SetDependObjectName}
import CacheImpl.RefCache
import cats.effect.IO
import munit.CatsEffectSuite
import cats.syntax.all._
import cats.effect.kernel.Ref
import data.User

import scala.concurrent.duration._


class CacheTests extends CatsEffectSuite {

  private val users = List(
    CacheEntity(User(1, "John", 34), Set("t_sys", "t_session")),
    CacheEntity(User(2, "Mark", 40), Set("t_users", "t_privs")),
    CacheEntity(User(3, "Anna", 22), Set.empty)
  )

  test("1) New Cache is empty.") {
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      cacheSize <- cache.size()
    } yield cacheSize).map(s => assertEquals(s, 0))
  }

  test("2) 3 elements added to Cache.") {
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      _ <- cache.save(users)
      cacheSize <- cache.size()
    } yield cacheSize).map(s => assertEquals(s, 3))
  }

  test("3) 3 elements added to Cache and removed. Check size = 0.") {
    import cats.implicits._
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      _ <- cache.save(users)
      _ <- users.map(user => cache.remove(user.hashCode())).sequence.map(_ => ())
      cacheSize <- cache.size()
    } yield cacheSize).map(s => assertEquals(s, 0))
  }

  test("4) get method in empty Cache.") {
    val john = CacheEntity(User(1, "John", 34), Set("t_sys", "t_session"))
    val keyJohn = john.hashCode()
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      userJohn <- cache.get(keyJohn)
    } yield userJohn).map(s => assertEquals(s, None))
  }

  test("5) Cache with 3 elements. We check CacheEntity exists.") {
    val john = CacheEntity(User(1, "John", 34),Set("t_sys","t_session"))
    val keyJohn = john.hashCode()
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      _ <- cache.save(users)
      userJohn <- cache.get(keyJohn)
    } yield userJohn).map(s => assertEquals(s, Some(john)))
  }

  test("6) Cache with 3 elements. We check CacheEntity not exists .") {
    val john = CacheEntity(User(1, "John", 34), Set("t_sys", "t_session"))
    val keyJohn = john.hashCode()
    val failJohn = CacheEntity(User(10, "John", 44), Set("t_sys"))
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
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
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
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
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      _ <- cache.save(usersLocal)
      cacheSizeBefore <- cache.size()
      _ <- cache.dependsChanged(1.seconds,funcChangeChecker).foreverM.start
      _ <- IO.sleep(2.seconds)
      cacheSizeAfter <- cache.size()
      _ <- cache.dependsChanged(1.seconds,funcChangeChecker2).foreverM.start
      _ <- IO.sleep(1.seconds)
      cacheSizeAfter2 <- cache.size()
    } yield (cacheSizeBefore,cacheSizeAfter,cacheSizeAfter2)).map(res => assertEquals(res, (3,1,0)))
  }

  test("9) Single save into Cache with 3 elements. Size=4") {
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      _ <- cache.save(users)
      _ <- cache.save(List(CacheEntity(User(10, "John", 64), Set("t_sys", "t_session"))))
      cacheSize <- cache.size()
    } yield cacheSize).map(s => assertEquals(s, 4))
  }

  test("10) Single save into Cache with 3 elements. And get this CacheEntity.") {
    val user = CacheEntity(User(10, "John", 64),Set("t_sys", "t_session"))
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
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
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
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
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
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


}
