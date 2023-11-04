import CacheDataModel.{Cache, CacheEntity, DependObjectName}
import CacheImpl.RefCache
import cats.effect.IO
import munit.CatsEffectSuite
import cats.syntax.all._
import cats.effect.kernel.Ref
import data.User


class CacheTests extends CatsEffectSuite {

  private val changeChecker: IO[Unit] => IO[Set[DependObjectName]] = _ => Set("t_users", "t_roles").pure[IO]

  private val users = List(
    CacheEntity(User(1, "John", 34), Set("t_sys", "t_session")),
    CacheEntity(User(2, "Mark", 40), Set("t_users", "t_privs")),
    CacheEntity(User(3, "Anna", 22), Set.empty)
  )

  test("New Cache is empty.") {
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      cacheSize <- cache.size()
    } yield cacheSize).map(s => assertEquals(s, 0))
  }

  test("3 elements added to Cache.") {
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      _ <- cache.saveEntitiesInCache(users)
      cacheSize <- cache.size()
    } yield cacheSize).map(s => assertEquals(s, 3))
  }

  test("get method in empty Cache.") {
    val john = CacheEntity(User(1, "John", 34), Set("t_sys", "t_session"))
    val keyJohn = john.hashCode()
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      userJohn <- cache.get(keyJohn)
    } yield userJohn).map(s => assertEquals(s, None))
  }

  test("Cache with 3 elements. We check CacheEntity exists.") {
    val john = CacheEntity(User(1, "John", 34),Set("t_sys","t_session"))
    val keyJohn = john.hashCode()
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      _ <- cache.saveEntitiesInCache(users)
      userJohn <- cache.get(keyJohn)
    } yield userJohn).map(s => assertEquals(s, Some(john)))
  }

  test("Cache with 3 elements. We check CacheEntity not exists .") {
    val john = CacheEntity(User(1, "John", 34), Set("t_sys", "t_session"))
    val keyJohn = john.hashCode()
    val failJohn = CacheEntity(User(10, "John", 44), Set("t_sys"))
    (for {
      ref <- Ref[IO].of(Cache[User]())
      cache = new RefCache[IO, CacheEntity[User], User](ref)
      _ <- cache.saveEntitiesInCache(users)
      userJohn <- cache.get(keyJohn)
    } yield userJohn).map(s => assertNotEquals(s, Some(failJohn)))
  }

}
