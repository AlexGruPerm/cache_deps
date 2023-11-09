package com.cd

import cats.effect.Clock
import cats.effect.kernel.Temporal
import cats.syntax.all._

import scala.concurrent.duration.FiniteDuration

object Common {

  /**
   * Return time as MICROSECONDS
   */
  def currTimeMcSec[F[_]](implicit F: Temporal[F], clock: Clock[F]): F[FiniteDuration] =
    for {
      time <- clock.realTime
    } yield time

}
