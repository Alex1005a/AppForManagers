package libs

import cats.data.{EitherT, NonEmptyList}

object EitherNelT {
  type EitherNelT[F[_], A, B] = EitherT[F, NonEmptyList[A], B]
}
