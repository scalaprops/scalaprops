package scalaprops
package scalazlaws

import scalaz.*

private[scalazlaws] object MonadStateLaws {
  def apply[F[_], S](implicit F: MonadState[F, S]): MonadStateLaws[F, S] = new MonadStateLaws(F)
}

private[scalazlaws] class MonadStateLaws[F[_], S] private (F: MonadState[F, S]) {
  def putPut(s1: S, s2: S)(implicit E: Equal[F[Unit]]): Boolean =
    E.equal(F.bind(F.put(s1))(_ => F.put(s2)), F.put(s2))

  def putGet(s: S)(implicit E: Equal[F[S]]): Boolean =
    E.equal(F.bind(F.put(s))(_ => F.get), F.bind(F.put(s))(_ => F.point(s)))

  def getPut(implicit E: Equal[F[Unit]]): Boolean =
    E.equal(F.bind(F.get)(F.put), F.point(()))

  def getGet(k: (S, S) => F[Unit])(implicit E: Equal[F[Unit]]): Boolean =
    E.equal(F.bind(F.get)(s => F.bind(F.get)(k(s, _))), F.bind(F.get)(s => k(s, s)))
}
