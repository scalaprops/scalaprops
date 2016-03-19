package scalaprops

import java.util.concurrent.atomic.AtomicInteger

private[scalaprops] final case class TestStatus(
  success: AtomicInteger = new AtomicInteger(0),
  failure: AtomicInteger = new AtomicInteger(0),
  error: AtomicInteger = new AtomicInteger(0),
  ignored: AtomicInteger = new AtomicInteger(0),
  all: AtomicInteger = new AtomicInteger(0)
)
