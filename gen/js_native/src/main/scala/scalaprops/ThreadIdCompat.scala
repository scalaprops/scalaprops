package scalaprops

private[scalaprops] object ThreadIdCompat {
  def threadId(): Long = Thread.currentThread().getId()
}
