package org.weirdcanada.distro.util

class RollingWindow[T](capacity: Int, empty: => T = null) {
  private var head = 0
  private var length = 0
  
  private val items = scala.collection.mutable.ArraySeq.fill[T](capacity)(empty)
  
  def add(item: T) {
    synchronized {
      items.update((head + length) % capacity, item)
      
      if (length < capacity)
        length = length + 1
      else
        head = (head + 1) % capacity
    }
  }
  
  def remove(item: T) {
    synchronized {
      if (length > 0) {
        head = (head + 1) % capacity
        length = length - 1
      }
    }
  }
  
  def toList: List[T] = {
    synchronized {
      items.drop(head).take(math.min(capacity - head, length)).toList ::: items.take(math.max(length - (capacity - head), 0)).toList
    }
  }
}