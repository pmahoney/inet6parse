package org.polycrystal

import scala.annotation.tailrec

object Inet6Address {

  // add methods to java.net.Inet6Address
  implicit class Compressed(val a: java.net.Inet6Address) {
    import SeqUtil._

    def toCompressed = getCompressedHostAddress

    /**
      * Return the compressed string representation of this
      * Inet6Address. The first, longest string of zero words is
      * compressed to a double colon, except a single zero is not
      * compressed.
      */
    def getCompressedHostAddress = {
      val words = getWords
      val (iLongestZero, lenLongestZero) = segmentIndexAndLength(words){ _ == 0 }

      val groups = lenLongestZero match {
        case 0 => Array(words) // no compression
        case 1 => Array(words) // no compression
        case _ => {
          Array(words.slice(0, iLongestZero),
                words.slice(iLongestZero + lenLongestZero, words.size))
        }
      }

      groups.map { words => words.map("%x".format(_)).mkString(":") }.mkString("::")
    }

    /**
      * Get the representation of this address as 16-bit words.
      */
    def getWords: Seq[Int] = {
      val bytes = a.getAddress

      for (i <- 0 until 16 by 2)
      yield ((bytes(i) & 0xff) << 8) | (bytes(i+1) & 0xff)
    }

  }

  def fromBytes(bytes: Array[Byte]): java.net.Inet6Address = {
    if (bytes.length != 16) {
      throw new IllegalArgumentException("Inet6Address is 16 bytes, not " + bytes.length)
    }
    java.net.InetAddress.getByAddress(bytes).asInstanceOf[java.net.Inet6Address]
  }

  /**
    * Parse an IPv6 address, which may be in abbreviated form such as
    * 1:2::3 for 1:2:0:0:0:0:0:3.
    *
    * @throws IllegalArgumentException if unable to parse as an ipv6 address
    */
  def parse(input: String): java.net.Inet6Address = {
    val (prefix,suffix): (Array[String],Array[String]) = input.split("::", 2).map(splitColon) match {
      case Array(a,b) if a.size + b.size <= 8 => (a, b)
      case Array(a) if a.size == 8 => (a, Array())
      case _ => throw new IllegalArgumentException("not an ipv6 address '%s'".format(input))
    }

    val suffixOffset = 2*(8 - suffix.size)
    val bytes = new Array[Byte](16)

    for (i <- 0 until prefix.size) {
      val (hi,lo) = hextetbytes(prefix(i))
      bytes(2*i) = hi
      bytes(2*i + 1) = lo
    }

    for (i <- 0 until suffix.size) {
      val (hi,lo) = hextetbytes(suffix(i))
      bytes(suffixOffset + 2*i) = hi
      bytes(suffixOffset + 2*i+1) = lo
    }

    java.net.InetAddress.getByAddress(bytes).asInstanceOf[java.net.Inet6Address]
  }

  /**
    * Convert an ipv6 hextet into a two-byte tuple.
    */
  private def hextetbytes(s: String): (Byte,Byte) = try {
    val i = Integer.parseInt(s, 16)
    (((i >> 8) & 0xff).byteValue, (i & 0xff).byteValue)
  } catch {
    case e: NumberFormatException =>
      throw new IllegalArgumentException("not an ipv6 address"+s, e)
  }

  /**
    * Like String#split, but only splits on colon and returns empty
    * array rather than array of empty string when there is no colon.
    */
  private def splitColon(s: String): Array[String] = s.split(":") match {
    case Array("") => Array()
    case a => a
  }

}

object SeqUtil {

  /**
    * Compute the starting index and length of the longest segment
    * whose elements satisfy some predicate.
    */
  def segmentIndexAndLength[A](seq: Seq[A])(p: (A) => Boolean): (Int, Int) = {
    // Combine indexWhere and segmentLength
    def segmentWhere(from: Int): (Int,Int) =
      seq.indexWhere(p, from) match {
        case -1 => (-1, 0)
        case i => (i, seq.segmentLength(p, i))
      }

    // max based on the length, returning the first in a tie
    def max(a: (Int,Int), b: (Int,Int)): (Int,Int) =
      if (b._2 > a._2) b else a

    @tailrec
    def maxSegment(from: Int, curMax: (Int, Int)): (Int,Int) =
      segmentWhere(from) match {
        case (-1, 0) => curMax
        case (i, len) => maxSegment(i+len, max(curMax, (i,len)))
      }

    maxSegment(0, (0,0))
  }

}
