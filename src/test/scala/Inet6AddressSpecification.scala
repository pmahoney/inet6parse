package org.polycrystal

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

import org.polycrystal.Inet6Address.Compressed

object Ipv6AddrSpecification extends Properties("Ipv6AddrSpecification") {

  // generate full inet6 address
  val genFullInet6 = for {
    ints <- Gen.listOfN(8, Gen.choose(0, 0xffff))
    words <- Gen.const(ints.map{ x => "%x".format(x) })
  } yield words.mkString(":")

  // generate abbreviated or compressed inet6 address
  val genAbbrevInet6 = for {
    prefixLen <- Gen.choose(0,7)
    suffixLen <- if (prefixLen > 0) Gen.choose(0,6-prefixLen) else Gen.choose(1,6-prefixLen)
    prefixWords <- Gen.listOfN(prefixLen, Gen.choose(1, 0xffff))
    suffixWords <- Gen.listOfN(suffixLen, Gen.choose(1, 0xffff))
    prefixStrs <- Gen.const(prefixWords.map{ x => "%x".format(x) })
    suffixStrs <- Gen.const(suffixWords.map{ x => "%x".format(x) })
  } yield Array(prefixStrs.mkString(":"), suffixStrs.mkString(":")).mkString("::")

  val genInet6 = oneOf(genFullInet6, genAbbrevInet6)

  property("roundtrip") = forAll(genInet6) { a =>
    a == Inet6Address.parse(a).toCompressed
  }

}
