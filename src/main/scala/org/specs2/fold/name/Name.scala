package org.specs2.fold.name

case class Name private(name: String) extends AnyVal

object Name {

  def fromString(string: String): Option[Name] =
    if (string contains "/") None
    else                     Some(Name(string))

  def unsafe(string: String) = Name(string)


}

