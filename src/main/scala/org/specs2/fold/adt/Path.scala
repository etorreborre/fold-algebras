package org.specs2.fold
package adt

import name._

/**
 * ADT for files and directories
 *
 * With a small DSL to create directories and files from directories
 */
sealed trait Path

case class Directory(names: List[Name]) extends Path {
  def </>(other: Directory) = Directory(names ++ other.names)
  def </>(name: Name)       = Directory(names :+ name)

  def <|>(name: Name) = File(this, name)
}

case class File(directory: Directory, name: Name) extends Path

/**
 * A typeclass definition
 */
trait Show[T] {
  def show(t: T): String
}

object Path {
  /**
   * "Interpretation" of the datatype to display paths.
   *
   * It uses pattern matching
   */
  implicit def ShowPath: Show[Path] = new Show[Path] {
    def show(p: Path): String = p match {
      case Directory(names)      => names.mkString("/")
      case File(directory, name) => show(directory)+"/"+name
    }
  }
}

