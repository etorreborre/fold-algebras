package org.specs2.fold
package relativepaths

import org.specs2.fold.dirsfold._
import org.specs2.fold.filesfold._
import RelativeType._
import name._

/**
 * Extension of the Files fold with the notion of a Root
 * to encode Absolute/Relative paths
 */
trait RelativePaths[T] extends Files[T] {
  def root: T
}

/**
 * A Relative directory is still able to be fold with an algebra
 * but it also has a type member to specify if it is relative or absolute
 */
trait RelativeDirectory {
  type T <: RelativeType
  def fold[T](path: RelativePaths[T]): T
}

/**
 * A Relative file
 */
trait RelativeFile {
  type T <: RelativeType
  def fold[T](path: RelativePaths[T]): T
}

object RelativePathsDsl {

  /** the Root directory, it is absolute */
  def Root = new RelativeDirectory {
    type T = Absolute.type
    def fold[T](paths: RelativePaths[T]): T =
      paths.root
  }

  /** a directory with just a Name is relative */
  def directory(name: Name) =
    new RelativeDirectory {
      type T = Relative.type
      def fold[T](paths: RelativePaths[T]): T =
        paths.dir(name)
    }


  implicit class ToRelativeDir(name: Name) {
    /**
     * we can create a directory by appending 2 Names,
     * the result is a Relative directory
     */
    def </>(other: Name) = new RelativeDirectory {
      type T = Relative.type
      def fold[T](paths: RelativePaths[T]): T =
        paths.prepend(name, paths.dir(other))
    }

    /**
     * we can create a directory by appending a directory onto a Name,
     * but only if the "other" directory is Relative
     * The result is a Relative directory
     */
    def </>(other: RelativeDirectory)(implicit ev: IsRelative[other.T]) =
      new RelativeDirectory {
        type T = Relative.type
        def fold[T](paths: RelativePaths[T]): T =
          paths.prepend(name, other.fold(paths))
      }
  }


  implicit class ToDirFromDirectory(val directory: RelativeDirectory) {
    def </>(other: Name) = new RelativeDirectory {
      type T = directory.T
      def fold[T](paths: RelativePaths[T]): T =
        paths.append(directory.fold(paths), other)
    }
    def </>(other: RelativeDirectory)(implicit ev: IsRelative[other.T]) =
      new RelativeDirectory {
        type T = directory.T
        def fold[T](paths: RelativePaths[T]): T =
          paths.concat(directory.fold(paths), other.fold(paths))
      }
  }

  implicit class ToRelativeFile(name: Name) {
    def <|>(other: Name) = new RelativeFile {
      type T = Relative.type
      def fold[T](paths: RelativePaths[T]): T =
        paths.fileName(paths.dir(name), other)
    }

    def </>(other: RelativeFile)(implicit ev: IsRelative[other.T]) = new RelativeFile {
      type T = Relative.type
      def fold[T](paths: RelativePaths[T]): T =
        paths.fileFile(paths.dir(name), other.fold(paths))
    }
  }

  implicit class ToAbsoluteFile(val directory: RelativeDirectory) {
    def <|>(other: Name) = new RelativeFile {
      type T = directory.T
      def fold[T](paths: RelativePaths[T]): T =
        paths.fileName(directory.fold(paths), other)
    }

    def </>(other: RelativeFile)(implicit ev: IsRelative[other.T]) =
      new RelativeFile {
        type T = directory.T
        def fold[T](paths: RelativePaths[T]): T =
          paths.fileFile(directory.fold(paths), other.fold(paths))
      }
  }
}

object RelativeType {

  // useful type aliases
  type IsRelative[T] = T =:= Relative.type
  type IsAbsolute[T] = T =:= Absolute.type

}

trait RelativeType
case object Relative extends RelativeType
case object Absolute extends RelativeType

/** show paths for absolute/relative dirs/paths */
object ShowRelativePaths extends RelativePaths[String] with ShowFiles {
  def root = ""
}
