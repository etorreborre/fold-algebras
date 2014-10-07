package org.specs2.fold
package relativepaths

import org.specs2.fold.dirsfold._
import org.specs2.fold.filesfold._
import RelativeType._
import name._

  trait RelativePaths[T] extends Files[T] {
    def root: T
  }

  trait RelativeDirectory {
    type T <: RelativeType
    def fold[T](path: RelativePaths[T]): T
  }

  trait RelativeFile {
    type T <: RelativeType
    def fold[T](path: RelativePaths[T]): T
  }

  object RelativePathsDsl {

    def Root = new RelativeDirectory {
      type T = Absolute.type
      def fold[T](paths: RelativePaths[T]): T =
        paths.root
    }

    def directory(name: Name) =
      new RelativeDirectory {
        type T = Relative.type
        def fold[T](paths: RelativePaths[T]): T =
          paths.dir(name)
      }


    implicit class ToRelativeDir(name: Name) {
      def </>(other: RelativeDirectory)(implicit ev: IsRelative[other.T]) =
        new RelativeDirectory {
          type T = Relative.type
          def fold[T](paths: RelativePaths[T]): T =
            paths.dir(List(paths.dir(name), other.fold(paths)))

        }



      def </>(other: Name) = new RelativeDirectory {
        type T = Relative.type
        def fold[T](paths: RelativePaths[T]): T =
          paths.dir(List(paths.dir(name), paths.dir(other)))
      }

    }


    implicit class ToDirFromDirectory(val directory: RelativeDirectory) {
      def </>(other: Name) = new RelativeDirectory {
        type T = directory.T
        def fold[T](paths: RelativePaths[T]): T =
          paths.dir(List(directory.fold(paths), paths.dir(other)))
      }
      def </>(other: RelativeDirectory)(implicit ev: IsRelative[other.T]) =
        new RelativeDirectory {
          type T = directory.T
          def fold[T](paths: RelativePaths[T]): T =
            paths.dir(List(directory.fold(paths), other.fold(paths)))
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

    type IsRelative[T] = T =:= Relative.type
    type IsAbsolute[T] = T =:= Absolute.type


    type Same[T, S] = T =:= S
    type Not[T] = T => Nothing
    type Different[T, S] = Not[Same[T, S]]

  }

  trait RelativeType
  case object Relative extends RelativeType
  case object Absolute extends RelativeType

  object ShowRelativePaths extends RelativePaths[String] with ShowFiles {
    def root = ""
  }
