package org.specs2.fold
package dirsfold

import org.specs2.reflect.Compat210.blackbox.Context
import name._
/**
 * Data as fold
 *
 * 1. encode directories
 * 2. encode files as a separate DSL (using the first)
 * 4. use interpreters to retrieve: ADT, pretty print, file system operations
 * 5. put everything under the same DSL (Paths)
 * 6. encode absolute/relative constraints as path dependent types
 *
 */

/**
 * DIRECTORIES
 */

/**
 * Church encoding of directories
 *
 * This is equivalent to encoding a list with a named empty element
 */


  trait Dirs[T] {
    def dir(name: Name): T
    def dir(dirs: List[T]): T
  }

  object Dirs {
    def apply[T : Dirs] = implicitly[Dirs[T]]
  }

  /**
   * A directory is a type capable of folding any Dirs algebra
   */


  trait Directory {
    def fold[T](dirs: Dirs[T]): T
  }

  /**
   * DSL for creating directories
   */
  trait DirectoryDsl {

    def directory(name: Name) =
      new Directory {
        def fold[T](dirs: Dirs[T]): T = dirs.dir(name)
      }

    implicit class ToDir(name: Name) {
      def </>(other: Name) = new Directory {
        def fold[T](dirs: Dirs[T]): T =
          dirs.dir(List(dirs.dir(name), dirs.dir(other)))
      }

      def </>(other: Directory) = new Directory {
        def fold[T](dirs: Dirs[T]): T =
          dirs.dir(List(dirs.dir(name), other.fold(dirs)))
      }
    }


    implicit class ToDirFromDirectory(directory: Directory) {
      def </>(other: Name) = new Directory {
        def fold[T](dirs: Dirs[T]): T =
          dirs.dir(List(directory.fold(dirs), dirs.dir(other)))
      }

      def </>(other: Directory) =
        new Directory {
          def fold[T](dirs: Dirs[T]): T =
            dirs.dir(List(directory.fold(dirs), other.fold(dirs)))
        }
    }

  }

  object DirectoryDsl extends DirectoryDsl {


    implicit def stringToDirDsl(string: String): DirectoryDsl.ToDir =
      macro stringToDirDslMacro

    def stringToDirDslMacro(c: Context)(string: c.Expr[String]): c.Expr[DirectoryDsl.ToDir] = {
      import c.universe._
      string match {
        case Expr(Literal(Constant(v: String))) => c.Expr(q"DirectoryDsl.ToDir(${nameFromString(c)(v)})")
        case _ => c.abort(c.enclosingPosition, s"Not a literal ${showRaw(string)}")
      }
    }
  }

  trait ShowDirs extends Dirs[String] {
    def dir(name: Name): String = name.name
    def dir(dirs: List[String]) = dirs.mkString("/")
  }

  object ShowDirs extends ShowDirs

  trait PrettyPrintDirs extends Dirs[PrettyPrint] {
    def dir(name: Name): PrettyPrint =
      new PrettyPrint { def pretty = name.name }

    def dir(dirs: List[PrettyPrint]): PrettyPrint =
      new PrettyPrint {
        def pretty = dirs.map(_.pretty).mkString("/")
      }
  }

  object PrettyPrintDirs extends PrettyPrintDirs

  trait PrettyPrint {
    def pretty: String
  }

