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
    def concat(dir1: T, dir2: T): T

    def prepend(name: Name, dir1: T): T =
      concat(dir(name), dir1)

    def append(dir1: T, name: Name): T =
      concat(dir1, dir(name))
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
          dirs.prepend(name, dirs.dir(other))
      }

      def </>(other: Directory) = new Directory {
        def fold[T](dirs: Dirs[T]): T =
          dirs.prepend(name, other.fold(dirs))
      }
    }


    implicit class ToDirFromDirectory(directory: Directory) {
      def </>(other: Name) = new Directory {
        def fold[T](dirs: Dirs[T]): T =
          dirs.append(directory.fold(dirs), other)
      }

      def </>(other: Directory) =
        new Directory {
          def fold[T](dirs: Dirs[T]): T =
            dirs.concat(directory.fold(dirs), other.fold(dirs))
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
    def dir(name: Name): String =
      name.name

    def concat(dir1: String, dir2: String): String =
      dir1+"/"+dir2
  }

  object ShowDirs extends ShowDirs

  trait PrettyPrintDirs extends Dirs[PrettyPrint] {
    def dir(name: Name): PrettyPrint =
      new PrettyPrint { def pretty = name.name }

    def concat(dir1: PrettyPrint, dir2: PrettyPrint): PrettyPrint =
      new PrettyPrint { def pretty = dir1.pretty+"/"+dir2.pretty }
  }

  object PrettyPrintDirs extends PrettyPrintDirs

  trait PrettyPrint {
    def pretty: String
  }

