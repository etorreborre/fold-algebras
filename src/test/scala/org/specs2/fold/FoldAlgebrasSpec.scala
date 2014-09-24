package org.specs2
package fold

import scalaz._, Scalaz._
import matcher._

class FoldAlgebrasSpec extends Specification with ThrownExpectations { def is = s2"""
  valid paths with a dsl       $dsl
"""

  def dsl = {
    import PathsDsl._

    val (d1: Show1, d2) = (dir("d1"), dir("d2"))
    val d3 = d1 </> d2
    val f1 = d1 <|> "f1"

    d3.show must_== "d1/d2"
    f1.show must_== "d1/f1"
  }

  implicit def PathsShow: Paths[Show1] = new Paths[Show1] {
    def dir(name: String): Show1 with DirType = new Show1 with DirType {
      def show = name
    }
    def dir(dir1: Show1 with DirType, dir2: Show1 with DirType): Show1 with DirType = new Show1 with DirType {
      def show = dir1.show+"/"+dir2.show
    }
    def file(dir: Show1 with DirType, name: String): Show1 with FileType = new Show1 with FileType {
      def show = dir.show+"/"+name
    }
  }
}

/**
* path := dir | file
* dir := add dir dir | name
* file := add dir name
* name := string
*/

trait ForallHigh[A] {
  def apply[F[_]]: F[A]
}

trait PathFold {
  def fold[A](algebra: Paths[A]): A
}

trait Show1 {
  def show: String
}

object PathsDsl {
  def dir[T : Paths](name: String) = Paths[T].dir(name)

  implicit class DirAppend[T, S](dir1: S)(implicit pt: Paths[T], ev: S =:= T with DirType) {
    def </>(dir2: T with DirType): T with DirType = Paths[T].dir(dir1, dir2)
    def <|>(name: String): T with FileType = Paths[T].file(dir1, name)
  }
}

trait Algebra1 {
  def apply[F[_], T](f: F[T]): T
}
trait Algebra2[F[_]] {
  def apply[T](f: F[T]): T
}

trait Paths[T] {
  def dir(name: String): T with DirType
  def dir(dir1: T with DirType, dir2: T with DirType): T with DirType
  def file(dir: T with DirType, name: String): T with FileType
}

object Paths {
  def apply[T](implicit pt: Paths[T]): Paths[T] = pt
}

trait Names[T] {
  def name(name: String): T
}

trait PathType
case object Dir extends PathType
case object File extends PathType

trait DirType
trait FileType

//class TestSpec extends Specification with ThrownExpectations { def is = s2"""
//  test $e1
//  test $e2
//"""
//
//  def e1 = {
//    import PathsShow._
//    val d = dir(dir("d1"), dir("d2"))
//    val f1 = file(dir("d1"), "f1")
//    // val d2 = dir(f1, dir("d2")) doesn't compile
//    d.show must_== "d1/d2"
//  }
//
//  def e2 = {
//    import PathsDsl._
//
//    val (d1, d2) = (dir("d1"), dir("d2"))
//    val d3 = d1 </> d2
//    val f1 = d1 <|> "f1"
//
//    d3.show must_== "d1/d2"
//    f1.show must_== "d1/f1"
//  }
//
//  implicit def PathsTypesShow: PathsTypes[Show2] = new PathsTypes[Show2] {
//    def dir(name: String): Show2 with DirType = new Show2 with DirType {
//      def show = name
//    }
//    def dir(dir1: Show2 with DirType, dir2: Show2 with DirType): Show2 with DirType = new Show2 with DirType {
//      def show = dir1.show+"/"+dir2.show
//    }
//    def file(dir: Show2 with DirType, name: String): Show2 with FileType = new Show2 with FileType {
//      def show = dir.show+"/"+name
//    }
//  }
//
//
//}
//
///**
// * path := dir | file
// * dir := add dir dir | name
// * file := add dir name
// * name := string
// *
// *
// */
//
//trait ForallHigh[A] {
//  def apply[F[_]]: F[A]
//}
//
//trait PathFold {
//  def fold[A](algebra: PathsTypes[A]): A
//}
//
//
//trait Paths[T[_]] {
//  def dir(name: String): T[Dir.type]
//  def dir(dir1: T[Dir.type], dir2: T[Dir.type]): T[Dir.type]
//  def file(dir: T[Dir.type], name: String): T[File.type]
//}
//trait Show1[A] {
//  def show: String
//}
//object PathsShow extends Paths[Show1] {
//  def dir(name: String): Show1[Dir.type] = new Show1[Dir.type] {
//    def show = name
//  }
//  def dir(dir1: Show1[Dir.type], dir2: Show1[Dir.type]): Show1[Dir.type] = new Show1[Dir.type] {
//    def show = dir1.show+"/"+dir2.show
//  }
//  def file(dir: Show1[Dir.type], name: String): Show1[File.type] = new Show1[File.type] {
//    def show = dir.show+"/"+name
//  }
//}
//
//trait Show2 {
//  def show: String
//}
//
//object PathsDsl {
//  def dir[T : PathsTypes](name: String) = PathsTypes[T].dir(name)
//
//  implicit class DirAppend[T, S](dir1: S)(implicit pt: PathsTypes[T], ev: S =:= T with DirType) {
//    def </>(dir2: T with DirType): T with DirType = PathsTypes[T].dir(dir1, dir2)
//    def <|>(name: String): T with FileType = PathsTypes[T].file(dir1, name)
//  }
//}
//
//trait Algebra1 {
//  def apply[F[_], T](f: F[T]): T
//}
//trait Algebra2[F[_]] {
//  def apply[T](f: F[T]): T
//}
//
//trait PathsTypes[T] {
//  def dir(name: String): T with DirType
//  def dir(dir1: T with DirType, dir2: T with DirType): T with DirType
//  def file(dir: T with DirType, name: String): T with FileType
//}
//
//object PathsTypes {
//  def apply[T](implicit pt: PathsTypes[T]): PathsTypes[T] = pt
//}
//
//trait Names[T] {
//  def name(name: String): T
//}
//
//trait PathType
//case object Dir extends PathType
//case object File extends PathType
//
//trait DirType
//trait FileType