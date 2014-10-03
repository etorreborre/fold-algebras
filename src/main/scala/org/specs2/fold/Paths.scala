package org.specs2.fold

import scalaz.effect.IO

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
 * This is equivalent to encoding a rose tree where the leaves are strings
 */
trait Dirs[T] {
  def dir(name: String): T
  def dir(dirs: List[T]): T
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
object DirectoryDsl {
  
  def directory(name: String) = new Directory { def fold[T](dirs: Dirs[T]): T = dirs.dir(name) }

  implicit class ToDir(name: String) {
    def </>(other: String) = new Directory { def fold[T](dirs: Dirs[T]): T = dirs.dir(List(dirs.dir(name), dirs.dir(other))) }
    def </>(other: Directory)   = new Directory { def fold[T](dirs: Dirs[T]): T = dirs.dir(List(dirs.dir(name), other.fold(dirs))) }
  }

  implicit class ToDir2(directory: Directory) {
    def </>(other: String) = new Directory { def fold[T](dirs: Dirs[T]): T      = dirs.dir(List(directory.fold(dirs), dirs.dir(other))) }
    def </>(other: Directory)   = new Directory { def fold[T](dirs: Dirs[T]): T = dirs.dir(List(directory.fold(dirs), other.fold(dirs)) }
  }
}

object ShowDirs extends Dirs[String] {
  def dir(name: String): String = name
  def dir(dirs: List[String]) = dirs.mkString("/")
}

/**
 * FILES
 */
trait Files[T] {
  def file(dir1: Directory, name: String): T
}

/**
 * A file is a type capable of folding any Files algebra
 * given a Dirs algebra because files live in directories
 */
trait File {
  def fold[T : Dirs](file: Files[T]): T
}


object FilesDsl {

  implicit class ToFile(directory: Directory) {
    def <|>(other: String) = new File { def fold[T : Dirs](files: Files[T]): T = files.file(directory, other) }
    def </>(other: File)  = new File { def fold[T : Dirs](files: Files[T]): T = files.file(directory.fold(implicitly[Dirs[T]]), other.fold(files)) }
  }
}


trait Show1 {
  def show: String
}

object ShowFiles extends Files[String] {
  def file(dir: Directory, name: String) =
    dir.fold(ShowDirs) + "/" + name
}


trait Path[T] {
  def file(name: String): T
  def dir(name: String, dirs: List[T] = Nil, files: List[T] = Nil): T
}

trait RelativePath[T] {
  def file(context: RelativeContext, name: String): T
  def dir(context: RelativeContext, name: String): T
  def dir(context: RelativeContext, name: String, dirs: List[T], files: List[T]): T
}

object create extends RelativePath[IO[Unit]] {
  def file(context: RelativeContext, name: String): IO[Unit] =
    IO(())

  def dir(context: RelativeContext, name: String): IO[Unit] =
    IO(())

  def dir(context: RelativeContext, dirs: List[IO[Unit]], files: List[IO[Unit]]) =
    IO(())
}


trait RelativePath2[T, C] {
  def file(context: RelativeContext, name: String): (T, C)
  def dir(context: RelativeContext, name: String): (T, C)
  def dir(context: RelativeContext, dirs: List[(T, C)], files: List[(T, C)]): (T, C)
}

trait RelativePath3[T] {
  def file(name: String): T
  def dir(name: String): T
  def root: T
  def dir(dirs: List[T], files: List[T]): T
}

trait RelativePath4[T] {
  def root: T
  def path(root: T, p: Path[T]): T
}

trait AFile {
  def fold[T](path: Path[T]): T
}
trait ADir {
  def fold[T](path: Path[T]): T
}

trait ARelativeDir {
  type T <: RelativeContext
  def fold[T](path: RelativePath[T]): T
}
trait ARelativeFile {
  type T <: RelativeContext
  def fold[T](path: RelativePath[T]): T
}

trait RelativeContext
case object Relative extends RelativeContext
case object Absolute extends RelativeContext

object PathDsl {

  def directory(name: String) = new ADir  { def fold[T](path: Path[T]): T = path.dir(name) }
  def file(name: String)      = new AFile { def fold[T](path: Path[T]): T = path.file(name) }

  implicit class ToDir(name: String) {
    def </>(other: String) = new ADir  { def fold[T](path: Path[T]): T = path.dir(List(path.dir(name),   path.dir(other)), Nil) }
    def <|>(other: String) = new AFile { def fold[T](path: Path[T]): T = path.dir(List(path.dir(name)),  List(path.file(other))) }
    def </>(others: ADir*) = new ADir  { def fold[T](path: Path[T]): T = path.dir(path.dir(name) +: others.map(_.fold(path))) }
  }

  implicit class ToDir2(directory: ADir) {
    def </>(other: String) = new ADir  { def fold[T](path: Path[T]): T = path.dir(List(directory.fold(path),  path.dir(other))) }
    def <|>(other: String) = new AFile { def fold[T](path: Path[T]): T = path.dir(List(directory.fold(path)), List(path.file(other))) }
    def </>(others: ADir*) = new ADir  { def fold[T](path: Path[T]): T = path.dir(directory.fold(path) +: others.map(_.fold(path))) }
  }
}

object RelativePathDsl {

  def root = new ARelativeDir {
    type T = Absolute.type
    def fold[T](path: RelativePath[T]): T = path.dir(Absolute, "/")

    def </>(other: String) = new ARelativeDir  {
      type T = Absolute.type
      def fold[T](path: Path[T]): T = path.dir(Absolute, "", List(path.dir(other)))
    }
    def <|>(other: String) = new ARelativeFile {
      type T = Absolute.type
      def fold[T](path: Path[T]): T = path.dir(Absolute, "", Nil, List(List(path.file(other))))
    }
    def </>(other: ARelativeDir)(implicit ev: Relative.type =:= other.T) = new ARelativeDir  {
      type T = Absolute.type
      def fold[T](path: Path[T]): T = path.dir(Absolute, "", List(other.fold(path)))
    }
  }

  def directory(name: String) = new ARelativeDir  {
    type T = Relative.type
    def fold[T](path: RelativePath[T]): T = path.dir(Relative, name)
  }
  def absoluteDirectory(name: String) = new ARelativeDir  {
    type T = Absolute.type
    def fold[T](path: RelativePath[T]): T = path.dir(Absolute, name)
  }

  def file(name: String) = new ARelativeFile {
    type T = Relative.Type
    def fold[T](path: RelativePath[T]): T = path.file(Relative, name)
  }
  def absoluteFile(name: String) = new ARelativeFile {
    type T = Absolute.type
    def fold[T](path: RelativePath[T]): T = path.file(Absolute, name)
  }

  implicit class ToRelativeDir(name: String) {
    def </>(other: String) = new ARelativeDir  {
      type T = Relative.Type
      def fold[T](path: RelativePath[T]): T = path.dir(Relative, name, List(path.dir(other)), Nil)
    }
    def <|>(other: String) = new ARelativeFile {
      type T = Relative.Type
      def fold[T](path: RelativePath[T]): T = path.dir(Relative, name, Nil,  List(path.file(other)))
    }
    def </>(other: ARelativeDir)(implicit ev: other.T =:= Relative.type) = new ARelativeDir  {
      type T = Relative.Type
      def fold[T](path: RelativePath[T]): T = path.dir(Relative, name, List(other.fold(path)))
    }
  }

  implicit class ToRelativeDir2(dir: ARelativeDir) {
    def </>(other: String) = new ARelativeDir  {
      type T = Relative.Type
      def fold[T](path: RelativePath[T]): T = path.dir(Relative, List(path.dir(name),   path.dir(other)), Nil)
    }
    def <|>(other: String) = new ARelativeFile {
      type T = Relative.Type
      def fold[T](path: RelativePath[T]): T = path.dir(Relative, List(path.dir(name)),  List(path.file(other)))
    }
    def </>(other: ARelativeDir) = new ARelativeDir  {
      type T = Relative.Type
      def fold[T](path: RelativePath[T]): T = path.dir(Relative, path.dir(name) +: others.map(_.fold(path)))
    }
  }

}

trait PathADT
case class FileADT(name: String) extends PathADT
case class DirectoryADT(name: String, dirs: List[DirectoryADT] = Nil, files: List[FileADT] = Nil) extends PathADT {
  def </>(other: String) = DirectoryADT(name, List(DirectoryADT(name)))
  def <|>(other: String) = DirectoryADT(name, Nil, List(FileADT(name)))
  def </>(others: DirectoryADT*) = DirectoryADT(name, dirs ++ others)
}


