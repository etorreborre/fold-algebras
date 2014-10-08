package org.specs2.fold
package filesfold

import dirsfold._
import name._

/**
 * Fold algebra for Files, extends the Dirs one
 *
 * In terms of the Expression problem we add a new data type variant here.
 */
trait Files[T] extends Dirs[T] {
  def fileName(directory: T, name: Name): T
  def fileFile(directory: T, file: T): T
}

/**
 * A file is a type capable of folding any Files algebra
 */
trait File {
  def fold[T](file: Files[T]): T
}

/**
 * DSL to create files by appending names or files to a Directory
 */
object FilesDsl {

  implicit class ToFile(directory: Directory) {
    def <|>(name: Name) = new File {
      def fold[T](files: Files[T]): T =
        files.fileName(directory.fold(files), name)
    }

    def </>(file: File)  = new File {
      def fold[T](files: Files[T]): T =
        files.fileFile(directory.fold(files), file.fold(files))
    }
  }
}

/**
 * Pretty-print files by extending the existing ShowDirs interpreter
 */
trait ShowFiles extends ShowDirs with Files[String] {
  def fileName(directory: String, name: Name) =
    directory+"/"+name.name

  def fileFile(directory: String, file: String) =
    directory+"/"+file
}

object ShowFiles extends ShowFiles

