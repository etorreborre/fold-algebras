package org.specs2
package fold
package filesfold

import dirsfold._
import DirectoryDsl._
import FilesDsl._
import name._

class FilesSpec extends Specification { def is = s2"""

 Fold algebras can be used to represent file paths
    it is possible to instantiate an expression with an algebra for pretty-printing $prettyPrint


"""

  def prettyPrint =

    file1.fold(ShowFiles) === "dir1/file1"

  def dir1: Directory = directory("dir1")
  def file1: File     = dir1 <|> "file1"

}

