package org.specs2
package fold
package dirsfold

import DirectoryDsl._
import org.specs2.fold._
import org.specs2.fold.name.Name

class DirsSpec extends Specification { def is = s2"""

 Fold algebras can be used to represent directory paths
    it is possible to instantiate an expression with an algebra for pretty-printing $prettyPrint


"""

  def prettyPrint =

    dir3.fold(ShowDirs) === "dir3/dir1/dir2"

  def dir1: Directory = directory("dir1")
  def dir2: Directory = "dir1" </> "dir2"
  def dir3: Directory = "dir3" </> dir2


  val name = Name("tmp")

  def dir: Directory =
    "dir1" </> "dir2" </> "dir3"

  Name("dir1") </> Name("dir2") </> Name("dir3")

    "dir1" </> dir2


}
