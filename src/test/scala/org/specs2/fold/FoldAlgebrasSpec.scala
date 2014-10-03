package org.specs2
package fold

import scalaz._, Scalaz._
import matcher._
import DirectoryDsl._

class FoldAlgebrasSpec extends Specification with ThrownExpectations { def is = s2"""
  Fold algebras can be used to represent files and directory paths
    it is possible to instantiate an expression with an algebra for pretty-printing $prettyPrint


"""

  def prettyPrint = dir3.fold(ShowDirs) === "dir3/dir1/dir2"

  def dir1: Directory = directory("dir1")
  def dir2: Directory = "dir1" </> "dir2"
  def dir3: Directory = "dir3" </> dir2

}
