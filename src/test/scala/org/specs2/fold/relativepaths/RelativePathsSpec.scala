package org.specs2
package fold
package relativepaths

import dirsfold._
import filesfold._
import RelativePathsDsl._
import name._

class RelativePathsSpec extends Specification { def is = s2"""
 Fold algebras can be used to represent directory paths
    it is possible to instantiate an expression with an algebra for pretty-printing $prettyPrint
"""

  def prettyPrint =
    dir1.fold(ShowRelativePaths) === "/dir1"

  val dir1 = Root </> directory("dir1")

/**
 * doesn't and shouldn't compile
 *
 * val dir2 = Root </> Root
 */
}
