package org.specs2.fold.listfold

/**
 * Encoding of a List[T] as a Fold-algebra
 */
trait ListFolder[T, R] {
  def onEmpty: R
  def onAppend(t: T, rest: R): R
}

/**
 * The List[T] type
 */
trait List[T] {
  def fold[R](folder: ListFolder[T, R]): R
}

object List {

  /** we can create an empty list */
  def nil[T] = new List[T] {
    def fold[R](folder: ListFolder[T, R]): R =
      folder.onEmpty
  }

  /** we can append an element to a list */
  def append[T](t: T, rest: List[T]) = new List[T] {
    def fold[R](folder: ListFolder[T, R]): R =
      folder.onAppend(t, rest.fold(folder))
  }

  /**
   * we can trivially implement foldRight, which means that we can easily
   * implement map, filter, flatMap,... for the List[T] type
   */
  def foldRight[T, R](r: R)(function: (T, R) => R)(list: List[T]): R =
    list.fold(new ListFolder[T, R] {
      def onEmpty = r
      def onAppend(t: T, rest: R) = function(t, rest)
    })

}

