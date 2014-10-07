package org.specs2.fold.listfold

  trait ListFolder[T, R] {
    def onEmpty: R
    def onAppend(t: T, rest: R): R
  }

  trait List[T] {
    def fold[R](folder: ListFolder[T, R]): R
  }

  object List {

    def nil[T] = new List[T] {
      def fold[R](folder: ListFolder[T, R]): R =
        folder.onEmpty
    }

    def append[T](t: T, rest: List[T]) = new List[T] {
      def fold[R](folder: ListFolder[T, R]): R =
        folder.onAppend(t, rest.fold(folder))
    }

    def foldRight[T, R](r: R)(function: (T, R) => R)(list: List[T]): R =
      list.fold(new ListFolder[T, R] {
        def onEmpty = r
        def onAppend(t: T, rest: R) = function(t, rest)
      })

  }

