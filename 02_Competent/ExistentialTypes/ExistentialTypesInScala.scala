val xs = Array[String]("foo", "bar", "baz")

def foo0(xs: Array[Any]) =
  println(xs.length)

// mismatch:
// foo0(Array[String]("foo", "bar", "baz"))


def foo1[T](xs: Array[T]) =
  println(xs.length)

// foo1[String](xs)
foo1(xs)


/*
 forSome t. Array t
 --> all arrays
 --> ops for arrays OK, ops for elements undefined.
 */
def foo2(xs: Array[T] forSome { type T }) =
  println(xs.length)

foo2(xs)


def foo2s(xs: Array[_]) =
  println(xs.length)

foo2s(xs)


/** With bounds (not view bounds due to tech limitations). */
def foo3(xs: Array[T] forSome {type T <: CharSequence}) =
  xs foreach {y => println(y.length)}

foo3(xs)


/** 
  Array[T] forSome {type T}
  ==
  Array[_]
  === all arrays
  
  Array[T forSome {type T}]
  === array of Any
  */


// Map[Class[T forSome {type T}], String] === Map[Class[Any], String]
// Map[Class[T] forSome {type T}, String] === Map[Class[_], String]
// Map[Class[T], String] forSome {type T} === supertype of all (Map[Class[T], String])
