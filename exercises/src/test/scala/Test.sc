def twice(i: => Int): Int = i + i

twice({println("oops"); 10})

Stream(1, 2, 3).flatMap { a => Stream(a, a * 10) }.toList