
import org.scalatest.funsuite._
import lightarrow.Arrow
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Future, Promise }
import scala.util.{ Success, Failure }

class ArrowTest extends AsyncFunSuite {
  test("Arrow should map") {
    Arrow.resolve(1)
      .map(a => a * 3)
      .runAsFuture(null)
      .map {
        case Right(r) => assert(r == 3)
      }
  }

  test("Arrow should be immutable - a") {
    val a = Arrow.resolve(1)
    val b = a.map(x => x * 2)
    val c = a.map(x => x * 3)
    b.runAsFuture(null)
    .map {
      case Right(r) => assert(r == 2)
    }
  }

  test("Arrow should be immutable - b") {
    val a = Arrow.resolve(1)
    val b = a.map(x => x * 2)
    a.map(x => x * 3)
      .runAsFuture(null)
      .map {
        case Right(r) => assert(r == 3)
      }
  }

  test("Arrow should map - E") {
    Arrow.resolve(1)
    .flatMap(b => {
      if (b > 0) {
        Arrow.reject(1)
      } else {
        Arrow.resolve(1)
      }
    })
    .map(x => x * 3)
      .runAsFuture(null)
      .map {
        case Left(r) => assert(r == 1)
      }
  }

  test("Arrow should flatMap") {
    Arrow.resolve(1)
      .flatMap(a => Arrow.resolve(a * 3))
      .runAsFuture(null)
      .map {
        case Right(r) => assert(r == 3)
      }
  }

  test("Arrow should flatMap - fail") {
    Arrow.resolve(1)
      .flatMap(a => Arrow.reject(a * 3))
      .runAsFuture(null)
      .map {
        case Left(r) => assert(r == 3)
      }
  }

  test("Arrow should leftMap") {
    Arrow.reject(1)
      .leftMap(a => a * 3)
      .runAsFuture(null)
      .map {
        case Left(r) => assert(r == 3)
      }
  }

  test("Arrow should biMap") {
    Arrow.construct[Int, Int]((resolve, reject) => {
      if (3 < 5) {
        resolve(5)
      } else {
        reject(5)
      }
    })
      .biMap(
        a => a * 2,
        a => a * 3
      )
      .runAsFuture(null)
      .map {
        case Right(r) => assert(r == 15)
      }
  }

  test("Arrow should biMap - left") {
    Arrow.construct[Int, Int]((resolve, reject) => {
      if (5 < 3) {
        resolve(5)
      } else {
        reject(5)
      }
    })
      .biMap(
        a => a * 2,
        a => a * 3
      )
      .runAsFuture(null)
      .map {
        case Left(r) => assert(r == 10)
      }
  }

  test("Arrow should group") {
    Arrow.resolve(1).group(Arrow.resolve(2))
      .runAsFuture(null)
      .map {
        case Right((a, b)) => {
          assert(a == 1)
          assert(b == 2)
        }
      }
  }

  test("Arrow should group - fail") {
    Arrow.resolve(1).group(Arrow.reject(2))
      .runAsFuture(null)
      .map {
        case Left((b)) => {
          assert(b == 2)
        }
      }
  }

  test("Arrow should groupFirst") {
    var x = 0
    Arrow.resolve(1).groupFirst(Arrow((a) => {
      x += 1
      Future(Right(2))
    }))
      .runAsFuture(null)
      .map {
        case Right(b) => {
          assert(x == 1)
          assert(b == 1)
        }
    }
  }

  test("Arrow should groupSecond") {
    var x = 0
    Arrow((_) => {
      x += 1
      Future(Right(2))
    }).groupSecond(Arrow.resolve(1))
      .runAsFuture(null)
      .map {
        case Right(b) => {
          assert(x == 1)
          assert(b == 1)
        }
    }
  }

  test("Arrow should andThen") {
    Arrow.resolve[Int](2)
      .andThen[Nothing, Int](Arrow((a: Int) => Future(Right(a + 1))))
      .runAsFuture(null)
      .map {
        case Right(b) => {
          assert(b == 3)
        }
    }
  }

  test("Arrow should orElse") {
    Arrow.reject(2)
      .orElse(Arrow.resolve(3))
      .runAsFuture(null)
      .map {
        case Right(b) => {
          assert(b == 3)
        }
    }
  }

  test("Arrow should bracket") {
    var flag = false
    val a = Arrow.resolve(3)
      .bracket(
        (b) => {
          assert(flag == false)
          flag = true
          Arrow.resolve(null)
        }, (c) => {
          assert(flag == false)
          Arrow.resolve(10)
        })
    a
      .map(b => b + 1)
      .runAsFuture(null)
      .map {
        case Right(a) => {
          assert(a == 11)
          assert(flag == true)
        }
      }
    }

  test("Arrow should bracket - fail") {
    var flag = false
    val a = Arrow.resolve(3)
      .bracket(
        (b) => {
          assert(flag == false)
          flag = true
          Arrow.resolve(null)
        }, (c) => {
          assert(flag == false)
          Arrow.reject(10)
        })
    a
      .runAsFuture(null)
      .map {
        case Left(a) => {
          assert(a == 10)
          assert(flag == true)
        }
      }
    }

  test("Arrow should run - success") {
    var x: Any = 1
    Arrow.resolve(5)
      .run(
        null,
        a => {
          x = a + 1
        },
        a => {
          x = a
        },
        a => {
          x = a
        }
      )
    Thread.sleep(100)
    assert(x == 6)
  }

  test("Arrow should run - error") {
    var x: Any = 1
    Arrow.reject(5)
      .run(
        null,
        a => {
          x = a
        },
        a => {
          x = a + 1
        },
        a => {
          x = a
        }
      )
    Thread.sleep(100)
    assert(x == 6)
  }

  test("Arrow should run - exception") {
    var x: Any = 1
    Arrow((_) => {
      throw new Exception("hello")
      Future(Right(5))
    })
      .run(
        null,
        a => {
          x = a
        },
        a => {
          x = 6
        },
        a => {
          x = 7
        }
      )
    Thread.sleep(100)
    assert(x == 7)
  }

  test("Arrow should run - context") {
    val p = Promise[Any]()
    Arrow((a: Int) => {
      Future(Right(a))
    })
      .run(
        10,
        a => {
          println("z2")
          println(a)
          p.success(a)
        },
        a => { },
        a => { }
      )
    p.future.map(a => assert(a == 10))
  }

  test("Arrow should run") {
    var x: Any = 1
    val cancel = Arrow((a: Int) => {
      Thread.sleep(100)
      Future(Right(a))
    })
      .run(
        10,
        a => {
          println("should be run")
          x = a
        },
        a => { },
        a => { }
      )
    sleep(2000)(assert(x == 10))
  }

  test("Arrow should run and cancel") {
    var x: Any = 1
    val cancel = Arrow((a: Int) => {
      Thread.sleep(100)
      Future(Right(a))
    })
      .run(
        10,
        a => {
          x = a
        },
        a => { },
        a => { }
      )
    cancel()
    sleep(2000)(assert(x == 1))
  }

  test("Arrow should runAsFuture") {
    var x: Any = 1
    val a = Arrow((a: Int) => {
      Thread.sleep(100)
      Future(Right(a))
    })
      .runAsFuture(
        10
      )
      .onComplete {
        case Success(_) => {
          x = 10
        }
        case Failure(_) => {
          x = 10
        }
      }
    sleep(2000)(assert(x == 10))
  }

}