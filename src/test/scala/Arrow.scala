
import org.scalatest.funsuite._
import lightarrow.Arrow
import scala.concurrent.ExecutionContext.Implicits.global
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

// it('Arrow should group - fail', async () => {
//   const { result, error } = await Arrow<{}, never, number>(async () => Right(1))
//     .group(Arrow<{}, number, never>(async () => Left(2)))
//     .runAsPromise({})
//   expect(result).toEqual(1)
//   expect(error).toEqual(2)
// })

// it('Arrow should group first', async () => {
//   const result = await Arrow<{}, never, number>(async () => Right(1))
//     .groupFirst(Arrow<{}, never, number>(async () => Right(2)))
//     .runAsPromiseResult({})
//   expect(result).toEqual(1)
// })

// it('Arrow should group second', async () => {
//   const result = await Arrow<{}, never, number>(async () => Right(1))
//     .groupSecond(Arrow<{}, never, number>(async () => Right(2)))
//     .runAsPromiseResult({})
//   expect(result).toEqual(2)
// })

// it('Arrow should group', async () => {
//   const result = await Arrow<{}, never, number>(async () => Right(1))
//     .group(Arrow<{}, never, number>(async () => Right(2)))
//     .runAsPromiseResult({})
//   expect(result).toEqual([1, 2])
// })

// it('Arrow should andThen', async () => {
//   const result = await Arrow<{}, never, number>(async () => Right(1))
//     .andThen(Arrow<number, never, number>(async (a) => Right(a + 2)))
//     .runAsPromiseResult({})
//   expect(result).toEqual(3)
// })

// it('Arrow should orElse', async () => {
//   const result = await Arrow<{}, number, never>(async () => Left(1))
//     .orElse(Arrow<{}, never, number>(async () => Right(2)))
//     .runAsPromiseResult({})
//   expect(result).toEqual(2)
// })

// it('Arrow should orElse', async () => {
//   const a = Arrow<{}, number, never>(async () => Left(1))
//     .orElse(Arrow<{}, number, never>(async () => Left(2)))

//   const result = await a.orElse(Arrow<{}, never, number>(async () => Right(2)))
//     .runAsPromiseResult({})
//   expect(result).toEqual(2)
// })

// it('Arrow should bracket', async () => {
//   let flag = false
//   const a = Arrow<{}, never, { ok: number }>(async () => Right({ ok: 123 }))
//     .bracket(
//       (b) => {
//         expect(flag).toEqual(false)
//         flag = true
//         return resolve(null)
//       }
//     )((c) => {
//       expect(flag).toEqual(false)
//       return resolve<number, {}>(10)
//     })
//   const { result, context } = await a
//     .runAsPromise({})
//   expect(flag).toEqual(true)
//   expect(result).toEqual(10)
// })

// it('Arrow should bracket - fail case', async () => {
//   let flag = false
//   const a = Arrow<{}, never, { ok: number }>(async () => Right({ ok: 123 }))
//     .bracket(
//       (b) => {
//         expect(flag).toEqual(false)
//         flag = true
//         return resolve(null)
//       }
//     )(
//       (c) => {
//         expect(flag).toEqual(false)
//         return reject(10)
//       }
//     )
//   const { result, error, context } = await a
//     .runAsPromise({})
//   expect(flag).toEqual(true)
//   expect(error).toEqual(10)
// })

// it('Arrow should run - success', async () => {
//   const a = Arrow<{ok:() => number }, never, number>(async (a) => Right(a.ok()))
//   const result = await a.run(
//     { ok: () => 2 },
//     result => {
//       expect(result).toEqual(2)
//     },
//     error => { },
//     failure => { }
//   )
// })

// it('Arrow should run - error', async () => {
//   const a = Arrow<{ok:() => number }, number, never>(async (a) => Left(a.ok()))
//   const result = a.run(
//     { ok: () => 2 },
//     result => { },
//     error => {
//       expect(error).toEqual(2)
//     },
//     failure => { }
//   )
// })

// it('Arrow should run - failure', async () => {
//   const a = Arrow<{ok:() => number }, number, never>(async (a) => { throw new Error('boom') })
//   const result = a.run(
//     { ok: () => 2 },
//     result => { },
//     error => { },
//     failure => {
//       expect(failure?.message).toEqual('boom')
//     }
//   )
// })

// it('Arrow should run - context', async () => {
//   const a = Arrow<{ok:() => number }, never, number>(async (a) => Right(a.ok()))
//   const result = a.run(
//     { ok: () => 2 },
//     result => {
//       expect(result).toEqual(2)
//     },
//     error => { },
//     failure => { },
//     context => {
//       expect(context.ok()).toEqual(2)
//     }
//   )
// })

// it('Arrow should run no cancel', async () => {
//   let res = 0
//   const a = Arrow<{ok:() => number }, never, number>(async (a) => {
//     await sleep(100)
//     return Right(a.ok())
//   })
//   const cancel = await a.run(
//     { ok: () => 2 },
//     result => {
//       res = result
//       expect(result).toEqual(2)
//     },
//     error => { }
//   )
//   await sleep(200)
//   expect(res).toEqual(2)
// })

// it('Arrow should run and cancel', async () => {
//   let res = 0
//   const a = Arrow<{ok:() => number }, never, number>(async (a) => {
//     await sleep(100)
//     res = 2
//     return Right(a.ok())
//   })
//   const cancel = await a.run(
//     { ok: () => 2 },
//     result => {
//       res = result
//     },
//     error => { }
//   )
//   cancel()
//   await sleep(200)
//   expect(res).toEqual(0)
// })


// it('Arrow should run as promise result - success', async () => {
//   const a = Arrow<{ok:() => number }, never, number>(async (a) => Right(a.ok()))
//   const result = await a.runAsPromiseResult({ ok: () => 2 })
//   expect(result).toEqual(2)
// })

}