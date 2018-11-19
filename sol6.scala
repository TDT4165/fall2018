import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Hello extends App {
	
	// 1a
	println("Hello World")

	// 1b
	var generated: Array[Int] = Array()
	for (i <- 1 to 50) generated :+= i

	// 1c
	(51 to 100).map((x: Int) => generated :+= x)
	println("generated:" + generated.mkString(" "))

	// 1d
	def sum_for_loop(input: Array[Int]): Int = {
		var sum: Int = 0
		for (i <- input) sum += i
		sum
	}

	// 1e
	def sum_recursive(input: Array[Int]): Int = {
		if (input.isEmpty) {
			0
		} else {
			input{0} + sum_recursive(input.drop(1))
		}
	}

	// 1f
	def nth_fibonacci(n: BigInt): BigInt = {
		if (n == 0 || n == 1) {
			n
		} else {
			nth_fibonacci(n - 1) + nth_fibonacci(n - 2)
		}
	}
	// Question: bigint vs int difference
	// They take up different amounts of space and they have different ranges of acceptable values. (int has 4 bytes, bigint 8)
	// The bigint data type is intended for use when integer values might exceed the range that is supported by the int data type.
	
	// Test: (43 to 45).map((i) => println(nth_fibonacci(i)))



	/**************************************************** TASK 2 ****************************************************/
	// 2a
	def my_func(f: () => BigInt, b: Boolean) = {
		lazy val t = f()
		// println("Inside lazy execution")
		if (b) println(t)
	}
	/** Example of call: my_func(() => nth_fibonacci(30), false)

		Arguments: a lambda () => BigInt, and a boolean
		t: a lazy immutable val. It is evaluated only once on the initial access, where the result is stored and 
		returned immediately on subsequent access.
	**/

	// 2b
	def my_func2(f: () => BigInt, b: Boolean) = {
		val t = f()
		// Check difference using: println("Inside nonlazy execution")
		if (b) println(t)
	}
	// Difference: a val is executed when it is defined whereas a lazy val is executed when it is accessed the first time.

	// 2c
	// Question: When is lazy eval useful?
	// Save memory. Infinite lists. Streams. 


	/**************************************************** TASK 3 ****************************************************/
	// 3a
	def thread(f: () => Unit): Thread = {
		// This creates a thread by overriding a class method with another implementation. The thread is not yet started.
		new Thread {
			override def run {
				f()
			}
		}
	}

	// 3b
	// Note the return type being Array[() => Unit]. This means that we're returning an array of functions that return Unit.
	def fib_lambdas(n: BigInt): Array[() => Unit] = {
		if (n <= 0) {
			Array(() => println(nth_fibonacci(0)))
		} else {
			fib_lambdas(n - 1) ++ Array(() => println(nth_fibonacci(n)))
		}
	}

	// 3c
	val fibs = 33
	val threads = fib_lambdas(fibs).map((x) => thread(x))

	// 3d
	threads.map((x) => x.start)
	threads.map((x) => x.join)

	// Or more compactly
	fib_lambdas(fibs).map((x) => thread(x).start)
	// Or, if you use chaining
	fib_lambdas(fibs).map((x) => thread(x)).map((x) => x.start)
	
    // 3e
    private val counter: AtomicInteger = new AtomicInteger
    def increaseCounter: Integer = {
		counter.incrementAndGet
	}

    // 3f
    /** A deadlock is a state in which each member of a group is waiting for some other member to take action, typically when
		sending messages or releasing a lock.
		To prevent deadlock, we can eliminate any of the deadlock conditions: Mutual Exclusion. Hold and Wait. No preemption. 
		Circular wait. 
	**/
	object A {
		lazy val base = 42
		lazy val start = B.step   // (1) A tries to access B
	}

	object B {
		lazy val step = A.base    // (2) B tries to access a lazy val in A
	}

	object DeadlockScenario {
		def run = {
			val result = Future.sequence(Seq(
			Future { A.start },                        // (1) 
			Future { B.step }                          // (2) 
			))
			Await.result(result, 1.minute)
		}
	}


} 