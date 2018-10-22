import scala.concurrent.forkjoin.ForkJoinPool
import java.util.concurrent.{Executors, ExecutorService}
import java.util.concurrent.atomic.AtomicLong

class Bank(val allowedAttempts: Integer = 3) {

	private val uid: AtomicLong = new AtomicLong
	private val transactionsQueue: TransactionQueue = new TransactionQueue()
	private val processedTransactions: TransactionQueue = new TransactionQueue()
	private val executorContext = Executors newFixedThreadPool(1000)

	def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
		val t = new Transaction(
			transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
		transactionsQueue push(t)
		executorContext submit(new Runnable {
			def run() {
				processTransactions
			}
		})
	}

	def generateAccountId: Long = {
		uid.incrementAndGet
	}

	private def processTransactions: Unit = {
		val t: Transaction = transactionsQueue.pop
		executorContext submit(t)
		Thread sleep(80)
		if (t.synchronized { t.status == TransactionStatus.PENDING }) {
			transactionsQueue.push(t)
			// Thread sleep(50)
			executorContext submit(new Runnable {
				def run() {
					processTransactions
				}
			})
		}
		else
			processedTransactions push(t)
	}

	def addAccount(initialBalance: Double): Account = {
		new Account(this, initialBalance)
	}

	def getProcessedTransactionsAsList: List[Transaction] = {
		processedTransactions.iterator.toList
	}

}
