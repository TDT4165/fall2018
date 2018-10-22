import org.scalatest.FunSuite
import exceptions._

class AccountTests extends FunSuite {

  val bank = new Bank()

  test("Test 01: Valid account withdrawal") {
    val acc = new Account(bank, 500)
    acc.withdraw(250)
    assert(acc.getBalanceAmount == 250)
  }

  test("Test 02: Invalid account withdrawal should throw exception") {
    val acc = new Account(bank, 500)
    intercept[NoSufficientFundsException] {
		  acc.withdraw(750)
		}
  }

  test("Test 03: Withdrawal of negative amount should throw exception") {
    val acc = new Account(bank, 500)
    intercept[IllegalAmountException] {
		  acc.withdraw(-100)
		}
  }

  test("Test 04: Valid account deposit") {
    val acc = new Account(bank, 500)
    acc.deposit(250)
    assert(acc.getBalanceAmount == 750)
  }

  test("Test 05: Deposit of negative amount should throw exception") {
    val acc = new Account(bank, 500)
    intercept[IllegalAmountException] {
		  acc.deposit(-50)
		}
  }

  test("Test 06: Correct balance amount after several withdrawals and deposits") {
    val acc = new Account(bank, 50000)
    val first = Main.thread {
      for (i <- 0 until 100) {
        acc.withdraw(10); Thread.sleep(10)
      }
    }
    val second = Main.thread {
      for (i <- 0 until 100) {
        acc.deposit(5); Thread.sleep(20)
      }
    }
    val third = Main.thread {
      for (i <- 0 until 100) {
        acc.withdraw(50); Thread.sleep(10)
      }
    }
    val fourth = Main.thread {
      for (i <- 0 until 100) {
        acc.deposit(100); Thread.sleep(10)
      }
    }
    first.join()
    second.join()
    third.join()
    fourth.join()
    assert(acc.getBalanceAmount == 54500)
  }

  test("Test 07: Account IDs are unique") {
    val firstId = bank.generateAccountId
    val threads = (1 until 10000).map(_ => Main.thread {
      bank generateAccountId
    })
    for (t <- threads) { t.join }
    val nextId = bank.generateAccountId
    assert((nextId - 10000) == firstId)
  }


}

class AccountTransferTests extends FunSuite {


  test("Test 08: Valid transfer between accounts") {
    val bank = new Bank()

    val acc1 = bank.addAccount(100)
    val acc2 = bank.addAccount(200)

    acc1 transferTo(acc2, 50)

    while (bank.getProcessedTransactionsAsList.size != 1) {
      Thread.sleep(100)
    }

    assert(bank.getProcessedTransactionsAsList.last.status == TransactionStatus.SUCCESS)
    assert((acc1.getBalanceAmount == 50) && (acc2.getBalanceAmount == 250))
  }

  test("Test 09: Transfer of negative amount between accounts should fail") {
    val bank = new Bank()

    val acc1 = bank.addAccount(500)
    val acc2 = bank.addAccount(1000)

    acc1 transferTo(acc2, -100)

    while (bank.getProcessedTransactionsAsList.size != 1) {
      Thread.sleep(100)
    }

    assert(bank.getProcessedTransactionsAsList.last.status == TransactionStatus.FAILED)
    assert((acc1.getBalanceAmount == 500) && (acc2.getBalanceAmount == 1000))
  }


  test("Test 10: Invalid transfer between accounts due to insufficient funds should lead to transaction status FAILED and no money should be transferred between accounts") {
    val bank = new Bank()
    val acc1 = new Account(bank, 100)
    val acc2 = new Account(bank, 1000)

    acc1 transferTo(acc2, 150)

    while (bank.getProcessedTransactionsAsList.size != 1) {
      Thread.sleep(100)
    }

    assert(bank.getProcessedTransactionsAsList.last.status == TransactionStatus.FAILED)
    assert((acc1.getBalanceAmount == 100) && (acc2.getBalanceAmount == 1000))

  }


  test("Test 11: Correct balance amounts after several transfers") {
    val bank = new Bank()

    val acc1 = new Account(bank, 3000)
    val acc2 = new Account(bank, 5000)
    val first = Main.thread {
      for (i <- 0 until 100) {
        bank addTransactionToQueue(acc1, acc2, 30)
      }
    }
    val second = Main.thread {
      for (i <- 0 until 100) {
        bank addTransactionToQueue(acc2, acc1, 23)
      }
    }
    first.join()
    second.join()

    while (bank.getProcessedTransactionsAsList.size != 200) {
      Thread.sleep(100)
    }

    assert((acc1.getBalanceAmount == 2300) && (acc2.getBalanceAmount == 5700))

  }

  test("Test 12: Failed transactions should retry and potentially succeed with multiple allowed attempts") {
    var failed = 0
    for (x <- 1 to 100) {
      val bank = new Bank(allowedAttempts = 3)
  
      val acc1 = new Account(bank, 100)
      val acc2 = new Account(bank, 100)
      val acc3 = new Account(bank, 100)
  
      for (i <- 1 to 6) { acc1 transferTo (acc2, 50) }
      for (j <- 1 to 2) { acc3 transferTo (acc1, 50) }
      
      while (bank.getProcessedTransactionsAsList.size != 8) {
        Thread.sleep(100)
      }
  
      if (!(acc1.getBalanceAmount == 0
        && acc2.getBalanceAmount == 300
        && acc3.getBalanceAmount == 0)) failed += 1
    }
    assert(failed <= 5)

  }

  test("Test 13: Some transactions should be stopped with only one allowed attempt") {
    var failed = 0
    for (x <- 1 to 100) {
      val bank = new Bank(allowedAttempts = 1)
  
      val acc1 = new Account(bank, 100)
      val acc2 = new Account(bank, 100)
      val acc3 = new Account(bank, 100)
  
      for (i <- 1 to 6) { acc1 transferTo (acc2, 50) }
      for (j <- 1 to 2) { acc3 transferTo (acc1, 50) }
  
      while (bank.getProcessedTransactionsAsList.size != 8) {
        Thread.sleep(100)
      }

      if (!(acc2.getBalanceAmount != 300 && acc3.getBalanceAmount == 0)) failed += 1
    }
    assert(failed <= 5)
  }

}
