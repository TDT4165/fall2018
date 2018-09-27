import scala.concurrent.duration._
import org.scalatest.FunSuite
import akka.actor._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout

import scala.util.Random

object TestHelper {

  def createBank(bankId: String): (ActorRef, Bank) = {
    val bankRef: ActorRef = BankManager.createBank(bankId)
    implicit val timeout = Timeout(5 seconds)
    val bank = Await.result(ask(bankRef, IdentifyActor).mapTo[Bank], 10 seconds)

    (bankRef, bank)
  }

  def createBankAccount(bankId: String, amount: Double): (ActorRef, Account) = {
    val bank: ActorRef = BankManager.findBank(bankId)
    implicit val timeout = Timeout(5 seconds)
    val accountRef = Await.result(ask(bank, CreateAccountRequest(amount)).mapTo[ActorRef], 10 seconds)
    val account = Await.result(ask(accountRef, IdentifyActor).mapTo[Account], 10 seconds)

    (accountRef, account)
  }

  def waitUntilAllTransactionsAreCompleted(accounts: List[Account]): Unit = {
    var completed = false
    while (!completed) {
      Thread.sleep(500)
      var completedNow = true
      accounts.foreach(a => {
        completedNow = completedNow && a.allTransactionsCompleted
      })
      completed = completedNow
    }
  }
}


class Test01 extends FunSuite {

  test("Add new bank") {
    val bankRef: ActorRef = BankManager.createBank("2001")
    implicit val timeout = Timeout(5 seconds)
    val bank: Bank = Await.result(ask(bankRef, IdentifyActor).mapTo[Bank], 10 seconds)
    assert(bank.bankId == "2001")
  }

}


class Test02 extends FunSuite {

  test("Add new bank account") {
    val bank: ActorRef = BankManager.createBank("2002")
    val (accountRef, account) = TestHelper.createBankAccount("2002", 1000)
    assert(account.accountId == "1001" && account.getBalanceAmount == 1000)
  }

}

class Test03 extends FunSuite {

  test("Valid transaction within same bank, accounts should have correct balance.") {
    val bank: ActorRef = BankManager.createBank("2003")
    val (accountRef1, account1) = TestHelper.createBankAccount("2003", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("2003", 1000)

    implicit val timeout = Timeout(5 seconds)

    account1.transferTo(account2.accountId, 200)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2))
    assert(account1.getBalanceAmount == 800 && account2.getBalanceAmount == 1200)
  }
}

class Test04 extends FunSuite {

  test("Valid transaction between two different banks, accounts should have correct balance.") {
    val (bank1Ref, bank1): (ActorRef, Bank) = TestHelper.createBank("2010")
    val (bank2Ref, bank2): (ActorRef, Bank) = TestHelper.createBank("2011")

    val (accountRef1, account1) = TestHelper.createBankAccount("2010", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("2011", 1000)

    implicit val timeout = Timeout(5 seconds)

    account1.transferTo(account2.getFullAddress, 200)
    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2))
    assert(account1.getBalanceAmount == 800 && account2.getBalanceAmount == 1200)

  }
}


class Test05 extends FunSuite {

  test("Valid transaction between two different banks, sender transaction list should have the correct status information.") {
    val (bank1Ref, bank1): (ActorRef, Bank) = TestHelper.createBank("2015")
    val (bank2Ref, bank2): (ActorRef, Bank) = TestHelper.createBank("2016")

    val (accountRef1, account1) = TestHelper.createBankAccount("2015", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("2016", 1000)

    implicit val timeout = Timeout(5 seconds)

    account1.transferTo(account2.getFullAddress, 200)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2))

    account1.getTransactions.foreach(t => {
      assert(t.isCompleted)
      assert(t.isSuccessful)
    })

  }
}

class Test06 extends FunSuite {

  test("Two valid transactions back and forth between two banks, account balances should be correct.") {
    val (bank1Ref, bank1): (ActorRef, Bank) = TestHelper.createBank("6000")
    val (bank2Ref, bank2): (ActorRef, Bank) = TestHelper.createBank("6001")

    val (accountRef1, account1) = TestHelper.createBankAccount("6000", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("6001", 1000)

    implicit val timeout = Timeout(5 seconds)

    val sendToAddress = bank2.bankId + account2.accountId

    account1.transferTo(account2.getFullAddress, 250)
    account2.transferTo(account1.getFullAddress, 200)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2))

    assert(account1.getBalanceAmount == 950 && account2.getBalanceAmount == 1050)

  }

}

class Test07 extends FunSuite {

  test("Four valid transactions back and forth between two banks, account balances should be correct.") {
    val (bank1Ref, bank1): (ActorRef, Bank) = TestHelper.createBank("7000")
    val (bank2Ref, bank2): (ActorRef, Bank) = TestHelper.createBank("7001")

    val (accountRef1, account1) = TestHelper.createBankAccount("7000", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("7001", 1000)

    implicit val timeout = Timeout(5 seconds)

    val sendToAddress = bank2.bankId + account2.accountId

    account1.transferTo(account2.getFullAddress, 250)
    account2.transferTo(account1.getFullAddress, 50)
    account1.transferTo(account2.getFullAddress, 100)
    account2.transferTo(account1.getFullAddress, 150)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2))

    assert(account1.getBalanceAmount == 850 && account2.getBalanceAmount == 1150)

  }

}


class Test08 extends FunSuite {

  test("Several transactions between two banks and three accounts, all transaction lists should hold correct status information, and account balances should be correct.") {
    val (bank1Ref, bank1): (ActorRef, Bank) = TestHelper.createBank("8000")
    val (bank2Ref, bank2): (ActorRef, Bank) = TestHelper.createBank("8001")

    val (accountRef1, account1) = TestHelper.createBankAccount("8000", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("8001", 1000)
    val (accountRef3, account3) = TestHelper.createBankAccount("8001", 1000)

    implicit val timeout = Timeout(5 seconds)

    account1.transferTo(account2.getFullAddress, 250)
    account1.transferTo(account2.getFullAddress, 150)
    account2.transferTo(account1.getFullAddress, 130)
    account2.transferTo(account1.getFullAddress, 150)
    account3.transferTo(account1.getFullAddress, 100)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2, account3))

    account1.getTransactions.foreach(t => {
      assert(t.isSuccessful)
    })

    account2.getTransactions.foreach(t => {
      assert(t.isSuccessful)
    })

    account3.getTransactions.foreach(t => {
      assert(t.isSuccessful)
    })

    assert(account1.getBalanceAmount == 980
      && account2.getBalanceAmount == 1120
      && account3.getBalanceAmount == 900)

  }

}

class Test09 extends FunSuite {

  test("Failed transactions should lead to correct status information in all transaction lists, and no balances should be affected.") {
    val bank: ActorRef = BankManager.createBank("9000")
    val (accountRef1, account1) = TestHelper.createBankAccount("9000", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("9000", 1000)

    implicit val timeout = Timeout(5 seconds)

    account1.transferTo(account2.accountId, 2000)
    account2.transferTo(account2.accountId, 2000)
    account2.transferTo(account1.accountId, -2000)
    account2.transferTo(account1.accountId, 2000)
    account1.transferTo(account2.accountId, -400)
    account1.transferTo(account2.accountId, -300)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2))

    account1.getTransactions.foreach(t => {
      assert(t.isCompleted)
      assert(!t.isSuccessful)
    })

    account2.getTransactions.foreach(t => {
      assert(t.isCompleted)
      assert(!t.isSuccessful)
    })

    assert(account1.getBalanceAmount == 1000)
    assert(account2.getBalanceAmount == 1000)

  }

}


class Test10 extends FunSuite {

  test("Valid transactions within one bank, transaction list should have correct status information.") {
    val bank: ActorRef = BankManager.createBank("1000")
    val (accountRef1, account1) = TestHelper.createBankAccount("1000", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("1000", 1000)

    implicit val timeout = Timeout(5 seconds)

    account1.transferTo(account2.accountId, 200)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2))

    account1.getTransactions.foreach(t => {
      assert(t.isCompleted)
      assert(t.isSuccessful)
    })

    account2.getTransactions.foreach(t => {
      assert(t.isCompleted)
      assert(t.isSuccessful)
    })

  }

}


class Test11 extends FunSuite {

  test("Invalid transaction within one bank, transaction lists should have correct status information.") {
    val bank: ActorRef = BankManager.createBank("1100")
    val (accountRef1, account1) = TestHelper.createBankAccount("1100", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("1100", 1000)

    implicit val timeout = Timeout(5 seconds)

    account1.transferTo(account2.accountId, 1500)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2))

    account1.getTransactions.foreach(t => {
      assert(t.isCompleted)
      assert(!t.isSuccessful)
    })

  }

}


class Test12 extends FunSuite {

  test("Invalid transactions within one bank, account balances should not be affected.") {
    val bank: ActorRef = BankManager.createBank("1200")
    val (accountRef1, account1) = TestHelper.createBankAccount("1200", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("1200", 1000)

    implicit val timeout = Timeout(5 seconds)

    account1.transferTo(account2.accountId, 1500)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2))

    assert(account1.getBalanceAmount == 1000)
    assert(account2.getBalanceAmount == 1000)

  }

}


class Test13 extends FunSuite {

  test("Create and find two different banks.") {
    BankManager.createBank("1301")
    BankManager.createBank("1302")

    implicit val timeout = Timeout(5 seconds)

    val bank1: Bank = Await.result(ask(BankManager.findBank("1301"), IdentifyActor).mapTo[Bank], 10 seconds)
    val bank2: Bank = Await.result(ask(BankManager.findBank("1302"), IdentifyActor).mapTo[Bank], 10 seconds)

    assert(bank1.bankId.equals("1301") && bank2.bankId.equals("1302"))

  }
}

class Test14 extends FunSuite {

  test("Valid transactions between two banks using full account address, account balances should be correct. Also, sending a BalanceRequest to an account should yield the correct balance.") {
    val bank1: ActorRef = BankManager.createBank("1400")
    val bank2: ActorRef = BankManager.createBank("1401")

    val (accountRef1, account1) = TestHelper.createBankAccount("1400", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("1401", 1000)
    val (accountRef3, account3) = TestHelper.createBankAccount("1401", 1000)

    implicit val timeout = Timeout(30 seconds)

    account1.transferTo(account2.getFullAddress, 100)
    account2.transferTo(account3.getFullAddress, 4)
    account3.transferTo(account2.getFullAddress, 100)
    account3.transferTo(account1.getFullAddress, 8)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2))

    val acc1Balance: Double = Await.result(ask(accountRef1, BalanceRequest).mapTo[Double], 10 seconds)
    val acc2Balance: Double = Await.result(ask(accountRef2, BalanceRequest).mapTo[Double], 10 seconds)
    val acc3Balance: Double = Await.result(ask(accountRef3, BalanceRequest).mapTo[Double], 10 seconds)

    assert(acc1Balance + acc2Balance + acc3Balance == 3000)

    assert(acc1Balance == 908 && acc1Balance == account1.getBalanceAmount)
    assert(acc2Balance == 1196 && acc2Balance == account2.getBalanceAmount)
    assert(acc3Balance == 896 && acc2Balance == account2.getBalanceAmount)

  }

}

class Test15 extends FunSuite {

  test("Several valid transactions between several accounts in several banks. All information should be correct.") {

    var accounts = List[(ActorRef, Account)]()
    val numberOfBanks = 4
    val numberOfAccountsPerBank = 4

    for (bank <- 1 to numberOfBanks) {
      val b: ActorRef = BankManager.createBank(s"150$bank")
      for (account <- 1 to numberOfAccountsPerBank) {
        val a: (ActorRef, Account) = TestHelper.createBankAccount(s"150$bank", 1000)
        accounts = a :: accounts
      }
    }

    for (x <- 1 until 10) {
      val randomBankId = s"150${Random.nextInt(numberOfBanks) + 1}"
      val randomAccountId = s"100${Random.nextInt(numberOfAccountsPerBank) + 1}"
      val randomAmount = Random.nextInt(1000)
      accounts(Random.nextInt(15))._2.transferTo(s"$randomBankId$randomAccountId", randomAmount)
    }

    val accountsList = accounts.map((acc: (ActorRef, Account)) => acc._2)
    TestHelper.waitUntilAllTransactionsAreCompleted(accountsList)

    val balances = accountsList.map((acc: Account) => acc.getBalanceAmount)

    assert(balances.sum == 16000)

    var notAllBalancesIs1000 = false

    balances foreach ((i: Double) => {
      if (i != 1000) {
        notAllBalancesIs1000 = true
      }
    })

    assert(notAllBalancesIs1000)

  }

}

class Test16 extends FunSuite {

  test("Transaction to a non-existing bank should fail, and account balance should not be affected and transaction list should hold correct status information.") {
    val bank1: ActorRef = BankManager.createBank("1600")

    val (accountRef1, account1) = TestHelper.createBankAccount("1600", 1000)

    account1.transferTo("99998888", 200)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1))

    account1.getTransactions.foreach(t => {
      assert(t.isCompleted)
      assert(!t.isSuccessful)
    })

    assert(account1.getBalanceAmount == 1000)

  }
}


class Test17 extends FunSuite {

  test("Transaction to a non-existing account should fail, account balance should not be affected and transaction list should hold correct status information.") {
    val bank1: ActorRef = BankManager.createBank("1700")

    val (accountRef1, account1) = TestHelper.createBankAccount("1700", 1000)

    account1.transferTo("9999", 200)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1))

    account1.getTransactions.foreach(t => {
      assert(t.isCompleted)
      assert(!t.isSuccessful)
    })

    assert(account1.getBalanceAmount == 1000)

  }
}