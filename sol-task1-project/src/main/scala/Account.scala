import exceptions._


class Account(val bank: Bank, initialBalance: Double) {

	class Balance(var amount: Double) {}

	val balance = new Balance(initialBalance)
	val uid = bank.generateAccountId

	def withdraw(amount: Double): Unit = {
		balance.synchronized {
			if (balance.amount - amount < 0) throw new NoSufficientFundsException()
			if (amount <= 0) throw new IllegalAmountException()
			balance.amount -= amount
		}
	}

	def deposit(amount: Double): Unit = {
		balance.synchronized {
			if (amount <= 0) throw new IllegalAmountException()
			balance.amount += amount
		}
	}

	def getBalanceAmount: Double = balance.amount

	def transferTo(account: Account, amount: Double) = {
		bank addTransactionToQueue (this, account, amount)
	}

}
