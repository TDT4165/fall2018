object TransactionStatus extends Enumeration {
    val SUCCESS, PENDING, FAILED = Value
}

class Transaction(val from: String,
                  val to: String,
                  val amount: Double,
                  var status: TransactionStatus.Value = TransactionStatus.PENDING,
                  val id: String = java.util.UUID.randomUUID.toString,
                  var receiptReceived: Boolean = false) {

    def isCompleted: Boolean = {
        this.status != TransactionStatus.PENDING
    }

    def isSuccessful: Boolean = {
        isCompleted && this.status == TransactionStatus.SUCCESS
    }

}
