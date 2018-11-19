import scala.collection.mutable
import akka.actor._

object BankManager {

  private val actorSystem = ActorSystem("BankManager")

  private val banks = new mutable.HashMap[String, ActorRef] with mutable.SynchronizedMap[String, ActorRef]
  private val accounts = new mutable.HashMap[String, ActorRef] with mutable.SynchronizedMap[String, ActorRef]

  def createBank(bankId: String): ActorRef = {
    val name = s"bank$bankId"
    banks += (name -> actorSystem.actorOf(Props(classOf[Bank], bankId), name = name))
    banks(name)
  }

  def findBank(bankId: String): ActorRef = {
    banks(s"bank$bankId")
  }

  def createAccount(accountId: String, bankId: String, initialBalance: Double): ActorRef = {
    val name = s"account$bankId$accountId"
    accounts += (name ->
      actorSystem.actorOf(Props(classOf[Account], accountId, bankId, initialBalance), name = name))
    accounts(name)
  }

  def findAccount(bankId: String, accountId: String): ActorRef = {
    val name = s"account$bankId$accountId"
    accounts(name)
  }

}
