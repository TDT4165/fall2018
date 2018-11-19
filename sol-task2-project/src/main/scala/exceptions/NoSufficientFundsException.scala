package exceptions

class NoSufficientFundsException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause) {
  
}