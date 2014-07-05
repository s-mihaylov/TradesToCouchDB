import dispatch.CouchDbRequestDispatcher

object Main {

  def main(args: Array[String]) {
    val docRetriever = new CouchDbRequestDispatcher
    docRetriever.getDocument("gi_test1", "")
    docRetriever.getDocument("gi_test1", "9a6d38f8881e81f54070966d31000465")
  }

}