package dispatch

import dispatch._, Defaults._
import json.Trade
import scala.util.{Success, Failure}
import org.json4s.native.JsonMethods._

class CouchDbRequestDispatcher {

  def couchDbReq: Req = {
    url(CouchDbProperties.address)
  }

  def getDocument(databaseId: String, documentId: String){
    val req = couchDbReq / databaseId / documentId
    simpleExecute(req)
  }
  
  def postTrade(databaseId: String, trade: Trade){
    val req = couchDbReq.PUT / databaseId / trade. tradeId << compact(render(trade.tradeJson))
    val jsonReq = req.setContentType("application/json", "UTF-8")

    simpleExecute(jsonReq)
  }

  def updateTrade(databaseId: String, trade: Trade){
    val req = couchDbReq.POST / databaseId / trade.tradeId << compact(render(trade.tradeJson))
    val jsonReq = req.setContentType("application/json", "UTF-8")

    simpleExecute(jsonReq)
  }
  
  def deleteTrade(databaseId: String, tradeId: String){
    val req = couchDbReq.DELETE / databaseId / tradeId
    simpleExecute(req)
  }

  def simpleExecute(req: Req) {
    val respFuture = Http(req OK as.String)

    respFuture.onComplete{
      case Success(txt) => println(txt)
      case Failure(f) => throw new RuntimeException("error: " + f.getMessage)
    }
  }
}
