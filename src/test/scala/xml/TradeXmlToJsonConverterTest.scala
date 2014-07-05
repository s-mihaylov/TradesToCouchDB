package xml

import scala.xml.XML
import java.io.File
import org.json4s.native.JsonMethods._
import json.Trade
import org.json4s.JsonAST.{JArray, JString, JValue}
import dispatch.CouchDbRequestDispatcher


object TradeXmlToJsonConverterTest {
  val xml = XML.loadFile(new File("src/test/resources/xml/TradeExample.xml"))
  val json = new TradeXmlToJsonConverter().mapTradeXmlToJson(xml)

  private val tradeId : String = (json \\ "tradeId").apply(0).asInstanceOf[JString].s //LOL

  val trade = new Trade(tradeId, json)

  val requestDispatcher = new CouchDbRequestDispatcher()

  def main(args: Array[String]){
    println("Trade Id is " + trade.tradeId)
    println()
    println(pretty(render(json)))
    assert(compact(render(json)).equals(
    """{"trade":{"nominal":"3319.00","tradeDate":"20140120","tradeId":"9288DA","leg":[{"legId":"IAKIJ","cashflow":[{"startDate":"20140121","type":"INT","amount":"2","endDate":"20140421"},{"startDate":"20140421","type":"INT","amount":"1","endDate":"20140721"}]},{"legId":"UAHNNN","cashflow":[{"type":"XNL","amount":"2000","pOrS":"P","aDate":"20140120"}]}]}}"""
    ))

//    requestDispatcher.deleteTrade("gi_test1", tradeId)
    requestDispatcher.postTrade("gi_test1", trade)
  }
}

