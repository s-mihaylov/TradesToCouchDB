package xml
import scala.xml.NodeSeq
import org.json4s.{ JArray, JObject, JField}

class TradeXmlToJsonConverter {

  def mapTradeXmlToJson(xml: NodeSeq) = {
    val json = XmlConverter.toJson(xml)

    json mapField  {
      case JField("leg", x: JObject)  => JField("leg", JArray(x :: Nil))
      case JField("cashflow", x: JObject)  => JField("cashflow", JArray(x :: Nil))
      case x => x
    }

  }
}
