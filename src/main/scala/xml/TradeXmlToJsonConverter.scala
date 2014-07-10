package xml
import scala.xml.NodeSeq
import org.json4s.{ JArray, JObject, JField}

class TradeXmlToJsonConverter {

  val arrayLabels = Set("leg", "cashflow")

  def mapTradeXmlToJson(xml: NodeSeq) = {
    val json = XmlConverter.toJson(xml)

    json mapField  {
      case JField(s, x: JObject) if arrayLabels.contains(s) => JField(s, JArray(x :: Nil))
      case x => x
    }

  }
}
