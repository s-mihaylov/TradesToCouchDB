package xml

import scala.xml._
import org.json4s._

/*
the class is taken from json4s Xml.toJson with 2 changes:
 - xml elements on the same level are grouped by name. Each group with 2 or more elements is converted to array
 - attributes don't exist
 */
object XmlConverter {

  def toJson(xml: NodeSeq): JValue = {

    def isEmpty(node: Node) = node.child.isEmpty

    def isLeaf(node: Node) = {
      def descendant(n: Node): List[Node] = n.child.toList.flatMap { x => x :: descendant(x) }
      !descendant(node).exists(_.isInstanceOf[Elem])
    }

    def isArray(nodeNames: Seq[String]) = nodeNames.size != 1 && nodeNames.toList.distinct.size == 1
    def directChildren(n: Node): NodeSeq = n.child.filter(c => c.isInstanceOf[Elem])
    def nameOf(n: Node) = (if (n.prefix ne null) n.prefix + ":" else "") + n.label

    sealed trait XElem
    case class XValue(value: String) extends XElem
    case class XLeaf(value: (String, XElem)) extends XElem
    case class XNode(fields: List[(String, XElem)]) extends XElem
    case class XArray(elems: List[XElem]) extends XElem

    def toJValue(x: XElem): JValue = x match {
      case XValue(s) => JString(s)
      case XLeaf((name, value)) => JObject((name, toJValue(value)))
      case XNode(xs) => JObject(mkFields(xs))
      case XArray(elems) => JArray(elems.map(toJValue))
    }

    def mkFields(xs: List[(String, XElem)]) =
      xs.flatMap { case (name, value) => (value, toJValue(value)) match {
        // This special case is needed to flatten nested objects which resulted from
        // XML attributes. Flattening keeps transformation more predictable.
        // <a><foo id="1">x</foo></a> -> {"a":{"foo":{"foo":"x","id":"1"}}} vs
        // <a><foo id="1">x</foo></a> -> {"a":{"foo":"x","id":"1"}}
        case (XLeaf(v), o: JObject) => o.obj
        case (_, json) => JField(name, json) :: Nil }}


    def buildNodes(xml: NodeSeq): List[XElem] = xml match {
      case n: Node =>
        if (isEmpty(n)) XLeaf((nameOf(n), XValue(""))) :: Nil
        else if (isLeaf(n)) XLeaf((nameOf(n), XValue(n.text))) :: Nil
        else XNode(directChildren(n).map(nameOf).toList.zip(buildNodes(directChildren(n)))) :: Nil

      case nodes: NodeSeq =>
        val allLabels = nodes.map(_.label)
        if (isArray(allLabels)) {
          val arr = XArray(nodes.toList.flatMap { n =>
            if (isLeaf(n)) XValue(n.text) :: Nil
            else buildNodes(n)
          })
          XLeaf((allLabels(0), arr)) :: Nil

        } else {

          nodes.groupBy(x => x.label).map {
            case a if a._2.size > 1 => buildNodes(a._2)
            case b => buildNodes(b._2.head)
            }.toList.flatten
        }
    }

    buildNodes(xml) match {
      case List(x @ XLeaf(_)) => toJValue(x)
      case List(x) => JObject(JField(nameOf(xml.head), toJValue(x)) :: Nil)
      case x => JArray(x.map(toJValue))
    }
  }

}
