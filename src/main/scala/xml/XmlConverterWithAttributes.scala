package xml

import scala.xml._
import org.json4s._
import scala.xml.Group

object XmlConverterWithAttributes {

  def toJson(xml: NodeSeq): JValue = {

    def isEmpty(node: Node) = node.child.isEmpty

    /* Checks if given node is leaf element. For instance these are considered leafs:
     * <foo>bar</foo>, <foo>{ doSomething() }</foo>, etc.
     */
    def isLeaf(node: Node) = {
      def descendant(n: Node): List[Node] = n match {
        case g: Group => g.nodes.toList.flatMap(x => x :: descendant(x))
        case _ => n.child.toList.flatMap { x => x :: descendant(x) }
      }

      !descendant(node).exists(_.isInstanceOf[Elem])
    }

    def isArray(nodeNames: Seq[String]) = nodeNames.size != 1 && nodeNames.toList.distinct.size == 1
    def directChildren(n: Node): NodeSeq = n.child.filter(c => c.isInstanceOf[Elem])
    def nameOf(n: Node) = (if (n.prefix ne null) n.prefix + ":" else "") + n.label
    def buildAttrs(n: Node) = n.attributes.map((a: MetaData) => (a.key, XValue(a.value.text))).toList

    sealed trait XElem
    case class XValue(value: String) extends XElem
    case class XLeaf(value: (String, XElem), attrs: List[(String, XValue)]) extends XElem
    case class XNode(fields: List[(String, XElem)]) extends XElem
    case class XArray(elems: List[XElem]) extends XElem

    def toJValue(x: XElem): JValue = x match {
      case XValue(s) => JString(s)
      case XLeaf((name, value), attrs) => (value, attrs) match {
        case (_, Nil) => toJValue(value)
        case (XValue(""), xs) =>
          JObject(mkFields(xs))
        case (_, xs) =>
          JObject((name, toJValue(value)) :: mkFields(xs))
      }
      case XNode(xs) => JObject(mkFields(xs))
      case XArray(elems) => JArray(elems.map(toJValue))
    }

    def mkFields(xs: List[(String, XElem)]) =
      xs.flatMap { case (name, value) => (value, toJValue(value)) match {
        // This special case is needed to flatten nested objects which resulted from
        // XML attributes. Flattening keeps transformation more predictable.
        // <a><foo id="1">x</foo></a> -> {"a":{"foo":{"foo":"x","id":"1"}}} vs
        // <a><foo id="1">x</foo></a> -> {"a":{"foo":"x","id":"1"}}
        case (XLeaf(v, x :: xs), o: JObject) => o.obj
        case (_, json) => JField(name, json) :: Nil }}

    def aggregateToArrays(xml: NodeSeq) = {
      var groupedByName : Map[String, NodeSeq] = xml.groupBy(x => x.label)


    }

    def buildNodes(xml: NodeSeq): List[XElem] = xml match {
      case n: Node =>
        if (isEmpty(n)) XLeaf((nameOf(n), XValue("")), buildAttrs(n)) :: Nil
        else if (isLeaf(n)) XLeaf((nameOf(n), XValue(n.text)), buildAttrs(n)) :: Nil
        else {
          val children = directChildren(n)
          if(isArray(children.map(_.label))){
            buildNodes(children)
          } else {
            XNode(buildAttrs(n) ::: children.map(nameOf).toList.zip(buildNodes(children))) :: Nil
          }
        }
      case nodes: NodeSeq =>
        val allLabels = nodes.map(_.label)

        if (isArray(allLabels)) {
          val arr = XArray(nodes.toList.flatMap { n =>
            if (isLeaf(n) && n.attributes.length == 0) XValue(n.text) :: Nil
            else buildNodes(n)
          })
          XLeaf((allLabels(0), arr), Nil) :: Nil

        } else {
            nodes.groupBy(x => x.label).map {
              case a if a._2.size > 1 => buildNodes(a._2)
              case b => buildNodes(b._2.head)
            }.toList.flatten

        }
    }

    buildNodes(xml) match {
      case List(x @ XLeaf(_, _ :: _)) => toJValue(x)
      case List(x) => JObject(JField(nameOf(xml.head), toJValue(x)) :: Nil)
      case x => JArray(x.map(toJValue))
    }
  }

}
