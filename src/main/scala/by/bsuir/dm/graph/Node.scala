package by.bsuir.dm.graph

import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._

case class Node(id: String, label: String) {
  val (a, b) = (Node("A", "Something"), Node("B", "SomethingElse"))
  val g = Graph[Node, DiEdge](a ~> b)

  type MyGraph = Graph[Node, DiEdge]
  val root = DotRootGraph(directed = true, id = None)

  def myEdgeTransformer(innerEdge: MyGraph#EdgeT): Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
    case from ~> to => Some(root -> DotEdgeStmt(NodeId(from.value.id), NodeId(to.value.id)))
  }

  def myNodeTransformer(innerNode: MyGraph#NodeT): Option[(DotGraph, DotNodeStmt)] = {
    val myNode = innerNode.value
    Some((root, DotNodeStmt(NodeId(myNode.id), DotAttr(Id("label"), Id(myNode.label)) :: Nil)))
  }

  val dot = g.toDot(dotRoot = root,
    edgeTransformer = myEdgeTransformer,
    cNodeTransformer = Some(myNodeTransformer),
    spacing = Spacing(TwoSpaces))
}