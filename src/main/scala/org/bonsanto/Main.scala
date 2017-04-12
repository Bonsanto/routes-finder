package org.bonsanto

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable


object Main {
  val sc = new java.util.Scanner(System.in)

  case class Vertex(x: Int, y: Int)

  type Vertices = List[Vertex]

  type PQueue[T] = mutable.PriorityQueue[T]

  def createVertex(str: String): Vertex = {
    val values = str.split(",")

    (values.apply(0), values.apply(1)) match {
      case (x, y) => Vertex(x.toInt, y.toInt)
      case _ => throw new Exception("Not supported tokens. Coordinates must me integers.")
    }
  }

  def readNodes(nodesCount: Int): Vertices = {
    if (nodesCount == 0) Nil
    else createVertex(sc.next) :: readNodes(nodesCount - 1)
  }

  def calculateEdges(nodes: Vertices, f: (Vertex, Vertex) => (Int) = (a, b) => Math.abs(a.x - b.x) + Math.abs(a.y - b.y)): IndexedSeq[((Int, Int), Int)] = {
    for {
      i <- nodes.indices
      j <- i + 1 until nodes.size
    } yield ((i, j), f(nodes(i), nodes(j)))
  }

  def prioritize(edges: IndexedSeq[((Int, Int), Int)]): PQueue[((Int, Int), Int)] = {
    mutable.PriorityQueue(edges: _*)(Ordering.by(_._2))
  }

  def main(args: Array[String]): Unit = {
    val nodesCount = sc.nextInt
    val routes = sc.nextInt
    val minimumDistance = sc.nextInt

    val nodes = readNodes(nodesCount)

    val edges = calculateEdges(nodes).filterNot(_._2 < minimumDistance)

    val queue = prioritize(edges)

    println(queue)

    while (queue.nonEmpty){
      println(queue.dequeue())
    }
  }
}
