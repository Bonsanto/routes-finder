package org.bonsanto.kruskalvariant

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.io.Source


object Main {
  val sc = new java.util.Scanner(System.in)

  case class Vertex(x: Int, y: Int)

  case class Edge(i: Int, j: Int, weight: Int)

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

  def calculateEdges(nodes: Vertices, threshold: Int,
                     f: (Vertex, Vertex) => (Int) = (a, b) => Math.abs(a.x - b.x) + Math.abs(a.y - b.y)): PQueue[Edge] = {
    val edges = for {
      i <- nodes.indices
      j <- i + 1 until nodes.size
      w = f(nodes(i), nodes(j))
      if w >= threshold
    } yield Edge(i, j, w)

    mutable.PriorityQueue[Edge](edges: _*)(Ordering.by(_.weight))
  }

  def anyMissingEdge(minimumRoutes: Int, edgesByVertex: mutable.Map[Int, Int]): Boolean = {
    edgesByVertex.foreach(pair => if (pair._2 < minimumRoutes) return true)
    false
  }

  def findBestRoutes(requiredRoutes: Int, nodes: Vertices, edges: PQueue[Edge]): mutable.Set[Edge] = {

    val completed = mutable.Set[Int]()
    val bestRoutes = mutable.Set[Edge]()
    val routesPerNode = Array.ofDim[Int](nodes.size)

    while (completed.size != nodes.size){
    if (edges.isEmpty) throw new Exception("The graph can't be solved with this algorithm.")
      var bestRoute = edges.dequeue()
      var i = bestRoute.i
      var j = bestRoute.j

      while (completed.contains(i) || completed.contains(j)) {
        bestRoute = edges.dequeue()
        i = bestRoute.i
        j = bestRoute.j
      }

      routesPerNode.update(i, routesPerNode(i) + 1)
      routesPerNode.update(j, routesPerNode(j) + 1)

      if(routesPerNode(i) == requiredRoutes) completed.add(i)
      if(routesPerNode(j) == requiredRoutes) completed.add(j)

      bestRoutes.add(bestRoute)
    }
    bestRoutes
  }

  def readFile(path: String): Unit = {
    val source = Source.fromFile(path).getLines()

    def readNodesFromFile(nodesCount: Int): Vertices = {
      if (nodesCount == 0) Nil
      else createVertex(source.next()) :: readNodesFromFile(nodesCount - 1)
    }

    for {
      i <- 0 until source.next.toInt
    } {
      println("Processing " + i)
      val nodes = source.next.toInt
      val requiredRoutes = source.next.toInt
      val minDistance = source.next.toInt
      val vertices = readNodesFromFile(nodes)

      val edges = calculateEdges(vertices, minDistance)

      findBestRoutes(requiredRoutes = requiredRoutes, nodes = vertices, edges = edges).foreach(println)
      println("Completed")
    }
  }
  def main(args: Array[String]): Unit = {
    readFile("E:\\Documents\\routes-finder\\commands.in")
    /*
    val nodesCount = sc.nextInt
    val requiredRoutes = sc.nextInt
    val minimumDistance = sc.nextInt


    val nodes = readNodes(nodesCount)

    val edges = calculateEdges(nodes, minimumDistance)

    findBestRoutes(requiredRoutes = requiredRoutes, nodes = nodes, edges = edges).foreach(println)
    */
  }
}
