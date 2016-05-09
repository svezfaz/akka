/**
 * Copyright (C) 2009-2016 Lightbend Inc. <http://www.lightbend.com>
 */

package akka.util

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

private[akka] final case class WildcardIndex[T](wildcardTree: WildcardTree[T] = WildcardTree[T](), doubleWildcardTree: WildcardTree[T] = WildcardTree[T]()) {

  val empty = WildcardTree[T]()

  def insert(elems: Array[String], d: T): WildcardIndex[T] = elems.lastOption match {
    case Some("**") ⇒ copy(doubleWildcardTree = doubleWildcardTree.insert(elems.iterator, d))
    case Some(_)    ⇒ copy(wildcardTree = wildcardTree.insert(elems.iterator, d))
    case _          ⇒ this
  }

  def forNonEmpty(tree: WildcardTree[T])(f: WildcardTree[T] ⇒ WildcardTree[T]) = tree match {
    case `empty` ⇒ empty
    case _       ⇒ f(tree)
  }

  def find(elems: Iterable[String]): Option[T] = {
    (forNonEmpty(wildcardTree)(_.findWithSingleWildcard(elems.iterator)) match {
      case `empty` ⇒ forNonEmpty(doubleWildcardTree)(_.findWithTerminalDoubleWildcard(elems.iterator))
      case tree    ⇒ tree
    }).data
  }
}

private[akka] object WildcardTree {
  private val empty = new WildcardTree[Nothing]()
  def apply[T](): WildcardTree[T] = empty.asInstanceOf[WildcardTree[T]]
}

private[akka] final case class WildcardTree[T](data: Option[T] = None, children: Map[String, WildcardTree[T]] = HashMap[String, WildcardTree[T]]()) {

  def insert(elems: Iterator[String], d: T): WildcardTree[T] =
    if (!elems.hasNext) {
      copy(data = Some(d))
    } else {
      val e = elems.next()
      copy(children = children.updated(e, children.getOrElse(e, WildcardTree[T]()).insert(elems, d)))
    }

  @tailrec def find(elems: Iterator[String]): WildcardTree[T] =
    if (!elems.hasNext) this
    else {
      (children.get(elems.next()) orElse children.get("*")) match {
        case Some(branch) ⇒ branch.find(elems)
        case None         ⇒ WildcardTree()
      }
    }

  @tailrec def findWithSingleWildcard(elems: Iterator[String]): WildcardTree[T] =
    if (!elems.hasNext) this
    else {
      children.get(elems.next()) match {
        case Some(branch) ⇒ branch.findWithSingleWildcard(elems)
        case None ⇒ children.get("*") match {
          case Some(branch) ⇒ branch.findWithSingleWildcard(elems)
          case None         ⇒ WildcardTree[T]()
        }
      }
    }

  @tailrec def findWithTerminalDoubleWildcard(elems: Iterator[String], alt: WildcardTree[T] = WildcardTree[T]()): WildcardTree[T] = {
    if (!elems.hasNext) this
    else {
      val newAlt = children.getOrElse("**", alt)
      children.get(elems.next()) match {
        case Some(branch) ⇒ branch.findWithTerminalDoubleWildcard(elems, newAlt)
        case None ⇒ children.get("*") match {
          case Some(branch) ⇒ branch.findWithTerminalDoubleWildcard(elems, newAlt)
          case None         ⇒ newAlt
        }
      }
    }
  }
}

