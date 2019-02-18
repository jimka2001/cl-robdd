package dimacs

import scala.io.StdIn
import scala.math._
import scala.collection.mutable.HashMap

object nonneg {
  import scala.language.implicitConversions

  class NonNegInt private (val value: Int) extends AnyVal

  object NonNegInt {
    def apply(v: Int) = {
      require(v >= 0, "NonNegInt forbids negative integer values")
      new NonNegInt(v)
    }

    implicit def toNonNegInt(v: Int) = NonNegInt(v)
  }

  implicit def toInt(nn: NonNegInt) = nn.value
}

object dimacsApp {
  import nonneg._
  type Clause = List[Integer]
  type Clauses = List[Clause]

  def every2[A,B](f:(A,B)=>Boolean,L1:List[A],L2:List[B]):Boolean = {
    (L1,L2) match {
      case (a::as,b::bs) => f(a,b) && every2(f,as,bs)
      case (_,_) => false
    }
  }
  def equalAbs(x:Integer,y:Integer):Boolean = {
    abs(x) == abs(y)
  }
  def equalAbsList(clause1:Clause,clause2:Clause):Boolean = {
    every2( equalAbs, clause1, clause2)
  }
  def qmCompatible_?(clause1:Clause, clause2:Clause):Boolean = {
    def loop(clause1:Clause,clause2:Clause,diff:Integer):Boolean = {
      if (diff > 1) false
      else (clause1,clause2) match {
       case (a::as,b::bs) => (
          equalAbs(a,b) && loop(as,bs,(if (a==b) diff else diff+1)))
        case (_,_) => (1==diff)
      }
    }
    loop(clause1,clause2,0)
  }
  def clauseLess(clause1:Clause,clause2:Clause):Boolean = {
    (clause1,clause2) match {
      case (Nil,Nil) => false
      case (c1::c1s,c2::c2s) if (c1==c2) => clauseLess(c1s,c2s)
      case (c1::_,c2::_) if (abs(c1)==abs(c2)) => c1 < c2
      case (c1::_,c2::_) => abs(c1) < abs(c2)
    }
  }
  def removeDuplicatesSorted(clauses:Clauses):Clauses = {
    def loop(clauses:Clauses,acc:list[Clause]):Clauses = {
      clauses match {
        case Nil => acc.reverse
        case a1::a2::as if a1 == a2 => loop(a2::as,acc)
        case a::as => loop(as,a::acc)
      }
      loop(clauses,Nil)
  }
  def merge(clauses1:Clauses,clauses2:Clauses):Clauses = {
    def loop(clauses1:Clauses,clauses2:Clauses,acc:Clauses):Clauses = {
      (clauses1,clauses2) match {
        case (Nil,Nil) => acc.reverse
        case (a::as,Nil) => loop(as,Nil,a::acc)
        case (Nil,b::bs) => loop(Nil,bs,b::acc)
        case (a::as,b::bs) if (clauseLess(a,b) ) => loop(as,b::bs,a::acc)
        case (a::as,b::bs) => loop(a::as,bs,b::acc)
      }
    }
    loop(clauses1,clauses2,Nil)
  }
  def merge(clause:Clause,clauses:Clauses,testUnique:Boolean,sort:Boolean):Clauses = {
    (testUnique,sort) match {
      case (false,false) => clause::clauses
      case (true, _) if clauses.contains(clause) => clauses
      case (true, false) => clause::clauses
      case (_, true) => merge(List(clause), clauses)
    }
  }
  case class QmVec() {
    val hash:HashMap[NonNegInt,HashMap[NonNegInt,Clauses]] = new HashMap
    var numVars:Integer = 0

    def addClause(clause:Clause,posCount:NonNegInt,length:NonNegInt,testUnique:Boolean,sort:Boolean):Unit = {
      hash.get(posCount) match {
        case None => hash += (posCount -> new HashMap)
        case _ => Unit
      }
      hash(posCount).get(length) match {
        case None => {
          hash(posCount) = new HashMap
          hash(posCount) += (length -> List(clause))
        }
        case Some(clauses) => hash(posCount) += (length -> merge(clause,clauses,testUnique,sort))
      }
    }
    def addClause(clause:Clause):Unit = {
      addClause(clause, countPositive(clause), clause.length, true, true)
    }
    def sortQmVec():Unit = {
      for ((posCount,lengthHash)<-hash) {
        for ((length,clauses)<-lengthHash) {
          lengthHash += (length, removeDuplicatesSorted(clauses.sortWith(clauseLess)))
        }
      }
    }
  }
  def calcNumVars(clauses:Clauses):NonNegInt = {
    val hash:HashMap[Int,Boolean] = new HashMap
    clauses map ((clause:List[Integer])=> clause map ((num:Integer)=>hash += ((abs(num) -> true))))
    hash.size
  }
  def countPositive(clause:Clause):NonNegInt ={
    clause.count((num) => (num >= 0))
  }

  def main(args:Array[String]):Unit ={

  }
  
}


