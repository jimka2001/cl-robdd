package dimacs

import scala.collection.mutable
import scala.io.StdIn
import scala.math._
import scala.collection.mutable.HashMap

object dimacsApp  {
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
  def absLess(a:Integer,b:Integer):Boolean = {
    abs(a) < abs(b)
  }
  def clauseLess(clause1:Clause,clause2:Clause):Boolean = {
    (clause1,clause2) match {
      case (Nil,Nil) => false
      case (c1::c1s,c2::c2s) if (c1==c2) => clauseLess(c1s,c2s)
      case (c1::_,c2::_) if (abs(c1)==abs(c2)) => c1 < c2
      case (c1::_,c2::_) => absLess(c1,c2)
    }
  }
  def removeDuplicatesSorted(clauses:Clauses):Clauses = {
    def loop(clauses: Clauses, acc: Clauses): Clauses = {
      clauses match {
        case Nil => acc.reverse
        case a1 :: a2 :: as if a1 == a2 => loop(a2 :: as, acc)
        case a :: as => loop(as, a :: acc)
      }
    }
    loop(clauses, Nil)
  }

  def merge(clauses1: Clauses, clauses2: Clauses): Clauses = {
    def loop(clauses1: Clauses, clauses2: Clauses, acc: Clauses): Clauses = {
      (clauses1, clauses2) match {
        case (Nil, Nil) => acc.reverse
        case (a :: as, Nil) => loop(as, Nil, a :: acc)
        case (Nil, b :: bs) => loop(Nil, bs, b :: acc)
        case (a :: as, b :: bs) if (clauseLess(a, b)) => loop(as, b :: bs, a :: acc)
        case (a :: as, b :: bs) => loop(a :: as, bs, b :: acc)
      }
    }
    loop(clauses1, clauses2, Nil)
  }

  def merge(clause: Clause, clauses: Clauses, testUnique: Boolean, sort: Boolean): Clauses = {
    (testUnique, sort) match {
      case (false, false) => clause :: clauses
      case (true, _) if clauses.contains(clause) => clauses
      case (true, false) => clause :: clauses
      case (_, true) => merge(List(clause), clauses)
    }
  }

  case class QmVec() {
    val hash: HashMap[Integer, HashMap[Integer, Clauses]] = new HashMap
    var numVars: Integer = 0

    def addClause(clause: Clause, posCount: Integer, length: Integer, testUnique: Boolean, sort: Boolean): Unit = {
      hash.get(posCount) match {
        case None => hash += (posCount -> new HashMap)
        case _ => Unit
      }
      hash(posCount).get(length) match {
        case None => {
          hash(posCount) = new HashMap
          hash(posCount) += (length -> List(clause))
        }
        case Some(clauses) => hash(posCount) += (length -> merge(clause, clauses, testUnique, sort))
      }
    }

    def addClause(clause: Clause): Unit = {
      addClause(clause, countPositive(clause), clause.length, true, true)
    }

    def removeClause(clause: Clause): Unit = {
      removeClause(clause, countPositive(clause), clause.length)
    }

    def remove1(clause:Clause, clauses:Clauses):Clauses = {
      // remove the given clauses from list of clauses assuming that it
      // appears zero or one times, but no more
      // order of clauses is maintained
      def loop (clauses:Clauses, acc:Clauses):Clauses = {
        clauses match {
          case Nil => acc.reverse
          case c::cs if clause != c => loop(cs,c::acc)
          case c::cs => acc.reverse ++ cs
        }
      }
      loop(clauses,Nil)
    }
    def removeClause(clause: Clause, numPos: Integer, length: Integer): Unit = {
      hash.get(numPos) match {
        case None => Unit
        case Some(lengthHash) => lengthHash.get(length) match {
          case None => Unit
          case Some(clauses) => lengthHash += (length -> remove1(clause,clauses))
        }
      }
    }

    def getClauses(numPos: Integer, length: Integer): Clauses = {
      val maybeLengthHash:Option[HashMap[Integer,Clauses]] = hash.get(numPos)
      maybeLengthHash match {
        case None => Nil
        case Some(lengthHash) => lengthHash.getOrElse(length,Nil)
      }
    }
    def mapClauses(consume: Clause => Unit): Unit = {
      for ((numPos, lengthHash) <- hash) {
        for ((length, clauses) <- lengthHash) {
          clauses map consume
        }
      }
    }

    def sortClause(clause: Clause): Clause = {
      clause.sortWith(absLess)
    }

    def quineMccluskeyReduce(clauses: Clauses): Clauses = {
      val vec = new QmVec
      clauses map ((clause) => addClause(sortClause(clause)))
      quineMccluskeyReduce()
    }
    def mapcan[A1,A2,B](f:(A1,A2)=>List[B],L1:List[A1],L2:List[A2]):List[B] = {
      (L1,L2).zipped.flatMap(f)
    }
    def reduceOneVar (clause1:Clause,clause2:Clause):Clause = {
      mapcan( ((v1:Integer,v2:Integer)=>(if (v1 == v2) List(v1) else Nil)), clause1 ,clause2)
    }
    //                         clause, posCount, length
    type HashUpdateFunction = (Clause,Integer,Integer)=>Unit
    def reduceOne(posCount:Integer, addFunction:HashUpdateFunction, removeFunction:HashUpdateFunction):Unit = {
      val posCountDec = posCount - 1

      (hash.get(posCount),hash.get(posCountDec)) match {
        case (None,_) => Unit
        case (_,None) => Unit
        case (lengthHashA,lengthHashB) => {
          for((length -> clausesA)<-lengthHashA){
            val clausesB = lengthHashB.getOrElse(length,Nil)
            def loop(clausesA:Clauses,clausesB:Clauses):Unit = {
              (clausesA,clausesB) match {
                case (Nil,_) => Unit
                case (_,Nil) => Unit
                case (a::as,b::bs) => {
                  if (equalAbsList(a,b)) {
                    val (ca,ta) = clausesA.span(equalAbsList(_,a))
                    val (cb,tb) = clausesB.span(equalAbsList(_,b))
                    for (aa <- ca){
                      for (bb <- cb){
                        if (qmCompatible_?(aa,bb)) {
                          removeFunction(aa,posCount,length)
                          removeFunction(bb,posCountDec,length)
                          addFunction(reduceOneVar(aa,bb),posCountDec,length)
                        }
                      }
                    }
                    loop(ta,tb)
                  }
                  else if (clauseLess(a,b)) {
                    loop(as,clausesB)
                  }
                  else {
                    loop(clausesA,bs)
                  }
                }
              }
            }
            loop(clausesA,clausesB)
          }
        }
      }
    }
    def qmReduce():Unit = {
      var maxPosCount:Integer = hash.foldLeft(0){ case (acc, (k, _)) => max(k,acc) }
      def loop():Unit = {
        var changed = false
        var removes:List[Unit=>Unit] = Nil
        def addCBF(clause:Clause,posCount:Integer,length:Integer):Unit = {
          changed = true
          addClause(clause,posCount,length,false,false)
        }
        def removeCBF(clause:Clause,posCount:Integer,length:Integer):Unit = {
          def thunk():Unit = {
            removeClause(clause,posCount,length)
          }
          removes = thunk::removes
        }
        for((posCount -> _) <- hash) {
          if (posCount < maxPosCount)
            reduceOne(posCount,addCBF,removeCBF)
        }
        for(thunk <- removes)
          thunk()
        maxPosCount = hash.foldLeft(0){
          case (acc, (k, _)) => if (k<maxPosCount) max(k,acc) else acc
        }
        if (changed)
          loop()
      }
      loop()
    }
    def quineMccluskeyReduce(): Clauses = {

    }

    def sortQmVec(): Unit = {
      for ((posCount, lengthHash) <- hash) {
        for ((length, clauses) <- lengthHash) {
          lengthHash += (length -> removeDuplicatesSorted(clauses.sortWith(clauseLess)))
        }
      }
    }
  }
  def calcNumVars(clauses: Clauses): Integer = {
    val hash: HashMap[Int, Boolean] = new HashMap
    clauses map ((clause: List[Integer]) => clause map ((num: Integer) => hash += ((abs(num) -> true))))
    hash.size
  }

  def countPositive(clause: Clause): Integer = {
    clause.count((num) => (num >= 0))
  }

  def main(args: Array[String]): Unit = {

  }


}


