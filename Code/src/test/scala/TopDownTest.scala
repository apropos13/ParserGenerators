package myparser

import myparser.GrammarDef._

import Top_down_parser._


import org.scalatest._

class TopDownSpec extends FlatSpec{

    val  top_down_rules =
       List(
         Rule (NT('Ios), List( NT('Linux),NT ('Unix) )),
         Rule (NT('Ios), List( NT ('Windows))),
         Rule (NT('Ios), List(T ("I"), NT ('Windows), T("S"))),
         Rule (NT('Unix), List( T("U"))),
         Rule (NT('Windows), List( T("P"))),
         Rule (NT('Windows), List( T("U"))),
         Rule (NT('Windows), List( T("L"))),
         Rule (NT('Unix), List( T("("), NT('Linux), T(")")   )),
         Rule (NT('Linux), List ( T("I")))
       )

      val other_rules=
       List(
         Rule (NT('Goal), List( NT('Expr))),
         Rule (NT('Expr), List( NT ('Term), T("+"), NT('Expr))), //notice right recursion
         Rule (NT('Expr), List( NT ('Term), T("-"), NT('Expr))),
         Rule (NT('Expr), List( NT('Term))),
         Rule (NT('Term), List( NT ('Factor), T("*"), NT('Term))),
         Rule (NT('Term), List( NT('Factor), T("-div-"), NT('Term))),
           Rule (NT('Term), List( NT('Factor))),
         Rule (NT('Factor), List( T("10"))),
         Rule (NT('Factor), List( T("x")   )),
         Rule (NT('Factor), List( T("y")   )),
         Rule (NT('Factor), List( T("z")   )),
          Rule (NT('Factor), List( T("2")   )),
         Rule (NT('Factor), List( T("("), NT('Expr), T(")")   ))
         
        
       )


      def accept(frag:List[String], derivation:List[Rule])= frag match {
    case Nil => Some((derivation, List[String]()))
    case _ => None

      }


  "An AST of fragment 'IUS':" must " be unique from Os rules and start IOS  " in {
    val os_grammar=CFG(NT('Ios), top_down_rules)
    val frag= List( "I","U","S")
    val td=top_down_parse(os_grammar, accept, frag)
    assert( td== Some((List(Rule(NT('Windows),List(T("U"))), Rule(NT('Ios),List(T("I"),
      NT('Windows), T("S")))),List())))
    


  }

  "An AST of frgament 'x+2*y-x*z':" must "be unique from epxr_rules and start Goal" in {

     val expr_gram=CFG(NT('Goal), other_rules)


     //val expr_frag= "x+2*y-x*z"
    val expr_frag= List("x","+","2","*","y","-","x","*","z")
    val deriv= top_down_parse(expr_gram, accept, expr_frag)
    println("Derivation of: " +expr_frag.mkString("{ ", " , ", " }") )
    assert( deriv == Some((
      List(Rule(NT('Factor),List(T("z"))),
        Rule(NT('Term),List(NT('Factor))),
        Rule(NT('Factor),
          List(T("x"))),
        Rule(NT('Term),List(NT('Factor), T("*"), NT('Term))),
        Rule(NT('Expr),List(NT('Term))),
        Rule(NT('Factor),List(T("y"))),
        Rule(NT('Term),List(NT('Factor))),
        Rule(NT('Factor),List(T("2"))),
        Rule(NT('Term),List(NT('Factor), T("*"), NT('Term))),
        Rule(NT('Expr),List(NT('Term), T("-"), NT('Expr))),
        Rule(NT('Factor),List(T("x"))),
        Rule(NT('Term),List(NT('Factor))),
        Rule(NT('Expr),List(NT('Term), T("+"), NT('Expr))),
        Rule(NT('Goal),List(NT('Expr)))),List())))
    

  }
 



}


