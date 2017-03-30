package myparser

import myparser.GrammarDef._

 /* 
README: 
This project was compiled using a Scala compiler;
 Scala 2.12.1 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_121)
 To Run:fsc -d classes FilterBlindAlleyRules.scala && scala -classpath classes FilterBlindAlleyRules

Assignment #4
Program: FilterBlindAlleyRules.scala 
Authors: Panos Karagiannis (ID: 1309484), Kostas Zambetakis

  */



object Top_down_parser{

   /* --------------TOP DOWN-----------------------
 The generated parser is top-down, left-to-right, (and depth-first), thus cannot handle left recursion:
 N -> NS would cause itself to be applied recursively with no terminal symbols resolved.
 Similarly, indirect left recursions like N -> At, A -> Na would also cause stack overflow.
 Also, the generated parser is considered inefficient, which is common in  designs that involve backtracking
 (whose time complexity is hard to evaluate in most cases).
 Efficiency is usually gained by introducing premises in the language; 
for example, LL(1); lookaheads, first() set, and backtracking removal.
   */

  //Returns the rules for a given NT symbol 
  def gimme_rules( gram:CFG, symbol:NT):List[List [RHS] ]= gram match {
    case CFG(start, rules)=> rules match {
      case Nil => Nil
      case hd::tl => hd match {
        case Rule(a,b) =>
          if (a ==symbol) {b::(gimme_rules(CFG(start, tl), symbol)) }
          else { gimme_rules(CFG(start, tl), symbol)}

      }

    }

  }


  //Examines each symbol of the RHS of a rule and returns whatever the acceptor tells it.
  // the role of the acceptor is to be able to return incomplete derivations given a fragment.
  //So if a fragment is not able to be parsed by the grammar then maybe some part of it can be parsed
  //The specifications for what is accepted and what is not accepted as a valid derivation is
  //incorporated in the accept_derivation function.
  //If the accept_derivation only accepts empty suffixes, then only a complete derivation will be accepted

  def examine_rhs( gram:CFG,  rhs: List[RHS],
    accept_derivation:(List[String],List[Rule])=>  Option[(List[Rule], List[String])],
    frag:List[String], derivation:List[Rule] ): Option[(List[Rule], List[String])] = rhs match{
    case Nil => accept_derivation(frag,derivation)
    case hd::tl =>  hd match {
        case NT(nt)=>init_parser(gram, NT(nt),(examine_rhs(gram,tl, accept_derivation, _ :List[String] , _ :List[Rule] ) ), frag, derivation)
        case T(t) => 
          if (frag==Nil) {None}
          else if ( t== frag.head ) {examine_rhs(gram , tl ,accept_derivation, frag.tail, derivation)}
          else {None}
      }
  }
  

  //Examines each rule seperately for any given symbol. This function helps us traverse through all the rules of
  //a non terminal symbol up until we find an acceptable derivation
  def examine_rules( gram:CFG,symbol:NT, ListOfRhs: List[List[RHS]] , accept_derivation:(List[String],List[Rule])=>  Option[(List[Rule], List[String])],
    frag:List[String],derivation:List[Rule] ): Option[(List[Rule], List[String])] = ListOfRhs  match{
    case Nil => None
    case first_rule::rest_rule => (examine_rhs(gram, first_rule,accept_derivation, frag, Rule(symbol, first_rule)::derivation) ) match
    {
      case None => examine_rules( gram, symbol, rest_rule,accept_derivation, frag, derivation)
      case Some((d,a)) => Some((d,a))
    }

     
  }

  //initiates the parsing process by examining all the rules for a given NT symbol
  def init_parser (gram:CFG, symbol:NT, accept_derivation:(List[String],List[Rule])=>  Option[(List[Rule], List[String])], frag:List[String], derivation:List[Rule])={
    if (gimme_rules(gram, symbol) != Nil)
    { examine_rules(gram , symbol, gimme_rules(gram, symbol),accept_derivation, frag, derivation) }
    else
      {None}

  }

  //parser definition
  def top_down_parse( gram:CFG,  accept_derivation:(List[String],List[Rule])=>  Option[(List[Rule], List[String])], frag:List[String])= gram match {
    case CFG(start,rules) => init_parser(gram, start, accept_derivation, frag, List[Rule]() )

  }

 

  //strips the rule from NT and T symbols , for visually nice representation
  def strip_rule( r:List[RHS]):List[Any] = r match {
    case hd::tl => hd match {
      case NT(a)=> a::strip_rule(tl)
      case T(a)=> a::strip_rule(tl)
    }
    case Nil=> Nil

  }

   //visually nice representation of derivations
  //this function prints them in reverse order since when we create the derivations
  //for efficiency purposes we only append at the end of the list
  //also note that the derivation contains the acceptable frag, which we discard since we dont need it
  def print_derivations( d:Option[(List[Rule], List[String])]):Unit= d match {
    case Some((rules,f))=> rules match{
      case hd::tl => hd match {
        case Rule(NT(a),b)=> print_derivations(Some((tl,List())) ); println(a+"->"+ strip_rule(b).mkString(" ", "  ", " ")  )
      }
      case Nil => Nil
    }
    case None=> Nil

  }
   

}
