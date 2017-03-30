package myparser

import myparser.GrammarDef._

import myparser.FilterBlindAlleyRules._

import org.scalatest._


class FilterSpec extends FlatSpec {
   val rules=List(
         Rule( NT('Fox) , List(T("5") , NT ('Wolf), T("j") )),
         Rule (NT('Dolphin), List(NT('Wolf))),
         Rule (NT('Bear), List(NT('Shark))),
         Rule (NT('Fox), List(NT('Dolphin), NT('Shark))),
         Rule (NT('Shark), List(T("6")))
   )

  "Seperate rules:" must " find those rules consisting only of terminal strings and add them to a seperate list " in {

    val term_rules= seperate_rules(rules)
    assert( term_rules == List (  Rule (NT('Shark), List(T("6")))  ) )

  }

  "Take one step:" should "mark a rule as non-blind alley if we find the rhs of this rule to be a subset of the lhs of non-blind alley rules" in{
    val term_rules= seperate_rules(rules) 
    val step= take_one_step(rules, term_rules)
    assert( step == List(Rule(NT('Bear),List(NT('Shark))), Rule(NT('Shark),List(T("6")))))

  }

   "A filter:" should "erase all the blind alley rules" in {

      val filtered_rules= List(
         Rule(NT('Bear),List(NT('Shark))), 
         Rule(NT('Shark),List(T("6")))
         )
     
      val my_grammar= CFG(NT('Fox), rules)
      val filtered_gram= filter_blind_alleys(my_grammar)
      assert(filtered_gram== filtered_rules)


   }

	 
}
