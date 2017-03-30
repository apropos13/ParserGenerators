package myparser

import myparser.GrammarDef._

import CYK._


import org.scalatest._

class CYKSpec extends FlatSpec {

   val ab_rules=
       List(
         Rule (NT('S), List( NT('A), NT('B))),
         Rule (NT('S), List( NT('X), NT('B))),

         Rule (NT('T), List( NT('A), NT('B))),
         Rule (NT('T), List( NT('X), NT('B))),

         Rule (NT('X), List( NT('A), NT('T))),


         Rule (NT('A), List( T("a"))),
         Rule (NT('B), List( T("b")))

       )

  "String aabbb ':" must "not be parsed given ab_rules (defined in spec)  " in {
    val ab_gram=CFG( NT('S), ab_rules)
    val ab_frag=List("a","a","a","b","b")
    assert(cyk_recogniser(ab_gram, ab_frag)==false)
  }

  "String aaabbb ':" must " be parsed given ab_rules (defined in spec)  " in {
    val ab_gram=CFG( NT('S), ab_rules)
    val ab_good_frag=List("a","a","a","b","b","b")
    assert( cyk_recogniser(ab_gram, ab_good_frag) == true)

  }
   

    var sentence_rules=
      List(
        Rule (NT('S), List( NT('NP), NT('VP))),
        Rule (NT('NP), List( NT('DET), NT('N))),
        Rule (NT('NP), List( NT('NP), NT('PP))),

        Rule (NT('PP), List( NT('P), NT('NP))),

        Rule (NT('VP), List( NT('V), NT('NP))),
        Rule (NT('VP), List( NT('VP), NT('PP))),
       


        Rule (NT('DET), List( T("the"))),
        Rule (NT('NP), List( T("I"))),
        Rule (NT('N), List( T("man"))),
        Rule (NT('N), List( T("baby"))),
        Rule (NT('P), List( T("with"))),
        Rule (NT('V), List( T("saw"))),

        Rule (NT('N), List( T("cat"))),
        Rule (NT('N), List( T("dog"))),
        Rule (NT('N), List( T("pig"))),
        Rule (NT('N), List( T("Panos"))),
        Rule (NT('N), List( T("Poutsis"))),
        Rule (NT('N), List( T("telescope"))),


        Rule (NT('P), List( T("from"))),
        Rule (NT('P), List( T("on"))),
        Rule (NT('P), List( T("in")))

      )

    "String I saw the man with the telescope ':" must " be parsed given sentence_rules (defined in spec)  " in {

      val full_gram=CFG(NT('S), sentence_rules)
      val full_frag= List("I","saw","the","man","with","the","telescope")
      assert( cyk_recogniser(full_gram, full_frag)==true)

    }


  

}
