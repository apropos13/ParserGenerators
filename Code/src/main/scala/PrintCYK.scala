package myparser

import myparser.GrammarDef._

import myparser.CYK._

object PrintCYK {

  def main(args: Array[String]) {

     val other_rules=
       List(
         Rule (NT('S), List( NT('NP), NT('VP))),
         Rule (NT('S), List( NT('NP), NT('VB))),

         Rule (NT('VP), List( NT('VI))),
         Rule (NT('VP), List( NT('VT), NT('NP))),
         Rule (NT('VP), List( NT('VP), NT('PP))),

         Rule (NT('NP), List( NT('DT), NT('NN))),
         Rule (NT('NP), List( NT('NP), NT('PP))),

         Rule (NT('PP), List( NT('P), NT('NP))),

         Rule (NT('VI), List( T("sleeps"))),
         Rule (NT('VT), List( T("saw")   )),

         Rule (NT('NN), List( T("dog")   )),
         Rule (NT('NN), List( T("man")   )),
         Rule (NT('NN), List( T("telescope")  )),

         Rule (NT('DT), List( T("the"))),

         Rule (NT('IN), List( T("with")   )),
         Rule (NT('IN), List( T("in")   ))
 
        
       )

    val expr_gram=CFG(NT('S), other_rules)


     //val expr_frag= "x+2*y-x*z"
    val expr_frag= List("the","dog","saw","the","man")

    var foo= List( List( NT('DT), NT('NN)),  List( NT('VT), NT('NP)) )
    var dummylhs= get_producing_lhs(foo, other_rules)
    //println("dummylhs="+dummylhs)
           

    //cyk_parser(expr_gram, expr_frag)

    val another_frag= List("the","man","saw","the","telescope")
    //cyk_parser(expr_gram, another_frag)


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

    val ab_gram=CFG( NT('S), ab_rules)
    val ab_frag=List("a","a","a","b","b")
    cyk_recogniser(ab_gram, ab_frag)

    val ab_bad_frag=List("a","a","a","b","b","b")
    cyk_recogniser(ab_gram, ab_bad_frag)

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

    val full_gram=CFG(NT('S), sentence_rules)
    val full_frag= List("I","saw","the","man","with","the","telescope")
    cyk_recogniser(full_gram, full_frag)



  }




}
