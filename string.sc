//questions 1, 2, 4, 5, 8, 9
import scala.util.control.Breaks._
import scala.util.Random
object string {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val r = scala.util.Random                       //> r  : util.Random.type = scala.util.Random$@17f6480
  class NumberFormatException extends Exception
  class NotANumberException extends Exception
  	class IncorrectSignException extends Exception
  	
  	
  //problem 1
  def isPal(word: String) = {
  		var word1 = word.replaceAll(" ", "").toLowerCase
  		var isPalandrome = true
	  	for (i <- 0 until word1.size - 1){
	  		if (word1.substring(i,i+1) != word1.substring(word1.size-i-1, word1.size-i))
	  			isPalandrome = false
	 	}
	  	isPalandrome
  }                                               //> isPal: (word: String)Boolean
  
  
  isPal("rotator")                                //> res0: Boolean = true
  isPal("car")                                    //> res1: Boolean = false
  isPal("ROTatoR")                                //> res2: Boolean = true
  
  
  
  //problem 2
 	def isPal2(word: String) = {
 		isPal(word.replaceAll("[^a-zA-Z ]", ""))
 	}                                         //> isPal2: (word: String)Boolean
 
 	isPal2("A man, a plan, a canal, Panama!") //> res3: Boolean = true
 	isPal2("rAce Car!!!")                     //> res4: Boolean = true
 
 
 
 	//problem 4
 	def mkWord(num: Integer = 5) = {
		var word = "";
		for(i <- 0 until num) {
			word += (97 + r.nextInt((122-97)+1)).toChar   //97 to 122 in decimal
		}
		word
	}                                         //> mkWord: (num: Integer)String
	
	mkWord()                                  //> res5: String = bcatv
	mkWord()                                  //> res6: String = zcqkc
 	mkWord(17)                                //> res7: String = laknekdptmnlwreie
 
 
 	//problem 5
 	def mkSentence(num: Integer = ( 1 + r.nextInt(20))) = {
 		var sentence = "";
		for(i <- 0 until num) {
			if (i > 0) sentence += " " + mkWord()
			else sentence = mkWord()
		}
		sentence = sentence.substring(0,1).toUpperCase() + sentence.substring(1) + "."
		sentence
 }                                                //> mkSentence: (num: Integer)String
 
 mkSentence()                                     //> res8: String = Ptsfh fxybh qocre euhkr nixfr txvsn.
 mkSentence(3)                                    //> res9: String = Bfwck vaepn vvjyo.
  
  
  //problem 8
  

   def eval(nums: String) = {
   	try {
	    val input = nums.replaceAll("\\s", "")
	    val split = input.split("\\+")
	    val sign = input.indexOf("+")
	    if(sign < 0) throw new IncorrectSignException
	   	val op1 = split(0).toDouble
	    val op2 = split(1).toDouble
      op1 + op2
     } catch {
     	case e:NumberFormatException => println("values entered must be numbers")
     	case e:NotANumberException => println("values entered must be numbers")
     	case e:IncorrectSignException => println("expression must be addition")
     	case e: Exception => println(e)
     }
  }                                               //> eval: (nums: String)AnyVal
  
  eval("31.14+42")                                //> res10: AnyVal = 73.14
 	eval("  -26  +  -49.99  ")                //> res11: AnyVal = -75.99000000000001
  eval(" 27 + 18")                                //> res12: AnyVal = 45.0
  eval("21 * 43")                                 //> expression must be addition
                                                  //| res13: AnyVal = ()
  eval("abc + 8")                                 //> java.lang.NumberFormatException: For input string: "abc"
                                                  //| res14: AnyVal = ()
                                                  
  //problem 9
  
   def eval1(nums: String) = {
   	try {
     	val input = nums.replaceAll("\\s", "")
       val sign = input.indexOf("+")
       val mult = input.indexOf("*")
       if((sign < 0) && (mult < 0)) throw new IncorrectSignException
       var split = Array[String]()
       if(sign > 0) split = input.split("\\+")
       else split = input.split("\\*")
       val num1 = split(0).toDouble
       val num2 = split(1).toDouble
       if(sign > 0) num1 + num2
       else num1 * num2
     } catch {
     	case e:NumberFormatException => println("values entered must be numbers")
     	case e:NotANumberException => println("values entered must be numbers")
     	case e:IncorrectSignException => println("expression must be addition or multiplication")
     	case e: Exception => println(e)
     }
  }                                               //> eval1: (nums: String)AnyVal
  
  eval1("31.14+42")                               //> res15: AnyVal = 73.14
 	eval1("  -26  +  -49.99  ")               //> res16: AnyVal = -75.99000000000001
  eval1(" 27 + 18")                               //> res17: AnyVal = 45.0
  eval1("21 * 43")                                //> res18: AnyVal = 903.0
  eval1("abc + 8")                                //> java.lang.NumberFormatException: For input string: "abc"
                                                  //| res19: AnyVal = ()
}