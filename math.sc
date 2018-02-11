import scala.math._
import scala.util.Random
//questions 1, 2, 3, 6, 7, 8

class NegativeAmountException extends Exception
class IllegalArguementException extends Exception
object math {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	val r = scala.util.Random                 //> r  : util.Random.type = scala.util.Random$@7f690630

//problem 1
  def solve(coef: (Double, Double, Double)) = {
  		val (a,b,c) = coef
  		val discriminant = scala.math.pow(b,2) - 4*a*c
  		discriminant match {
  			case discriminant if discriminant < 0 => None
  			case discriminant if discriminant == 0 => print("B")
  			case _	 => print("C")
  		}
  }                                               //> solve: (coef: (Double, Double, Double))Any
  
  
 //problem 2      Add a comment to your session in which you write out the exact type of dist. Do you understand the notation?
 	def dist(points: ((Int, Int), (Int, Int))) = {
 		val (point1, point2) = points
 		val (x1, y1) = point1
 		val (x2, y2) = point2
 		scala.math.sqrt(scala.math.pow((x2-x1),2) + scala.math.pow((y2-y1),2))
 	}                                         //> dist: (points: ((Int, Int), (Int, Int)))Double
 	
 	dist((1, 1), (0, 0))                      //> res0: Double = 1.4142135623730951
 	dist((3, 0), (0, 0))                      //> res1: Double = 3.0
 	
 	
 	
 	//problem 3
 	def dot(vector: ((Double, Double, Double), (Double, Double, Double))) = {
 		val (point1, point2) = vector
 		val (x1,y1,z1) = point1
 		val (x2,y2,z2) = point2
 		x1*x2 + y1*y2 + z1*z2
 	}                                         //> dot: (vector: ((Double, Double, Double), (Double, Double, Double)))Double
 	
 	
 	dot((2.0, 3, 4), (2, 2.0, 2))             //> res2: Double = 18.0
 	
 	
 	
 	
 	//problem 6
 	def isPrime(input: Int) = {
 		var isPrime = true;
 		try {
 			if (input < 0) {
 				throw new NegativeAmountException
 			} else {
 				if (input == 0 || input == 1) isPrime = false
 				else {
	 				for (i <- 2 until input){
	 					if (input % i == 0) isPrime = false		//not prime, has a factor that's not one or itself
	 				}
	 			}
 			}
 		} catch {
 			case e: NegativeAmountException => println("input must be positive")
 		}
 		isPrime
 	}                                         //> isPrime: (input: Int)Boolean
 	
 	
 	isPrime(0)                                //> res3: Boolean = false
 	isPrime(1)                                //> res4: Boolean = false
 	isPrime(2)                                //> res5: Boolean = true
 	isPrime(8)                                //> res6: Boolean = false
 	isPrime(-1)                               //> input must be positive
                                                  //| res7: Boolean = true
 	
 	
 	//problem 7
 	def phi(n: Int) = {
 		try{
 			if (n < 2) throw new IllegalArguementException
 		//find gcd of two numbers. if the gcd == 1 then they are relatively prime
	 		var total = 0;
	 		for (i <- 1 to n) {
	 			var temp = 0
	 			var a = i
	 			var b = n
	 			while (b != 0) {
	 				temp = a
	 				a = b
	 				b = temp % b
	 			}
	 			if (a == 1)  total = total + 1
	 		}
	 		total
	 	} catch {
	 		case e: IllegalArguementException => println("input must be greater than 0")
	 	}
 	}                                         //> phi: (n: Int)AnyVal
 	
 	
 	phi(9)                                    //> res8: AnyVal = 6
 	phi(10)                                   //> res9: AnyVal = 4
 	phi(20)                                   //> res10: AnyVal = 8
 	phi(-1)                                   //> input must be greater than 0
                                                  //| res11: AnyVal = ()
 	
 	
 	
 	//problem 8
 	def rollDice(): (Double, Double) = {
 		(1 + r.nextInt(6), 1 + r.nextInt(6))
 	}                                         //> rollDice: ()(Double, Double)
 	
 	rollDice()                                //> res12: (Double, Double) = (1.0,1.0)
 	rollDice()                                //> res13: (Double, Double) = (3.0,1.0)
 	rollDice()                                //> res14: (Double, Double) = (6.0,6.0)
 	
}