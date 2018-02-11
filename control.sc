//questions 1 - 5
import scala.util.control.Breaks._

object control {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  class NegativeAmountException extends Exception
  
  
  //problem 1
  def elboniaTax(income : Double) =
		try {
	  		income match {
	  			case income if income < 0 => throw new NegativeAmountException
		  		case income if income < 20000 => 0
		  		case income if income < 30000 => .05 * income
		  		case income if income < 40000 => .11 * income
		  		case income if income < 60000 => .23 * income
		  		case income if income < 100000 => .32 * income
		  		case _ => .5 * income
			}
    } catch {
    		case e:NegativeAmountException => println("amount must be positive")
    }                                             //> elboniaTax: (income: Double)AnyVal
  		
  
  
  		elboniaTax(-10)                   //> amount must be positive
                                                  //| res0: AnyVal = ()
  		elboniaTax(99)                    //> res1: AnyVal = 0
  		elboniaTax(101)                   //> res2: AnyVal = 0
  		elboniaTax(100001)                //> res3: AnyVal = 50000.5
  
  
  //problem 2
	  def drawRectangle(n : Integer, m : Integer) = {
	  		var toPrint = "";
	  		for(i <- 0 until n; j <- 0 until m) {
	  			if (j == 0) toPrint += "\n" + "*"
	  			else toPrint += "*"
	  		}
	  		print(toPrint)
 	 	}                                 //> drawRectangle: (n: Integer, m: Integer)Unit
 	 	
 	 	drawRectangle(3,4)                //> 
                                                  //| ****
                                                  //| ****
                                                  //| ****
    drawRectangle(5,5)                            //> 
                                                  //| *****
                                                  //| *****
                                                  //| *****
                                                  //| *****
                                                  //| *****
    drawRectangle(6,1)                            //> 
                                                  //| *
                                                  //| *
                                                  //| *
                                                  //| *
                                                  //| *
                                                  //| *
                                        
  
  
  
  //problem 3
  def printSums(n : Integer, m : Integer) {
  		for(i <- 1 until n; j <- 1 until m if n < 0 if m < 0)
  			println(i + " + " + j + " = " + (i+j))
  		
  }                                               //> printSums: (n: Integer, m: Integer)Unit
  
  
  
  printSums(4,3)
  printSums(2,5)
  
  
  
  
  //problem 4
  /*
  public void mystery() {
        for(int i = 0; i < 100; i++) {
           if (i % 3 == 0) continue; // skip if i divisible by 3
           if (i == 10) break; // terminate loop when i is 10
           System.out.println("i = " + i);
        }
        System.out.println("done");
     }
  */
  
  def mystery() {
 		breakable {
  			for (i <- 0 to 100 if i % 3 != 0){
  				if (i == 10) break
  				println("i = " + i)
  			}
  		}
  		println("done")
  }                                               //> mystery: ()Unit
  
  
  
  mystery()                                       //> i = 1
                                                  //| i = 2
                                                  //| i = 4
                                                  //| i = 5
                                                  //| i = 7
                                                  //| i = 8
                                                  //| done
  
  
  
  //problem 5
  def root(x: Double): Option[Double] = if (x < 0) None else Some(scala.math.sqrt(x))
                                                  //> root: (x: Double)Option[Double]
	def below10(x: Double): Option[Double] = if (x < 10) Some(x) else None
                                                  //> below10: (x: Double)Option[Double]
//	def pureRoot(x: Option[Double]): Option[Double] = root(y) if x = Some(y) else None
//	def pureBelow10(x: Option[Double]): Option[Double] = below10(y) if x = Some(y) else None
	
  	def pureRoot(x: Option[Double]): Option[Double] = {
  		x match {
  			case None => None
  			case Some(x) => root(x)
  		}
  }                                               //> pureRoot: (x: Option[Double])Option[Double]
  
  
  def pureBelow10(x: Option[Double]): Option[Double] = {
  		x match {
  			case None => None
  			case Some(x) => below10(x)
  		}
  }                                               //> pureBelow10: (x: Option[Double])Option[Double]
  
  def below10root (x: Option[Double]): Option[Double] = {
  		pureRoot(pureBelow10(x))
  }                                               //> below10root: (x: Option[Double])Option[Double]
  
  pureRoot(Some(-49))                             //> res4: Option[Double] = None
  pureRoot(Some(49))                              //> res5: Option[Double] = Some(7.0)
  pureRoot(None)                                  //> res6: Option[Double] = None
  pureRoot(Some(-81))                             //> res7: Option[Double] = None
  pureRoot(Some(81))                              //> res8: Option[Double] = Some(9.0)
  
  pureBelow10(None)                               //> res9: Option[Double] = None
  pureBelow10(Some(-10))                          //> res10: Option[Double] = Some(-10.0)
  pureBelow10(Some(20))                           //> res11: Option[Double] = None
  
}