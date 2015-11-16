/*======================================================================
Written by Bryan Schrock at Trinity University in November 2015
	Generates the mandelbrot set in the command line with Ascii.
	
========================================================================*/




import scala.concurrent._
import scala.io.StdIn
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure
import scala.util.Success
	
var numCols = 80
	var numRows = 24
	var scrollSpeed = 0.1
	var moveSpeed = 0.1
	var rmax: Double = .5
	var rmin: Double = -1.5
	var imin: Double = -1.0
	var imax: Double = 1.0
	var numComplete = 0
  val rand = scala.util.Random
  var MaxIterations = rand.nextInt(30) + 30
  def mandelCount(c: Complex2, maxIters: Int): Int = {
    var z = new Complex2(0, 0)
    var cnt = 0
    while (z.magSqr < 16 && cnt < maxIters) {
      cnt += 1
      z = z * z + c
    }
    cnt
  }
  def drawAsciiMandel() {
    val numP = numCols / 5
    val toShow = Array.fill(numCols, numRows)("0")
		val ret = Array.fill(numCols, numRows)(0)
		val lowerBound = 0
		val higherBound = numCols
		for (i <- lowerBound.toInt until higherBound.toInt) {
			for (j <- 0 until numRows) {
				val c = new Complex2(rmin + i * (rmax - rmin) / numCols, imin + j * (imax - imin) / numRows)
				val ret = mandelCount(c, MaxIterations)
				toShow(i)(j) = if(ret == MaxIterations) "0" else 
											 if(ret > MaxIterations * (3.0/4.0)) "o" else
											 if(ret > MaxIterations / 2) "=" else
											 if(ret > MaxIterations / 4) "-" else " "
			}
		}
		for(i <- 0 until numRows){
			for(j <- 0 until numCols){
				print(toShow(j)(i))
			}
			println
		}
		println
  }
def convertXToR(x: Int) = ((x.toDouble / numCols.toDouble) * (rmax - rmin)) + rmin

println("How wide is your terminal?")
numCols = scala.io.StdIn.readInt
println("How tall?")
numRows = scala.io.StdIn.readInt
drawAsciiMandel
var cmd = scala.io.StdIn.readLine
var cmds = cmd.split(" ")
var lastCmd = cmd
while(cmds(0) != "quit"){
	try{
		if(cmds(0) == "draw"){
			drawAsciiMandel
		}else if(cmds(0) == "zoom" || cmds(0) == "z"){
			val multBy = 	if(cmds.length > 1) cmds(1).toInt else 1
			if(multBy > 0){
				for(i <- 0 until multBy){
					rmin += scrollSpeed
					rmax -= scrollSpeed
					imin += scrollSpeed
					imax -= scrollSpeed
					scrollSpeed *= 1- (scrollSpeed*2)/(rmax-rmin)
					moveSpeed *= 1- (moveSpeed*2)/(rmax-rmin)	
				}
			} else{
				for(i <- 0 until -multBy){
					rmin -= scrollSpeed
					rmax += scrollSpeed
					imin -= scrollSpeed
					imax += scrollSpeed
					scrollSpeed /= 1- (scrollSpeed*2)/(rmax-rmin)
					moveSpeed /= 1- (moveSpeed*2)/(rmax-rmin)	
				}
			}			
			drawAsciiMandel
		}else if(cmds(0) == "reset"){
			scrollSpeed = 0.1
			moveSpeed = 0.1
			rmax= .5
			rmin= -1.5
			imin= -1.0
			imax= 1.0
			drawAsciiMandel		
		}else if(cmds(0) == "up" || cmds(0) == "u"){
			val multBy = 	if(cmds.length > 1) cmds(1).toDouble else 1.0
			imin -= moveSpeed*multBy
			imax -= moveSpeed*multBy
			drawAsciiMandel		
		}else if(cmds(0) == "down" || cmds(0) == "d"){
			val multBy = 	if(cmds.length > 1) cmds(1).toDouble else 1.0
			imin += moveSpeed*multBy
			imax += moveSpeed*multBy
			drawAsciiMandel		
		}else if(cmds(0) == "left" || cmds(0) == "l"){
			val multBy = 	if(cmds.length > 1) cmds(1).toDouble else 1.0
			rmin -= moveSpeed*multBy
			rmax -= moveSpeed*multBy
			drawAsciiMandel		
		}else if(cmds(0) == "right" || cmds(0) == "r"){
			val multBy = 	if(cmds.length > 1) cmds(1).toDouble else 1.0
			rmin += moveSpeed*multBy
			rmax += moveSpeed*multBy
			drawAsciiMandel		
		}else{
			try{
				val num = cmds(0).toInt
				MaxIterations = num
				drawAsciiMandel
			}catch{
				case e: NumberFormatException => {
					println("not a valid command")
				}
			}
		}
		println("enter command (help for help)")
		cmd = scala.io.StdIn.readLine
		if(cmd == "."){
			cmd = lastCmd
		}else{
			lastCmd = cmd
		}
		cmds = cmd.split(" ")
	} catch{
		case nfe : NumberFormatException => {
			println("Error in parsing your command, try again.")
			cmd = readLine
			cmds = cmd.split(" ")
		}
	}
}

class Complex2(var r: Double, var i: Double) {
  def +(o: Complex2): Complex2 = new Complex2(r + o.r, i + o.i)
  def -(o: Complex2): Complex2 = new Complex2(r - o.r, i - o.i)
  def *(o: Complex2): Complex2 = new Complex2(r * o.r - i * o.i, r * o.i + i * o.r)
  def update(o: Complex2): Complex2 = {
    r = o.r;
    i = o.i;
    this
  }
  def stringFormat: String = r + "+i" + i
  def mag: Double = math.sqrt(r * r + i * i)
  def magSqr: Double = r * r + i * i
}


