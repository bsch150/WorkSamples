/*
	This program written by Bryan Schrock in March 2015

	This program has no functional purpose besides showing off javascript and looking cool.
	Go to "www.cs.trinity.edu/~bschrock/Drawing/one.php" to see it in action.
*/

var config = document.getElementById("config");
var c = document.getElementById("wholePage");
var g = c.getContext('2d');
c.width  = window.innerWidth;
c.height = window.innerHeight;
var cX = c.width/2;
var cY = c.height/2;
var oldR = 1;
var newR = 1;
var minR = 10;
var maxR = 250;
var maxNewH = 40;
var minDots = 50;
var maxDots = 600;
var size = cX;
var startRed = 0;
var endRed = 0;
var startBlue = 250;
var endBlue = 0;
var startGreen = 0;
var endGreen = 0;
var numConnections = 2;
var maxColor = getRandBetween(8000000,16777215);
var minColor = getRandBetween(0,8000000);
var oldRing = [0];
var newRing = [0];
console.log("width: " + cX);
console.log("height: " + cY);
function setTextBoxes(){
	$("#startRed").val(startRed);
	$("#endRed").val(endRed);
	$("#startGreen").val(startGreen);
	$("#endGreen").val(endGreen);
	$("#startBlue").val(startBlue);
	$("#endBlue").val(endBlue);
	$("#minDots").val(minDots);
	$("#maxDots").val(maxDots);
	$("#maxR").val(maxR);
	$("#minR").val(minR);
	$("#width").val(c.width);
	$("#height").val(c.height);
	$("#cX").val(cX);
	$("#cY").val(cY);
	$("#size").val(size);
}
randomize();
setTextBoxes();

function redraw(){
	oldRing = [0];
	newRing = [0];
	startRed = Math.floor($("#startRed").val());
	endRed =  Math.floor($("#endRed").val());
	startGreen =  Math.floor($("#startGreen").val());
	endGreen =  Math.floor($("#endGreen").val());
	startBlue =  Math.floor($("#startBlue").val());
	endBlue = Math.floor($("#endBlue").val());
	minDots = Math.floor($("#minDots").val());
	maxDots = Math.floor($("#maxDots").val());
	maxR = Math.floor($("#maxR").val());
	minR = Math.floor($("#minR").val());
	c.width = Math.floor($("#width").val());
	c.height = Math.floor($("#height").val());
	cX = Math.floor($("#cX").val());
	cY = Math.floor($("#cY").val());
	size = Math.floor($("#size").val());
	
	console.log("redrawing, startRed: " + typeof(startRed));
	g.fillStyle = 'black';
	g.fillRect(0,0,10000,10000);
	oldR = 1;
	newR = 1;
}

function getRandBetween(low,high){
	return Math.floor(Math.random() * (high - low + 1)) + low;
}

function pMoveTo(r,theta){
	theta = theta * Math.PI/180;
	var x = r * Math.cos(theta) + cX;
	var y = r * Math.sin(theta) + cY;
	g.moveTo(x, y);
	g.stroke();
}

function pLineTo(r,theta){
	theta = theta * Math.PI/180;
	var x = r * Math.cos(theta) + cX;
	var y = r * Math.sin(theta) + cY;
	g.lineTo(x, y);
	g.stroke();
}

function findClosestDots(theta){
	var minInd = 0;
	for(var i = 0; i < oldRing.length; i++){
		if(Math.abs(theta - oldRing[i]) < Math.abs(theta - oldRing[minInd])){
			minInd = i;
		}
	}
	var ret = [minInd];
	var change = 1;
	var currInd = minInd;
	for(var i = 1; i < numConnections; i++){
		currInd += change;
		if(currInd < 0){
			currInd += oldRing.length;
		}else if(currInd > oldRing.length - 1){
			currInd -= oldRing.length;
		}
		ret[i] = currInd;
		if(change < 0){
			change -= 1
		}else{
			change += 1;
		}
		change *= -1;
	}
	return ret;
}

function getHex(num){
	num = Math.floor(num)
	if(num < 0){
		return "00";
	}else if(num > 255){
		return "FF";
	}else{
		var ret = num.toString(16);
		if(ret.length < 2){
			ret = "0" + ret;
		}
		return ret;
	}
}

function randomize(){
	oldRing = [0];
	newRing = [0];
	startRed = getRandBetween(-255,255);
	endRed =  getRandBetween(-255,255);
	startGreen =  getRandBetween(-255,255);
	endGreen =  getRandBetween(-255,255);
	startBlue =  getRandBetween(-255,255);
	endBlue = getRandBetween(-255,255);
	minDots = getRandBetween(2,60);
	maxDots = getRandBetween(0,1000);
	minR = getRandBetween(4,100);
	maxR = minR + getRandBetween(0,500);
	cX = getRandBetween(-100,c.width + 100);
	cY = getRandBetween(-100,c.height+100);
	size = getRandBetween(600,3000);
	
	console.log("redrawing, startRed: " + typeof(startRed));
	g.fillStyle = 'black';
	g.fillRect(0,0,10000,10000);
	oldR = 1;
	newR = 1;
	setTextBoxes();
}

function setColor(){
	var percent = newR / size;
	hex = '#' + hex;
	var blue = getHex((((endBlue - startBlue) * percent) + startBlue));
	var red = getHex((((endRed - startRed) * percent) + startRed));
	var green = getHex((((endGreen - startGreen) * percent) + startGreen));
	//var green = Math.floor(((endGreen - startGreen) * percent) + startGreen).toString(16);
	//var red = Math.floor(((endRed - startRed) * percent) + startRed).toString(16);
	var hex = "#" + red + green + blue;
	console.log(hex);
	g.fillStyle = hex;
}

function drawRing(){
	console.log("drawing Ring");
	oldRing = [-1];
	oldRing = newRing;
	newRing = [-1];
	oldR = newR;
	newR += getRandBetween(minR,maxR);
	var numDots = getRandBetween(minDots,maxDots);
	var inc = 360/numDots;
	var offset = getRandBetween(0,359);
	pMoveTo(newR,offset);
	while(numDots > 0){
		var thisTheta = ((inc * numDots) + offset) % 360;
		newRing[numDots - 1] = thisTheta;
		pMoveTo(newR,thisTheta);
		var otherLines = findClosestDots(thisTheta);
		for(var k = 0; k < otherLines.length - 1; k++){
			setColor();
			g.beginPath();
			pLineTo(oldR,oldRing[otherLines[k]]);
			pLineTo(oldR,oldRing[otherLines[k + 1]]);
			pLineTo(newR,thisTheta);
			g.fill();
		}
		numDots--;
	}
	//pLineTo(newR,offset);
}
g.fillRect(0,0,10000,10000);
var Main = function(){
	if(newR < size){
		drawRing();
	}
	requestAnimationFrame(Main);
}
Main();

document.addEventListener("keydown", function(e) {
	if(e.keyCode == 72){
		$("#config").toggle();
		console.log("h");
	}
});
