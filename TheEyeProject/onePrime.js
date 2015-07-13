/*
	This program written by Bryan Schrock in March 2015
*/
var config = document.getElementById("config");
$("#config").toggle();
var c = document.getElementById("wholePage");
var colorWheel = document.getElementById("colorButton");
var g = c.getContext('2d');
c.width  = window.innerWidth;
c.height = window.innerHeight;
var tempC = getRandomColorHex();
var colorPainter = ColorPainter(c.width - 60, c.height - 60,tempC);
colorPainter.updateColor(tempC);
$('#colorButton').on('input', colorOkFunction);
var help = Help();
help.setGroupFive(true);
help.setGroupTwo(true);

function colorOkFunction(){
    colorPainter.updateColor($("#colorButton").val());
    drawAll();
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
function getRandBetween(low,high){
    return Math.floor(Math.random() * (high - low + 1)) + low;
}
function getRandomColorHex(){
    var blue = getHex(getRandBetween(10,255));
    var red = getHex(getRandBetween(10,255));
    var green = getHex(getRandBetween(10,255));
    return "#" + red + green + blue;
}
var Eye = (function(x,y,r){
    var doneFlag = false;
	var radii = [];
	var cX = (x == undefined ? getRandBetween(0, c.width) : x);
	var cY = (y == undefined ? getRandBetween(0, c.height): y);
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
	var ringData = [];
	var centerSelected = false;
	var selectedRadius = -1;
	var selectedX = -1;
    var pupilR = -1;
	varselectedY =-1;

	randomize();

	

	
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
	
	function findClosestDots(theta,oldRing){
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

    function getDecimal(hexNum){
        var temp = parseInt(hexNum,16);
        //console.log("decimal returned is "+temp)
        return temp;
    }
	
	function randomize(){
		oldRing = [0];
		newRing = [0];
		startRed = getRandBetween(5,255);
		endRed =  getRandBetween(20,255);
		startGreen =  getRandBetween(5,255);
		endGreen =  getRandBetween(20,255);
		startBlue =  getRandBetween(5,255);
		endBlue = getRandBetween(20,255);
		minDots = getRandBetween(2,60);
		maxDots = getRandBetween(0,1000);
		minR = getRandBetween(4,100);
		maxR = minR + getRandBetween(0,500);
		oldR = 1;
		newR = 1;
	}
	
	function setColor(percent){
		hex = '#' + hex;
		var blue = getHex((((endBlue - startBlue) * percent) + startBlue));
		var red = getHex((((endRed - startRed) * percent) + startRed));
		var green = getHex((((endGreen - startGreen) * percent) + startGreen));
		//var green = Math.floor(((endGreen - startGreen) * percent) + startGreen).toString(16);
		//var red = Math.floor(((endRed - startRed) * percent) + startRed).toString(16);
		var hex = "#" + red + green + blue;
		//console.log(hex);
		g.fillStyle = hex;
        return hex;
	}
    function getNewRing(inc,counter,offset){
        var ret = [];
        while(counter > 0) {
            var thisTheta = ((inc * counter) + offset) % 360;
            ret[counter - 1] = thisTheta;
            counter--;
        }
        return ret;

    }
	function drawRing(){
		//console.log("drawing Ring");
		oldRing = [-1];
		oldRing = newRing;
		newRing = [-1];
		oldR = newR;
		newR += getRandBetween(minR,maxR);
		//console.log("newR is " + newR +" right now");
        if(oldR != 1) {
            radii.push(newR);

            var numDots = getRandBetween(minDots, maxDots);
            var inc = 360 / numDots;
            var offset = getRandBetween(0, 359);
            pMoveTo(newR, offset);
            g.strokeStyle = 'black';
            var addedColor = -1;
            var counter = numDots;
            var otherLines = [];
            var counter2 = counter;
            newRing = getNewRing(inc, counter2, offset);
            while (counter > 0) {
                pMoveTo(newR, newRing[counter - 1]);
                otherLines[counter] = findClosestDots(newRing[counter - 1], oldRing);
                var otherLinesPrime = otherLines[counter];
                for (var k = 0; k < otherLinesPrime.length - 1; k++) {
                    addedColor = setColor(newR / size);
                    g.lineWidth = 0;
                    g.beginPath();
                    pLineTo(oldR, oldRing[otherLinesPrime[k]]);
                    pLineTo(oldR, oldRing[otherLinesPrime[k + 1]]);
                    pLineTo(newR, newRing[counter - 1]);
                    g.strokeStyle = 'black';
                    g.lineWidth = 0;
                    g.fill();
                }
                counter--;
            }
            var assignedColor = radii.length - 1 == 0 || newR > size;
            ringData.push({
                newR: newR,
                oldR: oldR,
                numDots: numDots,
                offset: offset,
                oldRing: oldRing,
                color: addedColor,
                assignedColor: assignedColor,
                otherLines: otherLines
            });
            //pLineTo(newR,offset);
        }else {
            pupilR = newR;
        }
	}
	function redrawAll(){
		g.fillStyle = 'black';
		g.fillRect(0,0,10000,10000);
		for(var i=0; i<eyes.length; i++){
			eyes[i].draw();
		}
        help.writeHelp();
	}
		
	function draw(){	
		//console.log("draw?");
		for(var i=0; i<ringData.length; i++){
			//console.log("trying to redraw ring number "+i);
			var r = ringData[i];
			var numDots = r.numDots;
			//console.log("numDots = "+numDots);
			var newR = r.newR;
			var oldR = r.oldR;
			var oldRing = r.oldRing;
			var offset = r.offset;
			var inc = 360/numDots;
            g.strokeStyle = 'black';
			pMoveTo(newR,offset);
            var counter = numDots;
            var newRing = getNewRing(inc,counter,offset);
			while(numDots > 0){
                var otherLinesPrime = (r.otherLines == undefined ? undefined : r.otherLines[numDots]);
                otherLinesPrime = (otherLinesPrime == undefined ? findClosestDots(newRing[numDots-1],oldRing) : otherLinesPrime);
				pMoveTo(newR,newRing[numDots-1]);
				for(var k = 0; k < otherLinesPrime.length - 1; k++){
                    g.fillStyle = r.color;
                    g.strokeStyle = 'black';
                    g.lineWidth = 0;
					g.beginPath();
					pLineTo(oldR,oldRing[otherLinesPrime[k]]);
					pLineTo(oldR,oldRing[otherLinesPrime[k + 1]]);
					pLineTo(newR,newRing[numDots-1]);
					g.fill();
				}
				numDots--;
			}
            if(i + 1 < ringData.length){
                var ring = ringData[i + 1];
                ringData[i+1] = {newR:ring.newR,oldR:ring.oldR,numDots:ring.numDots,offset:ring.offset,oldRing:newRing,color:ring.color,assignedColor:ring.assignedColor,otherLines:ring.otherLines};
            }
		}
	}
		
	function distanceTo(otherX,otherY){
		var diffX = otherX - cX;
		var diffY = otherY - cY;
		//console.log("otherY = "+otherY);
		var dist = Math.sqrt(((diffX)*(diffX)) + ((diffY)*(diffY)));	
		return dist;
	}
    function calculateColor(percent,startGreen,endGreen,startRed,endRed,startBlue,endBlue){
        var blue = getHex((((endBlue - startBlue) * percent) + startBlue));
        var red = getHex((((endRed - startRed) * percent) + startRed));
        var green = getHex((((endGreen - startGreen) * percent) + startGreen));
        var hex = "#" + red + green + blue;
        return hex;
    }
    function setRingColorsFrom(ind){
        var sRed = -1;
        var eRed = -1;
        var sGreen = -1;
        var eGreen = -1;
        var sBlue = -1;
        var eBlue = -1;
        var stopTop = -1;
        var stopBottom = -1;
        for(var i=ind+1; i < ringData.length;i++){
            if(ringData[i].assignedColor){
                var startColor = ringData[ind].color;
                var endColor = ringData[i].color;
                sRed = getDecimal(startColor.slice(1,3));
                eRed = getDecimal(endColor.slice(1,3));
                sGreen = getDecimal(startColor.slice(3,5));
                eGreen = getDecimal(endColor.slice(3,5));
                sBlue = getDecimal(startColor.slice(5,7));
                eBlue = getDecimal(endColor.slice(5,7));
                //console.log("found assignedColors");
                stopTop = i;
                i = ringData.length;
            }else{
                //console.log("not assignedColor at "+i)
            }
        }
        for(var i=ind+1; i<stopTop; i++){
            var temp = calculateColor(ringData[i].newR/size,sGreen,eGreen,sRed,eRed,sBlue,eBlue);
            //console.log(temp);
            ringData[i].color = temp;
        }
        for(var i=ind-1; i >= 0;i--){
            if(ringData[i].assignedColor){
                var endColor = ringData[ind].color;
                var startColor = ringData[i].color;
                sRed = getDecimal(startColor.slice(1,3));
                eRed = getDecimal(endColor.slice(1,3));
                sGreen = getDecimal(startColor.slice(3,5));
                eGreen = getDecimal(endColor.slice(3,5));
                sBlue = getDecimal(startColor.slice(5,7));
                eBlue = getDecimal(endColor.slice(5,7));
                stopBottom = i;
                i = 0;
            }
        }
        for(var i=ind-1; i > stopBottom;i--){
            var temp = calculateColor(ringData[i].newR/size,sGreen,eGreen,sRed,eRed,sBlue,eBlue);
           // console.log(temp);
            ringData[i].color = temp;
        }
    }
	return{
		drawRing: function(){
			if(newR < size && !doneFlag){
				drawRing();
			}else{
                doneFlag = true
                drawAll();
            }
		},
		draw: function(){
			draw();
		},
		testMouse: function(x,y){
			return !(distanceTo(x,y) > radii[radii.length-1]);
			//tests if click is inside this Eye.
		},
		handleMouseDown: function(x,y){
			var dist = distanceTo(x,y) < pupilR;
            selectedX = x;
            selectedY = y;
			//console.log("dist = "+distanceTo(x,y));
			if(dist){
				centerSelected = true;
                help.setGroupThree(true);
                help.writeHelp();
				//console.log("selected an Eye of size: "+size+", and "+radii.length+" rings.");
			}else{
                help.setGroupFour(true);
                help.writeHelp();
						//console.log("selcted a radius");
				for(var i=0; i<radii.length; i++){
					if(distanceTo(x,y) < radii[i]){
						selectedRadius = i;
						i = radii.length;
					}
				}
			}
						
		},
		handleMouseMove: function(x,y){
			if(centerSelected){
				//console.log("moving to "+x+", "+y);
				cX += x - selectedX;
				cY += y - selectedY;
                selectedX = x;
                selectedY = y;
			}else if(selectedRadius != -1) {
                var diffX = x - selectedX;
                selectedX = x;
                if (ringData[selectedRadius].newR + diffX > ringData[selectedRadius].oldR + 20) {
                    ringData[selectedRadius].newR += diffX;
                    radii[selectedRadius] += diffX;
                    for (var i = selectedRadius + 1; i < ringData.length; i++) {
                        var ring = ringData[i];
                        ring.oldR += diffX;
                        ring.newR += diffX;
                        ring.otherLines = undefined;
                        radii[i] += diffX;
                    }
                    size += diffX;
                }
			}
			drawAll();
		},
		handleQ: function(){
			if(selectedRadius != -1){
				ringData[selectedRadius].numDots = Math.floor(ringData[selectedRadius].numDots * 1.2);
                ringData[selectedRadius].otherLines = undefined;
                if (selectedRadius + 1 < ringData.length) {
                    ringData[selectedRadius + 1].otherLines = undefined;
                }
				
			}else if(centerSelected){
                pupilR *= 1.2;
                for(var i=0; i<ringData.length; i++){
                    var ring = ringData[i];
                    ring.oldR = ring.oldR * 1.2;
                    ring.newR = ring.newR * 1.2;
                    ring.otherLines = undefined;
                }
                for(var i=0; i<radii.length; i++){
                    radii[i] *= 1.2;
                    ringData[i].otherLines = undefined;
                }

            }
			drawAll();
		},
		handleW: function(){
			if(selectedRadius != -1) {
                ringData[selectedRadius].numDots = Math.floor(ringData[selectedRadius].numDots / 1.2);
                ringData[selectedRadius].otherLines = undefined;
                if (selectedRadius + 1 < ringData.length) {
                    ringData[selectedRadius + 1].otherLines = undefined;
                }
				
			}else if(centerSelected){
                pupilR /= 1.2;
                for(var i=0; i<ringData.length; i++){
                    var ring = ringData[i];
                    ring.oldR = ring.oldR / 1.2;
                    ring.newR = ring.newR / 1.2;
                    ring.otherLines = undefined;
                }
                for(var i=0; i<radii.length; i++){
                    radii[i] /= 1.2;
                }
            }
			drawAll();
		},
		unselect: function(){
			selectedX = -1;
			selectedY = -1;
			centerSelected = false;
			selectedRadius = -1;
            help.setGroupThree(false);
            help.setGroupFour(false);
            help.writeHelp();
		},
        handleA: function() {
            if (centerSelected) {
                ringData[0].otherLines = undefined;
                ringData[1].oldR *= 1.2;
                pupilR *= 1.2;
            }else if(selectedRadius != -1){
                ringData[selectedRadius].color = colorPainter.getColor();
                ringData[selectedRadius].assignedColor = true;
                setRingColorsFrom(selectedRadius);
            }
            drawAll();
        },
        handleS: function(){
            if (centerSelected) {
                ringData[0].newR /= 1.2;
                ringData[1].oldR /= 1.2;
                ringData[0].otherLines = undefined;
                ringData[1].otherLines = undefined;
                pupilR /= 1.2;
            }else if(selectedRadius != -1){
                colorPainter.updateColor(ringData[selectedRadius].color);
            }
            drawAll();
        },handleZ: function(){
            if(centerSelected) {
                ringData[0].numDots = Math.round(1.2 * ringData[0].numDots);
                ringData[0].otherLines = undefined;
                ringData[1].otherLines = undefined;
            }
            drawAll();
        },handleX: function(){
            if(centerSelected) {
                ringData[0].numDots = Math.round(ringData[0].numDots / 1.2);
                ringData[0].otherLines = undefined;
                ringData[1].otherLines = undefined;
            }
            drawAll();
        },
        handleE: function(){
          if(centerSelected){
              var oldR2 = ringData[ringData.length-1].newR;
              var newR2 = oldR2 + getRandBetween(minR,maxR);
              var numDots2 = getRandBetween(minDots,maxDots);
              var counter = numDots2;
              var oldRing2 = getNewRing(360/numDots2,counter,ringData[ringData.length-1].offset);
              radii.push(newR2);
              ringData.push({newR:newR2,oldR:oldR2,numDots:numDots2,offset:getRandBetween(0,359),oldRing:oldRing2,color:"white",assignedColor:false,otherLines:undefined})
          }
          drawAll();
        },
        handleD: function(){
          if(centerSelected){
              ringData.splice(ringData.length-1,1);
              radii.splice(radii.length-1,1);
          }else if(selectedRadius != -1){
              var diffX = ringData[selectedRadius].newR - ringData[selectedRadius].oldR;
              ringData.splice(selectedRadius,1);
              radii.splice(selectedRadius,1);
              for (var i = selectedRadius; i < ringData.length; i++) {
                  var ring = ringData[i];
                  ring.oldR -= diffX;
                  ring.newR -= diffX;
                  ring.otherLines = undefined;
                  radii[i] -= diffX;
              }
              size -= diffX;
          }drawAll();
        },
        handleR: function(){
            ringData[0].assignedColor = true;
            ringData[ringData.length-1].assignedColor = true;
          for(var i=1;i<ringData.length-1;i++){
            ringData[i].assignedColor = false;
          }
            drawAll();
        },
        redrawAll: function(){
            redrawAll();
        }
	}
});
var eyes = [];
var selected = undefined;
function fillEyes(num){
	for(var i=0;i<num;i++){
		eyes[i] = Eye();
	}
	
}
fillEyes(1);
drawAll();
var selectedIndex = -1;
var Main = function(){
	for(var i=0;i<eyes.length;i++){
		eyes[i].drawRing();
	}
	requestAnimationFrame(Main);
}
Main();

function drawAll(){
    g.fillStyle = 'black';
    g.fillRect(0,0, c.width, c.height);
    if(eyes.length > 0){
        eyes[0].draw();
        if(eyes.length > 1){
            eyes[0].redrawAll();
        }
    }
    help.writeHelp();
    colorPainter.draw();
}

function redraw(){
    c.width = Math.floor($("#width").val());
    c.height = Math.floor($("#height").val());
    eyes = [];
    g.fillStyle = 'black';
    g.fillRect(0,0, c.width, c.height);

}
document.addEventListener("keydown", function(e) {
	if(e.keyCode == 72){
		help.toggle();
        drawAll();
		//console.log("h");
	}else if(e.keyCode == 81){
		if(selected != undefined){
			selected.handleQ();
		}
	}else if(e.keyCode == 87){
		if(selected != undefined){
			selected.handleW();
		}
	}else if(e.keyCode == 8){
		eyes.splice(selectedIndex,1);
        drawAll();
			
	}else if(e.keyCode == 65){
        if(selected != undefined){
            selected.handleA();
        }
    }else if(e.keyCode == 83){
        if(selected != undefined){
            selected.handleS();
        }
    }else if(e.keyCode == 90){
        if(selected != undefined){
            selected.handleZ();
        }
    }else if(e.keyCode == 88){
        if(selected != undefined){
            selected.handleX();
        }
    }else if(e.keyCode == 79) {
        help.increaseFont();
        drawAll();

    }else if(e.keyCode == 80) {
        help.decreaseFont();
        drawAll();
    }else if(e.keyCode == 67) {
        colorPainter.toggle();
        drawAll();
    }else if(e.keyCode == 69) {
        if(selected != undefined){
            selected.handleE();
        }
    } else if(e.keyCode == 68) {
        if(selected != undefined){
            selected.handleD();
        }
    }else if(e.keyCode == 82){
        if(selected != undefined){
            selected.handleR();
        }
    }else if(e.keyCode == 77) {
        $("#config").toggle();
        $("#wholePage").toggle();
    }else{
		//console.log(e.keyCode);
	}
});
document.addEventListener("mousedown", function(e){
    if(e.which != 3) {
        if(colorPainter.testMouse(e.pageX, e.pageY)){
            $("#colorButton").trigger("click");
        }else {
            for (var i = 0; i < eyes.length; i++) {
                var ret = eyes[i].testMouse(e.pageX, e.pageY);
                if (ret) {
                    selected = eyes[i];
                    help.setGroupTwo(false);
                    selectedIndex = i;
                }
            }
            if (selected != undefined) {
                selected.handleMouseDown(e.pageX, e.pageY);
            } else {
                console.log("should be pushing a new Eye");
                eyes.push(Eye(e.pageX, e.pageY));
            }
            drawAll();
        }
    }
},false);
document.addEventListener("mousemove", function(e){
	if(selected != undefined){
		selected.handleMouseMove(e.pageX,e.pageY);
        pictureChanged = true;
        drawAll();
	}
},false);
document.addEventListener("mouseup", function(e){
	if(selected != undefined){
		selected.unselect();
		selected = undefined;
        help.setGroupTwo(true);
        drawAll();
		selectedIndex = -1;
	}
},false);
