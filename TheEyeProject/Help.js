var HelpData = (function(tName,tDescription){
    var name = tName;
    var description = tDescription;
    var active = false;
    return{
        name: function(){
            return name;
        },
        description: function(){
            return description;
        },
        setActive: function(flag){
            active = flag;
        },
        active: function(){
            return active;
        }

    }
});

function getHelpDataArray(){
    return [
        HelpData("Toggle Help","Press H to toggle tutorial"),
        HelpData("Toggle Color Chooser","Press C to toggle the Color Painter, located on the bottom right."),

        HelpData("Create an Eye","Click a blank area to create a random new Eye there."),
        HelpData("Select an Eye","Click and hold an Eye's pupil or one of its rings to interact with it."),
        HelpData("Change Color","Click in the circle in the bottom right to change the Color Painter's color."),
        HelpData("Delete Latest Eye","Press backspace to delete the most recently created Eye."),

        HelpData("Move","Move your mouse to move the Eye."),
        HelpData("Scale","Press Q or W to scale this Eye up or down."),
        HelpData("Scale Pupil","Press A or S to scale the pupil up or down."),
        HelpData("Add New Ring","Press E to add a new Ring to this Eye."),
        HelpData("Delete Last Ring","Press D to delete the biggest Ring from this Eye."),
        HelpData("Change Inner Vertices","Press Z or X to increase or decrease the number of vertices of the inner edge of the first ring."),
        HelpData("Delete This Eye","Press backspace to delete this Eye."),

        HelpData("Scale Ring","Move your mouse left or right to grow or shrink the size of this ring."),
        HelpData("Delete This Ring","Press D to delete this Ring."),
        HelpData("Change Number of Vertices","Press Q or W to increase or decrease the number of vertices of the outer edge of the selected ring."),
        HelpData("Apply Color","Press A to apply the Color Painter's color to this Ring, shown on the bottom right."),
        HelpData("SampleColor","Press S to sample this Ring's color."),

        HelpData("Change Font Size","You can press O or P to increase or decrease the tutorial's font size."),
        HelpData("Save","Usually, you can save your image by right-clicking it.")
    ];
}
var Help = (function(){
    var helpData = getHelpDataArray();
    helpData[0].setActive(true);
    helpData[1].setActive(true);
    var active = true;
    var fontSize = 40;
    function writeHelp(){
        if(active) {
            var y = fontSize;
            for (var i = 0; i < helpData.length; i++) {

                if (helpData[i].active()) {
                    var x = 20;
                    g.font = fontSize+"px Montserrat";
                    g.fillStyle = 'black';
                    g.scale(.5,.5);
                    g.fillText(helpData[i].name()+": "+helpData[i].description(), x, y);
                    g.strokeStyle = 'white';
                    g.strokeText(helpData[i].name()+": "+helpData[i].description(), x, y);
                    g.scale(2,2);
                    y += (1.5*fontSize);
                }
            }
        }
    }
    function setHelpData(name,state){
        if (active) {
            for (var i = 0; i < helpData.length; i++) {
                if (helpData[i].name() == name) {
                    helpData[i].setActive(state);
                }else{

                }
            }
        }
        writeHelp();
    }
    return {
        setHelpData: function(name,state) {
            setHelpData(name,state);
        },
        enable: function(){
            active = true;
        },
        disable: function(){
            active = false;
        },
        toggle: function(){
            active = !active;
        },
        writeHelp: function(){
            writeHelp();
        },
        increaseFont: function(){
            fontSize *= 1.2;
        },
        decreaseFont: function(){
            fontSize /= 1.2;
            fontSize = Math.round(fontSize);
        },
        setGroupOne: function(flag){
            setHelpData("Toggle Help",flag);
            setHelpData("Toggle Color Chooser",flag);
        },
        setGroupTwo: function(flag){
            setHelpData("Create an Eye",flag);
            setHelpData("Select an Eye",flag);
            setHelpData("Change Color",flag);
            setHelpData("Delete Latest Eye",flag);
        },
        setGroupThree: function(flag){
            setHelpData("Move",flag);
            setHelpData("Scale",flag);
            setHelpData("Scale Pupil",flag);
            setHelpData("Add New Ring",flag);
            setHelpData("Delete Last Ring",flag);
            setHelpData("Change Inner Vertices",flag);
            setHelpData("Delete This Eye",flag);
        },
        setGroupFour: function(flag){
            setHelpData("Scale Ring",flag);
            setHelpData("Delete This Ring",flag),
            setHelpData("Change Number of Vertices",flag);
            setHelpData("Apply Color",flag);
            setHelpData("SampleColor",flag);
        },
        setGroupFive: function(flag){
            setHelpData("Change Font Size",flag);
            setHelpData("Save",flag);
        }
    }

});
