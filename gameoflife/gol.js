var c = document.getElementById("mainCanvas");
var ctx = c.getContext("2d");

var colors = {
    cell : {
        live : '#FF0000',
        dead : '#FFFFFF'
    }
}
var liveCellList = {};
var cellListNeighbors = {};
var state = {running: false, runTimer: null};

var CellList = {
    addCell : function(xpos, ypos) {
        if (liveCellList[xpos] == undefined) {
            liveCellList[xpos] = [ypos];  
        } else {
            liveCellList[xpos].push(ypos);
        }
    },
    delCell : function(xpos, ypos) {
        var idx = liveCellList[xpos].indexOf(ypos);
        liveCellList[xpos].splice(idx, 1);
    },
    isLive : function(xpos, ypos) {
        if (liveCellList[xpos] == undefined) {
            return false;
            // Cell is in list
        } else if (liveCellList[xpos].indexOf(ypos) != -1) {
            return true;
        } else {
            return false;
        }
    },
    drawCells : function(){
        for (i in liveCellList) {
            for (j in liveCellList[i]) {
                ctx.fillStyle = colors.cell.live;
                ctx.fillRect(i, liveCellList[i][j], 10, 10);
            }
        }
    }
}

var examples = {
    glider : function() {
        console.log("glider called");
        reset();
        liveCellList = { "430": ["160"], "440": ["170"], "450": ["150", "160", "170"]};
        CellList.drawCells();
    },
    gliderGun: function() {
        console.log("glider gun called");
        reset();
        liveCellList = {"240": ["160", "170"], "250": ["160", "170"], "340": ["160", "170", "180"], "350": ["150", "190"], "360": ["140", "200"], "370": ["140", "200"], "380": ["170"], "390": ["150", "190"], "400": ["160", "170", "180"], "410": ["170"], "440": ["140", "150", "160"], "450": ["140", "150", "160"], "460": ["130", "170"], "480": ["120", "130", "170", "180"], "580": ["140", "150"], "590": ["140", "150"]}
        CellList.drawCells();
    },
    pulsar: function() {
        console.log("pulsar called");
        reset();
        liveCellList= {"380": ["180", "240"], "390":["180", "240"], "400": ["180", "190", "230", "240"], "420": ["140", "150", "160", "190", "200", "220", "230", "260", "270", "280"], "430": ["160", "180", "200", "220", "240", "260"], "440":["180", "190", "230", "240"], "460": ["180", "190", "230", "240"], "470": ["160", "180", "200", "220", "240", "260"], "480": ["140", "150", "160", "190", "200", "220", "230", "260", "270", "280"], "500": ["180", "190", "230", "240"], "510": ["180", "240"], "520": ["180","240"]};
        CellList.drawCells();
    },
    stillLife: function() {
        console.log("still life called");
        reset();
        liveCellList = {"160": ["290", "300"], "170": ["100", "200", "280", "310"], "180": ["90", "110", "190", "210", "280", "310"], "190": ["90", "110", "200", "290", "300"], "200": ["100"], "280": ["180", "190", "290"], "290": ["100", "180", "280", "300"], "300": ["90", "110", "190", "200", "210", "290", "310"], "310": ["90", "120", "210", "310", "300"], "320": ["100", "110"], "380": ["280", "290"], "390": ["180", "190", "270", "300"], "400": ["180", "270", "280", "300"], "410": ["100", "110", "200", "280", "300", "310"], "420": ["100", "110", "190", "200", "280", "310"], "430": ["290", "300"], "490": ["180", "190", "280"], "500": ["180", "200", "270", "290"], "510": ["90", "100", "200", "270", "300"], "520": ["90", "110", "190", "280", "300"], "530": ["100", "190", "200", "290"], "600": ["170", "180", "200", "210", "280"], "610": ["100", "110", "180", "200", "270", "290"], "620": ["90", "110", "180", "200", "280", "300"], "630": ["90", "100", "170", "180", "200", "210", "290"]}
        CellList.drawCells();
    },
    oscillator: function() {
        console.log("oscillators");
        reset();
        liveCellList = {"110": ["180", "190"], "120": ["180", "190"], "130": ["40"], "140": ["30", "50", "180", "190", "200", "210", "360"], "150": ["170", "190", "220", "240", "250", "350", "360"], "160": ["20", "30", "70", "170", "210", "220", "240", "250", "350", "400"], "170": ["20", "80", "140", "150", "170", "200", "220", "360", "370", "380", "400"], "180": ["50", "70", "140", "150", "170", "220"], "190": ["40", "50", "180", "190", "200", "210", "360", "370", "380", "400"], "200": ["350", "400"], "210": ["200", "210", "350", "360"], "220": ["200", "210", "360"], "370": ["50"], "380": ["50", "330", "340"], "390": ["40", "50", "60", "160", "220", "330", "350", "430", "440"], "400": ["160", "220", "350", "440"], "410": ["160", "170", "210", "220", "360", "420", "430"], "420": ["40", "50", "60", "370", "410"], "430": ["50", "120", "130", "140", "170", "180", "200", "210", "240", "250", "260", "370", "390", "400"], "440": ["50", "140", "160", "180", "200", "220", "240"], "450": ["50", "160", "170", "210", "220", "360", "370", "390"], "460": ["50", "350", "390"], "470": ["40", "50", "60", "160", "170", "210", "220", "330", "340", "400"], "480": ["140", "160", "180", "200", "220", "240", "320", "410"], "490": ["120", "130", "140", "170", "180", "200", "210", "240", "250", "260", "320", "330", "410", "430"], "500": ["40", "50", "60", "420", "430"], "510": ["50", "160", "170", "210", "220"], "520": ["50", "160", "220"], "530": ["160", "220"], "660": ["50", "60", "70", "200", "210"], "670": ["90", "190", "220", "360"], "680": ["40", "90", "180", "230", "350", "370"], "690": ["80", "170", "240", "340", "380"], "700": ["50", "60", "170", "240", "350", "390"], "710": ["50", "180", "230", "360", "400"], "720": ["50", "190", "220", "370", "410"], "730": ["50", "200", "210", "380", "400"], "740": ["390"]}
        CellList.drawCells();
    }
}

function printList(){
    console.log(liveCellList);
}

c.addEventListener("mousedown",selectCell, false);

function getMousePos(c, evt) {
    var rect = c.getBoundingClientRect();
    return {x: evt.clientX - rect.left, y: evt.clientY - rect.top};
}

function selectCell(event) {
    var mousePos = getMousePos(c,event);
    var xsq = String((mousePos.x/10 >> 0) * 10);
    var ysq = String((mousePos.y/10 >> 0) * 10);
    // console.log("GRID: ", xsq, ysq);

    if (!CellList.isLive(xsq,ysq)) {
        // console.log("Selected was DEAD");
        ctx.fillStyle = colors.cell.live;
        CellList.addCell(xsq,ysq);
    } else {
        // console.log("Selected was ALIVE");
        ctx.fillStyle = colors.cell.dead;
        CellList.delCell(xsq,ysq);
    }

    // fill in the rectangle
    ctx.fillRect(xsq, ysq, 10, 10);
    // console.log(xsq,ysq);
}

function playToggle() {
    // If it is not running, start the running
    if (state.running == false) {
        state.running = true;
        state.runTimer = setInterval(nextState, 50);
        document.getElementById("play").innerHTML = "Pause";
        console.log("running", state.running);
    } else {
        clearInterval(state.runTimer);
        state.running = false;
        document.getElementById("play").innerHTML = "Run";
        console.log("interval cleared", state.running);
    }
}

function reset() {
    state.running = false;
    document.getElementById("play").innerHTML = "Run";
    clearInterval(state.runTimer);
    for (i in liveCellList) {
        for (j in liveCellList[i]) {
            console.log(i, liveCellList[i][j]);
            ctx.fillStyle = colors.cell.dead;
            ctx.fillRect(i, liveCellList[i][j], 10, 10);
        }
    }
    liveCellList = {};
    cellListNeighbors = {};
}
    
function nextState() {
    //Take live cell list, loop through all live cells.
    // chnages cellListNeighbors. Everything starts with 0.
    // add 10 to itself, and 1 to neighbors.
    // liveCellList = {'1': ['1']}
    // { 2: {1:5, 3:12}, 3:{0:1, 3:10}}
    // Creates
    var value = 0;
    var key = '';
    var delta = [-10, 0, 10]
    cellListNeighbors = {};
    for (var i in liveCellList) {
        for (var j = 0; j < liveCellList[i].length; j++) {
            for (var k = 0; k < 3; k++){
                for (var l = 0; l < 3; l++){
                    // if cell is alive, use value 10 instead
                    value = (delta[k] == 0 && delta[l] == 0) ? 10 : 1;
                    // if it doesn't exist yet should equal 10
                    if (cellListNeighbors[String(Number(i) + delta[k])] == undefined) {
                        // console.log(String(Number(i) + delta[k]));
                        // console.log("doesn't exist");
                        // console.log(String(Number(i) + delta[k]) + " is assigned " + key + " and " + value);
                        cellListNeighbors[String(Number(i) + delta[k])] = {};
                        cellListNeighbors[String(Number(i) + delta[k])][String(Number(liveCellList[i][j]) + delta[l])] = value;
                    }
                    // if first exists AND second doesn't either
                    else if (cellListNeighbors[String(Number(i) + delta[k])][String(Number(liveCellList[i][j]) + delta[l])] == undefined){
                        // console.log(String(Number(i) + delta[k]), Number(liveCellList[i][j]) + delta[l]);
                        // console.log("first exist, second doesn't");
                        // console.log(Number(i) + delta[k], Number(liveCellList[i][j]) + delta[l])
                        cellListNeighbors[String(Number(i) + delta[k])][String(Number(liveCellList[i][j]) + delta[l])] = value;
                    } 
                    // if it does exist, then add 10 to existing.
                    else {
                        cellListNeighbors[String(Number(i) + delta[k])][String(Number(liveCellList[i][j]) + delta[l])] += value;
                    }
                }
            }
        }
    }
    // Loop through the cellListNeighbors and render the world.
    for (i in cellListNeighbors) {
        for (j in cellListNeighbors[i]){
            // if live cell has more than 3 neighbors OR less than 2 neighbors you die
            if (cellListNeighbors[i][j] > 13 || 
                (cellListNeighbors[i][j] < 12 && cellListNeighbors[i][j] > 9)) {
                ctx.fillStyle = colors.cell.dead;
                CellList.delCell(i, j);
                ctx.fillRect(i, j, 10, 10);

            // if not alive, and have exactly three neighbors, a cell is born
            } else if (cellListNeighbors[i][j] == 3) {
                ctx.fillStyle = colors.cell.live;
                CellList.addCell(i, j);
                ctx.fillRect(i, j, 10, 10);
            }
        }
    }
}


// Sandbox
var timer;
function hello() {
    if (!timer) {
        timer = setInterval(repeat, 50);
    } else {
        clearInterval(timer);
        timer = null;
    }
}

function repeat() {
    console.log("Hello");
}



