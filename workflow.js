const Graph = require('./graph.js');
const GraphUI = require('./graphUI.js');
const StringSet = require('./stringSet.js');

///////////////////////////////////
//////// Data

var graphNodes = {
    "a": {
        "text": "do all the things plz",
        "x": 100,
        "y": 100,
        "parents": StringSet.empty(),
        "children": StringSet.fromArray([
            "b", "c",
        ]),
        "subgraphNodes": {},
    },
    "b": {
        "text": "TODO: woohoo!",
        "x": 150,
        "y": 200,
        "parents": StringSet.fromArray(["a"]),
        "children": StringSet.empty(),
        "subgraphNodes": {},
    },
    "c": {
        "text": "today I frink",
        "x": 100,
        "y": 150,
        "parents": StringSet.fromArray(["a"]),
        "children": StringSet.empty(),
        "subgraphNodes": {},
    },
    "d": {
        "text": "shopping list: ka-pow!",
        "x": 200,
        "y": 250,
        "parents": StringSet.empty(),
        "children": StringSet.empty(),
        "subgraphNodes": {},
    },
};

var graph = new Graph(graphNodes, "a", StringSet.fromArray(["b"]));
var graphUI = new GraphUI(graph);


///////////////////////////////////
//////// Main

graphUI.update();

// TODO: remove debugging hackz
var purs = require('./purescript/output/Main/index.js');
window.purs = purs;
window.graph = graph;
window.graphUI = graphUI;
window.StringSet = StringSet;
