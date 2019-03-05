const Graph = require('./graph.js');
const GraphUI = require('./graphUI.js');
const Set = require('./set.js');

///////////////////////////////////
//////// Data

var graphNodes = {
    "a": {
        "text": "do all the things plz",
        "x": 100,
        "y": 100,
        "parents": Set.empty(),
        "children": Set.fromArray([
            "b", "c",
        ]),
        "subgraph": new Graph({}, "", Set.empty()),
    },
    "b": {
        "text": "TODO: woohoo!",
        "x": 150,
        "y": 200,
        "parents": Set.fromArray(["a"]),
        "children": Set.empty(),
        "subgraph": new Graph({}, "", Set.empty()),
    },
    "c": {
        "text": "today I frink",
        "x": 100,
        "y": 150,
        "parents": Set.fromArray(["a"]),
        "children": Set.empty(),
        "subgraph": new Graph({}, "", Set.empty()),
    },
    "d": {
        "text": "shopping list: ka-pow!",
        "x": 200,
        "y": 250,
        "parents": Set.empty(),
        "children": Set.empty(),
        "subgraph": new Graph({}, "", Set.empty()),
    },
};

var graph = new Graph(graphNodes, "a", Set.fromArray(["b"]));
var graphUI = new GraphUI(graph);


///////////////////////////////////
//////// Main

graphUI.update();

// TODO: remove debugging hackz
var purs = require('./purescript/output/Main/index.js');
window.purs = purs;
window.graph= graph;
window.Set = Set;
