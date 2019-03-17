const Graph = require('./graph.js');
const GraphUI = require('./graphUI.js');
const StringSet = require('./stringSet.js');
const Utils = require('./utils.js');

///////////////////////////////////
//////// Data

var graphNodes = {
    "a": {
        "id": "a",
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
        "id": "b",
        "text": "TODO: woohoo!",
        "x": 150,
        "y": 200,
        "parents": StringSet.fromArray(["a"]),
        "children": StringSet.empty(),
        "subgraphNodes": {},
    },
    "c": {
        "id": "c",
        "text": "today I frink",
        "x": 100,
        "y": 150,
        "parents": StringSet.fromArray(["a"]),
        "children": StringSet.empty(),
        "subgraphNodes": {},
    },
    "d": {
        "id": "d",
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

function copyPursGraph(pursGraph) {
    return new Graph(Utils.deepCopyObject(pursGraph.nodes),
                     pursGraph.focusNode,
                     pursGraph.highlightedNodes);
};


///////////////////////////////////
//////// Main

graphUI.update();

// TODO: remove debugging hackz
var Purs = require('./purescript/output/Main/index.js');
window.Purs = Purs;
window.graphUI = graphUI;
window.StringSet = StringSet;

window.graphUI.graph = copyPursGraph(Purs.demo);
window.graphUI.update();
window.graphUI.graph.newNodeBelowFocus();
window.graphUI.update();
window.graphUI.graph.usePursGraph();
window.graphUI.update();
window.graph = window.graphUI.graph;
