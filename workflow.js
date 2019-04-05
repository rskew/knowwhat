const Graph = require('./graph.js');
const GraphUI = require('./graphUI.js');
const StringSet = require('./stringSet.js');
const Utils = require('./utils.js');
var Purs = require('./purescript/output/Main/index.js');


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

var graph = new Graph(graphNodes, Purs.FocusNode.create("a"), StringSet.fromArray(["b"]));
var graphUI = new GraphUI(graph);

function copyPursGraph(pursGraph) {
    return new Graph(Utils.deepCopyObject(pursGraph.nodes),
                     pursGraph.focus,
                     pursGraph.highlighted);
};


///////////////////////////////////
//////// Main

graphUI.update();

// TODO: remove debugging hackz
window.Purs = Purs;
window.graphUI = graphUI;
window.StringSet = StringSet;

window.copyPursGraph = copyPursGraph;
window.graphUI.graph = copyPursGraph(Purs.demo);
window.graphUI.update();
//window.graphUI.graph.newNodeBelowFocus();
window.graphUI.update();
window.graphUI.graph.usePursGraph();
window.graphUI.update();


const savedGraph = require("./Workflow.workflow-graph_v0.0_2019-04-03T12_36_57.884Z.json");
console.log(JSON.stringify(savedGraph));
console.log(window.graphUI.graph.nodes);
window.graphUI.loadGraph(JSON.stringify(savedGraph));
window.graphUI.update();
console.log(window.graphUI.graph.nodes);

window.graph = window.graphUI.graph;
