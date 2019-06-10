//const Graph = require('./graph.js');
const GraphUI = require('./graphUI.js');
const StringSet = require('./stringSet.js');
const Utils = require('./utils.js');
var PursCore = require('./purescript/output/Workflow.Core/index.js');
var PursInteraction = require('./purescript/output/Workflow.Interaction/index.js');
var Purs = {...PursCore, ...PursInteraction};
var Graphputer = require('./purescript/output/Graphputer.Core/index.js');
var GraphputerParser = require('./purescript/output/Graphputer.Parser/index.js');


///////////////////////////////////
//////// Data

var graphNodes = {
    "a": {
        "id": "a",
        "text": "do all the things plz",
        "valid": true,
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
        "valid": true,
        "x": 150,
        "y": 200,
        "parents": StringSet.fromArray(["a"]),
        "children": StringSet.empty(),
        "subgraphNodes": {},
    },
    "c": {
        "id": "c",
        "text": "today I frink",
        "valid": true,
        "x": 100,
        "y": 150,
        "parents": StringSet.fromArray(["a"]),
        "children": StringSet.empty(),
        "subgraphNodes": {},
    },
    "d": {
        "id": "d",
        "text": "shopping list: ka-pow!",
        "valid": true,
        "x": 200,
        "y": 250,
        "parents": StringSet.empty(),
        "children": StringSet.empty(),
        "subgraphNodes": {},
    },
};

var graph = Purs.Graph({"nodes": graphNodes,
                        "focus": Purs.FocusNode.create("a"),
                        "highlighted": StringSet.fromArray(["b"])});
var graphUI = new GraphUI(graph);

//function copyPursGraph(pursGraph) {
//    return new Graph(Utils.deepCopyObject(pursGraph.nodes),
//                     pursGraph.focus,
//                     pursGraph.highlighted);
//};


///////////////////////////////////
//////// Main

graphUI.update();

// TODO: remove debugging hackz
window.Purs = Purs;
window.graphUI = graphUI;
window.StringSet = StringSet;
window.Graphputer = Graphputer;
window.GraphputerParser = GraphputerParser;

window.graphUI.registerNodeValidHook(function (graph, node) {
    var valid = window.GraphputerParser.canParseNodeText(node.text.trim());
    console.log(node.text, valid);
    return {"isValid": valid,
            "allow": true};
});

window.graphUI.registerEdgeValidHook(function (graph, sourceNode, targetNode) {
    var valid =
        window.GraphputerParser.canCompose(
            targetNode.text.trim())(
            sourceNode.text.trim());
    return {"isValid": valid,
            "allow": true};
});

//window.copyPursGraph = copyPursGraph;
window.graphUI.graph = Purs.demo;
window.graphUI.update();
//window.graphUI.graph.newNodeBelowFocus();
//window.graphUI.update();
//window.graphUI.graph.usePursGraph();
window.graphUI.update();


//const savedGraph = require("./Workflow.workflow-graph_v0.0_2019-04-05T02_32_11.381Z.json");
//console.log(JSON.stringify(savedGraph));
//console.log(window.graphUI.graph.nodes);
//window.graphUI.loadGraph(JSON.stringify(savedGraph));
//window.graphUI.update();
//console.log(window.graphUI.graph.nodes);

window.graph = window.graphUI.graph;
