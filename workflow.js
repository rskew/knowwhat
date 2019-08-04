const GraphUI = require('./graphUI.js');
const Utils = require('./utils.js');
var PursCore = require('./output/Workflow.Core/index.js');
var PursInteraction = require('./output/Workflow.Interaction/index.js');
var PursInteractionImpl = require('./output/Workflow.Interaction.Impl/index.js');
var Purs = {...PursCore, ...PursInteraction, ...PursInteractionImpl};
var Graphputer = require('./output/Graphputer.Core/index.js');
var GraphputerParser = require('./output/Graphputer.Parser/index.js');

window.Purs = Purs;
window.Graphputer = Graphputer;
window.GraphputerParser = GraphputerParser;

/////////////////////////////////
////// Main

window.onload = function () {

    var graphUI = new GraphUI(Purs.demo);
    window.graphUI = graphUI;

    // uncomment to activate validation
    //window.graphUI.registerNodeValidHook(function (graph, node) {
    //    return window.GraphputerParser.canParseNodeText(node.text.trim());
    //});

    //window.graphUI.registerEdgeValidHook(function (graph, sourceNode, targetNode) {
    //    return window.GraphputerParser.canCompose(
    //        targetNode.text.trim())(
    //        sourceNode.text.trim());
    //});

    window.graph = window.graphUI.graph;

    console.log(graphUI.graph);

    graphUI.update();
};
