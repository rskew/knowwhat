const GraphUI = require('./graphUI.js');
const Utils = require('./utils.js');
var PursCore = require('./purescript/output/Workflow.Core/index.js');
var PursInteraction = require('./purescript/output/Workflow.Interaction/index.js');
var PursInteractionImpl = require('./purescript/output/Workflow.Interaction.Impl/index.js');
var Purs = {...PursCore, ...PursInteraction, ...PursInteractionImpl};
var Graphputer = require('./purescript/output/Graphputer.Core/index.js');
var GraphputerParser = require('./purescript/output/Graphputer.Parser/index.js');


///////////////////////////////////
//////// Main

var graphUI = new GraphUI(Purs.demo);
// TODO: remove debugging hackz
window.Purs = Purs;
window.graphUI = graphUI;
window.Graphputer = Graphputer;
window.GraphputerParser = GraphputerParser;

window.graphUI.registerNodeValidHook(function (graph, node) {
    return window.GraphputerParser.canParseNodeText(node.text.trim());
});

window.graphUI.registerEdgeValidHook(function (graph, sourceNode, targetNode) {
    return window.GraphputerParser.canCompose(
        targetNode.text.trim())(
        sourceNode.text.trim());
});

window.graph = window.graphUI.graph;

console.log(graphUI.graph);

graphUI.update();
