const Utils = require('./utils.js');
const StringSet = require('./stringSet.js');
var PursCore = require('./purescript/output/Workflow.Core/index.js');
var PursInteraction = require('./purescript/output/Workflow.Interaction/index.js');
var Purs = {...PursCore, ...PursInteraction};

function GraphNodeBody(id, text, x, y, parents, children) {
    this.id = id;
    this.text = text;
    this.valid = true;
    this.x = x;
    this.y = y;
    this.parents = parents;
    this.children = children;
    this.subgraphNodes = {};
}

function Graph(graphNodes, focus, highlighted) {
    var graph = this;

    graph.pursGraph = Purs.emptyGraph;
    graph.updatePurs = function(graphOp) {
        graph.pursGraph = Purs.applyGraphOp(graphOp)(graph.pursGraph);
    };

    graph.copyNode = function (node) {
        newNodeBody = new GraphNodeBody(node.id,
                                        node.text,
                                        node.x,
                                        node.y,
                                        node.parents,
                                        node.children);
        newNodeBody.subgraphNodes = node.subgraphNodes;
        return newNodeBody;
    };
    for (i=0; i<Object.keys(graphNodes).length; i++) {
        node = Object.values(graphNodes)[i];
        graph.updatePurs(Purs.AddNode.create(graph.copyNode(node)));
    }
    for (i=0; i<Object.keys(highlighted).length; i++) {
        graph.updatePurs(Purs.Highlight.create(StringSet.lookupIndex(i, highlighted)));
    }
    graph.updatePurs(Purs.UpdateFocus.create(focus));

    graph.usePursGraph = function() {
        graph.nodes = Utils.deepCopyObject(graph.pursGraph.nodes);
        graph.focus = graph.pursGraph.focus;
        graph.highlighted = graph.pursGraph.highlighted;

        return graph;
    };

    ///////////////////////////////////
    //////// Graph manipulation

    graph.newChildOfFocus = function () {
        graph.pursGraph = Purs.newChildOfFocus(graph.pursGraph)();
        return graph;
    };

    graph.newParentOfFocus = function () {
        graph.pursGraph = Purs.newParentOfFocus(graph.pursGraph)();
        return graph;
    };

    graph.createNode = function (x, y, parentIds, childIds) {
        graph.pursGraph = Purs.addNode({"x": x, "y": y})(parentIds)(childIds)(graph.pursGraph)();
        return graph;
    };

    graph.removeFocused = function () {
        graph.pursGraph = Purs.removeFocus(graph.pursGraph);
        return graph;
    };

    graph.addEdge = function(sourceId, targetId) {
        graph.updatePurs(Purs.AddEdge.create({"source": sourceId, "target": targetId}));

        return graph;
    };

    graph.deleteEdge = function(sourceId, targetId) {
        graph.updatePurs(Purs.RemoveEdge.create({"source": sourceId, "target": targetId}));

        return graph;
    };

    graph.updateText = function(nodeId, text) {
        graph.updatePurs(Purs.UpdateText.create(nodeId)(text));

        return graph;
    };

    graph.moveNode = function (nodeId, newPos) {
        graph.updatePurs(Purs.MoveNode.create(nodeId)(newPos));
        return graph;
    };

    graph.updateValidity = function (nodeId, validity) {
        graph.updatePurs(Purs.UpdateNodeValidity.create(nodeId)(validity));
        return graph;
    };


    ///////////////////////////////////
    //////// Grouping/ungrouping

    //graph.toggleGroupExpand = function () {
    //    graph.pursGraph = Purs.interactiveGraphCollapseExpand.toggleCollapseExpand(graph.pursGraph);
    //};


    ///////////////////////////////////
    //////// Traversal functions

    //graph.traverseUp = function () {
    //    graph.pursGraph = Purs.traverseUp(graph.pursGraph);
    //    return graph;
    //};

    //graph.traverseDown = function () {
    //    graph.pursGraph = Purs.traverseDown(graph.pursGraph);
    //    return graph;
    //};

    //graph.traverseLeft = function () {
    //    graph.pursGraph = Purs.traverseLeft(graph.pursGraph);
    //    return graph;
    //};

    //graph.traverseRight = function () {
    //    graph.pursGraph = Purs.traverseRight(graph.pursGraph);
    //    return graph;
    //};


    ///////////////////////////////////
    //////// Highlighting a selection/focusing

    //graph.focusOnNode = function (id) {
    //    graph.updatePurs(Purs.UpdateFocus.create(Purs.FocusNode.create(id)));
    //};

    //graph.focusOnEdge = function (edge) {
    //    graph.updatePurs(Purs.UpdateFocus.create(Purs.FocusEdge.create(edge)([])));
    //};

    //graph.unHighlightNode = function(nodeId) {
    //    graph.updatePurs(Purs.UnHighlight.create(nodeId));
    //};

    //graph.highlightNode = function(nodeId) {
    //    graph.updatePurs(Purs.Highlight.create(nodeId));
    //};

    //graph.toggleHighlightFocus = function () {
    //    graph.pursGraph = Purs.toggleHighlightFocus(graph.pursGraph);
    //    return graph;
    //};

    //graph.clearHighlights = function () {
    //    graph.pursGraph = Purs.clearHighlighted(graph.pursGraph);
    //    return graph;
    //};
};
module.exports = Graph;
