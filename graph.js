const Utils = require('./utils.js');
const StringSet = require('./stringSet.js');

function GraphNodeBody(text, x, y, parents, children) {
    this.text = text;
    this.x = x;
    this.y = y;
    this.parents = parents;
    this.children = children;
    this.subgraphNodes = {};
}

function Graph(graphNodes, focusedNodeId, highlightedNodes) {
    var graph = this;
    graph.nodes = graphNodes;
    graph.focusedNodeId = focusedNodeId;
    graph.highlightedNodes = highlightedNodes;

    ///////////////////////////////////
    //////// Constants

    // TODO: graph should go with layout logic
    var initNodePos = {"x": 100, "y": 100},
        newNodeOffset = {"x": 100, "y": 100},
        newNodeClearenceThreshold = 80;


    ///////////////////////////////////
    //////// Graph manipulation

    graph.getEdges = function () {
        return [].concat.apply(
            [], [].concat.apply(
                [], Object.entries(graph.nodes).map(
                    node => StringSet.toArray(node[1].children).map(
                        target => ({"source": graph.nodes[node[0]],
                                    "target": graph.nodes[target]})))));
    };

    graph.newNodeBelowFocus = function () {
        if (graph.focusedNodeId != null) {
            newNodePos = graph.getNewNodePosition(graph.focusedNodeId);
            graph.createNode(newNodePos.x, newNodePos.y, StringSet.singleton(graph.focusedNodeId), StringSet.empty());
        } else {
            graph.createNode(initNodePos.x, initNodePos.y, StringSet.empty(), StringSet.empty());
        }
        return graph;
    };

    graph.createNode = function (x, y, parentIds, childIds) {
        newNodeId = Utils.uuidv4();
        graph.nodes[newNodeId] = new GraphNodeBody(
            " ", x, y, parentIds, childIds);
        StringSet.map(parentIds, parentId => StringSet.insertInPlace(newNodeId, graph.nodes[parentId].children));
        StringSet.map(childIds, childId => StringSet.insertInPlace(newNodeId, graph.nodes[childId].parents));
        graph.focusedNodeId = newNodeId;
        return newNodeId;
    };

    graph.removeFocusedNode = function () {
        focusedNode = graph.nodes[graph.focusedNodeId];
        if (StringSet.cardinality(focusedNode.parents) > 0) {
            nextFocusId = StringSet.lookupIndex(0, focusedNode.parents);
        } else if (StringSet.cardinality(focusedNode.children) > 0) {
            nextFocusId = StringSet.lookupIndex(0, focusedNode.children);
        } else {
            // Give the focus to the first node in the list, because what else are
            // you going to do
            nextFocusId = Utils.arrayWithoutElement(graph.focusedNodeId, Object.keys(graph.nodes))[0];
        }
        for(i=0; i<Object.values(focusedNode.subgraphNodes).length; i++) {
        }
        graph.deleteNode(graph.focusedNodeId);
        graph.focusedNodeId = nextFocusId;
        return graph;
    };

    graph.deleteNode = function (nodeToRemoveId) {
        // Remove edges to/from the node in other
        // node objects
        for (i=0; i<StringSet.cardinality(graph.nodes[nodeToRemoveId].parents); i++) {
            parentId = StringSet.lookupIndex(i, graph.nodes[nodeToRemoveId].parents);
            StringSet.deleteInPlace(
                nodeToRemoveId,
                graph.nodes[parentId].children);
        }
        for (i=0; i<StringSet.cardinality(graph.nodes[nodeToRemoveId].children); i++) {
            childId = StringSet.lookupIndex(i, graph.nodes[nodeToRemoveId].children);
            StringSet.deleteInPlace(nodeToRemoveId,
                graph.nodes[childId].parents);
        }
        // Remove the node
        delete graph.nodes[nodeToRemoveId];
        return graph;
    };

    graph.addEdge = function(sourceId, targetId) {
        StringSet.insertInPlace(targetId, graph.nodes[sourceId].children);
        StringSet.insertInPlace(sourceId, graph.nodes[targetId].parents);
        return graph;
    };

    graph.removeEdgesToFromStringSet = function (nodeIdStringSet) {
        for (i=0; i<StringSet.cardinality(nodeIdStringSet); i++) {
            currentNodeId = StringSet.lookupIndex(i, nodeIdStringSet);
            for (j=0; j<StringSet.cardinality(graph.nodes[currentNodeId].parents); j++) {
                parentId = StringSet.lookupIndex(j, graph.nodes[currentNodeId].parents);
                if (!StringSet.isIn(parentId, nodeIdStringSet)) {
                    StringSet.deleteInPlace(
                        currentNodeId,
                        graph.nodes[parentId].children);
                }
            }
            for (j=0; j<StringSet.cardinality(graph.nodes[currentNodeId].children); j++) {
                childId = StringSet.lookupIndex(j, graph.nodes[currentNodeId].children);
                if (!StringSet.isIn(childId, nodeIdStringSet)) {
                    StringSet.deleteInPlace(
                        currentNodeId,
                        graph.nodes[childId].parents);
                }
            }
        }
        return graph;
    };

    graph.restoreEdgesToFromSubgraph = function (subgraphNodes) {
        for (i=0; i<Object.keys(subgraphNodes).length; i++) {
            subgraphNodeId = Object.keys(subgraphNodes)[i];
            for (j=0; j<StringSet.cardinality(subgraphNodes[subgraphNodeId].parents); j++) {
                parentId = StringSet.lookupIndex(j, subgraphNodes[subgraphNodeId].parents);
                graph.addEdge(parentId, subgraphNodeId);
            }
            for (j=0; j<StringSet.cardinality(subgraphNodes[subgraphNodeId].children); j++) {
                childId = StringSet.lookupIndex(j, subgraphNodes[subgraphNodeId].children);
                graph.addEdge(subgraphNodeId, childId);
            }
        }
        return graph;
    };

    graph.getParentsOfStringSet = function (nodeIdStringSet) {
        return StringSet.fromArray(
            Utils.concatenate(
                StringSet.toArray(nodeIdStringSet).map(
                    nodeId => StringSet.toArray(graph.nodes[nodeId].parents)
                )
            ).filter(
                nodeId => !StringSet.isIn(nodeId, nodeIdStringSet)
            )
        );
    };

    graph.getChildrenOfStringSet = function (nodeIdStringSet) {
        return StringSet.fromArray(
            Utils.concatenate(
                StringSet.toArray(nodeIdStringSet).map(
                    nodeId => StringSet.toArray(graph.nodes[nodeId].children)
                )
            ).filter(
                nodeId => !StringSet.isIn(nodeId, nodeIdStringSet)
            )
        );
    };

    graph.extractNodes = function (nodeIdStringSet) {
        extractedNodes = {};
        for (i=0; i<StringSet.cardinality(nodeIdStringSet); i++) {
            nodeId = StringSet.lookupIndex(i, nodeIdStringSet);
            extractedNodes[nodeId] =
                graph.nodes[nodeId];
            delete graph.nodes[nodeId];
        }
        return extractedNodes;
    };

    graph.restoreSubgraphNodes = function (newCenterPoint, subgraphNodes) {
        // Make up for motion of the group node by applying the change in
        // position of the group node to all subgraphNodes.
        // The initial position of the group node is known to be the
        // centroid of the subgraphNodes.
        centroid = Utils.centroidOfPoints(Object.values(subgraphNodes));
        groupMovementVector = {
            "x": newCenterPoint.x - centroid.x,
            "y": newCenterPoint.y - centroid.y,
        };
        for (i=0; i<Object.keys(subgraphNodes).length; i++) {
            subgraphNodeId = Object.keys(subgraphNodes)[i];
            subgraphNodes[subgraphNodeId].x += groupMovementVector.x;
            subgraphNodes[subgraphNodeId].y += groupMovementVector.y;
            // Add nodes to graph top level
            graph.nodes[subgraphNodeId] = subgraphNodes[subgraphNodeId];
        }

        return graph;
    };


    ///////////////////////////////////
    //////// Grouping/ungrouping

    graph.groupHighlighted = function () {
        parents = graph.getParentsOfStringSet(graph.highlightedNodes);
        children = graph.getChildrenOfStringSet(graph.highlightedNodes);
        centroid = Utils.centroidOfPoints(
            StringSet.toArray(graph.highlightedNodes).map(nodeId => graph.nodes[nodeId]));

        groupNodeId = graph.createNode(centroid.x, centroid.y, parents, children);

        graph.removeEdgesToFromStringSet(graph.highlightedNodes);

        // Hide the highlighted nodes inside the group node
        graph.nodes[groupNodeId].subgraphNodes =
            graph.extractNodes(graph.highlightedNodes);

        graph.highlightedNodes = StringSet.empty();
        graph.focusedNodeId = groupNodeId;

        return graph;
    };

    graph.expandGroup = function (groupNodeId) {
        // TODO: Move other nodes out of the way!
        graph.restoreSubgraphNodes(graph.nodes[groupNodeId],
                                   graph.nodes[groupNodeId].subgraphNodes);

        graph.removeEdgesToFromStringSet(StringSet.singleton(groupNodeId));
        graph.restoreEdgesToFromSubgraph(graph.nodes[groupNodeId].subgraphNodes);

        // Pick the first node of group to have the focus
        graph.focusedNodeId = Object.keys(graph.nodes[groupNodeId].subgraphNodes)[0];
        // Highlight expanded group
        graph.highlightedNodes = StringSet.fromArray(Object.keys(graph.nodes[groupNodeId].subgraphNodes));

        // Remove group node
        delete graph.nodes[groupNodeId];

        return graph;
    };

    graph.expandGroupInFocus = function () {
        if (!Utils.isEmptyObject(graph.nodes[graph.focusedNodeId].subgraphNodes)) {
            graph.expandGroup(graph.focusedNodeId);
        }
    };

    graph.toggleGroupExpand = function () {
        if (StringSet.cardinality(graph.highlightedNodes) == 0) {
            graph.expandGroupInFocus();
        } else {
            graph.groupHighlighted();
        }
    };


    ///////////////////////////////////
    //////// Traversal functions

    graph.traverseUp = function () {
        parents = graph.nodes[graph.focusedNodeId].parents;
        if (StringSet.cardinality(parents) > 0) {
            graph.focusedNodeId = StringSet.lookupIndex(0, parents);
        }
        return graph;
    };

    graph.traverseDown = function () {
        children = graph.nodes[graph.focusedNodeId].children;
        if (StringSet.cardinality(children) > 0) {
            graph.focusedNodeId = StringSet.lookupIndex(0, children);
        }
        return graph;
    };

    graph.traverseLeft = function () {
        graph.focusedNodeId = graph.getNeighboringSiblingOrCoparentIds(
            graph.focusedNodeId).left;
        return graph;
    };

    graph.traverseRight = function () {
        graph.focusedNodeId = graph.getNeighboringSiblingOrCoparentIds(
            graph.focusedNodeId).right;
        return graph;
    };

    graph.traverseAddGroup = function (traversalFunc) {
        return function () {
            focusedNodeId = graph.nodes[traverselFunc];
            StringSet.insertInPlace(focusedNodeId, graph.highlightedNodes);
            return graph;
        };
    };

    graph.getNeighboringSiblingOrCoparentIds = function (nodeId) {
        /*
          Find the nodes that share a parent or child of the given node.
          Sort the nodes by their x location, such that left/right movement
          is spatially coherent.
          */
        siblingsAndCoparentsIds = [];
        StringSet.map(graph.nodes[nodeId].parents,
            parentId => StringSet.map(graph.nodes[parentId].children,
                siblingId => siblingsAndCoparentsIds.push(siblingId)
            )
        );
        StringSet.map(graph.nodes[nodeId].children,
            childId => StringSet.map(graph.nodes[childId].parents,
                coparentId => siblingsAndCoparentsIds.push(coparentId))
        );
        // Sort siblings by x index.
        // Store graph so that the sorting comparison function can access it,
        // as it appears to run outside the constructor context.
        graphNodes = graph.nodes;
        siblingsAndCoparentsIds.sort((a, b) => graphNodes[a].x - graphNodes[b].x);
        onLeft = siblingsAndCoparentsIds.filter(
            nodeId => graph.nodes[nodeId].x < graph.nodes[graph.focusedNodeId].x);
        onRight = siblingsAndCoparentsIds.filter(
            nodeId => graph.nodes[nodeId].x > graph.nodes[graph.focusedNodeId].x);
        return {
            "left": onLeft.length > 0
                ? onLeft[onLeft.length - 1]
                : siblingsAndCoparentsIds[siblingsAndCoparentsIds.length-1],
            "right": onRight.length > 0 ? onRight[0] : siblingsAndCoparentsIds[0],
        };
    };

    ///////////////////////////////////
    //////// Highlighting a selection/focusing

    graph.focusOn = function (nodeId) {
        graph.focusedNodeId = nodeId;
    };

    graph.clearHighlights = function () {
        graph.highlightedNodes = StringSet.empty();
        return graph;
    };

    graph.highlightFocusNode = function () {
        StringSet.insertInPlace(graph.focusedNodeId, graph.highlightedNodes);
        return graph;
    };

    graph.unHighlightFocusNode = function () {
        StringSet.deleteInPlace(graph.focusedNodeId, graph.highlightedNodes);
        return graph;
    };

    graph.toggleHighlightFocusNode = function () {
        if (!StringSet.isIn(graph.focusedNodeId, graph.highlightedNodes)) {
            graph.highlightFocusNode();
        } else {
            graph.unHighlightFocusNode();
        }
        return graph;
    };

    ///////////////////////////////////
    //////// Node Spatial Arrangement

    graph.moveNode = function (nodeId, newPos) {
        graph.nodes[nodeId].x = newPos.x;
        graph.nodes[nodeId].y = newPos.y;
        return graph;
    };

    graph.getNewNodePosition = function (parentId) {
        // Find right-most child
        children = graph.nodes[parentId].children;
        if (StringSet.cardinality(children) > 0) {
            rightmostChildId = StringSet.lookupIndex(
                Utils.argMax(StringSet.toArray(StringSet.map(children, childId => graph.nodes[childId].x))),
                children);
            return graph.getNewPositionRightOf(graph.nodes[rightmostChildId]);
        } else {
            return graph.getNewPositionBelowOf(graph.nodes[parentId]);

        }
    };

    graph.getNewPositionRightOf = function (nodeObject) {
        attempt = {"x": nodeObject.x + newNodeOffset.x,
                   "y": nodeObject.y};
        if (Utils.distanceToClosestPoint2D(attempt, Object.values(graph.nodes)) < newNodeClearenceThreshold) {
            return graph.getNewPositionRightOf(attempt);
        } else {
            return attempt;
        }
    };

    graph.getNewPositionBelowOf = function (nodeObject) {
        attempt = {"x": nodeObject.x,
                   "y": nodeObject.y + newNodeOffset.y};
        if (Utils.distanceToClosestPoint2D(attempt, Object.values(graph.nodes)) < newNodeClearenceThreshold) {
            return graph.getNewPositionRightOf(attempt);
        } else {
            return attempt;
        }
    };
};
module.exports = Graph;
