const Utils = require('./utils.js');
const Set = require('./set.js');

function GraphNodeBody(text, x, y, parents, children) {
    this.text = text;
    this.x = x;
    this.y = y;
    this.parents = parents;
    this.children = children;
    this.subgraph = new Graph({}, "", Set.empty());
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
                    node => Set.toArray(node[1].children).map(
                        target => ({"source": graph.nodes[node[0]],
                                    "target": graph.nodes[target]})))));
    };

    graph.newNodeBelowFocus = function () {
        if (graph.focusedNodeId != null) {
            newNodePos = graph.getNewNodePosition(graph.focusedNodeId);
            graph.createNode(newNodePos.x, newNodePos.y, Set.singleton(graph.focusedNodeId), Set.empty());
        } else {
            graph.createNode(initNodePos.x, initNodePos.y, Set.empty(), Set.empty());
        }
        return graph;
    };

    graph.createNode = function (x, y, parentIds, childIds) {
        newNodeId = Utils.uuidv4();
        graph.nodes[newNodeId] = new GraphNodeBody(
            " ", x, y, parentIds, childIds);
        Set.map(parentIds, parentId => Set.insertInPlace(newNodeId, graph.nodes[parentId].children));
        Set.map(childIds, childId => Set.insertInPlace(newNodeId, graph.nodes[childId].parents));
        graph.focusedNodeId = newNodeId;
        return newNodeId;
    };

    graph.removeFocusedNode = function () {
        focusedNode = graph.nodes[graph.focusedNodeId];
        if (Set.cardinality(focusedNode.parents) > 0) {
            nextFocusId = Set.lookupIndex(0, focusedNode.parents);
        } else if (Set.cardinality(focusedNode.children) > 0) {
            nextFocusId = Set.lookupIndex(0, focusedNode.children);
        } else {
            // Give the focus to the first node in the list, because what else are
            // you going to do
            nextFocusId = Utils.arrayWithoutElement(graph.focusedNodeId, Object.keys(graph.nodes))[0];
        }
        for(i=0; i<Object.values(focusedNode.subgraph.nodes).length; i++) {
        }
        graph.deleteNode(graph.focusedNodeId);
        graph.focusedNodeId = nextFocusId;
        return graph;
    };

    graph.deleteNode = function (nodeToRemoveId) {
        // Remove edges to/from the node in other
        // node objects
        for (i=0; i<Set.cardinality(graph.nodes[nodeToRemoveId].parents); i++) {
            parentId = setLookupIndex(i, graph.nodes[nodeToRemoveId].parents);
            Set.deleteInPlace(
                nodeToRemoveId,
                graph.nodes[parentId].children);
        }
        for (i=0; i<Set.cardinality(graph.nodes[nodeToRemoveId].children); i++) {
            childId = Set.lookupIndex(i, graph.nodes[nodeToRemoveId].children);
            Set.deleteInPlace(nodeToRemoveId,
                graph.nodes[childId].parents);
        }
        // Remove the node
        delete graph.nodes[nodeToRemoveId];
        return graph;
    };

    graph.addEdge = function(sourceId, targetId) {
        Set.insertInPlace(targetId, graph.nodes[sourceId].children);
        Set.insertInPlace(sourceId, graph.nodes[targetId].parents);
        return graph;
    };


    graph.removeEdgesToFromSet = function (nodeIdSet) {
        for (i=0; i<Set.cardinality(nodeIdSet); i++) {
            currentNodeId = Set.lookupIndex(i, nodeIdSet);
            for (j=0; j<Set.cardinality(graph.nodes[currentNodeId].parents); j++) {
                parentId = Set.lookupIndex(j, graph.nodes[currentNodeId].parents);
                if (!Set.isIn(parentId, nodeIdSet)) {
                    Set.deleteInPlace(
                        currentNodeId,
                        graph.nodes[parentId].children);
                }
            }
            for (j=0; j<Set.cardinality(graph.nodes[currentNodeId].children); j++) {
                childId = Set.lookupIndex(j, graph.nodes[currentNodeId].children);
                if (!Set.isIn(childId, nodeIdSet)) {
                    Set.deleteInPlace(
                        currentNodeId,
                        graph.nodes[childId].parents);
                }
            }
        }
        return graph;
    };

    graph.restoreEdgesToFromSubgraph = function (subgraph) {
        for (i=0; i<Object.keys(subgraph.nodes).length; i++) {
            subgraphNodeId = Object.keys(subgraph.nodes)[i];
            for (j=0; j<Set.cardinality(subgraph.nodes[subgraphNodeId].parents); j++) {
                parentId = Set.lookupIndex(j, subgraph.nodes[subgraphNodeId].parents);
                graph.addEdge(parentId, subgraphNodeId);
            }
            for (j=0; j<Set.cardinality(subgraph.nodes[subgraphNodeId].children); j++) {
                childId = Set.lookupIndex(j, subgraph.nodes[subgraphNodeId].children);
                graph.addEdge(subgraphNodeId, childId);
            }
        }
        return graph;
    };

    graph.getParentsOfSet = function (nodeIdSet) {
        return Set.subtract(
            Set.unionMap(
                nodeIdSet,
                nodeId => graph.nodes[nodeId].parents
            ),
            nodeIdSet
        );
    };

    graph.getChildrenOfSet = function (nodeIdSet) {
        return Set.subtract(
            Set.unionMap(
                nodeIdSet,
                nodeId => graph.nodes[nodeId].children
            ),
            nodeIdSet
        );
    };

    graph.extractNodes = function (nodeIdSet) {
        extractedNodes = {};
        for (i=0; i<Set.cardinality(nodeIdSet); i++) {
            nodeId = Set.lookupIndex(i, nodeIdSet);
            extractedNodes[nodeId] =
                graph.nodes[nodeId];
            delete graph.nodes[nodeId];
        }
        return extractedNodes;
    };

    graph.restoreSubgraphNodes = function (newCenterPoint, subgraph) {
        // Make up for motion of the group node by applying the change in
        // position of the group node to all subgraph nodes.
        // The initial position of the group node is known to be the
        // centroid of the subgraph nodes.
        centroid = Utils.centroidOfPoints(Object.values(subgraph.nodes));
        groupMovementVector = {
            "x": newCenterPoint.x - centroid.x,
            "y": newCenterPoint.y - centroid.y,
        };
        for (i=0; i<Object.keys(subgraph.nodes).length; i++) {
            subgraphNodeId = Object.keys(subgraph.nodes)[i];
            subgraph.nodes[subgraphNodeId].x += groupMovementVector.x;
            subgraph.nodes[subgraphNodeId].y += groupMovementVector.y;
        }

        // Add nodes to graph top level
        for (i=0; i<Object.keys(subgraph.nodes).length; i++) {
            subgraphNodeId = Object.keys(subgraph.nodes)[i];
            graph.nodes[subgraphNodeId] = subgraph.nodes[subgraphNodeId];
        }

        return graph;
    };


    ///////////////////////////////////
    //////// Grouping/ungrouping

    graph.groupHighlighted = function () {
        parents = graph.getParentsOfSet(graph.highlightedNodes);
        children = graph.getChildrenOfSet(graph.highlightedNodes);
        centroid = Utils.centroidOfPoints(
            Set.toArray(graph.highlightedNodes).map(nodeId => graph.nodes[nodeId]));

        groupNodeId = graph.createNode(centroid.x, centroid.y, parents, children);

        graph.removeEdgesToFromSet(graph.highlightedNodes);

        // Hide the highlighted nodes inside the group node
        graph.nodes[groupNodeId].subgraph.nodes =
            graph.extractNodes(graph.highlightedNodes);

        graph.highlightedNodes = Set.empty();
        graph.focusedNodeId = groupNodeId;

        return graph;
    };

    graph.expandGroup = function (groupNodeId) {
        // TODO: Move other nodes out of the way!
        graph.restoreSubgraphNodes(graph.nodes[groupNodeId],
                                   graph.nodes[groupNodeId].subgraph);

        graph.removeEdgesToFromSet(Set.singleton(groupNodeId));
        graph.restoreEdgesToFromSubgraph(graph.nodes[groupNodeId].subgraph);

        // Pick the first node of group to have the focus
        graph.focusedNodeId = Object.keys(graph.nodes[groupNodeId].subgraph.nodes)[0];
        // Highlight expanded group
        graph.highlightedNodes = Set.fromArray(Object.keys(graph.nodes[groupNodeId].subgraph.nodes));

        // Remove group node
        delete graph.nodes[groupNodeId];

        return graph;
    };

    graph.expandGroupInFocus = function () {
        if (!Utils.isEmptyObject(graph.nodes[graph.focusedNodeId].subgraph)) {
            graph.expandGroup(graph.focusedNodeId);
        }
    };

    graph.toggleGroupExpand = function () {
        if (Set.cardinality(graph.highlightedNodes) == 0) {
            graph.expandGroupInFocus();

        } else {
            graph.groupHighlighted();
        }
    };


    ///////////////////////////////////
    //////// Traversal functions

    graph.traverseUp = function () {
        parents = graph.nodes[graph.focusedNodeId].parents;
        if (Set.cardinality(parents) > 0) {
            graph.focusedNodeId = Set.lookupIndex(0, parents);
        }
        return graph;
    };

    graph.traverseDown = function () {
        children = graph.nodes[graph.focusedNodeId].children;
        if (Set.cardinality(children) > 0) {
            graph.focusedNodeId = Set.lookupIndex(children);
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
            Set.insertInPlace(focusedNodeId, graph.highlightedNodes);
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
        Set.map(graph.nodes[nodeId].parents,
            parentId => Set.map(graph.nodes[parentId].children,
                siblingId => siblingsAndCoparentsIds.push(siblingId)
            )
        );
        Set.map(graph.nodes[nodeId].children,
            childId => Set.map(graph.nodes[childId].parents,
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
        graph.highlightedNodes = Set.empty();
        return graph;
    };

    graph.highlightFocusNode = function () {
        Set.insertInPlace(graph.focusedNodeId, graph.highlightedNodes);
        return graph;
    };

    graph.unHighlightFocusNode = function () {
        Set.deleteInPlace(graph.focusedNodeId, graph.highlightedNodes);
        return graph;
    };

    graph.toggleHighlightFocusNode = function () {
        if (!Set.isIn(graph.focusedNodeId, graph.highlightedNodes)) {
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
        if (Set.cardinality(children) > 0) {
            rightmostChildId = Set.lookupIndex(
                Utils.argMax(Set.toArray(Set.map(children, childId => graph.nodes[childId].x))),
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
