const Utils = require('./utils.js');
const { kdTree, BinaryHeap } = require('./kdTree.js');

function GraphNodeBody(text, x, y, parents, children) {
    this.text = text;
    this.x = x;
    this.y = y;
    this.parents = parents;
    this.children = children;
    this.subgraph = {};
}

module.exports = function Graph(graphNodes, focusedNodeId, highlightedNodes) {
    graph = this;
    graph.nodes = graphNodes;
    graph.focusedNodeId = focusedNodeId;
    graph.highlightedNodes = highlightedNodes;
    // https://github.com/ubilabs/kd-tree-javascript
    graph.kdTree = new kdTree(Object.values(graphNodes), Utils.euclideanDistance2D, ["x", "y"]);

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
                    node => node[1].children.map(
                        target => ({"source": graph.nodes[node[0]],
                                    "target": graph.nodes[target]})))));
    };

    graph.newNodeBelowFocus = function () {
        if (graph.focusedNodeId != null) {
            newNodePos = graph.getNewNodePosition(graph.focusedNodeId);
            graph.createNode(newNodePos.x, newNodePos.y, [graph.focusedNodeId], []);
        } else {
            graph.createNode(initNodePos.x, initNodePos.y, [], []);
        }
        return graph;
    };

    graph.createNode = function (x, y, parentIds, childIds) {
        newNodeId = Utils.uuidv4();
        graph.nodes[newNodeId] = new GraphNodeBody(
            " ", x, y, parentIds, childIds);
        parentIds.map(parentId => graph.nodes[parentId].children.push(newNodeId));
        childIds.map(childId => graph.nodes[childId].parents.push(newNodeId));
        graph.focusedNodeId = newNodeId;
        graph.kdTree.insert(graph.nodes[newNodeId]);
        return graph;
    };

    graph.removeFocusedNode = function () {
        focusedNode = graph.nodes[graph.focusedNodeId];
        if (focusedNode.parents.length > 0) {
            nextFocusId = focusedNode.parents[0];
        } else if (focusedNode.children.length > 0) {
            nextFocusId = focusedNode.children[0];
        } else {
            // Give the focus to the first node in the list, because what else are
            // you going to do
            nextFocusId = Utils.arrayWithoutElement(graph.focusedNodeId, Object.keys(graph.nodes))[0];
        }
        graph.kdTree.remove(focusedNode);
        graph.deleteNode(graph.focusedNodeId);
        graph.focusedNodeId = nextFocusId;
        return graph;
    };

    graph.deleteNode = function (nodeToRemoveId) {
        // Remove edges to/from the node in other
        // node objects
        for (i=0; i<graph.nodes[nodeToRemoveId].parents.length; i++) {
            parentId = graph.nodes[nodeToRemoveId].parents[i];
            Utils.arrayRemoveElementInPlace(
                nodeToRemoveId,
                graph.nodes[parentId].children);
        }
        for (i=0; i<graph.nodes[nodeToRemoveId].children.length; i++) {
            childId = graph.nodes[nodeToRemoveId].children[i];
            Utils.arrayRemoveElementInPlace(
                nodeToRemoveId,
                graph.nodes[childId].parents);
        }
        // Remove the node
        delete graph.nodes[nodeToRemoveId];
        return graph;
    };


    graph.removeEdgesToFromSet = function (nodeIdSet) {
        for (i=0; i<nodeIdSet.length; i++) {
            for (j=0; j<graph.nodes[nodeIdSet[i]].parents.length; j++) {
                parentId = graph.nodes[nodeIdSet[i]].parents[j];
                if (!Utils.isIn(parentId, nodeIdSet)) {
                    Utils.arrayRemoveElementInPlace(
                        nodeIdSet[i],
                        graph.nodes[parentId].children);
                }
            }
            for (j=0; j<graph.nodes[nodeIdSet[i]].children.length; j++) {
                childId = graph.nodes[nodeIdSet[i]].children[j];
                if (!Utils.isIn(childId, nodeIdSet)) {
                    Utils.arrayRemoveElementInPlace(
                        nodeIdSet[i],
                        graph.nodes[childId].parents);
                }
            }
        }
        return graph;
    };

    graph.restoreEdgesToFromSubgraph = function (subgraph) {
        for (i=0; i<Object.keys(subgraph).length; i++) {
            subgraphNodeId = Object.keys(subgraph)[i];
            for (j=0; j<subgraph[subgraphNodeId].parents.length; j++) {
                parentId = subgraph[subgraphNodeId].parents[j];
                if (!Utils.isIn(parentId, Object.keys(subgraph))) {
                    graph.nodes[parentId].children.push(subgraphNodeId);
                }
            }
            for (j=0; j<subgraph[subgraphNodeId].children.length; j++) {
                childId = subgraph[subgraphNodeId].children[j];
                if (!Utils.isIn(childId, Object.keys(subgraph))) {
                    graph.nodes[childId].parents.push(subgraphNodeId);
                }
            }
        }
        return graph;
    };

    graph.getParentsOfSet = function (nodeIdSet) {
        return Utils.concatenate(
            graph.highlightedNodes.map(
                nodeId => graph.nodes[nodeId].parents.filter(
                    parentId => !Utils.isIn(parentId, graph.highlightedNodes))));
    };

    graph.getChildrenOfSet = function (nodeIdSet) {
        return Utils.concatenate(
            graph.highlightedNodes.map(
                nodeId => graph.nodes[nodeId].children.filter(
                    childId => !Utils.isIn(childId, graph.highlightedNodes))));
    };

    graph.extractNodeSet = function (nodeIdSet) {
        extractedNodes = {};
        for (i=0; i<nodeIdSet.length; i++) {
            extractedNodes[nodeIdSet[i]] =
                graph.nodes[nodeIdSet[i]];
            delete graph.nodes[nodeIdSet[i]];
        }
        return extractedNodes;
    };

    graph.restoreSubgraphNodes = function (newCenterPoint, subgraph) {
        // Make up for motion of the group node by applying the change in
        // position of the group node to all subgraph nodes.
        // The initial position of the group node is known to be the
        // centroid of the subgraph nodes.
        centroid = Utils.centroidOfPoints(Object.values(subgraph));
        groupMovementVector = {
            "x": newCenterPoint.x - centroid.x,
            "y": newCenterPoint.y - centroid.y,
        };
        for (i=0; i<Object.keys(subgraph).length; i++) {
            subgraphNodeId = Object.keys(subgraph)[i];
            subgraph[subgraphNodeId].x += groupMovementVector.x;
            subgraph[subgraphNodeId].y += groupMovementVector.y;
        }

        // Add nodes to graph top level
        for (i=0; i<Object.keys(subgraph).length; i++) {
            subgraphNodeId = Object.keys(subgraph)[i];
            graph.nodes[subgraphNodeId] = subgraph[subgraphNodeId];
        }

        return graph;
    };


    ///////////////////////////////////
    //////// Grouping/ungrouping

    graph.groupHighlighted = function () {
        parents = graph.getParentsOfSet(graph.highlightedNodes);
        children = graph.getChildrenOfSet(graph.highlightedNodes);
        centroid = Utils.centroidOfPoints(
            graph.highlightedNodes.map(nodeId => graph.nodes[nodeId]));

        graph.createNode(centroid.x, centroid.y, parents, children);
        groupNodeId = graph.focusedNodeId;

        graph.removeEdgesToFromSet(graph.highlightedNodes);

        // Hide the highlighted nodes inside the group node
        graph.nodes[groupNodeId].subgraph =
            graph.extractNodeSet(graph.highlightedNodes);

        graph.highlightedNodes = [];
        graph.focusedNodeId = groupNodeId;

        return graph;
    };

    graph.expandGroup = function (groupNodeId) {
        graph.removeEdgesToFromSet([groupNodeId]);
        graph.restoreEdgesToFromSubgraph(graph.nodes[groupNodeId].subgraph);

        // TODO: Move other nodes out of the way!
        graph.restoreSubgraphNodes(graph.nodes[groupNodeId],
                                  graph.nodes[groupNodeId].subgraph);

        // Pick the first node of group to have the focus
        graph.focusedNodeId = Object.keys(graph.nodes[groupNodeId].subgraph)[0];
        // Highlight expanded group
        graph.highlightedNodes = Object.keys(graph.nodes[groupNodeId].subgraph);

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
        if (graph.highlightedNodes.length == 0) {
            graph.expandGroupInFocus();

        } else {
            graph.groupHighlighted();
        }
    };


    ///////////////////////////////////
    //////// Traversal functions

    graph.traverseUp = function () {
        parents = graph.nodes[graph.focusedNodeId].parents;
        if (parents.length > 0) {
            graph.focusedNodeId = parents[0];
        }
        return graph;
    };

    graph.traverseDown = function () {
        children = graph.nodes[graph.focusedNodeId].children;
        if (children.length > 0) {
            graph.focusedNodeId = children[0];
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
            focusedNodeIdId = graph.nodes[traverselFunc];
            graph.highlightedNodes.push(focusedNodeIdId);
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
        graph.nodes[nodeId].parents.map(
            parentId => graph.nodes[parentId].children.map(
                siblingId => siblingsAndCoparentsIds.push(siblingId)
            )
        );
        graph.nodes[nodeId].children.map(
            childId => graph.nodes[childId].parents.map(
                coparentId => siblingsAndCoparentsIds.push(coparentId)
            )
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
        graph.highlightedNodes = [];
        return graph;
    };

    graph.highlightFocusNode = function () {
        if (!Utils.isIn(graph.focusedNodeId, graph.highlightedNodes)) {
            graph.highlightedNodes.push(graph.focusedNodeId);
        }
        return graph;
    };

    graph.unHighlightFocusNode = function () {
        graph.highlightedNodes = Utils.arrayWithoutElement(
            graph.focusedNodeId, graph.highlightedNodes);
        return graph;
    };

    graph.toggleHighlightFocusNode = function () {
        if (!Utils.isIn(graph.focusedNodeId, graph.highlightedNodes)) {
            graph.highlightFocusNode();
        } else {
            graph.unHighlightFocusNode();
        }
        return graph;
    };

    ///////////////////////////////////
    //////// Node Spatial Arrangement

    graph.moveNode = function (nodeId, newPos) {
        graph.kdTree.remove(graph.nodes[nodeId]);
        graph.nodes[nodeId].x = newPos.x;
        graph.nodes[nodeId].y = newPos.y;
        graph.kdTree.insert(graph.nodes[nodeId]);
        return graph;
    };

    graph.getNewNodePosition = function (parentId) {
        // Find right-most child
        children = graph.nodes[parentId].children;
        if (children.length > 0) {
            rightmostChildId = children[
                Utils.argMax(children.map(childId => graph.nodes[childId].x))];
            return graph.getNewPositionRightOf(graph.nodes[rightmostChildId]);
        } else {
            return graph.getNewPositionBelowOf(graph.nodes[parentId]);

        }
    };

    graph.getNewPositionRightOf = function (nodeObject) {
        attempt = {"x": nodeObject.x + newNodeOffset.x,
                   "y": nodeObject.y};
        if (graph.kdTree.nearest(attempt, 1)[0][1] < newNodeClearenceThreshold) {
            return graph.getNewPositionRightOf(attempt);
        } else {
            return attempt;
        }
    };

    graph.getNewPositionBelowOf = function (nodeObject) {
        attempt = {"x": nodeObject.x,
                   "y": nodeObject.y + newNodeOffset.y};
        if (graph.kdTree.nearest(attempt, 1)[0][1] < newNodeClearenceThreshold) {
            return graph.getNewPositionRightOf(attempt);
        } else {
            return attempt;
        }
    };
};
