const Utils = require('./utils.js');
const StringSet = require('./stringSet.js');
const Purs = require('./purescript/output/Main/index.js');

function GraphNodeBody(id, text, x, y, parents, children) {
    this.id = id;
    this.text = text;
    this.x = x;
    this.y = y;
    this.parents = parents;
    this.children = children;
    this.subgraphNodes = {};
}

function Graph(graphNodes, focusNode, highlightedNodes) {
    var graph = this;
    graph.nodes = graphNodes;
    graph.focusNode = focusNode;
    graph.highlightedNodes = highlightedNodes;

    // Create the Purescript graph to play along with the JS one
    graph.pursGraph = Purs.emptyUndoableGraph;
    graph.updatePurs = function(graphOp) {
        graph.pursGraph = Purs.doOp(graphOp)(graph.pursGraph);
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
    for (i=0; i<Object.keys(highlightedNodes).length; i++) {
        graph.updatePurs(Purs.Highlight.create(StringSet.lookupIndex(i, highlightedNodes)));
    }
    graph.updatePurs(Purs.UpdateFocus.create(focusNode));

    graph.usePursGraph = function() {
        builtPursGraph = Purs.buildGraph(graph.pursGraph);
        graph.nodes = Utils.deepCopyObject(builtPursGraph.nodes);
        graph.focusNode = builtPursGraph.focusNode;
        graph.highlightedNodes = builtPursGraph.highlightedNodes;
        console.log("Graph Length: ", Purs.graphLength(graph.pursGraph));
        return graph;
    };

    ///////////////////////////////////
    //////// Constants

    // TODO: should go with layout logic
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
        if (graph.focusNode != null) {
            newNodePos = graph.getNewNodePosition(graph.focusNode);
            graph.createNode(newNodePos.x,
                             newNodePos.y,
                             StringSet.singleton(graph.focusNode),
                             StringSet.empty());
        } else {
            graph.createNode(initNodePos.x,
                             initNodePos.y,
                             StringSet.empty(),
                             StringSet.empty());
        }
        return graph;
    };

    graph.createNode = function (x, y, parentIds, childIds) {
        newNodeId = Utils.uuidv4();
        newNodeBody = new GraphNodeBody(
            newNodeId, " ", x, y, parentIds, childIds);

        //// Old JS graph
        //graph.nodes[newNodeId] = newNodeBody;
        //StringSet.map(parentIds, parentId => StringSet.insertInPlace(
        //    newNodeId, graph.nodes[parentId].children));
        //StringSet.map(childIds, childId => StringSet.insertInPlace(
        //    newNodeId, graph.nodes[childId].parents));

        // New PS graph
        graph.updatePurs(Purs.AddNode.create(graph.copyNode(newNodeBody)));
        for (i=0; i<StringSet.cardinality(parentIds); i++) {
            graph.updatePurs(Purs.AddEdge.create(
                {"from": StringSet.lookupIndex(i, parentIds), "to":   newNodeId}));
        }
        for (i=0; i<StringSet.cardinality(childIds); i++) {
            graph.updatePurs(Purs.AddEdge.create(
                {"from": newNodeId, "to":   StringSet.lookupIndex(i, childIds)}));
        }

        graph.focusOn(newNodeId);

        return newNodeId;
    };

    graph.removeFocusedNode = function () {
        focusedNode = graph.nodes[graph.focusNode];
        if (StringSet.cardinality(focusedNode.parents) > 0) {
            nextFocusId = StringSet.lookupIndex(0, focusedNode.parents);
        } else if (StringSet.cardinality(focusedNode.children) > 0) {
            nextFocusId = StringSet.lookupIndex(0, focusedNode.children);
        } else {
            // Give the focus to the first node in the list, because what else are
            // you going to do
            nextFocusId = Utils.arrayWithoutElement(graph.focusNode, Object.keys(graph.nodes))[0];
        }
        for(i=0; i<Object.values(focusedNode.subgraphNodes).length; i++) {
        }
        graph.deleteNode(graph.focusNode);
        graph.focusOn(nextFocusId);
        return graph;
    };

    graph.deleteNode = function (nodeToRemoveId) {
        // Remove edges to/from the node in other
        // node objects
        for (i=0; i<StringSet.cardinality(graph.nodes[nodeToRemoveId].parents); i++) {
            parentId = StringSet.lookupIndex(i, graph.nodes[nodeToRemoveId].parents);
            graph.deleteEdge(parentId, nodeToRemoveId);
        }
        for (i=0; i<StringSet.cardinality(graph.nodes[nodeToRemoveId].children); i++) {
            childId = StringSet.lookupIndex(i, graph.nodes[nodeToRemoveId].children);
            graph.deleteEdge(nodeToRemoveId, childId);
        }
        // Remove the node
        //delete graph.nodes[nodeToRemoveId];
        graph.updatePurs(Purs.RemoveNode.create(graph.nodes[nodeToRemoveId]));
        return graph;
    };

    graph.addEdge = function(sourceId, targetId) {
        //// Old JS graph
        //StringSet.insertInPlace(targetId, graph.nodes[sourceId].children);
        //StringSet.insertInPlace(sourceId, graph.nodes[targetId].parents);

        // New PS graph
        graph.updatePurs(Purs.AddEdge.create({"from": sourceId, "to": targetId}));

        return graph;
    };

    graph.deleteEdge = function(sourceId, targetId) {
        //// Old JS graph
        //StringSet.deleteInPlace(targetId, graph.nodes[sourceId].children);
        //StringSet.deleteInPlace(sourceId, graph.nodes[targetId].parents);

        // New PS graph
        graph.updatePurs(Purs.RemoveEdge.create({"from": sourceId, "to": targetId}));

        return graph;
    };

    graph.updateText = function(nodeId, text) {
        //graph.nodes[nodeId].text = text;
        graph.updatePurs(Purs.UpdateText.create(nodeId)(text));

        return graph;
    };

    graph.moveNode = function (nodeId, newPos) {
        //graph.nodes[nodeId].x = newPos.x;
        //graph.nodes[nodeId].y = newPos.y;
        graph.updatePurs(Purs.MoveNode.create(nodeId)(newPos));
        return graph;
    };

    graph.removeEdgesToFromStringSet = function (selection) {
        for (i=0; i<StringSet.cardinality(selection); i++) {
            currentNodeId = StringSet.lookupIndex(i, selection);
            for (j=0; j<StringSet.cardinality(graph.nodes[currentNodeId].parents); j++) {
                parentId = StringSet.lookupIndex(j, graph.nodes[currentNodeId].parents);
                if (!StringSet.isIn(parentId, selection)) {
                    //// Old JS graph
                    //StringSet.deleteInPlace(
                    //    currentNodeId,
                    //    graph.nodes[parentId].children);
                    // New PS graph
                    graph.updatePurs(Purs.RemoveChild.create(parentId)(currentNodeId));
                }
            }
            for (j=0; j<StringSet.cardinality(graph.nodes[currentNodeId].children); j++) {
                childId = StringSet.lookupIndex(j, graph.nodes[currentNodeId].children);
                if (!StringSet.isIn(childId, selection)) {
                    //// Old JS graph
                    //StringSet.deleteInPlace(
                    //    currentNodeId,
                    //    graph.nodes[childId].parents);
                    // New PS graph
                    graph.updatePurs(Purs.RemoveParent.create(childId)(currentNodeId));
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
        return graph.getParentsOfNodes(
            StringSet.toArray(nodeIdStringSet).map(nodeId => graph.nodes[nodeId])
        );
    };

    graph.getParentsOfNodes = function (nodeArr) {
        return StringSet.fromArray(
            Utils.concatenate(
                nodeArr.map(
                    node => StringSet.toArray(node.parents)
                )
            ).filter(
                nodeId => !Utils.isIn(nodeId, nodeArr.map(node => node.id))
            )
        );
    };

    graph.getChildrenOfStringSet = function (nodeIdStringSet) {
        return graph.getChildrenOfNodes(
            StringSet.toArray(nodeIdStringSet).map(nodeId => graph.nodes[nodeId])
        );
    };

    graph.getChildrenOfNodes = function (nodeArr) {
        return StringSet.fromArray(
            Utils.concatenate(
                nodeArr.map(
                    node => StringSet.toArray(node.children)
                )
            ).filter(
                nodeId => !Utils.isIn(nodeId, nodeArr.map(node => node.id))
            )
        );
    };

    graph.extractNodes = function (nodeIdStringSet) {
        extractedNodes = {};
        for (i=0; i<StringSet.cardinality(nodeIdStringSet); i++) {
            nodeId = StringSet.lookupIndex(i, nodeIdStringSet);
            extractedNodes[nodeId] =
                graph.nodes[nodeId];
            //delete graph.nodes[nodeId];
            graph.updatePurs(Purs.RemoveNode.create(graph.nodes[nodeId]));
        }
        return extractedNodes;
    };

    graph.restoreSubgraphNodes = function (newCenterPoint, subgraphNodes, groupText) {
        // Make up for motion of the group node by applying the change in
        // position of the group node to all subgraphNodes.
        terminalestNode = Purs.fromMaybe(undefined)(
            Purs.terminalestNode(Object.values(subgraphNodes))
        );
        groupMovementVector = {
            "x": newCenterPoint.x - terminalestNode.x,
            "y": newCenterPoint.y - terminalestNode.y,
        };
        for (i=0; i<Object.keys(subgraphNodes).length; i++) {
            subgraphNode = graph.copyNode(Object.values(subgraphNodes)[i]);
            subgraphNode.x += groupMovementVector.x;
            subgraphNode.y += groupMovementVector.y;
            // Add nodes to graph top level
            //graph.nodes[subgraphNodeId] = subgraphNode;
            graph.updatePurs(Purs.AddNode.create(subgraphNode));
        }
        graph.updatePurs(Purs.UpdateText.create(terminalestNode.id)(groupText));

        return graph;
    };

    // Adds changes to gorup node edges to the terminal node of the subgraph.
    // Adding edges only makes sense after subgraph nodes are restored.
    graph.copyEdgeModsToTerminalestNode = function (groupNode) {
        groupNodesStringSet = StringSet.fromArray(Object.keys(groupNode.subgraphNodes));
        groupNodes = Object.values(groupNode.subgraphNodes);

        groupParents = graph.getParentsOfNodes(groupNodes);
        groupChildren = graph.getChildrenOfNodes(groupNodes);

        groupNodeParents = graph.getParentsOfNodes([groupNode]);
        groupNodeChildren = graph.getChildrenOfNodes([groupNode]);

        newParents = StringSet.subtract(groupNodeParents, groupParents);
        deletedParents = StringSet.subtract(groupParents, groupNodeParents);

        newChildren = StringSet.subtract(groupNodeChildren, groupChildren);
        deletedChildren = StringSet.subtract(groupChildren, groupNodeChildren);

        terminalestNode = Purs.fromMaybe(undefined)(
            Purs.terminalestNode(groupNodes));

        for (i=0; i<StringSet.cardinality(newParents); i++) {
            graph.addEdge(StringSet.lookupIndex(i, newParents),
                          terminalestNode.id);
        };
        for (i=0; i<StringSet.cardinality(deletedParents); i++) {
            for (j=0; j<StringSet.cardinality(groupNodesStringSet); j++) {
                graph.deleteEdge(
                    StringSet.lookupIndex(i, deletedParents),
                    StringSet.lookupIndex(j, groupNodesStringSet));
            };
        };
        for (i=0; i<StringSet.cardinality(newChildren); i++) {
            graph.addEdge(terminalestNode.id,
                          StringSet.lookupIndex(i, newChildren));
        };
        for (i=0; i<StringSet.cardinality(deletedChildren); i++) {
            for (j=0; j<StringSet.cardinality(groupNodesStringSet); j++) {
                graph.deleteEdge(
                    StringSet.lookupIndex(j, groupNodesStringSet),
                    StringSet.lookupIndex(i, deletedChildren));
            };
        };

        return graph;
    };


    ///////////////////////////////////
    //////// Grouping/ungrouping

    graph.groupHighlighted = function () {
        parents = graph.getParentsOfStringSet(graph.highlightedNodes);
        children = graph.getChildrenOfStringSet(graph.highlightedNodes);
        terminalestNode = Purs.fromMaybe(undefined)(
            Purs.terminalestNode(Purs.lookupNodes(graph)(graph.highlightedNodes)));

        groupNodeId = graph.createNode(terminalestNode.x, terminalestNode.y, parents, children);
        graph.updateText(groupNodeId, terminalestNode.text);

        graph.removeEdgesToFromStringSet(graph.highlightedNodes);

        // Hide the highlighted nodes inside the group node
        newSubgraphNodes = graph.extractNodes(graph.highlightedNodes);
        //graph.nodes[groupNodeId].subgraphNodes = newSubgraphNodes;
        graph.updatePurs(Purs.UpdateSubgraphNodes.create(groupNodeId)(newSubgraphNodes));

        graph.clearHighlights();
        graph.focusOn(groupNodeId);

        return graph;
    };

    graph.expandGroup = function (groupNodeId) {
        groupNode = graph.nodes[groupNodeId];

        graph.restoreSubgraphNodes(groupNode,
                                   groupNode.subgraphNodes,
                                   groupNode.text);

        graph.restoreEdgesToFromSubgraph(graph.nodes[groupNodeId].subgraphNodes);

        graph.copyEdgeModsToTerminalestNode(groupNode);

        graph.removeEdgesToFromStringSet(StringSet.singleton(groupNodeId));

        // Pick the first node of group to have the focus
        // TODO: terminalestNode
        newFocusedNodeId = Object.keys(graph.nodes[groupNodeId].subgraphNodes)[0];
        graph.focusOn(newFocusedNodeId);

        // Highlight expanded group
        newHighlightedNodes = StringSet.fromArray(Object.keys(graph.nodes[groupNodeId].subgraphNodes));
        graph.replaceHighlighted(newHighlightedNodes);

        // Remove group node
        graph.deleteNode(groupNodeId);

        return graph;
    };

    graph.expandGroupInFocus = function () {
        if (!Utils.isEmptyObject(graph.nodes[graph.focusNode].subgraphNodes)) {
            graph.expandGroup(graph.focusNode);
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
        parents = graph.nodes[graph.focusNode].parents;
        if (StringSet.cardinality(parents) > 0) {
            newFocus = StringSet.lookupIndex(0, parents);
            graph.focusOn(newFocus);
        }
        return graph;
    };

    graph.traverseDown = function () {
        children = graph.nodes[graph.focusNode].children;
        if (StringSet.cardinality(children) > 0) {
            newFocus = StringSet.lookupIndex(0, children);
            graph.focusOn(newFocus);
        }
        return graph;
    };

    graph.traverseLeft = function () {
        newFocus = graph.getNeighboringSiblingOrCoparentIds(
            graph.focusNode).left;
        graph.focusOn(newFocus);
        return graph;
    };

    graph.traverseRight = function () {
        newFocus = graph.getNeighboringSiblingOrCoparentIds(
            graph.focusNode).right;
        graph.focusOn(newFocus);
        return graph;
    };

    graph.traverseAddGroup = function (traversalFunc) {
        return function () {
            graph.focusOn(graph.nodes[traverselFunc]);
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
            nodeId => graph.nodes[nodeId].x < graph.nodes[graph.focusNode].x);
        onRight = siblingsAndCoparentsIds.filter(
            nodeId => graph.nodes[nodeId].x > graph.nodes[graph.focusNode].x);
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
        //graph.focusNode = nodeId;
        graph.updatePurs(Purs.UpdateFocus.create(nodeId));
    };

    graph.unHighlightNode = function(nodeId) {
        //// Old JS graph
        //StringSet.deleteInPlace(nodeId, graph.highlightedNodes);
        // New PS graph
        graph.updatePurs(Purs.UnHighlight.create(nodeId));
    };

    graph.highlightNode = function(nodeId) {
        //// Old JS graph
        //StringSet.insertInPlace(nodeId, graph.highlightedNodes);
        // New PS graph
        graph.updatePurs(Purs.Highlight.create(nodeId));
    };

    graph.highlightFocusNode = function () {
        graph.highlightNode(graph.focusNode);
        return graph;
    };

    graph.unHighlightFocusNode = function () {
        graph.unHighlightNode(graph.focusNode);
        return graph;
    };

    graph.toggleHighlightFocusNode = function () {
        if (!StringSet.isIn(graph.focusNode, graph.highlightedNodes)) {
            graph.highlightFocusNode();
        } else {
            graph.unHighlightFocusNode();
        }
        return graph;
    };

    graph.replaceHighlighted = function(newHighlighted) {
        oldHighlightedNodes = StringSet.copy(graph.highlightedNodes);
        for (i=0; i<StringSet.cardinality(oldHighlightedNodes); i++) {
            graph.unHighlightNode(
                StringSet.lookupIndex(i, oldHighlightedNodes));
        };
        for (i=0; i<StringSet.cardinality(newHighlighted); i++) {
            graph.highlightNode(
                StringSet.lookupIndex(i, newHighlighted));
        };
    };

    graph.clearHighlights = function () {
        graph.replaceHighlighted(StringSet.empty());
        return graph;
    };

    ///////////////////////////////////
    //////// Node Spatial Arrangement

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
