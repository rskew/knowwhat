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

function Graph(graphNodes, focus, highlighted) {
    var graph = this;
    //graph.nodes = graphNodes;
    //graph.focus = focus;
    //graph.highlighted= highlighted;

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
    for (i=0; i<Object.keys(highlighted).length; i++) {
        graph.updatePurs(Purs.Highlight.create(StringSet.lookupIndex(i, highlighted)));
    }
    graph.updatePurs(Purs.UpdateFocus.create(focus));

    graph.usePursGraph = function() {
        builtPursGraph = Purs.buildGraph(graph.pursGraph);
        graph.nodes = Utils.deepCopyObject(builtPursGraph.nodes);
        graph.focus = builtPursGraph.focus;
        graph.highlighted = builtPursGraph.highlighted;
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

    graph.getEdgeNodes = function () {
        return [].concat.apply(
            [], [].concat.apply(
                [], Object.entries(graph.nodes).map(
                    node => StringSet.toArray(node[1].children).map(
                        target => ({"source": graph.nodes[node[0]],
                                    "target": graph.nodes[target]})))));
    };

    graph.newNodeBelowFocus = function () {
        if (Purs.fromFocus(graph.focus) != null) {
            console.log(graph.focus);
            console.log(Purs.fromFocus(graph.focus));
            newNodePos = graph.getNewNodePosition(Purs.fromFocus(graph.focus));
            graph.createNode(newNodePos.x,
                             newNodePos.y,
                             StringSet.singleton(Purs.fromFocus(graph.focus)),
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
            console.log(StringSet.lookupIndex(i, parentIds));
            console.log(newNodeId);
            graph.updatePurs(Purs.AddEdge.create(
                {"source": StringSet.lookupIndex(i, parentIds), "target": newNodeId}));
        }
        for (i=0; i<StringSet.cardinality(childIds); i++) {
            graph.updatePurs(Purs.AddEdge.create(
                {"source": newNodeId, "target":   StringSet.lookupIndex(i, childIds)}));
        }

        graph.focusOnNode(newNodeId);

        return newNodeId;
    };

    graph.removeFocused = function () {
        graph.pursGraph = Purs.removeFocus(graph)(graph.pursGraph);
        //focusedNode = graph.nodes[Purs.fromFocus(graph.focus)];
        //if (StringSet.cardinality(focusedNode.parents) > 0) {
        //    nextFocusId = StringSet.lookupIndex(0, focusedNode.parents);
        //} else if (StringSet.cardinality(focusedNode.children) > 0) {
        //    nextFocusId = StringSet.lookupIndex(0, focusedNode.children);
        //} else {
        //    // Give the focus to the first node in the list, because what else are
        //    // you going to do
        //    nextFocusId = Utils.arrayWithoutElement(Purs.fromFocus(graph.focus), Object.keys(graph.nodes))[0];
        //}
        //for(i=0; i<Object.values(focusedNode.subgraphNodes).length; i++) {
        //}
        //graph.deleteNode(Purs.fromFocus(graph.focus));
        //graph.focusOnNode(nextFocusId);
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
        graph.updatePurs(Purs.AddEdge.create({"source": sourceId, "target": targetId}));

        return graph;
    };

    graph.deleteEdge = function(sourceId, targetId) {
        //// Old JS graph
        //StringSet.deleteInPlace(targetId, graph.nodes[sourceId].children);
        //StringSet.deleteInPlace(sourceId, graph.nodes[targetId].parents);

        // New PS graph
        graph.updatePurs(Purs.RemoveEdge.create({"source": sourceId, "target": targetId}));

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
        parents = graph.getParentsOfStringSet(graph.highlighted);
        children = graph.getChildrenOfStringSet(graph.highlighted);
        terminalestNode = Purs.fromMaybe(undefined)(
            Purs.terminalestNode(Purs.lookupNodes(graph)(graph.highlighted)));

        groupNodeId = graph.createNode(terminalestNode.x, terminalestNode.y, parents, children);
        graph.updateText(groupNodeId, terminalestNode.text);

        graph.removeEdgesToFromStringSet(graph.highlighted);

        // Hide the highlighted nodes inside the group node
        newSubgraphNodes = graph.extractNodes(graph.highlighted);
        //graph.nodes[groupNodeId].subgraphNodes = newSubgraphNodes;
        graph.updatePurs(Purs.UpdateSubgraphNodes.create(groupNodeId)(newSubgraphNodes));

        graph.clearHighlights();
        graph.focusOnNode(groupNodeId);

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
        graph.focusOnNode(newFocusedNodeId);

        // Highlight expanded group
        newHighlightedNodes = StringSet.fromArray(Object.keys(graph.nodes[groupNodeId].subgraphNodes));
        graph.replaceHighlighted(newHighlightedNodes);

        // Remove group node
        graph.deleteNode(groupNodeId);

        return graph;
    };

    graph.expandGroupInFocus = function () {
        if (!Utils.isEmptyObject(graph.nodes[Purs.fromFocus(graph.focus)].subgraphNodes)) {
            graph.expandGroup(Purs.fromFocus(graph.focus));
        }
    };

    graph.toggleGroupExpand = function () {
        if (StringSet.cardinality(graph.highlighted) == 0) {
            graph.expandGroupInFocus();
        } else {
            graph.groupHighlighted();
        }
    };


    ///////////////////////////////////
    //////// Traversal functions

    graph.traverseUp = function () {
        graph.pursGraph = Purs.traverseUp(graph)(graph.pursGraph);
        //parents = graph.nodes[Purs.fromFocus(graph.focus)].parents;
        //if (StringSet.cardinality(parents) > 0) {
        //    newFocus = StringSet.lookupIndex(0, parents);
        //    graph.focusOnNode(newFocus);
        //}
        return graph;
    };

    graph.traverseDown = function () {
        graph.pursGraph = Purs.traverseDown(graph)(graph.pursGraph);
        //children = graph.nodes[Purs.fromFocus(graph.focus)].children;
        //if (StringSet.cardinality(children) > 0) {
        //    newFocus = StringSet.lookupIndex(0, children);
        //    graph.focusOnNode(newFocus);
        //}
        return graph;
    };

    graph.traverseLeft = function () {
        graph.pursGraph = Purs.traverseLeft(graph)(graph.pursGraph);
        //newFocus = graph.getNeighboringSiblingOrCoparentIds(
        //    Purs.fromFocus(graph.focus)).left;
        //graph.focusOnNode(newFocus);
        return graph;
    };

    graph.traverseRight = function () {
        graph.pursGraph = Purs.traverseRight(graph)(graph.pursGraph);
        //newFocus = graph.getNeighboringSiblingOrCoparentIds(
        //    Purs.fromFocus(graph.focus)).right;
        //graph.focusOnNode(newFocus);
        return graph;
    };

    //graph.traverseAddGroup = function (traversalFunc) {
    //    return function () {
    //        graph.focusOnNode(graph.nodes[traverselFunc]);
    //        return graph;
    //    };
    //};

    //graph.getNeighboringSiblingOrCoparentIds = function (nodeId) {
    //    /*
    //      Find the nodes that share a parent or child of the given node.
    //      Sort the nodes by their x location, such that left/right movement
    //      is spatially coherent.
    //      */
    //    siblingsAndCoparentsIds = [];
    //    StringSet.map(graph.nodes[nodeId].parents,
    //        parentId => StringSet.map(graph.nodes[parentId].children,
    //            siblingId => siblingsAndCoparentsIds.push(siblingId)
    //        )
    //    );
    //    StringSet.map(graph.nodes[nodeId].children,
    //        childId => StringSet.map(graph.nodes[childId].parents,
    //            coparentId => siblingsAndCoparentsIds.push(coparentId))
    //    );
    //    // Sort siblings by x index.
    //    // Store graph so that the sorting comparison function can access it,
    //    // as it appears to run outside the constructor context.
    //    graphNodes = graph.nodes;
    //    siblingsAndCoparentsIds.sort((a, b) => graphNodes[a].x - graphNodes[b].x);
    //    onLeft = siblingsAndCoparentsIds.filter(
    //        nodeId => graph.nodes[nodeId].x < graph.nodes[Purs.fromFocus(graph.focus)].x);
    //    onRight = siblingsAndCoparentsIds.filter(
    //        nodeId => graph.nodes[nodeId].x > graph.nodes[Purs.fromFocus(graph.focus)].x);
    //    return {
    //        "left": onLeft.length > 0
    //            ? onLeft[onLeft.length - 1]
    //            : siblingsAndCoparentsIds[siblingsAndCoparentsIds.length-1],
    //        "right": onRight.length > 0 ? onRight[0] : siblingsAndCoparentsIds[0],
    //    };
    //};

    ///////////////////////////////////
    //////// Highlighting a selection/focusing

    graph.focusOnNode = function (id) {
        graph.focusOn(Purs.FocusNode.create(id));
    };

    graph.focusOnEdge = function (id) {
        graph.focusOn(Purs.FocusEdge.create(id));
    };

    graph.focusOn = function (focus) {
        graph.updatePurs(Purs.UpdateFocus.create(focus));
    };

    graph.unHighlightNode = function(nodeId) {
        //// Old JS graph
        //StringSet.deleteInPlace(nodeId, graph.highlighted);
        // New PS graph
        graph.updatePurs(Purs.UnHighlight.create(nodeId));
    };

    graph.highlightNode = function(nodeId) {
        //// Old JS graph
        //StringSet.insertInPlace(nodeId, graph.highlighted);
        // New PS graph
        graph.updatePurs(Purs.Highlight.create(nodeId));
    };

    graph.highlightFocus = function () {
        graph.highlightNode(Purs.fromFocus(graph.focus));
        return graph;
    };

    graph.unHighlightFocus = function () {
        graph.unHighlightNode(Purs.fromFocus(graph.focus));
        return graph;
    };

    graph.toggleHighlightFocus = function () {
        if (!StringSet.isIn(Purs.fromFocus(graph.focus), graph.highlighted)) {
            graph.highlightFocus();
        } else {
            graph.unHighlightFocus();
        }
        return graph;
    };

    graph.replaceHighlighted = function(newHighlighted) {
        oldHighlightedNodes = StringSet.copy(graph.highlighted);
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
