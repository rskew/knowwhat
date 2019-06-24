const d3 = require("./libs/d3.js");
const Utils = require("./utils");
var PursCore = require('./purescript/output/Workflow.Core/index.js');
var PursInteraction = require('./purescript/output/Workflow.Interaction/index.js');
var PursInteractionImpl = require('./purescript/output/Workflow.Interaction.Impl/index.js');
var Purs = {...PursCore, ...PursInteraction, ...PursInteractionImpl};
var FileSaver = require("./libs/FileSaver.min.js");
var CodeMirror = require('./libs/CodeMirror');
// Just importing vim.js is enough to make the vim bindings work apparently :/
var vim = require('./libs/CodeMirror/keymap/vim.js');


module.exports = function GraphUI(graph) {

    var graphUI = this;
    graphUI.graph = graph;

    ///////////////////////////////////
    //////// Actual External API (work in progress)

    graphUI.registerNodeValidHook = function (hookFn) {
        graphUI.nodeValidHook = hookFn;
    };

    graphUI.registerEdgeValidHook = function (hookFn) {
        graphUI.edgeValidHook = hookFn;
    };


    ///////////////////////////////////
    //////// Explicit state

    graphUI.mouseState = {
        "clickedNode": undefined,
        "drawingEdge": [],
        "drawingEdgeValid": true,
        "mouseoverNode": undefined,
    };

    graphUI.background_origin = { "x": 0, "y": 0};
    graphUI.background_drag_enable = true;

    // Keyboard modes: normal, insert, visual
    graphUI.keyboardMode = "normal";

    // Hooks
    graphUI.nodeValidHook = function (graph, node) {
        return true;
    };

    graphUI.edgeValidHook = function (graph, parentNode, childNode) {
        return true;
    };


    ///////////////////////////////////
    //////// Constants
    graphUI.fadeSpeed = 150;
    graphUI.gridSize = 50;
    graphUI.scrollAmplifier = 30;
    graphUI.scrollTickPeriod = 50; // ms
    graphUI.newNodeOffset = { x: 100.0, y: 100.0 };
    graphUI.dragDropEdgeThreshold = 50;

    graphUI.codeMirrorOptions = {
        inputStyle: "contenteditable",
        keyMap: "vim",
        matchBrackets: true,
        showCursorWhenSelecting: true,
    };


    ///////////////////////////////////
    //////// D3

    graphUI.svg = d3.select("body").append("svg");

    graphUI.nonsvg = d3.select("body").append("div")
        .classed("nonsvg", true);
    graphUI.svg
        .on("click", function (e) {
            console.log("click on backgnd");
            graphUI.graph =
                Purs.interGraph.updateFocus(
                    Purs.NoFocus.value)(
                    graphUI.graph
                );
            graphUI.keyboardMode = "normal";
            graphUI.update();
        })
        .on("dblclick", function (e) {
            var newNode =
                Purs.graphNode.createNode(
                    {})({})();
            graphUI.graph =
                Purs.graph.insertNode(
                    newNode
                )(
                    graphUI.graph
                );
            graphUI.graph = Purs.interGraph.updateNodePosition(
                {"x": d3.event.pageX - graphUI.background_origin.x,
                 "y": d3.event.pageY - graphUI.background_origin.y})(
                newNode.id)(
                     graphUI.graph
            );
            graphUI.graph =
                Purs.interGraph.updateFocus(
                    Purs.FocusNode.create(newNode.id)
                )(
                    graphUI.graph
                );
            graphUI.update();
        })
        .datum(graphUI.background_origin)
        .call(d3.drag()
              .on("start", dragstarted_background)
              .on("drag", dragged_background)
              .on("end", dragended_background));

    // Define Z axis ordering of elements
    graphUI.svg.append("g").attr("id", "nodeHalos");
    graphUI.svg.append("g").attr("id", "edges");
    graphUI.svg.append("g").attr("id", "edgeBorders");
    graphUI.svg.append("g").attr("id", "nodes");
    graphUI.svg.append("g").attr("id", "nodeBorders");
    graphUI.nonsvg.append("div").attr("id", "text");

    // Markers for arrowheads
    // Normal arrow
    graphUI.svg.append("defs").append("marker")
        .attr("id", "arrow")
        .attr("markerWidth", 10)
        .attr("markerHeight", 10)
        .attr("refX", 15)
        .attr("refY", 5)
        .attr("orient", "auto")
        .attr("markerUnits", "userSpaceOnUse")
        .append("path")
        .attr("d", "M0,0 L0,10 L8,5 z")
        .attr("fill", "#000");

    // For pointing to a group
    graphUI.svg.append("defs").append("marker")
        .attr("id", "arrow-to-group")
        .attr("markerWidth", 10)
        .attr("markerHeight", 10)
        .attr("refX", 19)
        .attr("refY", 5)
        .attr("orient", "auto")
        .attr("markerUnits", "userSpaceOnUse")
        .append("path")
        .attr("d", "M0,0 L0,10 L8,5 z")
        .attr("fill", "#000");

    // For the arrow being drawn (by dragging)
    graphUI.svg.append("defs").append("marker")
        .attr("id", "drawing-arrow")
        .attr("markerWidth", 10)
        .attr("markerHeight", 10)
        .attr("refX", 3)
        .attr("refY", 5)
        .attr("orient", "auto")
        .attr("markerUnits", "userSpaceOnUse")
        .append("path")
        .attr("d", "M0,0 L0,10 L8,5 z")
        .attr("fill", "#000");

    // Hidden file browse button for uploading files
    d3.select("body").append("input")
        .attr("type", "file")
        .on("change", function () {
            graphUI.processFile();
        })
        .style("display", "none");

    graphUI.updateEdges = function () {
        graphUI.svg.select("#edges").selectAll("line").filter(".edge")
            .data(Purs.resolvedGraphEdges(Purs.graph)(graphUI.graph), function (edgeNode) {
                return Purs.edgeId(edgeNode.source.id)(edgeNode.target.id);
            })
            .join(
                enter => enter.append("line")
                    .classed("edge", true)
                    .attr("x1", edgeNode => edgeNode.source.position.x + graphUI.background_origin.x)
                    .attr("y1", edgeNode => edgeNode.source.position.y + graphUI.background_origin.y)
                    .attr("x2", edgeNode => edgeNode.target.position.x + graphUI.background_origin.x)
                    .attr("y2", edgeNode => edgeNode.target.position.y + graphUI.background_origin.y)
                    .classed("focused", edgeNode => {
                        if (graphUI.graph.interactionState.focus.value0 == undefined) {
                            return false;
                        } else {
                            focusEdge = graphUI.graph.interactionState.focus.value0;
                            return Purs.edgeId(edgeNode.source.id)(edgeNode.target.id)
                                == Purs.edgeId(focusEdge.source)(focusEdge.target);
                        }
                    })
                    .attr("marker-end", edgeNode => {
                        if (Utils.isEmptyObject(edgeNode.target.subgraph.nodes)) {
                            return "url(#arrow)";
                        } else {
                            return "url(#arrow-to-group)";
                        }}),
                update => update
                    .attr("x1", edgeNode => edgeNode.source.position.x + graphUI.background_origin.x)
                    .attr("y1", edgeNode => edgeNode.source.position.y + graphUI.background_origin.y)
                    .attr("x2", edgeNode => edgeNode.target.position.x + graphUI.background_origin.x)
                    .attr("y2", edgeNode => edgeNode.target.position.y + graphUI.background_origin.y)
                    .classed("focused", edgeNode => {
                        if (graphUI.graph.interactionState.focus.value0 == undefined) {
                            return false;
                        } else {
                            focusEdge = graphUI.graph.interactionState.focus.value0;
                            return Purs.edgeId(edgeNode.source.id)(edgeNode.target.id)
                                == Purs.edgeId(focusEdge.source)(focusEdge.target);
                        }
                    })
                    .attr("marker-end", edgeNode => {
                        if (Utils.isEmptyObject(edgeNode.target.subgraph.nodes)) {
                            return "url(#arrow)";
                        } else {
                            return "url(#arrow-to-group)";
                        }
                    })
            );
    };

    graphUI.updateEdgeBorders = function () {
        graphUI.svg.select("#edgeBorders").selectAll("line").filter(".edgeBorder")
            .data(Purs.resolvedGraphEdges(Purs.graph)(graphUI.graph),
                  edgeNode => edgeNode.source.id + edgeNode.target.id)
            .join(
                enter => enter.append("line")
                    .classed("edgeBorder", true)
                    .attr("x1", edgeNodes => edgeNodes.source.position.x + graphUI.background_origin.x)
                    .attr("y1", edgeNodes => edgeNodes.source.position.y + graphUI.background_origin.y)
                    .attr("x2", edgeNodes => edgeNodes.target.position.x + graphUI.background_origin.x)
                    .attr("y2", edgeNodes => edgeNodes.target.position.y + graphUI.background_origin.y)
                    .attr("stroke-linecap", "butt")
                    .classed("focusGroup", edgeNodes => Purs.edgeInFocusGroup(Purs.eqInterEdgeImpl)(Purs.interGraph)(graphUI.graph)(
                        Purs.graphEdge.createEdge(edgeNodes.source.id)(edgeNodes.target.id)))
                    .classed("invalid", function (edgeNodes) {
                        // TODO: remove this hack and properly implement validity checking on creation of an edge
                        var edgeValidity = graphUI.edgeValidHook(
                            graphUI.graph, edgeNodes.source, edgeNodes.target);
                        graphUI.graph = Purs.validatableInterGraph.updateEdgeValidity(
                            edgeValidity)(
                            {"source": edgeNodes.source.id,
                             "target": edgeNodes.target.id})(
                                graphUI.graph
                            );
                        return edgeValidity;
                    })
                    .on("click", function(edgeNodes) {
                        d3.event.stopPropagation();
                        graphUI.graph =
                            Purs.interGraph.updateFocus(
                                Purs.FocusEdge.create(
                                    {"source": edgeNodes.source.id, "target": edgeNodes.target.id}
                                )([])
                            )(  graphUI.graph
                             );
                        graphUI.update();
                    })
                    .on("mouseover", function (d) {
                        Utils.fadeIn(this, graphUI.fadeSpeed);
                    })
                    .on("mouseout", function (d) {
                        if (!d3.select(this).classed("grouped") && !d3.select(this).classed("focusGroup")
                            && !d3.select(this).classed("invalid")) {
                            Utils.fadeOut(this, graphUI.fadeSpeed);
                        }
                    }),
                update => update
                    .attr("x1", edgeNodes => edgeNodes.source.position.x + graphUI.background_origin.x)
                    .attr("y1", edgeNodes => edgeNodes.source.position.y + graphUI.background_origin.y)
                    .attr("x2", edgeNodes => edgeNodes.target.position.x + graphUI.background_origin.x)
                    .attr("y2", edgeNodes => edgeNodes.target.position.y + graphUI.background_origin.y)
                    .classed("focusGroup", edgeNodes => Purs.edgeInFocusGroup(Purs.eqInterEdgeImpl)(Purs.interGraph)(graphUI.graph)(
                        Purs.graphEdge.createEdge(edgeNodes.source.id)(edgeNodes.target.id)))
                    .classed("invalid", function (edgeNodes) {
                        edgeDataMaybe = Purs.lookupEdgeData(
                            edgeNodes.source.id)(
                            edgeNodes.target.id)(
                            graphUI.graph
                        );
                        if (!Utils.isEmptyObject(edgeDataMaybe)) {
                            return !edgeDataMaybe.value0;
                        } else {
                            return true;
                        }
                    })
                    .each(function () {
                        if (d3.select(this).classed("focusGroup") ||
                            d3.select(this).classed("invalid")) {
                            Utils.fadeIn(this, graphUI.fadeSpeed);
                        } else {
                            Utils.fadeOut(this, graphUI.fadeSpeed);
                        }
                    })
            );
    };

    graphUI.updateDrawingEdge = function () {
        graphUI.svg.select("#edges").selectAll("line").filter(".drawing")
            .data(graphUI.mouseState.drawingEdge)
            .join(
                enter => enter.append("line")
                    .classed("drawing", true)
                    .attr("x1", d => d.source.x)
                    .attr("y1", d => d.source.y)
                    .attr("x2", d => d.target.x)
                    .attr("y2", d => d.target.y)
                    .classed("invalid", d => (graphUI.mouseState.mouseoverNode != undefined) && !graphUI.mouseState.drawingEdgeValid)
                    .attr("marker-end", "url(#drawing-arrow)"),
                update => update
                    .attr("x2", d => d.target.x)
                    .attr("y2", d => d.target.y)
                    .classed("invalid", d => (graphUI.mouseState.mouseoverNode != undefined)
                                              && !graphUI.mouseState.drawingEdgeValid)
            );
    };

    graphUI.updateText = function () {
        graphUI.nonsvg.select("#text").selectAll("div").filter(".CodeMirror")
            .data(Object.entries(graphUI.graph.nodes), d => d[0])
            .join(
                enter => enter
                    .append(function (d) {
                        var codeMirror = CodeMirror(
                            function(){},
                            graphUI.codeMirrorOptions);
                        codeMirror.setValue(d[1].text);
                        codeMirror.on("focus", function (e) {
                            console.log("focusing on node");
                            graphUI.graph = Purs.interGraph.updateFocus(
                                Purs.FocusNode.create(d[0]))(
                                graphUI.graph
                            );
                            graphUI.keyboardMode = "insert";
                            graphUI.update();
                        });
                        return codeMirror.getWrapperElement();
                    })
                    .style("left", d => (d[1].position.x + 20 + graphUI.background_origin.x) + "px")
                    .style("top", d => (d[1].position.y - 10 + graphUI.background_origin.y) + "px")
                    .each(function (d) {
                        this.CodeMirror.refresh();
                    })
                    .on("keyup", function (d) {
                        graphUI.graph = Purs.interGraph.updateText(
                            d[0])(
                            this.CodeMirror.getValue())(
                                graphUI.graph
                            );
                        updateValidityOfNodeAndIncidentEdges(
                            graphUI.graph.nodes[d[0]]
                        );
                        graphUI.update();
                    }),
                update => update
                    .style("left", d => (d[1].position.x + 20 + graphUI.background_origin.x) + "px")
                    .style("top", d => (d[1].position.y - 10 + graphUI.background_origin.y) + "px")
                    .each(function (d) {
                        if (graphUI.keyboardMode == "insert" &&
                              d[0] == graphUI.graph.interactionState.focus.value0) {
                            this.CodeMirror.focus();
                        } else {
                            this.CodeMirror.getInputField().blur();
                        }
                    })
            );};

    graphUI.updateNodes = function () {
        graphUI.svg.select("#nodes").selectAll("circle").filter(".node")
            .data(Object.entries(graphUI.graph.nodes), d => d[0])
            .join(
                enter => enter.append("circle")
                    .attr("class", graphUI.keyboardMode)
                    .classed("node", true)
                    .attr("r", d => {
                        if (Object.keys(d[1].subgraph.nodes).length > 0) {
                            return 12;
                        } else {
                            return 7;
                        }
                    })
                    .attr("cx", d => d[1].position.x + graphUI.background_origin.x)
                    .attr("cy", d => d[1].position.y + graphUI.background_origin.y)
                    .classed("focused", d => d[0] == graphUI.graph.interactionState.focus.value0),
                update => update
                    .attr("class", graphUI.keyboardMode)
                    .classed("node", true)
                    .attr("cx", d => d[1].position.x + graphUI.background_origin.x)
                    .attr("cy", d => d[1].position.y + graphUI.background_origin.y)
                    .classed("focused", d => d[0] == graphUI.graph.interactionState.focus.value0)
                    .attr("r", d => {
                        if (Object.keys(d[1].subgraph.nodes).length > 0) {
                            return 12;
                        } else {
                            return 7;
                        }
                    })
            );
    };

    graphUI.updateNodeBorders = function () {
        borders = graphUI.svg.select("#nodeBorders").selectAll("circle").filter(".nodeBorder")
            .data(Object.entries(graphUI.graph.nodes), d => d[0])
            .join(
                enter => enter.append("circle")
                    .classed("nodeBorder", true)
                    .attr("r", 28)
                    .attr("cx", d => d[1].position.x + graphUI.background_origin.x)
                    .attr("cy", d => d[1].position.y + graphUI.background_origin.y)
                    .classed("grouped", d => Purs.nodeIdSetMember(d[0])(graphUI.graph.interactionState.highlighted))
                    .classed("invalid", d => !d[1].isValid)
                    .on("click", function (d) {
                        d3.event.stopPropagation();
                        graphUI.graph = Purs.interGraph.updateFocus(
                            Purs.FocusNode.create(d[0]))(
                            graphUI.graph
                        );
                        graphUI.update();
                    })
                    .call(d3.drag()
                          .on("start", dragstarted_node)
                          .on("drag", dragged_node)
                          .on("end", dragended_node))
                    .on("mouseover", function (d) {
                        graphUI.mouseState.mouseoverNode = d[0];
                        Utils.fadeIn(this, graphUI.fadeSpeed);
                    })
                    .on("mouseout", function (d) {
                        graphUI.mouseState.mouseoverNode = undefined;
                        if (!d3.select(this).classed("grouped") &&
                            !d3.select(this).classed("invalid")) {
                            Utils.fadeOut(this, graphUI.fadeSpeed);
                        }
                    })
                    .on("dblclick", function (d) {
                        if (d3.event.defaultPrevented) return;
                        d3.event.stopPropagation();
                        graphUI.graph = Purs.interGraph.updateFocus(
                            Purs.FocusNode.create(d[0]))(
                            graphUI.graph
                         );
                        graphUI.graph = Purs.toggleHighlightFocus(Purs.interGraph)(graphUI.graph);
                        graphUI.update();
                    })
                    .each(function () {
                        if (d3.select(this).classed("grouped") ||
                            d3.select(this).classed("invalid")) {
                            Utils.fadeIn(this, graphUI.fadeSpeed);
                        } else {
                            Utils.fadeOut(this, graphUI.fadeSpeed);
                        }
                    }),
                update => update
                    .attr("cx", d => d[1].position.x + graphUI.background_origin.x)
                    .attr("cy", d => d[1].position.y + graphUI.background_origin.y)
                    .classed("grouped", d => Purs.nodeIdSetMember(d[0])(graphUI.graph.interactionState.highlighted))
                    .classed("invalid", d => !d[1].isValid)
                    .each(function () {
                        if (d3.select(this).classed("grouped") ||
                            d3.select(this).classed("invalid")) {
                            Utils.fadeIn(this, graphUI.fadeSpeed);
                        } else {
                            Utils.fadeOut(this, graphUI.fadeSpeed);
                        }
                    })
            );
    };

    graphUI.updateNodeHalos = function () {
        graphUI.svg.select("#nodeHalos").selectAll("circle").filter(".halo")
            .data(Object.entries(graphUI.graph.nodes), d => d[0])
            .join(
                enter => enter.append("circle")
                    .classed("halo", true)
                    .attr("r", "40")
                    .attr("cx", d => d[1].position.x + graphUI.background_origin.x)
                    .attr("cy", d => d[1].position.y + graphUI.background_origin.y)
                    .on("mouseover", function (d) {
                        graphUI.mouseState.mouseoverNode = d[0];
                        d3.select(this).classed("ready", false);
                        d3.select(this).classed("invalid", false);
                        var edgeProposed = validEdgeIsProposed();
                        if (edgeProposed.edgeIsProposed) {
                            if (edgeProposed.validity) {
                                d3.select(this).classed("ready", true);
                                graphUI.mouseState.drawingEdgeValid = true;
                            } else {
                                graphUI.mouseState.drawingEdgeValid = false;
                                console.log("Invalid edge!!");
                                d3.select(this).classed("invalid", true);
                            }
                        }
                        Utils.fadeIn(this, graphUI.fadeSpeed);
                        graphUI.update();
                    })
                    .on("mouseout", function () {
                        graphUI.mouseState.mouseoverNode = undefined;
                        Utils.fadeOut(this, graphUI.fadeSpeed);
                    })
                    .classed("focused", d => d[0] == graphUI.graph.interactionState.focus.value0)
                    .each(function () {
                        if (d3.select(this).classed("focused")) {
                            Utils.fadeIn(this, graphUI.fadeSpeed);
                        } else {
                            Utils.fadeOut(this, graphUI.fadeSpeed);
                        }
                    })
                    .call(d3.drag()
                          .on("start", dragstarted_halo)
                          .on("drag", dragged_halo)
                          .on("end", dragended_halo)),
                update => update
                    .attr("cx", d => d[1].position.x + graphUI.background_origin.x)
                    .attr("cy", d => d[1].position.y + graphUI.background_origin.y)
                    .classed("focused", d => d[0] == graphUI.graph.interactionState.focus.value0)
                    .each(function () {
                        if (d3.select(this).classed("focused")) {
                            Utils.fadeIn(this, graphUI.fadeSpeed);
                        } else {
                            Utils.fadeOut(this, graphUI.fadeSpeed);
                        }
                    })
            );
    };

    graphUI.update = function () {
        graphUI.updateEdges();

        graphUI.updateEdgeBorders();

        graphUI.updateDrawingEdge();

        graphUI.updateText();

        graphUI.updateNodes();

        graphUI.updateNodeBorders();

        graphUI.updateNodeHalos();

        // Reset states
        graphUI.svg.on("mouseup", function () {
            graphUI.update();
            graphUI.mouseState.clickedNode = undefined;
            graphUI.mouseState.mouseoverNode = undefined;
            graphUI.mouseState.drawingEdge = [];
            console.log(graphUI.mouseState);
        });
    };

    function dragstarted_background(d) {
        if (d3.event.sourceEvent.ctrlKey) {
            graphUI.background_drag_enable = false;
        } else {
            graphUI.background_drag_enable = true;
            graphUI.update();
        }
    }

    function dragged_background(d) {
        if (graphUI.background_drag_enable) {
            graphUI.background_origin.x = d3.event.x;
            graphUI.background_origin.y = d3.event.y;
            graphUI.update();
        }
    }

    function dragended_background(d) {
        graphUI.update();
    }

    function dragstarted_node(d) {
        graphUI.graph = Purs.interGraph.updateFocus(
            Purs.FocusNode.create(d[0]))(
            graphUI.graph
        );
        graphUI.update();
    }

    function dragged_node(d) {
        graphUI.graph = Purs.interGraph.updateNodePosition(
            { "x": Math.floor((d3.event.x - graphUI.background_origin.x) / graphUI.gridSize) * graphUI.gridSize
            , "y": Math.floor((d3.event.y - graphUI.background_origin.y) / graphUI.gridSize) * graphUI.gridSize
            })(
            d[0])(
                graphUI.graph
            );
        graphUI.update();
    }

    function dragended_node(d) {
        create_edge_if_possible();
        graphUI.update();
    }

    function dragstarted_halo(d) {
        if (Purs.hasFocus(Purs.interGraph)(d[0])(graphUI.graph)) {
            graphUI.mouseState.clickedNode = d[0];
            graphUI.mouseState.drawingEdge = [
                {"source": {"x": d[1].position.x + graphUI.background_origin.x,
                            "y": d[1].position.y + graphUI.background_origin.y},
                 "target": {"x": d3.event.x,
                            "y": d3.event.y}}
            ];
            graphUI.update();
        }
    }

    function dragged_halo(d) {
        if (Purs.hasFocus(Purs.interGraph)(d[0])(graphUI.graph)) {
            graphUI.mouseState.clickedNode = d[0];
            graphUI.mouseState.drawingEdge = [
                {"source": {"x": d[1].position.x + graphUI.background_origin.x,
                            "y": d[1].position.y + graphUI.background_origin.y},
                 "target": {"x": d3.event.x,
                            "y": d3.event.y}}
            ];
            graphUI.update();
        }
    }

    function dragended_halo(d) {
        if (Purs.hasFocus(Purs.interGraph)(d[0])(graphUI.graph)) {
            focusNodeId = Purs.interGraph.viewFocus(graphUI.graph).value0;
            console.log(graphUI.background_origin);
            nearestNeighbor = Purs.getNearestNeighbor(Purs.interGraph)(
                {"x": d3.event.x - graphUI.background_origin.x,
                 "y": d3.event.y - graphUI.background_origin.y})(
                graphUI.graph
            ).value0;
            if (nearestNeighbor.distance < graphUI.dragDropEdgeThreshold &&
                nearestNeighbor.nodeId != focusNodeId) {
                graphUI.graph = Purs.graph.addEdge(
                    Purs.graphEdge.createEdge(
                        focusNodeId)(
                        nearestNeighbor.nodeId))(
                    graphUI.graph
                );
                graphUI.graph = Purs.interGraph.updateFocus(
                    Purs.FocusNode.create(nearestNeighbor.nodeId))(
                    graphUI.graph
                );
            }
            graphUI.mouseState.drawingEdge = [];
            graphUI.mouseState.clickedNode = undefined;
            d3.selectAll("*").classed("ready", false);
            graphUI.update();
        }
    }


    //////
    // TODO: Move code from here down to purs

    function create_edge_if_possible() {
        if (graphUI.mouseState.clickedNode != undefined &&
            graphUI.mouseState.clickedNode != graphUI.mouseState.mouseoverNode &&
            graphUI.mouseState.mouseoverNode != undefined) {

            graphUI.graph = Purs.graph.addEdge(
                { "source": graphUI.mouseState.clickedNode,
                  "target": graphUI.mouseState.mouseoverNode,
                  "isValid": false})(
                graphUI.graph
            );
            graphUI.graph = Purs.interGraph.updateFocus(
                Purs.FocusNode.create(graphUI.mouseState.mouseoverNode))(
                graphUI.graph
            );
        };
    }

    function validEdgeIsProposed() {
        var edgeIsProposed =
            graphUI.mouseState.clickedNode != undefined &&
            graphUI.mouseState.mouseoverNode != undefined &&
            graphUI.mouseState.clickedNode != graphUI.mouseState.mouseoverNode &&
            !Purs.nodeIdSetMember(
                graphUI.mouseState.mouseoverNode)(
                graphUI.graph.nodes[graphUI.mouseState.clickedNode].children);
        if (edgeIsProposed) {
            var sourceNode = graphUI.graph.nodes[graphUI.mouseState.clickedNode];
            var targetNode = graphUI.graph.nodes[graphUI.mouseState.mouseoverNode];
            var validity = graphUI.edgeValidHook(graphUI.graph, sourceNode, targetNode);
            return {"edgeIsProposed": true, "validity": validity};
        }
        return {"edgeIsProposed": false, "validity": false};
    };


    // TODO: implement this subroutine properly in Graphputer
    function updateValidityOfNodeAndIncidentEdges(node) {
        nodeValidity = graphUI.nodeValidHook(graphUI.graph, {"text": node.text});
        graphUI.graph = Purs.validatableInterGraph.updateNodeValidity(
            nodeValidity)(
            node.id)(
                graphUI.graph
            );
        // Check validity of edges from parents
        parents = Purs.lookupNodes(Purs.graph)(
            graphUI.graph.nodes[node.id].parents)(
            graphUI.graph
        );
        for (i=0; i<parents.length; i++) {
            parent = parents[i];
            edgeValidity = graphUI.edgeValidHook(
                graphUI.graph,
                parent,
                node
            );
            graphUI.graph = Purs.validatableInterGraph.updateEdgeValidity(
                edgeValidity)(
                {"source": parent.id,
                 "target": node.id,
                 "isValid": edgeValidity})(
                    graphUI.graph
                );
        }
        // Check validity of edges from children
        children = Purs.lookupNodes(Purs.graph)(
            graphUI.graph.nodes[node.id].children)(
            graphUI.graph
        );
        for (i=0; i<children.length; i++) {
            child = children[i];
            edgeValidity = graphUI.edgeValidHook(
                graphUI.graph,
                node,
                child
            );
            graphUI.graph = Purs.validatableInterGraph.updateEdgeValidity(
                edgeValidity)(
                {"source": node.id,
                 "target": child.id,
                 "isValid": edgeValidity})(
                graphUI.graph
            );
        }
    }

    graphUI.moveFocusDown = function () {
        focusNodeId = graphUI.graph.interactionState.focus.value0;
        node = graphUI.graph.nodes[focusNodeId];
        graphUI.graph = Purs.interGraph.updateNodePosition(
            { "x": node.position.x
            , "y": node.position.y + graphUI.gridSize
            })(
            focusNodeId)(
                graphUI.graph
            );
        return graphUI.graph;
    };

    graphUI.moveFocusUp = function () {
        focusNodeId = graphUI.graph.interactionState.focus.value0;
        node = graphUI.graph.nodes[focusNodeId];
        graphUI.graph = Purs.interGraph.updateNodePosition(
            { "x": node.position.x
            , "y": node.position.y - graphUI.gridSize
            })(
            focusNodeId)(
                graphUI.graph
            );
        return graphUI.graph;
    };

    graphUI.moveFocusLeft = function () {
        focusNodeId = graphUI.graph.interactionState.focus.value0;
        node = graphUI.graph.nodes[focusNodeId];
        graphUI.graph = Purs.interGraph.updateNodePosition(
            { "x": node.position.x - graphUI.gridSize
            , "y": node.position.y
            })(
            focusNodeId)(
                graphUI.graph
            );
        return graphUI.graph;
    };

    graphUI.moveFocusRight = function () {
        focusNodeId = graphUI.graph.interactionState.focus.value0;
        node = graphUI.graph.nodes[focusNodeId];
        graphUI.graph = Purs.interGraph.updateNodePosition(
            { "x": node.position.x + graphUI.gridSize
            , "y": node.position.y
            })(
            focusNodeId)(
                graphUI.graph
            );
        return graphUI.graph;
    };


    ///////////////////////////////////
    //////// Save/loading graph

    graphUI.saveGraph = function (graph) {
        title = Purs.fromMaybe_("notitle")(Purs.graphTitle(Purs.interGraph)(graph));
        console.log(title);
        var timestamp = new Date().toISOString();
        var blob = new Blob(
            [JSON.stringify(
                {"graph": Purs.interGraphToJSON(graph),
                 "metadata": {
                     "version": Purs.version,
                     "title": title,
                     "timestamp": timestamp,
                 }})
                ],
            {type: "application/JSON;charset=utf-8"});
        FileSaver.saveAs(blob, title + ".graph.json");
    };

    graphUI.loadGraph = function (savedGraphJSON) {
        // TODO: fix types n stuff to remove ".value0"
        graphJSON = JSON.parse(savedGraphJSON).graph;
        metadata = JSON.parse(savedGraphJSON).metadata;
        newGraph = Purs.interGraphFromJSON(graphJSON);
        console.log('newgraph: ', newGraph);
        graphUI.graph = Purs.graph.emptyGraph;
        graphUI.update();
        graphUI.graph = newGraph.value0;
        graphUI.update();
    };

    graphUI.loadFile = function() {
        Utils.simulateClickOn(document.querySelector('input[type=file]'));
    };

    graphUI.processFile = function () {
        var file    = document.querySelector('input[type=file]').files[0];
        var reader  = new FileReader();

        reader.addEventListener("load", function () {
            graphUI.loadGraph(reader.result);
            graphUI.update();
        }, false);

        if (file) {
            reader.readAsText(file);
        }
    };

    ///////////////////////////////////
    //////// Keyboard input

    d3.select("body").on("keydown", function () {
        //console.log(d3.event);
        //console.log(d3.event.key);
        if (d3.event.key in keybindings[graphUI.keyboardMode]) {
            graphUI.graph =
                keybindings[graphUI.keyboardMode][d3.event.key](graphUI.graph, d3.event);
            graphUI.update();
        }
    });

    function normalMode() {
        graphUI.keyboardMode = "normal";
        return graphUI.graph;
    }

    function visualMode() {
        graphUI.graph.highlightFocus(Purs.interGraph)();
        graphUI.keyboardMode = "visual";
        return graphUI.graph;
    }

    function insertMode() {
        d3.event.preventDefault();
        graphUI.keyboardMode = "insert";
        return graphUI.graph;
    }

    function moveMode() {
        d3.event.preventDefault();
        graphUI.keyboardMode = "move";
        return graphUI.graph;
    }

    var keybindings = {
        "normal": {
            "j": graph => Purs.traverseDown(Purs.interGraph)(graph),
            "k": graph => Purs.traverseUp(Purs.interGraph)(graph),
            "h": graph => Purs.traverseLeft(Purs.interGraph)(graph),
            "l": function (graph) {
                if (d3.event.ctrlKey) {
                    graphUI.loadFile();
                    return graphUI.graph;
                } else {
                    return Purs.traverseRight(Purs.interGraph)(graph);
                };
            },
            "o": function (graph, event) {
                if (d3.event.ctrlKey) {
                    event.preventDefault();
                    event.stopPropagation();
                    graphUI.background_origin.x = 0;
                    graphUI.background_origin.y = 0;
                    return graphUI.graph;
                } else {
                    return Purs.newChildOfFocus(Purs.interGraph)(graphUI.newNodeOffset)(graph)();
                };
            },
            "O": graph => Purs.newParentOfFocus(Purs.interGraph)(graphUI.newNodeOffset)(graph)(),
            "x": graph => Purs.removeFocus(Purs.interGraph)(graph),
            "Delete": graph => Purs.removeFocus(Purs.interGraph)(graph),
            "s": function (graph, event) {
                if (d3.event.ctrlKey) {
                    event.preventDefault();
                    event.stopPropagation();
                    graphUI.saveGraph(graph);
                    return graphUI.graph;
                } else {
                    return Purs.toggleHighlightFocus(Purs.interGraph)(
                        graph
                    );
                };
            },
            "Escape": graph => Purs.clearHighlighted(Purs.interGraph)(graph),
            "i": insertMode,
            "v": visualMode,
            "m": moveMode,
            " ": function (graph, event) {
                event.preventDefault();
                event.stopPropagation();
                return Purs.toggleGroupExpand(Purs.interGraph)(
                        graph
                    );
            },
        },
        "insert": {
            "Escape": normalMode,
        },
        "visual": {
            "j": graph => {
                graph =
                    Purs.traverseDown(Purs.interGraph)(graph);
                return Purs.highlightFocus(Purs.interGraph)(graph);
            },
            "k": graph => {
                graph =
                    Purs.traverseUp(Purs.interGraph)(graph);
                return Purs.highlightFocus(Purs.interGraph)(graph);
            },
            "h": graph => {
                graph =
                    Purs.traverseLeft(Purs.interGraph)(graph);
                return Purs.highlightFocus(Purs.interGraph)(graph);
            },
            "l": graph => {
                graph =
                    Purs.traverseRight(Purs.interGraph)(graph);
                return Purs.highlightFocus(Purs.interGraph)(graph);
            },
            "s": graph => Purs.toggleHighlightFocus(Purs.interGraph)(graph),
            "Delete": graph => Purs.removeFocus(Purs.interGraph)(graph),
            "Escape": normalMode,
            " ": graph => Purs.toggleGroupExpand(Purs.interGraph)(graph),
        },
        "move": {
            "j": graph => graphUI.moveFocusDown(),
            "k": graph => graphUI.moveFocusUp(),
            "h": graph => graphUI.moveFocusLeft(),
            "l": graph => graphUI.moveFocusRight(),
            "Escape": normalMode,
            "s": normalMode,
            "m": normalMode,
        },
    };
};
