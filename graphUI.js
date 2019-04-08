const d3 = require("./libs/d3.js");
const Utils = require("./utils");
const StringSet = require("./stringSet.js");
const Purs = require('./purescript/output/Main/index.js');
var FileSaver = require("./libs/FileSaver.min.js");


module.exports = function GraphUI(graph) {
    ///////////////////////////////////
    //////// Explicit state

    var graphUI = this;
    graphUI.graph = graph;

    var mouseState = {
        "clickedNode": undefined,
        "drawingEdge": [],
        "mouseoverNode": undefined,
    };

    // Keyboard modes: normal, insert, visual
    graphUI.keyboardMode = "normal";


    ///////////////////////////////////
    //////// Constants
    graphUI.fadeSpeed = 150;
    graphUI.gridSize = 50;


    ///////////////////////////////////
    //////// D3

    graphUI.svg = d3.select("body").append("svg")
        .attr("width", screen.width)
        .attr("height", screen.height);

    // Define Z axis ordering of elements
    graphUI.svg.append("g").attr("id", "text");
    graphUI.svg.append("g").attr("id", "nodeHalos");
    graphUI.svg.append("g").attr("id", "edges");
    graphUI.svg.append("g").attr("id", "edgeBorders");
    graphUI.svg.append("g").attr("id", "nodes");
    graphUI.svg.append("g").attr("id", "nodeBorders");

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
        .attr("refX", 13)
        .attr("refY", 3)
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
        .attr("refY", 3)
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
            .data(graphUI.graph.getEdgeNodes(), function (edgeNode) {
                return Purs.computeEdgeId({"source": edgeNode.source.id, "target": edgeNode.target.id});
            })
            .join(
                enter => enter.append("line")
                    .classed("edge", true)
                    .attr("x1", edgeNode => edgeNode.source.x)
                    .attr("y1", edgeNode => edgeNode.source.y)
                    .attr("x2", edgeNode => edgeNode.target.x)
                    .attr("y2", edgeNode => edgeNode.target.y)
                    .classed("focused", edgeNode => Purs.computeEdgeId(
                        {"source": edgeNode.source.id, "target": edgeNode.target.id})
                             == Purs.fromFocus(graphUI.graph.focus))
                    .attr("marker-end", edgeNode => {
                        if (Utils.isEmptyObject(edgeNode.target.subgraphNodes)) {
                            return "url(#arrow)";
                        } else {
                            return "url(#arrow-to-group)";
                        }}),
                update => update
                    .attr("x1", edgeNode => edgeNode.source.x)
                    .attr("y1", edgeNode => edgeNode.source.y)
                    .attr("x2", edgeNode => edgeNode.target.x)
                    .attr("y2", edgeNode => edgeNode.target.y)
                    .classed("focused", edgeNode => Purs.computeEdgeId(
                        {"source": edgeNode.source.id, "target": edgeNode.target.id})
                            == Purs.fromFocus(graphUI.graph.focus))
                    .attr("marker-end", edgeNode => {
                        if (Utils.isEmptyObject(edgeNode.target.subgraphNodes)) {
                            return "url(#arrow)";
                        } else {
                            return "url(#arrow-to-group)";
                        }
                    })
            );
    };

    graphUI.updateEdgeBorders = function () {
        graphUI.svg.select("#edgeBorders").selectAll("line").filter(".edgeBorder")
            .data(graphUI.graph.getEdgeNodes(), function (edgeNode) {
                return Purs.computeEdgeId({"source": edgeNode.source.id, "target": edgeNode.target.id});
            })
            .join(
                enter => enter.append("line")
                    .classed("edgeBorder", true)
                    .attr("x1", edgeNodes => edgeNodes.source.x)
                    .attr("y1", edgeNodes => edgeNodes.source.y)
                    .attr("x2", edgeNodes => edgeNodes.target.x)
                    .attr("y2", edgeNodes => edgeNodes.target.y)
                    .attr("stroke-linecap", "butt")
                    .classed("focusGroup", edgeNode => Purs.edgeInFocusGroup(graphUI.graph.pursGraph)(
                        {"source": edgeNode.source.id, "target": edgeNode.target.id}))
                    .on("click", function(edgeNodes) {
                        graphUI.graph.focusOnEdge({"source": edgeNodes.source.id, "target": edgeNodes.target.id});
                        graphUI.update();
                    })
                    .on("mouseover", function (d) {
                        Utils.fadeIn(this, graphUI.fadeSpeed);
                    })
                    .on("mouseout", function (d) {
                        if (!d3.select(this).classed("grouped") && !d3.select(this).classed("focusGroup")) {
                            Utils.fadeOut(this, graphUI.fadeSpeed);
                        }
                    }),
                update => update
                    .attr("x1", edgeNodes => edgeNodes.source.x)
                    .attr("y1", edgeNodes => edgeNodes.source.y)
                    .attr("x2", edgeNodes => edgeNodes.target.x)
                    .attr("y2", edgeNodes => edgeNodes.target.y)
                    .classed("focusGroup", edgeNode => Purs.edgeInFocusGroup(graphUI.graph.pursGraph)(
                        {"source": edgeNode.source.id, "target": edgeNode.target.id}))
                    .each(function () {
                        if (d3.select(this).classed("focusGroup")) {
                            Utils.fadeIn(this, graphUI.fadeSpeed);
                        } else {
                            Utils.fadeOut(this, graphUI.fadeSpeed);
                        }
                    })
            );
    };

    graphUI.updateDrawingEdge = function () {
        graphUI.svg.select("#edges").selectAll("line").filter(".drawing")
            .data(mouseState.drawingEdge)
            .join(
                enter => enter.append("line")
                    .classed("drawing", true)
                    .attr("x1", d => d.source.x)
                    .attr("y1", d => d.source.y)
                    .attr("x2", d => d.target.x)
                    .attr("y2", d => d.target.y)
                    .attr("marker-end", "url(#drawing-arrow)"),
                update => update
                    .attr("x2", d => d.target.x)
                    .attr("y2", d => d.target.y)
            );
    };

    graphUI.updateText = function () {
        graphUI.svg.select("#text").selectAll("foreignObject")
            .data(Object.entries(graphUI.graph.nodes), d => d[0])
            .join(
                // lambda function was causing weird errors here :/
                function (enter) {
                    enter.append("foreignObject")
                    .attr("x", d => d[1].x + 20)
                    .attr("y", d => d[1].y - 10)
                    //.attr("width", d => Utils.arrayMax(d[1].text.split("\n").map(line => line.length)) * 12 + 12)
                    //.attr("height", d => d[1].text.split("\n").length * 20 + 20)
                    .attr("width", screen.width)
                    .attr("height", screen.height)
                    .append('xhtml:div')
                    .append('div')
                    .attr("contentEditable", true)
                    .text(d => d[1].text)
                    .on("keydown", function (d) {
                        graphUI.graph.updateText(d[0], this.innerText);
                    })
                    .on("keyup", function (d) {
                        graphUI.graph.updateText(d[0], this.innerText);
                    });},
                update => update
                    .attr("x", d => d[1].x + 20)
                    .attr("y", d => d[1].y - 10)
                    .each(function (d) {
                        if (graphUI.keyboardMode == "insert" &&
                              d[0] == Purs.fromFocus(graphUI.graph.focus)) {
                            d3.select(this).select("div").select("div").node().focus();
                        } else {
                            d3.select(this).select("div").select("div").node().blur();
                        }
                    })
            );
    };

    graphUI.updateNodes = function () {
        graphUI.svg.select("#nodes").selectAll("circle").filter(".node")
            .data(Object.entries(graphUI.graph.nodes), d => d[0])
            .join(
                enter => enter.append("circle")
                    .attr("class", graphUI.keyboardMode)
                    .classed("node", true)
                    .attr("r", d => {
                        if (Object.keys(d[1].subgraphNodes).length > 0) {
                            return 12;
                        } else {
                            return 7;
                        }
                    })
                    .attr("cx", d => d[1].x)
                    .attr("cy", d => d[1].y)
                    .classed("focused", d => d[0] == Purs.fromFocus(graphUI.graph.focus)),
                update => update
                    .attr("class", graphUI.keyboardMode)
                    .classed("node", true)
                    .attr("cx", d => d[1].x)
                    .attr("cy", d => d[1].y)
                    .classed("focused", d => d[0] == Purs.fromFocus(graphUI.graph.focus))
            );
    };

    graphUI.updateNodeBorders = function () {
        borders = graphUI.svg.select("#nodeBorders").selectAll("circle").filter(".nodeBorder")
            .data(Object.entries(graphUI.graph.nodes), d => d[0] + d[1].x + d[1].y)
            .join(
                enter => enter.append("circle")
                    .classed("nodeBorder", true)
                    .attr("r", 28)
                    .attr("cx", d => d[1].x)
                    .attr("cy", d => d[1].y)
                    .classed("grouped", d => StringSet.isIn(d[0], graphUI.graph.highlighted))
                    .call(d3.drag()
                          .on("start", dragstarted_node)
                          .on("drag", dragged_node)
                          .on("end", dragended_node))
                    .on("mouseover", function (d) {
                        mouseState.mouseoverNode = d[0];
                        Utils.fadeIn(this, graphUI.fadeSpeed);
                    })
                    .on("mouseout", function (d) {
                        mouseState.mouseoverNode = undefined;
                        if (!d3.select(this).classed("grouped")) {
                            Utils.fadeOut(this, graphUI.fadeSpeed);
                        }
                    })
                    .each(function () {
                        if (d3.select(this).classed("grouped")) {
                            Utils.fadeIn(this, graphUI.fadeSpeed);
                        } else {
                            Utils.fadeOut(this, graphUI.fadeSpeed);
                        }
                    }),
                update => update
                    .attr("cx", d => d[1].x)
                    .attr("cy", d => d[1].y)
                    .classed("grouped", d => StringSet.isIn(d[0], graphUI.graph.highlighted))
                    .each(function () {
                        if (d3.select(this).classed("grouped")) {
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
                    .attr("cx", d => d[1].x)
                    .attr("cy", d => d[1].y)
                    .on("mousedown", function (d) {
                        mouseState.clickedNode = d[0];
                    })
                    .on("mouseover", function (d) {
                        mouseState.mouseoverNode = d[0];
                        d3.select(this).classed("ready", false);
                        if (mouseState.clickedNode != undefined &&
                            mouseState.clickedNode != mouseState.mouseoverNode &&
                            !StringSet.isIn(d[0], graphUI.graph.nodes[mouseState.clickedNode].children)) {
                            d3.select(this).classed("ready", true);
                        }
                        Utils.fadeIn(this, graphUI.fadeSpeed);
                        graphUI.update();
                    })
                    .on("mouseout", function () {
                        mouseState.mouseoverNode = undefined;
                        Utils.fadeOut(this, graphUI.fadeSpeed);
                    })
                    .call(d3.drag()
                          .on("start", dragstarted_halo)
                          .on("drag", dragged_halo)
                          .on("end", dragended_halo)),
                update => update
                    .attr("cx", d => d[1].x)
                    .attr("cy", d => d[1].y)
            );
    };

    graphUI.resizeWindow = function () {
        xMax = Math.max.apply(null, Object.values(graphUI.graph.nodes).map(
            node => node.x));
        yMax = Math.max.apply(null, Object.values(graphUI.graph.nodes).map(
            node => node.y));
        if (xMax && yMax) {
            graphUI.svg
                .attr("width", xMax + screen.width)
                .attr("height", yMax + screen.height);
        }
    };

    graphUI.update = function () {
        graphUI.graph.usePursGraph();

        graphUI.updateEdges();

        graphUI.updateEdgeBorders();

        graphUI.updateDrawingEdge();

        graphUI.updateText();

        graphUI.updateNodes();

        graphUI.updateNodeBorders();

        graphUI.updateNodeHalos();

        // Reset states
        graphUI.svg.on("mouseup", function () {
            mouseState.clickedNode = undefined;
            graphUI.update();
        });

        graphUI.resizeWindow();
    };

    // Ctrl+click to create new unconnected node
    graphUI.svg.on("mousedown", function () {
        if (d3.event.ctrlKey) {
            graphUI.graph.createNode(d3.event.x, d3.event.y, StringSet.empty(), StringSet.empty());
            graphUI.update();
        }
    });

    function dragstarted_node(d) {
        d3.select(this).style("pointer-events", "none");
        graphUI.graph.focusOnNode(d[0]);
        graphUI.update();
    }

    function dragged_node(d) {
        graphUI.graph.moveNode(d[0], {
            "x": Math.floor(d3.event.x / graphUI.gridSize) * graphUI.gridSize,
            "y": Math.floor(d3.event.y / graphUI.gridSize) * graphUI.gridSize,
        });
        graphUI.update();
    }

    function dragended_node(d) {
        d3.select(this).style("pointer-events", "all");
        create_edge_if_possible();
        graphUI.update();
    }

    function dragstarted_halo(d) {
        d3.select(this).style("pointer-events", "none");
        mouseState.drawingEdge = [{"source": d[1],
                                   "target": {"x": d3.event.x,
                                              "y": d3.event.y}}];
        graphUI.update();
    }

    function dragged_halo(d) {
        mouseState.drawingEdge = [{"source": d[1],
                              "target": {"x": d3.event.x,
                                         "y": d3.event.y}}];
        graphUI.update();
    }

    function dragended_halo(d) {
        d3.select(this).style("pointer-events", "all");
        create_edge_if_possible();
        mouseState.drawingEdge = [];
        graphUI.update();
        mouseState.clickedNode = undefined;
        d3.selectAll("*").classed("ready", false);
        graphUI.update();
    }

    function create_edge_if_possible() {
        if (mouseState.clickedNode != undefined &&
            mouseState.clickedNode != mouseState.mouseoverNode &&
            mouseState.mouseoverNode != undefined) {
            graphUI.graph.addEdge(mouseState.clickedNode, mouseState.mouseoverNode);
            graphUI.graph.focusOnNode(mouseState.mouseoverNode);
            mouseState.clickedNode = undefined;
            mouseState.mouseoverNode = undefined;
        };
    }

    graphUI.moveFocusDown = function () {
        focusNodeId = Purs.fromFocus(graphUI.graph.focus);
        node = graphUI.graph.nodes[focusNodeId];
        graphUI.graph.moveNode(focusNodeId, {
            "x": node.x,
            "y": node.y + graphUI.gridSize,
        });
    };

    graphUI.moveFocusUp = function () {
        focusNodeId = Purs.fromFocus(graphUI.graph.focus);
        node = graphUI.graph.nodes[focusNodeId];
        graphUI.graph.moveNode(focusNodeId, {
            "x": node.x,
            "y": node.y - graphUI.gridSize,
        });
    };

    graphUI.moveFocusLeft = function () {
        focusNodeId = Purs.fromFocus(graphUI.graph.focus);
        console.log(focusNodeId);
        node = graphUI.graph.nodes[focusNodeId];
        graphUI.graph.moveNode(focusNodeId, {
            "x": node.x - graphUI.gridSize,
            "y": node.y,
        });
    };

    graphUI.moveFocusRight = function () {
        focusNodeId = Purs.fromFocus(graphUI.graph.focus);
        node = graphUI.graph.nodes[focusNodeId];
        graphUI.graph.moveNode(focusNodeId, {
            "x": node.x + graphUI.gridSize,
            "y": node.y,
        });
    };


    ///////////////////////////////////
    //////// Save/loading graph

    graphUI.saveGraph = function (graph) {
        title = Purs.fromMaybe("notitle")(Purs.graphTitle(graph.pursGraph));
        var blob = new Blob([Purs.graphToJSON(graph.pursGraph)], {type: "application/JSON;charset=utf-8"});
        FileSaver.saveAs(blob, title + ".workflow-graph_v" + Purs.version + "_" + new Date().toISOString() + ".json");
    };

    graphUI.loadGraph = function (graphJSON) {
        // TODO: fix types n stuff to remove ".value0"
        newGraph = Purs.graphFromJSON(graphJSON).value0;
        console.log('newgraph: ', newGraph);
        graphUI.graph.nodes = Utils.deepCopyObject(newGraph.nodes);
        graphUI.graph.focus = newGraph.focus;
        graphUI.graph.highlighted = newGraph.highlighted;
        console.log(graphUI.graph.highlighted);
        //graphUI.graph.pursGraph = Purs.listOpsFromGraph(newGraph);
        graphUI.graph.pursGraph = newGraph;
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
            console.log('after update nodes: ', graphUI.graph.nodes);
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
            keybindings[graphUI.keyboardMode][d3.event.key](graphUI.graph, d3.event);
            graphUI.update();
        }
    });

    function normalMode() {
        graphUI.keyboardMode = "normal";
    }

    function visualMode() {
        graphUI.graph.highlightFocus();
        graphUI.keyboardMode = "visual";
    }

    function insertMode() {
        d3.event.preventDefault();
        graphUI.keyboardMode = "insert";
    }

    function moveMode() {
        d3.event.preventDefault();
        graphUI.keyboardMode = "move";
    }

    var keybindings = {
        "normal": {
            "j": graph => graph.traverseDown(),
            "k": graph => graph.traverseUp(),
            "h": graph => graph.traverseLeft(),
            "l": function (graph) {
                if (d3.event.ctrlKey) {
                    graphUI.loadFile();
                } else {
                    graph.traverseRight();
                };
            },
            "o": graph => graph.newChildOfFocus(),
            "O": graph => graph.newParentOfFocus(),
            "x": graph => graph.removeFocused(),
            "Delete": graph => graph.removeFocused(),
            "s": function (graph, event) {
                if (d3.event.ctrlKey) {
                    event.preventDefault();
                    event.stopPropagation();
                    graphUI.saveGraph(graph);
                } else {
                    graph.toggleHighlightFocus();
                };
            },
            "Escape": graph => graph.clearHighlights(),
            "i": insertMode,
            "v": visualMode,
            "m": moveMode,
            " ": function (graph, event) {
                event.preventDefault();
                event.stopPropagation();
                graph.toggleGroupExpand();
            },
        },
        "insert": {
            "Escape": normalMode,
        },
        "visual": {
            "j": graph => graph.traverseDown().highlightFocus(),
            "k": graph => graph.traverseUp().highlightFocus(),
            "h": graph => graph.traverseLeft().highlightFocus(),
            "l": graph => graph.traverseRight().highlightFocus(),
            "s": graph => graph.toggleHighlightFocus(),
            "Delete": graph => graph.removeFocused(),
            "Escape": normalMode,
            " ": graph => graph.toggleGroupExpand,
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
