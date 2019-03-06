var d3 = require("./d3.js");
var Utils = require("./utils");
var StringSet = require("./stringSet.js");

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
    var fadeSpeed = 150,
        width = 500,
        height = 500;


    ///////////////////////////////////
    //////// D3

    graphUI.svg = d3.select("body").append("svg")
        .attr("width", width)
        .attr("height", height);

    // Define Z axis ordering of elements
    graphUI.svg.append("g").attr("id", "nodeHalos");
    graphUI.svg.append("g").attr("id", "links");
    graphUI.svg.append("g").attr("id", "nodes");
    graphUI.svg.append("g").attr("id", "nodeBorders");

    // Markers for arrowheads
    // Normal arrow
    graphUI.svg.append("defs").append("marker")
        .attr("id", "arrow")
        .attr("markerWidth", 10)
        .attr("markerHeight", 10)
        .attr("refX", 10)
        .attr("refY", 3)
        .attr("orient", "auto")
        .attr("marketUnits", "strokeWidth")
        .append("path")
        .attr("d", "M0,0 L0,6 L6,3 z")
        .attr("fill", "#000");

    // For pointing to a group
    graphUI.svg.append("defs").append("marker")
        .attr("id", "arrow-to-group")
        .attr("markerWidth", 10)
        .attr("markerHeight", 10)
        .attr("refX", 13)
        .attr("refY", 3)
        .attr("orient", "auto")
        .attr("marketUnits", "strokeWidth")
        .append("path")
        .attr("d", "M0,0 L0,6 L6,3 z")
        .attr("fill", "#000");

    // For the arrow being drawn (by dragging)
    graphUI.svg.append("defs").append("marker")
        .attr("id", "drawing-arrow")
        .attr("markerWidth", 10)
        .attr("markerHeight", 10)
        .attr("refX", 3)
        .attr("refY", 3)
        .attr("orient", "auto")
        .attr("marketUnits", "strokeWidth")
        .append("path")
        .attr("d", "M0,0 L0,6 L6,3 z")
        .attr("fill", "#000");

    graphUI.updateEdges = function () {
        graphUI.svg.select("#links").selectAll(".nodeLinks")
            .data(Object.entries(graphUI.graph.nodes), d => d[0])
            .join("g")
            .classed("nodeLinks", true)
            .selectAll("line")
            .data(d => StringSet.toArray(d[1].parents).map(
                parentId => ({"source": parentId, "target": d[0]})),
                  edge => edge)
            .join(
                enter => enter.append("line")
                    .attr("x1", edge => graphUI.graph.nodes[edge.source].x)
                    .attr("y1", edge => graphUI.graph.nodes[edge.source].y)
                    .attr("x2", edge => graphUI.graph.nodes[edge.target].x)
                    .attr("y2", edge => graphUI.graph.nodes[edge.target].y)
                    .attr("marker-end", edge => {
                        if (Utils.isEmptyObject(graphUI.graph.nodes[edge.target].subgraph.nodes)) {
                            return "url(#arrow)";
                        } else {
                            return "url(#arrow-to-group)";
                        }}),
                update => update
                    .attr("x1", edge => graphUI.graph.nodes[edge.source].x)
                    .attr("y1", edge => graphUI.graph.nodes[edge.source].y)
                    .attr("x2", edge => graphUI.graph.nodes[edge.target].x)
                    .attr("y2", edge => graphUI.graph.nodes[edge.target].y)
                    .attr("marker-end", edge => {
                        if (Utils.isEmptyObject(graphUI.graph.nodes[edge.target].subgraph.nodes)) {
                            return "url(#arrow)";
                        } else {
                            return "url(#arrow-to-group)";
                        }}),
            );
    };

    graphUI.updateDrawingEdge = function () {
        graphUI.svg.select("#links").selectAll("line").filter(".drawing")
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
        graphUI.svg.selectAll("foreignObject")
            .data(Object.entries(graphUI.graph.nodes), d => d[0])
            .join(
                enter => enter.append("foreignObject")
                    .attr("x", d => d[1].x + 20)
                    .attr("y", d => d[1].y - 10)
                    .attr("width", d => d[1].text.length * 12 + 12)
                    .attr("height", d => d[1].text.split("\n").length * 20 + 20)
                    .append('xhtml:div')
                    .append('div')
                    .attr("contentEditable", true)
                    .text(d => d[1].text)
                    .on("keydown", function (d) {
                        d[1].text = this.innerText;
                        graphUI.update();
                    })
                    .lower(),
                update => update
                    .attr("x", d => d[1].x + 20)
                    .attr("y", d => d[1].y - 10)
                    .attr("width", d => d[1].text.length * 20 + 20)
                    .attr("height", d => d[1].text.split("\n").length * 20 + 20)
                    .each(function (d) {
                        if (graphUI.keyboardMode == "insert" &&
                            d[0] == graphUI.graph.focusedNodeId) {
                            d3.select(this).select("div").select("div").node().focus();
                        } else {
                            d3.select(this).select("div").select("div").node().blur();
                        }
                    })
                    .lower()
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
                        if (Object.keys(d[1].subgraph.nodes).length > 0) {
                            return 12;
                        } else {
                            return 7;
                        }
                    })
                    .attr("cx", d => d[1].x)
                    .attr("cy", d => d[1].y)
                    .classed("focused", d => d[0] == graphUI.graph.focusedNodeId),
                update => update
                    .attr("class", graphUI.keyboardMode)
                    .classed("node", true)
                    .attr("cx", d => d[1].x)
                    .attr("cy", d => d[1].y)
                    .classed("focused", d => d[0] == graphUI.graph.focusedNodeId)
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
                    .classed("grouped", d => StringSet.isIn(d[0], graphUI.graph.highlightedNodes))
                    .call(d3.drag()
                          .on("start", dragstarted_node)
                          .on("drag", dragged_node)
                          .on("end", dragended_node))
                    .on("mouseover", function (d) {
                        mouseState.mouseoverNode = d[0];
                        Utils.fadeIn(this, fadeSpeed);
                    })
                    .on("mouseout", function (d) {
                        mouseState.mouseoverNode = undefined;
                        if (!d3.select(this).classed("grouped")) {
                            Utils.fadeOut(this, fadeSpeed);
                        }
                    })
                    .each(function () {
                        if (d3.select(this).classed("grouped")) {
                            Utils.fadeIn(this, fadeSpeed);
                        } else {
                            Utils.fadeOut(this, fadeSpeed);
                        }
                    }),
                update => update
                    .attr("cx", d => d[1].x)
                    .attr("cy", d => d[1].y)
                    .classed("grouped", d => StringSet.isIn(d[0], graphUI.graph.highlightedNodes))
                    .each(function () {
                        if (d3.select(this).classed("grouped")) {
                            Utils.fadeIn(this, fadeSpeed);
                        } else {
                            Utils.fadeOut(this, fadeSpeed);
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
                        Utils.fadeIn(this, fadeSpeed);
                        graphUI.update();
                    })
                    .on("mouseout", function () {
                        mouseState.mouseoverNode = undefined;
                        Utils.fadeOut(this, fadeSpeed);
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

    graphUI.update = function () {

        graphUI.updateEdges();

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
        graphUI.graph.focusOn(d[0]);
        graphUI.update();
    }

    function dragged_node(d) {
        graphUI.graph.moveNode(d[0], {
            "x": d3.event.x,
            "y": d3.event.y,
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
        //d3.selectAll("*").classed("hover", false);
        graphUI.update();
    }

    function create_edge_if_possible() {
        if (mouseState.clickedNode != undefined &&
            mouseState.clickedNode != mouseState.mouseoverNode &&
            mouseState.mouseoverNode != undefined) {
            graphUI.graph.addEdge(mouseState.clickedNode, mouseState.mouseoverNode);
            graphUI.graph.focusedNode = mouseState.mouseoverNode;
            mouseState.clickedNode = undefined;
            mouseState.mouseoverNode = undefined;
        };
    }


    ///////////////////////////////////
    //////// Keyboard input

    d3.select("body").on("keydown", function () {
        //console.log(d3.event);
        //console.log(d3.event.key);
        if (d3.event.key in keybindings[graphUI.keyboardMode]) {
            if (Utils.isIn(d3.event.key, preventDefaultKeys)) {
                d3.event.preventDefault();
                d3.event.stopPropagation();
            }
            keybindings[graphUI.keyboardMode][d3.event.key](graphUI.graph);
            graphUI.update();
        }
    });

    function normalMode() {
        graphUI.keyboardMode = "normal";
    }

    function visualMode() {
        graphUI.graph.highlightFocusNode();
        graphUI.keyboardMode = "visual";
    }

    function insertMode() {
        d3.event.preventDefault();
        graphUI.keyboardMode = "insert";
    }

    var preventDefaultKeys = [
        " ",
    ];

    var keybindings = {
        "normal": {
            "j": graph => graph.traverseDown(),
            "k": graph => graph.traverseUp(),
            "h": graph => graph.traverseLeft(),
            "l": graph => graph.traverseRight(),
            "o": graph => graph.newNodeBelowFocus(),
            "x": graph => graph.removeFocusedNode(),
            "Delete": graph => graph.removeFocusedNode(),
            "s": graph => graph.toggleHighlightFocusNode(),
            "Escape": graph => graph.clearHighlights(),
            "i": insertMode,
            "v": visualMode,
            " ": graph => graph.toggleGroupExpand(),
        },
        "insert": {
            "Escape": normalMode,
        },
        "visual": {
            "j": graph => graph.traverseDown().highlightFocusNode(),
            "k": graph => graph.traverseUp().highlightFocusNode(),
            "h": graph => graph.traverseLeft().highlightFocusNode(),
            "l": graph => graph.traverseRight().highlightFocusNode(),
            "s": graph => graph.toggleHighlightFocusNode(),
            "Delete": graph => graph.removeFocusNodeFromGroup(),
            "Escape": normalMode,
            " ": graph => graph.toggleGroupExpand,
        }
    };
};
