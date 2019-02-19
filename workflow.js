//// Requires graph.js
//// Requires graphUI.js
//// Requires utils.js

///////////////////////////////////
//////// Data

var graphNodes = {
    "a": {
        "text": "do all the things plz",
        "x": 100,
        "y": 100,
        "parents": [],
        "children": [
            "b", "c",
        ],
        "subgraph": {},
    },
    "b": {
        "text": "TODO: woohoo!",
        "x": 150,
        "y": 200,
        "parents": ["a"],
        "children": [],
        "subgraph": {},
    },
    "c": {
        "text": "today I frink",
        "x": 100,
        "y": 150,
        "parents": ["a"],
        "children": [],
        "subgraph": {},
    },
    "d": {
        "text": "shopping list: ka-pow!",
        "x": 200,
        "y": 250,
        "parents": [],
        "children": [],
        "subgraph": {},
    },
};

var graph = new Graph(graphNodes, "a", ["b"]),
    graphUI = new GraphUI(graph);


///////////////////////////////////
//////// Main

graphUI.update();
