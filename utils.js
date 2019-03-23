var d3 = require("./libs/d3.js");

var isIn = function(element, array) {
    return array.indexOf(element) > -1;
}
exports.isIn = isIn;

var fadeIn = function(element, speed) {
    d3.select(element)
        .transition()
        .duration(speed)
        .style("opacity", 1.0);
}
exports.fadeIn = fadeIn;

var fadeOut = function(element, speed) {
    d3.select(element)
        .transition()
        .duration(speed)
        .style("opacity", 0.0);
}
exports.fadeOut = fadeOut;

// https://gist.github.com/engelen/fbce4476c9e68c52ff7e5c2da5c24a28
var argMax = function(array) {
    return array.map((x, i) => [x, i]).reduce((r, a) => (a[0] > r[0] ? a : r))[1];
}
exports.argMax = argMax;

// https://gist.github.com/jed/982883
var uuidv4 = function() {
    asdf = function b(a){return a?(a^Math.random()*16>>a/4).toString(16):([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g,b)};
    return asdf();
}
exports.uuidv4 = uuidv4;

//// https://stackoverflow.com/questions/4467539/javascript-modulo-gives-a-negative-result-for-negative-numbers
//var mod = function(n, m) {
//    return ((n % m) + m) % m;
//}
//exports.mod = mod;

var arrayWithoutElement = function(element, array) {
    arrayCopy = array.slice();
    if (isIn(element, array)) {
        arrayCopy.splice(array.indexOf(element), 1);
    }
    return arrayCopy;
}
exports.arrayWithoutElement = arrayWithoutElement;

var arrayRemoveElementInPlace = function(element, array) {
    if (isIn(element, array)) {
        array.splice(array.indexOf(element), 1);
    }
}
exports.arrayRemoveElementInPlace = arrayRemoveElementInPlace;

var euclideanDistance2D = function(a, b) {
    return Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2));
}
exports.euclideanDistance2D = euclideanDistance2D;

var concatenate = function(listOfLists) {
    return [].concat.apply([], listOfLists);
}
exports.concatenate = concatenate;

var foldl = function(foldFunc, initialVal, arr) {
    accumulator = initialVal;
    for(i=0; i<arr.length; i++) {
        accumulator = foldFunc(accumulator, arr[i]);
    }
    return accumulator;
}
exports.foldl = foldl;

var isEmptyObject = function(someObject) {
    return Object.keys(someObject).length == 0;
}
exports.isEmptyObject = isEmptyObject;

var centroidOfPoints = function(points) {
    return {
        "x": (1 / points.length) *
            foldl((a,b)=>a+b, 0, points.map(point => point.x)),
        "y": (1 / points.length) *
            foldl((a,b)=>a+b, 0, points.map(point => point.y)),
    };
};
exports.centroidOfPoints = centroidOfPoints;

var distanceToClosestPoint2D = function(point, neighbors) {
    minDist = 99999999;
    for (i=0; i<neighbors.length; i++) {
        thisDist = euclideanDistance2D(point, neighbors[i]);
        if (thisDist < minDist) {
            minDist = thisDist;
        }
    }
    return minDist;
}
exports.distanceToClosestPoint2D = distanceToClosestPoint2D;

var deepCopyObject = function (o) {
    return JSON.parse(JSON.stringify(o));
};
exports.deepCopyObject = deepCopyObject;

var simulateClickOn = function (element) {
    var clickEvent = new MouseEvent('click', {
        view: window,
        bubbles: true,
        cancelable: true
    });
    element.dispatchEvent(clickEvent);
};
exports.simulateClickOn = simulateClickOn;
