//// Requires d3.js
// https://github.com/d3/d3

function isIn(element, array) {
    return array.indexOf(element) > -1;
}

function fadeIn(element, speed) {
    d3.select(element)
        .transition()
        .duration(speed)
        .style("opacity", 1.0);
}

function fadeOut(element, speed) {
    d3.select(element)
        .transition()
        .duration(speed)
        .style("opacity", 0.0);
}

// https://gist.github.com/engelen/fbce4476c9e68c52ff7e5c2da5c24a28
function argMax(array) {
    return array.map((x, i) => [x, i]).reduce((r, a) => (a[0] > r[0] ? a : r))[1];
}

// https://gist.github.com/jed/982883
function uuidv4() {
    asdf = function b(a){return a?(a^Math.random()*16>>a/4).toString(16):([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g,b)};
    return asdf();
}

// https://stackoverflow.com/questions/4467539/javascript-modulo-gives-a-negative-result-for-negative-numbers
function mod(n, m) {
    return ((n % m) + m) % m;
}

function arrayWithoutElement(element, array) {
    arrayCopy = array.slice();
    if (isIn(element, array)) {
        arrayCopy.splice(array.indexOf(element), 1);
    }
    return arrayCopy;
}

function arrayRemoveElementInPlace(element, array) {
    if (isIn(element, array)) {
        array.splice(array.indexOf(element), 1);
    }
}

function euclideanDistance2D(a, b) {
    return Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2));
}

function concatenate(listOfLists) {
    return [].concat.apply([], listOfLists);
}

function foldl(foldFunc, initialVal, list) {
    accumulator = initialVal;
    for(i=0; i<list.length; i++) {
        accumulator = foldFunc(accumulator, list[i]);
    }
    return accumulator;
}

function isEmptyObject(someObject) {
    return Object.keys(someObject).length == 0;
}

function centroidOfPoints(points) {
    return {
        "x": (1 / points.length) *
            foldl((a,b)=>a+b, 0, points.map(point => point.x)),
        "y": (1 / points.length) *
            foldl((a,b)=>a+b, 0, points.map(point => point.y)),
    };
};
