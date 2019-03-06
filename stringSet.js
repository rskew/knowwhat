// Set data structure.
// Implemented as an object with the elements as keys and the values
// filled with empty objects.
const Utils = require('./utils.js');

//////
// Constructors

var empty = function () {
    return {};
};
exports.empty = empty;

var fromArray = function(arr) {
    newSet = empty();
    for (i=0; i<arr.length; i++) {
        insertInPlace(arr[i], newSet);
    };
    return newSet;
};
exports.fromArray = fromArray;

var singleton = function(element) {
    return fromArray([element]);
};
exports.singleton = singleton;


var toArray = function(set) {
    return Object.keys(set);
};
exports.toArray = toArray;

var copy = function(set) {
    // fromArray does an explicit copy :thumbsup:
    return fromArray(toArray(set));
};
exports.copy = copy;

//////
// Modifiers

var insertInPlace = function(newElement, set) {
    set[newElement] = {};
};
exports.insertInPlace = insertInPlace;

var deleteInPlace = function(element, set) {
    delete set[element];
};
exports.deleteInPlace = deleteInPlace;

var lookupIndex = function(index, set) {
    return toArray(set)[index];
};
exports.lookupIndex = lookupIndex;

//////
// Properties

var cardinality = function(set) {
    return toArray(set).length;
};
exports.cardinality = cardinality;

var isIn = function(element, set) {
    return Utils.isIn(element, toArray(set));
};
exports.isIn = isIn;

//////
// Higher order iteration functions

var filter = function(set, func) {
    return fromArray(toArray(set).filter(func));
};
exports.filter = filter;

// Warning: StringSet.map returns a StringSet, so if
// you pass a function that returns something other than
// a string, it will be stringified by the conversion to
// a StringSet element!
var map = function(set, func) {
    return fromArray(toArray(set).map(func));
};
exports.map = map;

//////
// Set operations

var subtract = function(plus, minus) {
    return filter(
        copy(plus),
        element => !isIn(element, minus)
    );
};
exports.subtract = subtract;
