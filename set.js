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

// Private
var key = function(element) {
    if (element == undefined) {
        return 'undefined';
    } else {
        return element.toSource().substring(0,200);
    }
};

var fromArray = function(arr) {
    newSet = empty();
    for (i=0; i<arr.length; i++) {
        newSet[key(arr[i])] = arr[i];
    };
    return newSet;
};
exports.fromArray = fromArray;

var singleton = function(element) {
    return fromArray([element]);
};
exports.singleton = singleton;


var toArray = function(set) {
    return Object.values(set);
};
exports.toArray = toArray;

var copy = function(set) {
    // fromArray does an explicit copy :thumbsup:
    return fromArray(toArray(set));
}
exports.copy = copy;

//////
// Modifiers

var insertInPlace = function(newElement, set) {
    set[key(newElement)] = newElement;
};
exports.insertInPlace = insertInPlace;

var deleteInPlace = function(element, set) {
    delete set[key(element)];
};
exports.deleteInPlace = deleteInPlace;

var lookupIndex = function(index, set) {
    return Object.values(set)[index];
};
exports.lookupIndex = lookupIndex;

//////
// Properties

var cardinality = function(set) {
    return Object.keys(set).length;
};
exports.cardinality = cardinality;

var isIn = function(element, set) {
    return Utils.isIn(key(element), Object.keys(set));
};
exports.isIn = isIn;

//////
// Higher order iteration functions

var filter = function(set, func) {
    return fromArray(toArray(set).filter(func));
};
exports.filter = filter;

var map = function(set, func) {
    return fromArray(toArray(set).map(func));
};
exports.map = map;

var unionMap = function(set, func) {
    return union(map(set, func));
};
exports.unionMap = unionMap;

//////
// Set operations

var subtract = function(plus, minus) {
    result = copy(plus);
    map(
        minus,
        element => deleteInPlace(element, result)
    );
    return result;
};
exports.subtract = subtract;

var union = function(setOfSets) {
    result = Set.empty();
    map(
        setOfSets,
        set => map(
            set,
            element => insertInPlace(element, result)
        )
    );
    return result;
};
exports.union = union;
