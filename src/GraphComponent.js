var FileSaver = require("file-saver");

//////
// Save/loading graph

// saveJSON :: String -> String -> Effect Unit
var saveJSON = function(jsonStr) {
  return function(fname) {
    return function() {
      var blob = new Blob([jsonStr], {
        type: "application/JSON;charset=utf-8"
      });
      FileSaver.saveAs(blob, fname);
    };
  };
};

// loadFile :: Effect Unit
var loadFile = function() {
  simulateClickOn(document.querySelector("input[type=file]"));
};

var simulateClickOn = function(element) {
  var clickEvent = new MouseEvent("click", {
    view: window,
    bubbles: true,
    cancelable: true
  });
  element.dispatchEvent(clickEvent);
};

//exports.saveJSON = saveJSON;
exports.loadFile = loadFile;
exports.saveJSON = saveJSON;
