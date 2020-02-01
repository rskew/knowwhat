var FileSaver = require("file-saver");

//////
// Save/loading graph

// saveJSON :: String -> String -> Effect Unit
exports.saveJSON = function(jsonStr) {
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
exports.loadFile = function() {
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
