const path = require("path");
const Bundler = require("parcel-bundler");
const { spawn } = require("child_process");
const fs = require("fs");
const util = require("util");

// wait for a file to be available; NOTE: this uses fs.watch, which is not
// entirely consistent across platforms
function fileAvailable(file, waitingMessage) {
  const parentDir = path.dirname(file);
  const filename = path.basename(file);

  // make sure the parent directory exists
  try {
    fs.mkdirSync(parentDir, { recursive: true });
  } catch (e) {}

  return new Promise(resolve => {
    if (fs.existsSync(file)) {
      resolve();
      return;
    }

    console.log(waitingMessage);

    const watcher = fs.watch(parentDir, (eventType, name) => {
      if (name === filename && eventType === "rename") {
        watcher.close();
        resolve();
      }
    });
  });
}

(async function() {
  const pursDir = path.join(__dirname, "../purescript/");

  // launch a subprocess to build the PureScript files
  const pulp = spawn("pulp", ["--watch", "build"], {
    stdio: "inherit",
    cwd: pursDir,
    windowsHide: true
  });

  // wait for the output file to be available, but give up after 10 seconds
  await Promise.race([
    fileAvailable(
      path.join(pursDir, "output/Main/index.js"),
      "Waiting for PureScript to generate its output files..."
    ),
    util.promisify(setTimeout)(10000)
  ]);

  // exit on any errors from the PureScript code
  pulp.on(["exit", "error", "close"], (code, signal) => {
    system.exit(code);
  });

  const entryFiles = [path.join(__dirname, "../index.html")];
  const bundler = new Bundler(entryFiles);
  bundler.serve();
})();
