const fs = require("fs");
const { execSync } = require("child_process");

const watFile = process.argv[2];
if (!watFile) {
  console.error("Usage: node run_wasm.js <file.wat>");
  process.exit(1);
}

const wasmFile = watFile.replace(".wat", ".wasm");

try {
  // Convert WAT to WASM using wat2wasm
  console.log(`Converting ${watFile} to ${wasmFile}...`);
  execSync(`wat2wasm ${watFile} -o ${wasmFile}`);

  // Load and run the WASM
  const wasmBuffer = fs.readFileSync(wasmFile);

  WebAssembly.instantiate(wasmBuffer)
    .then((result) => {
      const exports = result.instance.exports;

      console.log("Available exports:", Object.keys(exports));

      // Try to call exported functions
      if (exports.main) {
        console.log("main() =", exports.main());
      }
    })
    .catch((error) => {
      console.error("Error running WASM:", error);
    });
} catch (error) {
  console.error("Error:", error.message);
}
