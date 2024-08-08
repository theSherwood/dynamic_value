import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

// Polyfill __dirname because we are doing some esm nonsense
const __dirname = path.dirname(fileURLToPath(import.meta.url));

// Function to recursively search for CSV files in a directory
function searchForCSVFiles(directoryPath, fileList) {
  const files = fs.readdirSync(directoryPath);

  files.forEach((file) => {
    const filePath = path.join(directoryPath, file);
    const stats = fs.statSync(filePath);

    if (stats.isDirectory()) {
      searchForCSVFiles(filePath, fileList); // Recursive call for subdirectories
    } else if (path.extname(file) === ".csv") {
      fileList.push(filePath);
    }
  });

  return fileList;
}

async function main() {
  const parentDirectory = __dirname;
  const fileList = searchForCSVFiles(parentDirectory, []);
  for (let file of fileList) {
    console.log("Deleting:", file);
    fs.unlinkSync(file)
  }
}

main();
