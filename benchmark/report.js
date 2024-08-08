import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import csvParser from "csv-parser";

// Polyfill __dirname because we are doing some esm nonsense
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const PRINT_CONCISE_REPORT = true;
const WRITE_FULL_REPORT = true;

// Where to output the report
const OUTPUT_PATH = "./benchmarks.csv";

const COLUMNS = "key sys desc runs minimum maximum mean median".split(" ");
const COLUMNS_INCL_MULT =
  "key sys desc runs run_m minimum min_m maximum max_m mean mean_m median med_m".split(" ");
const COLUMNS_MULT = "key sys desc run_m min_m max_m mean_m med_m".split(" ");

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

// Function to read contents of CSV files
async function readCSVFiles(fileList) {
  const csvData = [];
  await Promise.all(
    fileList.map((file) => {
      return new Promise((resolve, reject) =>
        fs
          .createReadStream(file)
          .pipe(csvParser())
          .on("data", (row) => {
            // Process each row as it is read
            csvData.push(row);
          })
          .on("end", () => {
            // This callback is called when all rows have been read from the CSV file
            console.log(`Finished reading ${file}`);
            resolve();
          })
          .on("error", (error) => {
            console.error(`Error reading ${file}: ${error.message}`);
            reject();
          })
      );
    })
  );
  return csvData;
}

function sort_csv_data(csv_data) {
  let sorted = csv_data.toSorted((a, b) => {
    if (a.key < b.key) return -1;
    if (b.key < a.key) return 1;
    if (a.sys < b.sys) return -1;
    if (b.sys < a.sys) return 1;
    if (a.desc < b.desc) return -1;
    if (b.desc < a.desc) return 1;
    return 0;
  });
  return sorted;
}

function split_into_tables(csv_data) {
  let tables = [];
  let last_idx = -1;
  let last_key = "";
  let i = 0;
  for (; i < csv_data.length; i++) {
    let row = csv_data[i];
    if (row.key !== last_key) {
      if (last_idx > -1) {
        tables.push(csv_data.slice(last_idx, i));
      }
      last_key = row.key;
      last_idx = i;
    }
  }
  if (i !== last_idx) tables.push(csv_data.slice(last_idx, i));
  return tables;
}

function enrich_table_with_multiples(table) {
  let row1 = table[0];
  for (let i = 0; i < table.length; i++) {
    let row = table[i];
    row.run_m = (row.runs / row1.runs).toFixed(2);
    row.min_m = (row.minimum / row1.minimum).toFixed(2);
    row.max_m = (row.maximum / row1.maximum).toFixed(2);
    row.mean_m = (row.mean / row1.mean).toFixed(2);
    row.med_m = (row.median / row1.median).toFixed(2);
  }
}

function get_column_widths(data) {
  let widths = {};
  COLUMNS_INCL_MULT.forEach((c) => (widths[c] = c.length));
  data.forEach((row) => {
    COLUMNS_INCL_MULT.forEach((c) => {
      widths[c] = Math.max(row[c].length, widths[c]);
    });
  });
  return widths;
}

function empty_column_widths() {
  let widths = {};
  COLUMNS_INCL_MULT.forEach((c) => (widths[c] = 0));
  return widths;
}

function delimited_table(table, column_widths, delimiter, columns = COLUMNS_INCL_MULT) {
  let row_strs = table.map((row) =>
    columns
      .map((c) => {
        return row[c].padStart(column_widths[c], " ");
      })
      .join(delimiter)
  );
  return row_strs.join("\n");
}

function delimited_column_headings(column_widths, delimiter, columns = COLUMNS_INCL_MULT) {
  return columns.map((c) => c.padStart(column_widths[c], " ")).join(delimiter);
}

async function main() {
  const parentDirectory = __dirname;
  const fileList = searchForCSVFiles(parentDirectory, []);

  if (fileList.length === 0) {
    console.log("No CSV files found in the directory.");
    return;
  }

  const csvData = await readCSVFiles(fileList);
  let sorted = sort_csv_data(csvData);
  let tables = split_into_tables(sorted);
  tables.forEach(enrich_table_with_multiples);
  let column_widths = get_column_widths(tables.flat(1));

  // Print concise report to console
  if (PRINT_CONCISE_REPORT) {
    const DELIM = " ";
    console.log(delimited_column_headings(column_widths, DELIM, COLUMNS_MULT));
    tables.forEach((t) => {
      // Add an empty row between tables
      console.log("");
      console.log(delimited_table(t, column_widths, DELIM, COLUMNS_MULT));
    });
  }

  // Write full report to csv
  if (WRITE_FULL_REPORT) {
    const DELIM = ",";
    let str = delimited_column_headings(empty_column_widths(), DELIM, COLUMNS_INCL_MULT) + "\n";
    tables.forEach((t) => {
      str += delimited_table(t, empty_column_widths(), DELIM, COLUMNS_INCL_MULT) + "\n";
    });
    fs.writeFileSync(OUTPUT_PATH, str);
  }
}

main();
