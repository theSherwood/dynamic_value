import path from "node:path";
import { fileURLToPath } from "node:url";

// Polyfill __dirname because we are doing some ESM nonsense
export const __dirname = path.dirname(fileURLToPath(import.meta.url));

export const OUTPUT_PATH = "./benchmark/results_js.csv";
export const WARMUP = 100_000; // microseconds
// We are probably going to be running into issues with JIT massively optimizing
// things if we are using a timeout this long. So we also include a LOW_TIMEOUT
// for use when we want it.
export const TIMEOUT = 100_000;
// export const LOW_TIMEOUT = 100_000;
export const LOW_TIMEOUT = 2;
export const RUN_NOOLS = true;

export let csv_rows = [];

export function get_time() {
  return Math.round(performance.now() * 1000);
}

export let form = (f) => f.toFixed(2);

export function to_row(tr) {
  let l = tr.runs.length,
    s = `"${tr.key}","js","${tr.desc}",${l},`,
    sorted_runs = tr.runs.toSorted(),
    sum = 0,
    minimum = Infinity,
    maximum = 0,
    mean = 0,
    median = 0,
    r = 0;
  for (let i = 0; i < l; i++) {
    r = sorted_runs[i];
    sum += r;
    minimum = Math.min(minimum, r);
    maximum = Math.max(maximum, r);
  }
  mean = sum / l;
  if (l == 1) median = sorted_runs[0];
  else median = (sorted_runs[Math.floor(l / 2)] + sorted_runs[Math.ceil(l / 2)]) / 2;
  s += `${form(minimum)},${form(maximum)},${form(mean)},${form(median)}`;
  return s;
}

export async function warmup() {
  return setTimeout(() => {}, WARMUP / 1000);
}

export function bench_sync(key, desc, fn, sz, iterations, timeout = TIMEOUT) {
  let tr = { key: `${key}_${sz}_${iterations}`, desc, runs: [] };
  csv_rows.push(tr);
  let start = get_time();
  let end = get_time();
  // Ensure that it runs at least once
  do {
    fn(tr, sz, iterations);
    end = get_time();
  } while (timeout > end - start);
  console.log(`done ${tr.key}`);
}

export async function bench_async(key, desc, fn, sz, iterations, timeout = TIMEOUT) {
  let tr = { key: `${key}_${sz}_${iterations}`, desc, runs: [] };
  csv_rows.push(tr);
  let start = get_time();
  let end = get_time();
  // Ensure that it runs at least once
  do {
    await fn(tr, sz, iterations);
    end = get_time();
  } while (timeout > end - start);
  console.log(`done js ${tr.key}`);
}
