import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { strict as assert } from "node:assert";
import nools from "nools";
import deep_eq from "deep-equal";
import { get_time } from "./common.js";
import { load_manners_data } from "../../data/manners.js";
import { load_waltz_db_data } from "../../data/waltz_db.js";

// Polyfill __dirname because we are doing some ESM nonsense
export const __dirname = path.dirname(fileURLToPath(import.meta.url));

const SEND_MORE_MONEY_ANSWER = { s: 9, e: 5, n: 6, d: 7, m: 1, o: 0, r: 8, y: 2 };

/**
 *
 * @link
 * https://github.com/noolsjs/nools/blob/master/examples/browser/sendMoreMoney.html
 */
export function send_more_money_nools(tr, sz, n) {
  let nools_code = fs
    .readFileSync(path.resolve(__dirname, "../nools/send_more_money.nools"))
    .toString();
  var flow = nools.compile(nools_code, { name: "SendMoreMoney" });
  let start = get_time();
  let session;
  for (let i = 0; i < n; i++) {
    // calculate
    (session = flow.getSession(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
      .on("solved", function (solved) {
        assert.deepEqual(solved, SEND_MORE_MONEY_ANSWER);
      })
      .match()
      .then(function () {
        session.dispose();
      });
  }
  tr.runs.push(get_time() - start);
}

function send_more_money_imperative_inner() {
  let stop = 10;
  let tup = [0, 0];
  let send_more = (s, e, n, d, m, o, r) =>
    s * 1000 + e * 100 + n * 10 + d + m * 1000 + o * 100 + r * 10 + e;
  let money = (m, o, n, e, y) => m * 10000 + o * 1000 + n * 100 + e * 10 + y;
  let test = (v, ...args) => !args.includes(v);
  for (let [i, s] = tup; i < stop; s = ++i)
    if (test(s, 0))
      for (let [i, e] = tup; i < stop; e = ++i)
        if (test(e, s))
          for (let [i, n] = tup; i < stop; n = ++i)
            if (test(n, s, e))
              for (let [i, d] = tup; i < stop; d = ++i)
                if (test(d, s, e, n))
                  for (let [i, m] = tup; i < stop; m = ++i)
                    if (test(m, s, e, n, d, 0))
                      for (let [i, o] = tup; i < stop; o = ++i)
                        if (test(o, s, e, n, d, m))
                          for (let [i, r] = tup; i < stop; r = ++i)
                            if (test(r, s, e, n, d, m, o))
                              for (let [i, y] = tup; i < stop; y = ++i)
                                if (test(y, s, e, n, d, m, o, r))
                                  if (send_more(s, e, n, d, m, o, r) === money(m, o, n, e, y)) {
                                    return { s, e, n, d, m, o, r, y };
                                  }
}
export function send_more_money_imperative(tr, sz, n) {
  let start = get_time();
  for (let i = 0; i < n; i++) {
    assert.deepEqual(send_more_money_imperative_inner(), SEND_MORE_MONEY_ANSWER);
  }
  tr.runs.push(get_time() - start);
}

/**
 * @link
 * https://github.com/noolsjs/nools/blob/master/examples/browser/manners.html
 *
 * @param {128 | 64 | 32 | 16 | 8 | 5} sz
 */
export async function manners_nools(tr, sz, _n) {
  let name = "manners_" + sz;
  let nools_code = fs.readFileSync(path.resolve(__dirname, "../nools/manners.nools")).toString();
  let session,
    flow = nools.compile(nools_code, { name }),
    Count = flow.getDefined("count"),
    guests = load_manners_data(flow, name);
  session = flow.getSession();
  for (var i = 0, l = guests.length; i < l; i++) {
    session.assert(guests[i]);
  }
  session.assert(new Count({ value: 1 }));
  let start = get_time();
  await new Promise((resolve, reject) => {
    session
      .on("pathDone", function (obj) {})
      .match()
      .then(
        function () {
          /* done */
          resolve();
        },
        function (e) {
          console.error(e);
          reject();
        }
      );
  });
  tr.runs.push(get_time() - start);
}

/**
 * @link
 * https://github.com/noolsjs/nools/blob/master/examples/browser/waltzDb.html
 *
 * @param {16 | 12 | 8 | 4} sz
 */
export async function waltz_db_nools(tr, sz, _n) {
  let name = "waltz_db_" + sz;
  let nools_code = fs.readFileSync(path.resolve(__dirname, "../nools/waltz_db.nools")).toString();
  let session,
    flow = nools
      .compile(nools_code, { name })
      .conflictResolution(["salience", "factRecency", "activationRecency"]),
    data = load_waltz_db_data(flow, name);
  session = flow.getSession();
  for (var i = 0, l = data.length; i < l; i++) {
    session.assert(data[i]);
  }
  session.assert(new (flow.getDefined("stage"))({ value: "DUPLICATE" }));
  let start = get_time();
  await new Promise((resolve, reject) => {
    session
      .on("log", function (obj) {})
      .match()
      .then(
        function () {
          /* done */
          resolve();
        },
        function (e) {
          console.error(e);
          reject();
        }
      );
  });
  tr.runs.push(get_time() - start);
}
