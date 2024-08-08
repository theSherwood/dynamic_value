import deep_eq from "deep-equal";
import { List as ImArr } from "immutable";
import { strict as assert } from "node:assert";
import { get_time } from "./common.js";

function setup_arr_of_arrs(sz, n, offset = 0) {
  let arrs = [];
  let i_off, k;
  for (let i = 0; i < n; i++) {
    i_off = i + offset;
    let arr = [i_off];
    for (let j = 1; j < sz; j++) {
      k = i_off + j * 17;
      arr.push[k];
    }
    arrs.push(arr);
  }
  return arrs;
}

export function setup_arr_of_immutable_arrs(sz, n, offset = 0) {
  let arrs = [];
  let i_off, k;
  for (let i = 0; i < n; i++) {
    i_off = i + offset;
    let arr = ImArr([i_off]);
    for (let j = 1; j < sz; j++) {
      k = i_off + j * 17;
      arr = arr.push(k);
    }
    arrs.push(arr);
  }
  return arrs;
}

export function plain_arr_create(tr, sz, n) {
  let start = get_time();
  let arrs = [];
  for (let i = 0; i < n; i++) {
    arrs.push([i]);
  }
  tr.runs.push(get_time() - start);
}

export function immutable_arr_create(tr, sz, n) {
  let start = get_time();
  let arrs = [];
  for (let i = 0; i < n; i++) {
    arrs.push(ImArr([i]));
  }
  tr.runs.push(get_time() - start);
}

export function plain_arr_push_by_mutation(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i].push(i);
  }
  tr.runs.push(get_time() - start);
}

export function plain_arr_push_by_spread(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = [...arrs[i], i];
  }
  tr.runs.push(get_time() - start);
}

export function immutable_arr_push(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_immutable_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = arrs[i].push(i);
  }
  tr.runs.push(get_time() - start);
}

export function plain_arr_pop_by_mutation(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i].pop();
  }
  tr.runs.push(get_time() - start);
}

export function plain_arr_pop_by_spread(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = [...arrs[i]];
    arrs[i].pop();
  }
  tr.runs.push(get_time() - start);
}

export function immutable_arr_pop(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_immutable_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = arrs[i].pop();
  }
  tr.runs.push(get_time() - start);
}

export function plain_arr_slice(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = arrs[i].slice(i, arrs[i].length / 2);
  }
  tr.runs.push(get_time() - start);
}

export function immutable_arr_slice(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_immutable_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = arrs[i].slice(i, arrs[i].length / 2);
  }
  tr.runs.push(get_time() - start);
}

export function plain_arr_get_existing(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = arrs[i][arrs[i].length / 2];
  }
  tr.runs.push(get_time() - start);
}

export function plain_arr_get_non_existing(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = arrs[i][arrs[i].length * 2];
  }
  tr.runs.push(get_time() - start);
}

export function immutable_arr_get_existing(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_immutable_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = arrs[i].get(arrs[i].length / 2);
  }
  tr.runs.push(get_time() - start);
}

export function immutable_arr_get_non_existing(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_immutable_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = arrs[i].get(arrs[i].length * 2);
  }
  tr.runs.push(get_time() - start);
}

export function plain_arr_set_by_mutation(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i][arrs[i].length / 2] = -1;
  }
  tr.runs.push(get_time() - start);
}

export function plain_arr_set_by_spread(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = { ...arrs[i] };
    arrs[i][arrs[i].length / 2] = -1;
  }
  tr.runs.push(get_time() - start);
}

export function immutable_arr_set(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_immutable_arrs(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    arrs[i] = arrs[i].set(arrs[i].length / 2, -1);
  }
  tr.runs.push(get_time() - start);
}

export function plain_arr_iter(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  let iters = [];
  let vals = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    vals = [];
    for (let v of arrs[i]) vals.push(v);
    iters.push(vals);
  }
  tr.runs.push(get_time() - start);
}

export function immutable_arr_iter(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_immutable_arrs(sz, n);
  let iters = [];
  let vals = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    vals = [];
    for (let v of arrs[i].values()) vals.push(v);
    iters.push(vals);
  }
  tr.runs.push(get_time() - start);
}

export function plain_arr_equal_true(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  let copies = setup_arr_of_arrs(sz, n);
  let bools = [];
  /* test */
  let start = get_time();
  let opts = { strict: true };
  for (let i = 0; i < n; i++) {
    bools[i] = deep_eq(arrs[i], copies[i], opts);
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => b)
  );
}

export function plain_arr_equal_false(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_arrs(sz, n);
  let arrs2 = setup_arr_of_arrs(sz, n, 3);
  let bools = [];
  /* test */
  let start = get_time();
  let opts = { strict: true };
  for (let i = 0; i < n; i++) {
    bools[i] = deep_eq(arrs[i], arrs2[i], opts);
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => !b)
  );
}

export function immutable_arr_equal_true(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_immutable_arrs(sz, n);
  let copies = setup_arr_of_immutable_arrs(sz, n);
  let bools = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    bools[i] = arrs[i].equals(copies[i]);
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => b)
  );
}

export function immutable_arr_equal_false(tr, sz, n) {
  /* setup */
  let arrs = setup_arr_of_immutable_arrs(sz, n);
  let arrs2 = setup_arr_of_immutable_arrs(sz, n, 3);
  let bools = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    bools[i] = arrs[i].equals(arrs2[i]);
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => !b)
  );
}
