import deep_eq from "deep-equal";
import { produce } from "immer";
import { Map as ImMap } from "immutable";
import { strict as assert } from "node:assert";
import { get_time } from "./common.js";

export function setup_arr_of_pojos(sz, n, offset = 0) {
  let pojos = [];
  let i_off, k;
  for (let i = 0; i < n; i++) {
    i_off = i + offset;
    let pojo = { [i_off]: i_off };
    for (let j = 1; j < sz; j++) {
      k = i_off + j * 17;
      pojo[k] = k;
    }
    pojos.push(pojo);
  }
  return pojos;
}

export function setup_arr_of_immutable_maps(sz, n, offset = 0) {
  let maps = [];
  let i_off, k;
  for (let i = 0; i < n; i++) {
    i_off = i + offset;
    let map = ImMap({ [i_off]: i_off });
    for (let j = 1; j < sz; j++) {
      k = i_off + j * 17;
      map = map.set(k, k);
    }
    maps.push(map);
  }
  return maps;
}

export function force_copy(m) {
  return m.set(-1, -1).delete(-1);
}

export function pojo_create(tr, sz, n) {
  let start = get_time();
  let objs = [];
  for (let i = 0; i < n; i++) {
    objs.push({ i: i });
  }
  tr.runs.push(get_time() - start);
}

export function immutable_map_create(tr, sz, n) {
  let start = get_time();
  let maps = [];
  for (let i = 0; i < n; i++) {
    maps.push(ImMap({ i: i }));
  }
  tr.runs.push(get_time() - start);
}

export function pojo_add_entry_by_mutation(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    objs[i][i + 1] = i + 1;
  }
  tr.runs.push(get_time() - start);
}

export function pojo_add_entry_by_spread(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    objs[i] = { ...objs[i], [i + 1]: i + 1 };
  }
  tr.runs.push(get_time() - start);
}

export function immutable_map_add_entry(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    maps[i] = maps[i].set(i + 1, i + 1);
  }
  tr.runs.push(get_time() - start);
}

export function immer_pojo_add_entry(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    maps[i] = produce(maps[i], (m) => {
      m[i + 1] = i + 1;
    });
  }
  tr.runs.push(get_time() - start);
}

export function pojo_add_entry_by_mutation_multiple(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    let o = objs[i];
    o[i + 1] = i + 1;
    o[i + 2] = i + 2;
    o[i + 3] = i + 3;
    o[i + 4] = i + 4;
    o[i + 5] = i + 5;
  }
  tr.runs.push(get_time() - start);
}

export function pojo_add_entry_by_spread_multiple(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    objs[i] = {
      ...{
        ...{
          ...{
            ...{
              ...objs[i],
              [i + 1]: i + 1,
            },
            [i + 2]: i + 2,
          },
          [i + 3]: i + 3,
        },
        [i + 4]: i + 4,
      },
      [i + 5]: i + 5,
    };
  }
  tr.runs.push(get_time() - start);
}

export function immutable_map_add_entry_multiple(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    maps[i] = maps[i]
      .set(i + 1, i + 1)
      .set(i + 2, i + 2)
      .set(i + 3, i + 3)
      .set(i + 4, i + 4)
      .set(i + 5, i + 5);
  }
  tr.runs.push(get_time() - start);
}

export function immer_pojo_add_entry_multiple(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    maps[i] = produce(maps[i], (m) => {
      m[i + 1] = i + 1;
    });
    maps[i] = produce(maps[i], (m) => {
      m[i + 2] = i + 2;
    });
    maps[i] = produce(maps[i], (m) => {
      m[i + 3] = i + 3;
    });
    maps[i] = produce(maps[i], (m) => {
      m[i + 4] = i + 4;
    });
    maps[i] = produce(maps[i], (m) => {
      m[i + 5] = i + 5;
    });
  }
  tr.runs.push(get_time() - start);
}

export function pojo_add_entry_by_spread_multiple_batched(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    objs[i] = {
      ...objs[i],
      [i + 1]: i + 1,
      [i + 2]: i + 2,
      [i + 3]: i + 3,
      [i + 4]: i + 4,
      [i + 5]: i + 5,
    };
  }
  tr.runs.push(get_time() - start);
}

export function immutable_map_add_entry_multiple_batched(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    maps[i] = maps[i].withMutations((m) =>
      m
        .set(i + 1, i + 1)
        .set(i + 2, i + 2)
        .set(i + 3, i + 3)
        .set(i + 4, i + 4)
        .set(i + 5, i + 5)
    );
  }
  tr.runs.push(get_time() - start);
}

export function immer_pojo_add_entry_multiple_batched(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    maps[i] = produce(maps[i], (m) => {
      m[i + 1] = i + 1;
      m[i + 2] = i + 2;
      m[i + 3] = i + 3;
      m[i + 4] = i + 4;
      m[i + 5] = i + 5;
    });
  }
  tr.runs.push(get_time() - start);
}

export function pojo_overwrite_entry(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    objs[i][i] = i + 1;
  }
  tr.runs.push(get_time() - start);
}

export function pojo_overwrite_entry_by_spread(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    objs[i] = { ...objs[i], [i]: i + 1 };
  }
  tr.runs.push(get_time() - start);
}

export function immutable_map_overwrite_entry(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    maps[i] = maps[i].set(i, i + 1);
  }
  tr.runs.push(get_time() - start);
}

export function immer_pojo_overwrite_entry(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    maps[i] = produce(maps[i], (m) => {
      m[i] = i + 1;
    });
  }
  tr.runs.push(get_time() - start);
}

export function pojo_del_entry_by_mutation(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    delete objs[i][i];
  }
  tr.runs.push(get_time() - start);
}

export function pojo_del_entry_by_spread(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    objs[i] = { ...objs[i] };
    delete objs[i][i];
  }
  tr.runs.push(get_time() - start);
}

export function immutable_map_del_entry(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    maps[i] = maps[i].delete(i);
  }
  tr.runs.push(get_time() - start);
}

export function immer_pojo_del_entry(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_pojos(sz, n);
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    maps[i] = produce(maps[i], (m) => {
      delete m[i];
    });
  }
  tr.runs.push(get_time() - start);
}

export function pojo_merge_by_mutation(tr, sz, n) {
  /* setup */
  let objs1 = setup_arr_of_pojos(sz, n);
  let objs2 = setup_arr_of_pojos(sz, n, 3);
  let objs3 = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    objs3[i] = Object.assign(objs1[i], objs2[i]);
  }
  tr.runs.push(get_time() - start);
}

export function pojo_merge_by_spread(tr, sz, n) {
  /* setup */
  let objs1 = setup_arr_of_pojos(sz, n);
  let objs2 = setup_arr_of_pojos(sz, n, 3);
  let objs3 = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    objs3[i] = { ...objs1[i], ...objs2[i] };
  }
  tr.runs.push(get_time() - start);
}

export function immutable_map_merge(tr, sz, n) {
  /* setup */
  let maps1 = setup_arr_of_immutable_maps(sz, n);
  let maps2 = setup_arr_of_immutable_maps(sz, n, 3);
  let maps3 = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    maps3[i] = maps1[i].merge(maps2[i]);
  }
  tr.runs.push(get_time() - start);
}

export function immer_pojo_merge(tr, sz, n) {
  /* setup */
  let objs1 = setup_arr_of_pojos(sz, n);
  let objs2 = setup_arr_of_pojos(sz, n, 3);
  let objs3 = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    objs3[i] = produce(objs1[i], (m) => {
      Object.assign(m, objs2[i]);
    });
  }
  tr.runs.push(get_time() - start);
}

export function pojo_has_key_true(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  let bools = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    bools[i] = i in objs[i];
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => b === true)
  );
}

export function pojo_has_key_false(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  let bools = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    bools[i] = i + 1 in objs[i];
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => b === false)
  );
}

export function immutable_map_has_key_true(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  let bools = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    bools[i] = maps[i].has(i + "");
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => b === true)
  );
}

export function immutable_map_has_key_false(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  let bools = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    bools[i] = maps[i].has(i + 1 + "");
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => b === false)
  );
}

export function pojo_get_existing(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  let vals = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    vals[i] = objs[i][i];
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    vals.every((v) => v !== undefined)
  );
}

export function pojo_get_non_existing(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  let vals = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    vals[i] = objs[i][i + 1];
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    vals.every((v) => v === undefined)
  );
}

export function immutable_map_get_existing(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  let vals = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    vals[i] = maps[i].get(i + "");
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    vals.every((v) => v !== undefined)
  );
}

export function immutable_map_get_non_existing(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  let vals = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    vals[i] = maps[i].get(i + 1 + "");
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    vals.every((v) => v === undefined)
  );
}

export function pojo_iter_keys(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  let iters = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    iters.push(Object.keys(objs[i]));
  }
  tr.runs.push(get_time() - start);
}

export function pojo_iter_values(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  let iters = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    iters.push(Object.values(objs[i]));
  }
  tr.runs.push(get_time() - start);
}

export function pojo_iter_entries(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  let iters = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    iters.push(Object.entries(objs[i]));
  }
  tr.runs.push(get_time() - start);
}

export function immutable_map_iter_keys(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  let iters = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    let vals = [];
    for (let v of maps[i].keys()) vals.push(v);
    iters.push(vals);
  }
  tr.runs.push(get_time() - start);
}

export function immutable_map_iter_values(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  let iters = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    let vals = [];
    for (let v of maps[i].values()) vals.push(v);
    iters.push(vals);
  }
  tr.runs.push(get_time() - start);
}

export function immutable_map_iter_entries(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  let iters = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    let vals = [];
    for (let v of maps[i].entries()) vals.push(v);
    iters.push(vals);
  }
  tr.runs.push(get_time() - start);
}

export function pojo_equal_true(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  let copies = objs.map((o) => Object.assign({}, o));
  let bools = [];
  /* test */
  let start = get_time();
  let opts = { strict: true };
  for (let i = 0; i < n; i++) {
    bools[i] = deep_eq(objs[i], copies[i], opts);
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => b)
  );
}

export function pojo_equal_false(tr, sz, n) {
  /* setup */
  let objs = setup_arr_of_pojos(sz, n);
  let objs2 = setup_arr_of_pojos(sz, n, 3);
  let bools = [];
  /* test */
  let start = get_time();
  let opts = { strict: true };
  for (let i = 0; i < n; i++) {
    bools[i] = deep_eq(objs[i], objs2[i], opts);
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => b === false)
  );
}

export function immutable_map_equal_true(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  let copies = maps.map(force_copy);
  let bools = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    bools[i] = maps[i].equals(copies[i]);
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => b)
  );
}

export function immutable_map_equal_false(tr, sz, n) {
  /* setup */
  let maps = setup_arr_of_immutable_maps(sz, n);
  let maps2 = setup_arr_of_immutable_maps(sz, n, 3);
  let bools = [];
  /* test */
  let start = get_time();
  for (let i = 0; i < n; i++) {
    bools[i] = maps[i].equals(maps2[i]);
  }
  tr.runs.push(get_time() - start);
  assert.equal(
    true,
    bools.every((b) => b === false)
  );
}
