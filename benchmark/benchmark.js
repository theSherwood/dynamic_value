import fs from "node:fs";
import {
  immutable_arr_create,
  immutable_arr_equal_false,
  immutable_arr_equal_true,
  immutable_arr_get_existing,
  immutable_arr_get_non_existing,
  immutable_arr_iter,
  immutable_arr_pop,
  immutable_arr_push,
  immutable_arr_set,
  immutable_arr_slice,
  plain_arr_create,
  plain_arr_equal_false,
  plain_arr_equal_true,
  plain_arr_get_existing,
  plain_arr_get_non_existing,
  plain_arr_iter,
  plain_arr_pop_by_mutation,
  plain_arr_pop_by_spread,
  plain_arr_push_by_mutation,
  plain_arr_push_by_spread,
  plain_arr_set_by_mutation,
  plain_arr_set_by_spread,
  plain_arr_slice,
} from "./src/js/arr.js";
import {
  LOW_TIMEOUT,
  OUTPUT_PATH,
  RUN_NOOLS,
  bench_async,
  bench_sync,
  csv_rows,
  get_time,
  to_row,
  warmup,
} from "./src/js/common.js";
import {
  immer_pojo_add_entry,
  immer_pojo_add_entry_multiple,
  immer_pojo_add_entry_multiple_batched,
  immer_pojo_del_entry,
  immer_pojo_merge,
  immer_pojo_overwrite_entry,
  immutable_map_add_entry,
  immutable_map_add_entry_multiple,
  immutable_map_add_entry_multiple_batched,
  immutable_map_create,
  immutable_map_del_entry,
  immutable_map_equal_false,
  immutable_map_equal_true,
  immutable_map_get_existing,
  immutable_map_get_non_existing,
  immutable_map_has_key_false,
  immutable_map_has_key_true,
  immutable_map_iter_entries,
  immutable_map_iter_keys,
  immutable_map_iter_values,
  immutable_map_merge,
  immutable_map_overwrite_entry,
  pojo_add_entry_by_mutation,
  pojo_add_entry_by_mutation_multiple,
  pojo_add_entry_by_spread,
  pojo_add_entry_by_spread_multiple,
  pojo_add_entry_by_spread_multiple_batched,
  pojo_create,
  pojo_del_entry_by_mutation,
  pojo_del_entry_by_spread,
  pojo_equal_false,
  pojo_equal_true,
  pojo_get_existing,
  pojo_get_non_existing,
  pojo_has_key_false,
  pojo_has_key_true,
  pojo_iter_entries,
  pojo_iter_keys,
  pojo_iter_values,
  pojo_merge_by_mutation,
  pojo_merge_by_spread,
  pojo_overwrite_entry,
  pojo_overwrite_entry_by_spread,
} from "./src/js/map.js";
import {
  manners_nools,
  send_more_money_imperative,
  send_more_money_nools,
  waltz_db_nools,
} from "./src/js/nools.js";

function sanity_check(tr, _sz, n) {
  let start = get_time();
  var s = 0.0;
  for (let f = 0; f < n; f++) {
    s += f;
    // Add these lines to keep this from getting optimized away
    if (tr.runs.length > 1000000) console.log(s);
    if (tr.runs.length > 10000000) console.log(s);
  }
  tr.runs.push(get_time() - start);
}

/* descriptions */
const PLAIN = "plain";
const PLAIN_MUTATION = "plain_mutation";
const PLAIN_SPREAD = "plain_spread";
const IMMER_POJO = "immer_pojo";
const IMMUTABLEJS = "_immutable.js"; /* add a leading _ so it sorts first; we compare against it */

async function run_benchmarks() {
  await warmup();
  bench_sync("sanity_check", "--", sanity_check, 0, 5000000);
  bench_sync("sanity_check", "--", sanity_check, 0, 50000);
  bench_sync("sanity_check", "--", sanity_check, 0, 500);

  /* value benchmarks */
  {
    /* prettier-ignore */
    for (let it of [10, 100, 1000]) {
      /* array */
      bench_sync("arr_create", PLAIN, plain_arr_create, 0, it, LOW_TIMEOUT);
      bench_sync("arr_create", IMMUTABLEJS, immutable_arr_create, 0, it, LOW_TIMEOUT);
      /* map */
      bench_sync("map_create", PLAIN, pojo_create, 0, it, LOW_TIMEOUT);
      bench_sync("map_create", IMMUTABLEJS, immutable_map_create, 0, it, LOW_TIMEOUT);
      for (let sz of [1, 10, 100, 1000]) {
        if (it < 100 && sz < 100) continue;
        if (it > 100 && sz >= 100) continue;
        if (it >= 100 && sz > 100) continue;
        /* array */
        {
          bench_sync("arr_push", PLAIN_MUTATION, plain_arr_push_by_mutation, sz, it, LOW_TIMEOUT);
          bench_sync("arr_push", PLAIN_SPREAD, plain_arr_push_by_spread, sz, it, LOW_TIMEOUT);
          bench_sync("arr_push", IMMUTABLEJS, immutable_arr_push, sz, it, LOW_TIMEOUT);
          bench_sync("arr_pop", PLAIN_MUTATION, plain_arr_pop_by_mutation, sz, it, LOW_TIMEOUT);
          bench_sync("arr_pop", PLAIN_SPREAD, plain_arr_pop_by_spread, sz, it, LOW_TIMEOUT);
          bench_sync("arr_pop", IMMUTABLEJS, immutable_arr_pop, sz, it, LOW_TIMEOUT);
          bench_sync("arr_slice", PLAIN, plain_arr_slice, sz, it, LOW_TIMEOUT);
          bench_sync("arr_slice", IMMUTABLEJS, immutable_arr_slice, sz, it, LOW_TIMEOUT);
          bench_sync("arr_get_existing", PLAIN, plain_arr_get_existing, sz, it, LOW_TIMEOUT);
          bench_sync("arr_get_existing", IMMUTABLEJS, immutable_arr_get_existing, sz, it, LOW_TIMEOUT);
          bench_sync("arr_get_non_existing", PLAIN, plain_arr_get_non_existing, sz, it, LOW_TIMEOUT);
          bench_sync("arr_get_non_existing", IMMUTABLEJS, immutable_arr_get_non_existing, sz, it, LOW_TIMEOUT);
          bench_sync("arr_set", PLAIN_MUTATION, plain_arr_set_by_mutation, sz, it, LOW_TIMEOUT);
          bench_sync("arr_set", PLAIN_SPREAD, plain_arr_set_by_spread, sz, it, LOW_TIMEOUT);
          bench_sync("arr_set", IMMUTABLEJS, immutable_arr_set, sz, it, LOW_TIMEOUT);
          bench_sync("arr_iter", PLAIN, plain_arr_iter, sz, it, LOW_TIMEOUT);
          bench_sync("arr_iter", IMMUTABLEJS, immutable_arr_iter, sz, it, LOW_TIMEOUT);
          bench_sync("arr_equal_true", PLAIN, plain_arr_equal_true, sz, it, LOW_TIMEOUT);
          bench_sync("arr_equal_true", IMMUTABLEJS, immutable_arr_equal_true, sz, it, LOW_TIMEOUT);
          bench_sync("arr_equal_false", PLAIN, plain_arr_equal_false, sz, it, LOW_TIMEOUT);
          bench_sync("arr_equal_false", IMMUTABLEJS, immutable_arr_equal_false, sz, it, LOW_TIMEOUT);
        }
        /* map */
        {
          bench_sync("map_add_entry", PLAIN_MUTATION, pojo_add_entry_by_mutation, sz, it, LOW_TIMEOUT);
          bench_sync("map_add_entry", PLAIN_SPREAD, pojo_add_entry_by_spread, sz, it, LOW_TIMEOUT);
          bench_sync("map_add_entry", IMMUTABLEJS, immutable_map_add_entry, sz, it, LOW_TIMEOUT);
          bench_sync("map_add_entry", IMMER_POJO, immer_pojo_add_entry, sz, it, LOW_TIMEOUT);
          bench_sync("map_add_entry_multiple", PLAIN_MUTATION, pojo_add_entry_by_mutation_multiple, sz, it, LOW_TIMEOUT);
          bench_sync("map_add_entry_multiple", PLAIN_SPREAD, pojo_add_entry_by_spread_multiple, sz, it, LOW_TIMEOUT);
          bench_sync("map_add_entry_multiple", IMMUTABLEJS, immutable_map_add_entry_multiple, sz, it, LOW_TIMEOUT);
          bench_sync("map_add_entry_multiple", IMMER_POJO, immer_pojo_add_entry_multiple, sz, it, LOW_TIMEOUT);
          bench_sync("map_add_entry_multiple_batched", PLAIN_MUTATION, pojo_add_entry_by_mutation_multiple, sz, it, LOW_TIMEOUT);
          bench_sync("map_add_entry_multiple_batched", PLAIN_SPREAD, pojo_add_entry_by_spread_multiple_batched, sz, it, LOW_TIMEOUT);
          bench_sync("map_add_entry_multiple_batched", IMMUTABLEJS, immutable_map_add_entry_multiple_batched, sz, it, LOW_TIMEOUT);
          bench_sync("map_add_entry_multiple_batched", IMMER_POJO, immer_pojo_add_entry_multiple_batched, sz, it, LOW_TIMEOUT);
          bench_sync("map_overwrite_entry", PLAIN_MUTATION, pojo_overwrite_entry, sz, it, LOW_TIMEOUT);
          bench_sync("map_overwrite_entry", PLAIN_SPREAD, pojo_overwrite_entry_by_spread, sz, it, LOW_TIMEOUT);
          bench_sync("map_overwrite_entry", IMMUTABLEJS, immutable_map_overwrite_entry, sz, it, LOW_TIMEOUT);
          bench_sync("map_overwrite_entry", IMMER_POJO, immer_pojo_overwrite_entry, sz, it, LOW_TIMEOUT);
          bench_sync("map_del_entry", PLAIN_MUTATION, pojo_del_entry_by_mutation, sz, it, LOW_TIMEOUT)
          bench_sync("map_del_entry", PLAIN_SPREAD, pojo_del_entry_by_spread, sz, it, LOW_TIMEOUT)
          bench_sync("map_del_entry", IMMUTABLEJS, immutable_map_del_entry, sz, it, LOW_TIMEOUT)
          bench_sync("map_del_entry", IMMER_POJO, immer_pojo_del_entry, sz, it, LOW_TIMEOUT)
          bench_sync("map_merge", PLAIN_MUTATION, pojo_merge_by_mutation, sz, it, LOW_TIMEOUT)
          bench_sync("map_merge", PLAIN_SPREAD, pojo_merge_by_spread, sz, it, LOW_TIMEOUT)
          bench_sync("map_merge", IMMUTABLEJS, immutable_map_merge, sz, it, LOW_TIMEOUT)
          bench_sync("map_merge", IMMER_POJO, immer_pojo_merge, sz, it, LOW_TIMEOUT)
          bench_sync("map_has_key_true", PLAIN, pojo_has_key_true, sz, it, LOW_TIMEOUT)
          bench_sync("map_has_key_true", IMMUTABLEJS, immutable_map_has_key_true, sz, it, LOW_TIMEOUT)
          bench_sync("map_has_key_false", PLAIN, pojo_has_key_false, sz, it, LOW_TIMEOUT)
          bench_sync("map_has_key_false", IMMUTABLEJS, immutable_map_has_key_false, sz, it, LOW_TIMEOUT)
          bench_sync("map_get_existing", PLAIN, pojo_get_existing, sz, it, LOW_TIMEOUT)
          bench_sync("map_get_existing", IMMUTABLEJS, immutable_map_get_existing, sz, it, LOW_TIMEOUT)
          bench_sync("map_get_non_existing", PLAIN, pojo_get_non_existing, sz, it, LOW_TIMEOUT)
          bench_sync("map_get_non_existing", IMMUTABLEJS, immutable_map_get_non_existing, sz, it, LOW_TIMEOUT)
          bench_sync("map_iter_keys", PLAIN, pojo_iter_keys, sz, it, LOW_TIMEOUT)
          bench_sync("map_iter_keys", IMMUTABLEJS, immutable_map_iter_keys, sz, it, LOW_TIMEOUT)
          bench_sync("map_iter_values", PLAIN, pojo_iter_values, sz, it, LOW_TIMEOUT)
          bench_sync("map_iter_values", IMMUTABLEJS, immutable_map_iter_values, sz, it, LOW_TIMEOUT)
          bench_sync("map_iter_entries", PLAIN, pojo_iter_entries, sz, it, LOW_TIMEOUT)
          bench_sync("map_iter_entries", IMMUTABLEJS, immutable_map_iter_entries, sz, it, LOW_TIMEOUT)
          bench_sync("map_equal_true", PLAIN, pojo_equal_true, sz, it, LOW_TIMEOUT)
          bench_sync("map_equal_true", IMMUTABLEJS, immutable_map_equal_true, sz, it, LOW_TIMEOUT)
          bench_sync("map_equal_false", PLAIN, pojo_equal_false, sz, it, LOW_TIMEOUT)
          bench_sync("map_equal_false", IMMUTABLEJS, immutable_map_equal_false, sz, it, LOW_TIMEOUT)
        }
      }
    }
  }
  /* rules benchmarks */
  {
    bench_sync("send_more_money", "imperative", send_more_money_imperative, 0, 1);
    /* nools */
    if (RUN_NOOLS) {
      await Promise.all([
        bench_sync("send_more_money", "nools", send_more_money_nools, 0, 1),
        bench_async("manners", "nools", manners_nools, 5, 1),
        bench_async("manners", "nools", manners_nools, 8, 1),
        // bench_async("manners", "nools", manners_nools, 16, 1),
        // bench_async("manners", "nools", manners_nools, 32, 1),
        // bench_async("manners", "nools", manners_nools, 64, 1),
        // bench_async("manners", "nools", manners_nools, 128, 1),
        bench_async("waltz_db", "nools", waltz_db_nools, 4, 1),
        bench_async("waltz_db", "nools", waltz_db_nools, 8, 1),
        // bench_async("waltz_db", "nools", waltz_db_nools, 12, 1),
        // bench_async("waltz_db", "nools", waltz_db_nools, 16, 1),
      ]);
    }
  }
}

run_benchmarks().then(() => {
  fs.writeFileSync(
    OUTPUT_PATH,
    '"key","sys","desc","runs","minimum","maximum","mean","median"\n' +
      csv_rows.map(to_row).join("\n")
  );
});
