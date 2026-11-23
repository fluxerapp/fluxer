// SPDX-License-Identifier: AGPL-3.0-or-later

macro_rules! absent {
    ($($name:ident),* $(,)?) => {
        $(
            #[unsafe(no_mangle)]
            extern "C" fn $name() {
                std::process::abort()
            }
        )*
    };
}

absent!(
    enif_alloc_binary,
    enif_alloc_env,
    enif_alloc_resource,
    enif_consume_timeslice,
    enif_free_env,
    enif_get_int,
    enif_get_list_cell,
    enif_get_list_length,
    enif_get_local_pid,
    enif_get_long,
    enif_get_resource,
    enif_get_tuple,
    enif_get_uint,
    enif_get_ulong,
    enif_inspect_binary,
    enif_is_atom,
    enif_is_empty_list,
    enif_is_exception,
    enif_is_list,
    enif_keep_resource,
    enif_make_atom_len,
    enif_make_badarg,
    enif_make_binary,
    enif_make_copy,
    enif_make_list_from_array,
    enif_make_new_binary,
    enif_make_resource,
    enif_make_resource_binary,
    enif_make_sub_binary,
    enif_make_tuple_from_array,
    enif_make_uint,
    enif_make_ulong,
    enif_monitor_process,
    enif_raise_exception,
    enif_realloc_binary,
    enif_release_binary,
    enif_release_resource,
    enif_schedule_nif,
    enif_self,
    enif_term_type,
    enif_thread_type,
);
