// SPDX-License-Identifier: AGPL-3.0-or-later

#![deny(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects
)]

use std::ffi::CStr;
use std::panic::{AssertUnwindSafe, catch_unwind};

use rustler::{Encoder, Env, NifResult, Term};

mod dirty;
#[cfg(all(test, target_os = "linux"))]
mod test_link;

mod atoms {
    rustler::atoms! {
        error,
        broken,
    }
}

fn guard<'a>(env: Env<'a>, body: impl FnOnce() -> Term<'a>) -> Term<'a> {
    match catch_unwind(AssertUnwindSafe(body)) {
        Ok(term) => term,
        Err(_) => (atoms::error(), atoms::broken()).encode(env),
    }
}

fn guard_raise<'a>(body: impl FnOnce() -> NifResult<Term<'a>>) -> NifResult<Term<'a>> {
    catch_unwind(AssertUnwindSafe(body)).unwrap_or(Err(rustler::Error::RaiseAtom("broken")))
}

#[rustler::nif]
fn perm_new<'a>(
    env: Env<'a>,
    guild: Term<'a>,
    config: Term<'a>,
    owner: Term<'a>,
) -> NifResult<Term<'a>> {
    guard_raise(|| perm_nif::nifs::new(env, guild, config, owner))
}

#[rustler::nif]
fn perm_load<'a>(env: Env<'a>, perms: Term<'a>, snapshot: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::load(env, perms, snapshot))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn perm_load_dirty<'a>(env: Env<'a>, perms: Term<'a>, snapshot: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::load_dirty(env, perms, snapshot))
}

#[rustler::nif]
fn perm_apply<'a>(env: Env<'a>, perms: Term<'a>, events: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::apply(env, perms, events))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn perm_apply_dirty<'a>(env: Env<'a>, perms: Term<'a>, events: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::apply_dirty(env, perms, events))
}

#[rustler::nif]
fn perm_compact<'a>(env: Env<'a>, perms: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::compact(env, perms))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn perm_compact_dirty<'a>(env: Env<'a>, perms: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::compact_dirty(env, perms))
}

#[rustler::nif]
fn perm_model<'a>(env: Env<'a>, reader: Term<'a>) -> NifResult<Term<'a>> {
    guard_raise(|| perm_nif::nifs::model(env, reader))
}

#[rustler::nif]
fn perm_permissions<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    scope: Term<'a>,
) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"perm_permissions", &[reader, user, scope], |on| {
            perm_nif::nifs::permissions(env, reader, user, scope, on)
        })
    })
}

#[rustler::nif]
fn perm_check<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    scope: Term<'a>,
    bits: Term<'a>,
) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"perm_check", &[reader, user, scope, bits], |on| {
            perm_nif::nifs::check(env, reader, user, scope, bits, on)
        })
    })
}

#[rustler::nif]
fn perm_visible<'a>(env: Env<'a>, reader: Term<'a>, user: Term<'a>, channel: Term<'a>) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"perm_visible", &[reader, user, channel], |on| {
            perm_nif::nifs::visible(env, reader, user, channel, on)
        })
    })
}

#[rustler::nif]
fn perm_has_virtual_view<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    channel: Term<'a>,
) -> Term<'a> {
    guard(env, || {
        dirty::run(
            env,
            c"perm_has_virtual_view",
            &[reader, user, channel],
            |on| perm_nif::nifs::has_virtual_view(env, reader, user, channel, on),
        )
    })
}

#[rustler::nif]
fn perm_viewable<'a>(env: Env<'a>, reader: Term<'a>, user: Term<'a>) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"perm_viewable", &[reader, user], |on| {
            perm_nif::nifs::viewable(env, reader, user, on)
        })
    })
}

#[rustler::nif]
fn perm_shares_view_many<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    candidates: Term<'a>,
) -> Term<'a> {
    guard(env, || {
        dirty::run(
            env,
            c"perm_shares_view_many",
            &[reader, user, candidates],
            |on| perm_nif::nifs::shares_view_many(env, reader, user, candidates, on),
        )
    })
}

#[rustler::nif]
fn perm_audience<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    channel: Term<'a>,
    need: Term<'a>,
) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"perm_audience", &[reader, channel, need], |on| {
            perm_nif::nifs::audience(env, reader, channel, need, on)
        })
    })
}

#[rustler::nif]
fn perm_viewers<'a>(env: Env<'a>, reader: Term<'a>, channel: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::viewers(env, reader, channel))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn perm_viewers_dirty<'a>(env: Env<'a>, reader: Term<'a>, channel: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::viewers_dirty(env, reader, channel))
}

#[rustler::nif]
fn perm_filter_viewers<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    channel: Term<'a>,
    users: Term<'a>,
) -> Term<'a> {
    guard(env, || {
        dirty::run(
            env,
            c"perm_filter_viewers",
            &[reader, channel, users],
            |on| perm_nif::nifs::filter_viewers(env, reader, channel, users, on),
        )
    })
}

#[rustler::nif]
fn perm_thread_permissions<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    parent: Term<'a>,
    private: Term<'a>,
    member: Term<'a>,
    timed_out: Term<'a>,
) -> Term<'a> {
    guard(env, || {
        dirty::run(
            env,
            c"perm_thread_permissions",
            &[reader, user, parent, private, member, timed_out],
            |on| {
                perm_nif::nifs::thread_permissions(
                    env,
                    reader,
                    user,
                    parent,
                    (private, member, timed_out),
                    on,
                )
            },
        )
    })
}

#[rustler::nif]
fn perm_query<'a>(env: Env<'a>, reader: Term<'a>, query: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::query(env, reader, query))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn perm_query_dirty<'a>(env: Env<'a>, reader: Term<'a>, query: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::query_dirty(env, reader, query))
}

#[rustler::nif]
fn perm_reply<'a>(env: Env<'a>, reader: Term<'a>, query: Term<'a>) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"perm_reply", &[reader, query], |on| {
            perm_nif::nifs::reply(env, reader, query, on)
        })
    })
}

#[rustler::nif]
fn perm_epoch<'a>(env: Env<'a>, reader: Term<'a>) -> NifResult<Term<'a>> {
    guard_raise(|| perm_nif::nifs::epoch(env, reader))
}

#[rustler::nif]
fn perm_revision<'a>(env: Env<'a>, reader: Term<'a>) -> NifResult<Term<'a>> {
    guard_raise(|| perm_nif::nifs::revision(env, reader))
}

#[rustler::nif]
fn perm_status<'a>(env: Env<'a>, perms: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::status(env, perms))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn perm_release<'a>(env: Env<'a>, perms: Term<'a>) -> Term<'a> {
    guard(env, || perm_nif::nifs::release(env, perms))
}

#[rustler::nif]
fn ml_new<'a>(
    env: Env<'a>,
    guild: Term<'a>,
    config: Term<'a>,
    owner: Term<'a>,
    perms: Term<'a>,
) -> Term<'a> {
    guard(env, || ml_nif::nifs::new(env, guild, config, owner, perms))
}

#[rustler::nif]
fn ml_load<'a>(env: Env<'a>, list: Term<'a>, snapshot: Term<'a>) -> Term<'a> {
    guard(env, || ml_nif::nifs::load(env, list, snapshot))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn ml_load_dirty<'a>(env: Env<'a>, list: Term<'a>, snapshot: Term<'a>) -> Term<'a> {
    guard(env, || ml_nif::nifs::load_dirty(env, list, snapshot))
}

#[rustler::nif]
fn ml_apply<'a>(env: Env<'a>, list: Term<'a>, events: Term<'a>, delta: Term<'a>) -> Term<'a> {
    guard(env, || ml_nif::nifs::apply(env, list, events, delta))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn ml_apply_dirty<'a>(env: Env<'a>, list: Term<'a>, events: Term<'a>, delta: Term<'a>) -> Term<'a> {
    guard(env, || ml_nif::nifs::apply_dirty(env, list, events, delta))
}

#[rustler::nif]
fn ml_fill<'a>(env: Env<'a>, list: Term<'a>, items: Term<'a>) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"ml_fill", &[list, items], |on| {
            ml_nif::nifs::fill(env, list, items, on)
        })
    })
}

#[rustler::nif]
fn ml_resume<'a>(env: Env<'a>, list: Term<'a>) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"ml_resume", &[list], |on| {
            ml_nif::nifs::resume(env, list, on)
        })
    })
}

#[rustler::nif]
fn ml_query<'a>(env: Env<'a>, list: Term<'a>, query: Term<'a>) -> Term<'a> {
    guard(env, || ml_nif::nifs::query(env, list, query))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn ml_query_dirty<'a>(env: Env<'a>, list: Term<'a>, query: Term<'a>) -> Term<'a> {
    guard(env, || ml_nif::nifs::query_dirty(env, list, query))
}

#[rustler::nif]
fn ml_counts<'a>(env: Env<'a>, counts: Term<'a>) -> Term<'a> {
    guard(env, || ml_nif::nifs::counts(env, counts))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn ml_release<'a>(env: Env<'a>, list: Term<'a>) -> Term<'a> {
    guard(env, || ml_nif::nifs::release(env, list))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn push_masks_set<'a>(env: Env<'a>, list: Term<'a>, updates: Term<'a>) -> Term<'a> {
    guard(env, || ml_nif::nifs::masks_set(env, list, updates))
}

#[rustler::nif]
fn push_masks_query<'a>(env: Env<'a>, list: Term<'a>, query: Term<'a>) -> Term<'a> {
    guard(env, || ml_nif::nifs::masks_query(env, list, query))
}

#[rustler::nif]
fn zstd_compressor_new<'a>(
    env: Env<'a>,
    level: Term<'a>,
    window_log: Term<'a>,
    max_block_size: Term<'a>,
) -> Term<'a> {
    guard(env, || {
        zstd_stream::nifs::compressor_new(env, level, window_log, max_block_size)
    })
}

#[rustler::nif]
fn zstd_compress<'a>(env: Env<'a>, compressor: Term<'a>, data: Term<'a>) -> Term<'a> {
    guard(env, || zstd_stream::nifs::compress(env, compressor, data))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn zstd_compress_dirty<'a>(env: Env<'a>, compressor: Term<'a>, data: Term<'a>) -> Term<'a> {
    guard(env, || {
        zstd_stream::nifs::compress_dirty(env, compressor, data)
    })
}

#[rustler::nif]
fn zstd_decompressor_new<'a>(env: Env<'a>, window_log_max: Term<'a>) -> Term<'a> {
    guard(env, || {
        zstd_stream::nifs::decompressor_new(env, window_log_max)
    })
}

#[rustler::nif]
fn zstd_decompress<'a>(
    env: Env<'a>,
    decompressor: Term<'a>,
    data: Term<'a>,
    max_size: Term<'a>,
) -> Term<'a> {
    guard(env, || {
        dirty::run(
            env,
            c"zstd_decompress",
            &[decompressor, data, max_size],
            |on| zstd_stream::nifs::decompress(env, decompressor, data, max_size, on),
        )
    })
}

#[rustler::nif(schedule = "DirtyCpu")]
fn md_render_push_preview<'a>(env: Env<'a>, content: Term<'a>, context: Term<'a>) -> Term<'a> {
    guard(env, || {
        md_plaintext::nifs::render_push_preview(env, content, context)
    })
}

#[rustler::nif]
fn push_job_body<'a>(env: Env<'a>, head: Term<'a>, users: Term<'a>, tail: Term<'a>) -> Term<'a> {
    guard(env, || push_core::nifs::job_body(env, head, users, tail))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn push_job_body_dirty<'a>(
    env: Env<'a>,
    head: Term<'a>,
    users: Term<'a>,
    tail: Term<'a>,
) -> Term<'a> {
    guard(env, || {
        push_core::nifs::job_body_dirty(env, head, users, tail)
    })
}

#[rustler::nif]
fn json_scan_object<'a>(env: Env<'a>, data: Term<'a>) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"json_scan_object", &[data], |on| {
            json_scan::nifs::object(env, data, on)
        })
    })
}

#[rustler::nif(schedule = "DirtyCpu")]
fn json_scan_object_dirty<'a>(env: Env<'a>, data: Term<'a>) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"json_scan_object_dirty", &[data], |on| {
            json_scan::nifs::object(env, data, on)
        })
    })
}

#[rustler::nif]
fn json_scan_array<'a>(env: Env<'a>, data: Term<'a>) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"json_scan_array", &[data], |on| {
            json_scan::nifs::array(env, data, on)
        })
    })
}

#[rustler::nif(schedule = "DirtyCpu")]
fn json_scan_array_dirty<'a>(env: Env<'a>, data: Term<'a>) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"json_scan_array_dirty", &[data], |on| {
            json_scan::nifs::array(env, data, on)
        })
    })
}

#[rustler::nif]
fn json_scan_locate<'a>(env: Env<'a>, data: Term<'a>, key: Term<'a>) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"json_scan_locate", &[data, key], |on| {
            json_scan::nifs::locate(env, data, key, on)
        })
    })
}

#[rustler::nif(schedule = "DirtyCpu")]
fn json_scan_locate_dirty<'a>(env: Env<'a>, data: Term<'a>, key: Term<'a>) -> Term<'a> {
    guard(env, || {
        dirty::run(env, c"json_scan_locate_dirty", &[data, key], |on| {
            json_scan::nifs::locate(env, data, key, on)
        })
    })
}

#[rustler::nif]
fn gateway_nif_metrics<'a>(env: Env<'a>) -> Term<'a> {
    guard(env, || guild_rt::metrics().encode(env))
}

#[rustler::nif]
fn gateway_nif_exports<'a>(env: Env<'a>) -> Term<'a> {
    guard(env, || exports().encode(env))
}

fn exports() -> Vec<(String, u32)> {
    rustler::codegen_runtime::inventory::iter::<rustler::Nif>()
        .map(|nif| {
            let name = unsafe { CStr::from_ptr(nif.name) };
            (name.to_string_lossy().into_owned(), nif.arity)
        })
        .collect()
}

fn load(env: Env, _info: Term) -> bool {
    guild_rt::start() && perm_nif::link(env) && ml_nif::link(env) && zstd_stream::link(env)
}

rustler::init!("fluxer_gateway@nif", load = load);

#[cfg(test)]
mod tests {
    use std::ffi::CString;
    use std::path::{Path, PathBuf};

    use super::{dirty, exports};

    fn sources(dir: &Path, out: &mut Vec<PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let name = entry.file_name();
            if path.is_dir() && name != "target" && name != "fuzz" {
                sources(&path, out);
            } else if path.extension().is_some_and(|ext| ext == "rs") && name != "test_link.rs" {
                out.push(path);
            }
        }
    }

    fn rescheduled(source: &str) -> Vec<(String, usize)> {
        source
            .split("dirty::run(")
            .skip(1)
            .filter_map(|call| {
                let name = call.split("c\"").nth(1)?.split('"').next()?;
                let args = call.split("&[").nth(1)?.split(']').next()?;
                Some((
                    name.to_owned(),
                    args.split(',').filter(|a| !a.trim().is_empty()).count(),
                ))
            })
            .collect()
    }

    #[test]
    fn every_rescheduled_nif_resolves_to_itself() {
        let found = rescheduled(include_str!("lib.rs"));
        assert!(found.len() >= 19, "{found:?}");
        for (name, arity) in found {
            let wanted = (name.clone(), u32::try_from(arity).unwrap_or(u32::MAX));
            assert!(exports().contains(&wanted), "{name}/{arity}");
            let cname = CString::new(name.clone()).unwrap_or_default();
            assert!(dirty::raw(&cname, arity).is_some(), "{name}/{arity}");
        }
    }

    #[test]
    fn only_guarded_payloads_become_resource_binaries() {
        let crates = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
        let mut files = Vec::new();
        sources(&crates, &mut files);
        assert!(files.len() > 50);
        let needles = [
            ["make_", "binary("].concat(),
            ["enif_make_", "resource_binary"].concat(),
            ["make_", "resource_binary"].concat(),
        ];
        let users: Vec<(PathBuf, usize, String)> = files
            .into_iter()
            .filter_map(|file| {
                let code = std::fs::read_to_string(&file).unwrap_or_default();
                let hits = needles
                    .iter()
                    .map(|needle| code.matches(needle.as_str()).count())
                    .sum::<usize>();
                (hits > 0).then_some((file, hits, code))
            })
            .collect();
        assert_eq!(
            users.len(),
            1,
            "{:?}",
            users.iter().map(|u| &u.0).collect::<Vec<_>>()
        );
        let (file, hits, code) = users.first().cloned().unwrap_or_default();
        assert!(file.ends_with("ml_nif/src/encode.rs"), "{file:?}");
        assert_eq!(hits, 1);
        assert!(code.contains(&["make_", "binary(env, PayloadRes::vm_bytes)"].concat()));
    }

    #[test]
    fn exports_list_every_nif_once() {
        let mut names: Vec<(String, u32)> = exports();
        let total = names.len();
        names.sort();
        names.dedup();
        assert_eq!(names.len(), total);
        assert!(names.contains(&("gateway_nif_exports".to_string(), 0)));
        assert!(names.contains(&("perm_thread_permissions".to_string(), 6)));
        assert!(names.contains(&("zstd_decompress".to_string(), 3)));
        assert!(names.contains(&("json_scan_locate_dirty".to_string(), 2)));
        assert!(names.contains(&("push_job_body_dirty".to_string(), 3)));
    }
}
