// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;
use std::hint::black_box;
use std::time::Instant;

use criterion::Criterion;
use fluxer_markdown_parser::ParserFlags;
use fluxer_markdown_parser::plaintext::{
    Names, PlaintextOptions, canonical_id, parse_and_render_plaintext,
};
use md_plaintext::{PreviewContext, render_push_preview};

const REGRESSION_MAX: f64 = 1.05;

const CONTEXT_SHARE_MAX: f64 = 0.5;

fn content_2k() -> String {
    let parts = [
        "hey <@1400000000000000001> did you see the **new build**? ",
        "it fixes the thing in <#1400000000000000100> :smile: ",
        "```rust\nfn main() {\n    println!(\"hello\");\n}\n```\n",
        "ping <@&1400000000000000200> and <@1400000000000000002> ",
        "> quoted *text* with ~~strike~~ and `inline code`\n",
        "see https://example.com/some/path?q=1 and [docs](https://example.com/docs) ",
        "||spoiler|| <:custom:1400000000000000300> \u{1F600} ",
        "- one\n- two\n- three\n",
    ];
    let mut out = String::with_capacity(2_100);
    let mut i = 0usize;
    while out.chars().count() < 2_000 {
        out.push_str(parts[i % parts.len()]);
        i += 1;
    }
    out.chars().take(2_000).collect()
}

fn context_json() -> String {
    String::from(
        r#"{"preserve_markdown":false,"include_emoji_names":true,"include_link_urls":false,
        "users":{"1400000000000000001":"alice","1400000000000000002":"bob"},
        "roles":{"1400000000000000200":"mods"},
        "channels":{"1400000000000000100":"general"}}"#,
    )
}

fn native_context() -> PreviewContext {
    PreviewContext {
        preserve_markdown: false,
        hide_spoilers: false,
        include_emoji_names: true,
        include_link_urls: false,
        users: vec![
            (1_400_000_000_000_000_001, "alice".to_owned()),
            (1_400_000_000_000_000_002, "bob".to_owned()),
        ],
        roles: vec![(1_400_000_000_000_000_200, "mods".to_owned())],
        channels: vec![(1_400_000_000_000_000_100, "general".to_owned())],
    }
}

#[derive(serde::Deserialize)]
struct JsonContext {
    #[serde(default)]
    preserve_markdown: bool,
    #[serde(default)]
    hide_spoilers: bool,
    #[serde(default = "yes")]
    include_emoji_names: bool,
    #[serde(default)]
    include_link_urls: bool,
    #[serde(default)]
    users: HashMap<String, String>,
    #[serde(default)]
    roles: HashMap<String, String>,
    #[serde(default)]
    channels: HashMap<String, String>,
}

fn yes() -> bool {
    true
}

fn json_options(context: &[u8]) -> Option<PlaintextOptions> {
    let context: JsonContext = serde_json::from_slice(context).ok()?;
    Some(PlaintextOptions {
        preserve_markdown: context.preserve_markdown,
        hide_spoilers: context.hide_spoilers,
        include_emoji_names: context.include_emoji_names,
        include_link_urls: context.include_link_urls,
        users: names(context.users),
        roles: names(context.roles),
        channels: names(context.channels),
    })
}

fn names(keyed: HashMap<String, String>) -> Names {
    keyed
        .into_iter()
        .filter_map(|(id, name)| Some((canonical_id(&id)?, name)))
        .collect()
}

fn same_options(a: &PlaintextOptions, b: &PlaintextOptions) -> bool {
    a.preserve_markdown == b.preserve_markdown
        && a.hide_spoilers == b.hide_spoilers
        && a.include_emoji_names == b.include_emoji_names
        && a.include_link_urls == b.include_link_urls
        && a.users == b.users
        && a.roles == b.roles
        && a.channels == b.channels
}

fn original_path(content: &[u8], context: &[u8]) -> Option<String> {
    let content = std::str::from_utf8(content).ok()?;
    let options = json_options(context)?;
    parse_and_render_plaintext(content, ParserFlags::ALL, "", &options).ok()
}

fn median_ns(runs: usize, mut f: impl FnMut()) -> f64 {
    let mut samples: Vec<f64> = (0..runs)
        .map(|_| {
            let start = Instant::now();
            f();
            start.elapsed().as_nanos() as f64
        })
        .collect();
    samples.sort_by(f64::total_cmp);
    samples.get(samples.len() / 2).copied().unwrap_or(f64::NAN)
}

fn main() {
    let content = content_2k();
    let context = context_json();
    let native = native_context();
    let mut c = Criterion::default().configure_from_args();
    let mut group = c.benchmark_group("md_preview_2k");
    group.bench_function("md_plaintext", |b| {
        b.iter(|| render_push_preview(black_box(content.as_bytes()), black_box(native.clone())))
    });
    group.bench_function("original_path", |b| {
        b.iter(|| original_path(black_box(content.as_bytes()), black_box(context.as_bytes())))
    });
    group.finish();
    let mut group = c.benchmark_group("md_context_native");
    group.bench_function("native_terms", |b| {
        b.iter(|| black_box(native.clone()).into_options())
    });
    group.bench_function("parsed_json", |b| {
        b.iter(|| json_options(black_box(context.as_bytes())))
    });
    group.finish();
    c.final_summary();

    let same = render_push_preview(content.as_bytes(), native.clone()).ok()
        == original_path(content.as_bytes(), context.as_bytes());
    let ours = median_ns(2_001, || {
        black_box(render_push_preview(
            black_box(content.as_bytes()),
            black_box(native.clone()),
        ))
        .ok();
    });
    let original = median_ns(2_001, || {
        black_box(original_path(
            black_box(content.as_bytes()),
            black_box(context.as_bytes()),
        ));
    });
    let ratio = ours / original;
    let preview_ok = same && ratio <= REGRESSION_MAX;

    let same_context = json_options(context.as_bytes())
        .is_some_and(|options| same_options(&options, &native.clone().into_options()));
    let native_ns = median_ns(20_001, || {
        black_box(black_box(native.clone()).into_options());
    });
    let json_ns = median_ns(20_001, || {
        black_box(json_options(black_box(context.as_bytes())));
    });
    let share = native_ns / json_ns;
    let context_ok = same_context && share <= CONTEXT_SHARE_MAX;

    println!("\ntarget check (median against the original path):");
    println!(
        "  {} md_preview_2k: {:.3} us against {:.3} us, ratio {ratio:.3}, target {REGRESSION_MAX:.2}, same output {same}",
        if preview_ok { "met   " } else { "MISSED" },
        ours / 1e3,
        original / 1e3,
    );
    println!(
        "  {} md_context_native: {:.0} ns against {:.0} ns for the JSON context, share {share:.3}, target {CONTEXT_SHARE_MAX:.2}, same options {same_context}",
        if context_ok { "met   " } else { "MISSED" },
        native_ns,
        json_ns,
    );
    if !(preview_ok && context_ok) {
        std::process::exit(1);
    }
}
