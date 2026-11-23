// SPDX-License-Identifier: AGPL-3.0-or-later

use rustler::types::binary::{Binary, OwnedBinary};
use rustler::{Encoder, Env, Term};

use crate::PreviewContext;

mod atoms {
    rustler::atoms! {
        ok,
        error,
        bad_input,
    }
}

type Names = Vec<(u64, String)>;

fn bad_input<'a>(env: Env<'a>, reason: &str) -> Term<'a> {
    (atoms::error(), (atoms::bad_input(), reason)).encode(env)
}

fn decode_context(term: Term<'_>) -> Option<PreviewContext> {
    let (
        preserve_markdown,
        hide_spoilers,
        include_emoji_names,
        include_link_urls,
        users,
        roles,
        channels,
    ) = term
        .decode::<(bool, bool, bool, bool, Names, Names, Names)>()
        .ok()?;
    Some(PreviewContext {
        preserve_markdown,
        hide_spoilers,
        include_emoji_names,
        include_link_urls,
        users,
        roles,
        channels,
    })
}

pub fn render_push_preview<'a>(env: Env<'a>, content: Term<'a>, context: Term<'a>) -> Term<'a> {
    guild_rt::dirty_call();
    let Ok(content) = content.decode::<Binary>() else {
        return bad_input(env, "content");
    };
    let Some(context) = decode_context(context) else {
        return bad_input(env, "context");
    };
    match crate::render_push_preview(content.as_slice(), context) {
        Ok(rendered) => {
            let mut out = OwnedBinary::new(rendered.len());
            out.as_mut_slice().copy_from_slice(rendered.as_bytes());
            (atoms::ok(), out.release(env)).encode(env)
        }
        Err(error) => bad_input(env, error.reason()),
    }
}
