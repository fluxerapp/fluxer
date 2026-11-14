// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;

use fluxer_markdown_parser::ParserFlags;
use fluxer_markdown_parser::plaintext::{PlaintextOptions, parse_and_render_plaintext};
use rustler::types::binary::{Binary, OwnedBinary};
use rustler::{Env, Error, NifResult, Term};
use serde::Deserialize;

const MAX_CONTENT_BYTES: usize = 16 * 1024;
const MAX_CONTEXT_JSON_BYTES: usize = 64 * 1024;
const MAX_CONTEXT_NAME_ENTRIES: usize = 512;
const MAX_RENDERED_BYTES: usize = 64 * 1024;

#[derive(Debug, Default, Deserialize)]
struct PushPlaintextContext {
    #[serde(default)]
    preserve_markdown: bool,
    #[serde(default = "default_include_emoji_names")]
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

fn default_include_emoji_names() -> bool {
    true
}

impl From<PushPlaintextContext> for PlaintextOptions {
    fn from(value: PushPlaintextContext) -> Self {
        Self {
            preserve_markdown: value.preserve_markdown,
            include_emoji_names: value.include_emoji_names,
            include_link_urls: value.include_link_urls,
            users: value.users,
            roles: value.roles,
            channels: value.channels,
        }
    }
}

impl PushPlaintextContext {
    fn validate(&self) -> NifResult<()> {
        if self.users.len() > MAX_CONTEXT_NAME_ENTRIES
            || self.roles.len() > MAX_CONTEXT_NAME_ENTRIES
            || self.channels.len() > MAX_CONTEXT_NAME_ENTRIES
        {
            return Err(Error::BadArg);
        }
        Ok(())
    }
}

#[rustler::nif(schedule = "DirtyCpu")]
fn render_push_preview_nif<'a>(
    env: Env<'a>,
    content: Binary<'a>,
    context_json: Binary<'a>,
) -> NifResult<Term<'a>> {
    if content.as_slice().len() > MAX_CONTENT_BYTES
        || context_json.as_slice().len() > MAX_CONTEXT_JSON_BYTES
    {
        return Err(Error::BadArg);
    }
    let content = std::str::from_utf8(content.as_slice()).map_err(|_| Error::BadArg)?;
    let context = serde_json::from_slice::<PushPlaintextContext>(context_json.as_slice())
        .map_err(|_| Error::BadArg)?;
    context.validate()?;
    let options = PlaintextOptions::from(context);
    let rendered = parse_and_render_plaintext(content, ParserFlags::ALL, "", &options)
        .map_err(|_| Error::BadArg)?;
    if rendered.len() > MAX_RENDERED_BYTES {
        return Err(Error::BadArg);
    }
    binary_term(env, rendered.as_bytes())
}

fn binary_term<'a>(env: Env<'a>, bytes: &[u8]) -> NifResult<Term<'a>> {
    Ok(OwnedBinary::from_slice(bytes).release(env).to_term(env))
}

rustler::init!("push_markdown_plaintext_nif");
