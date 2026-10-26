// SPDX-License-Identifier: AGPL-3.0-or-later

use std::time::{Duration, Instant};

use fluxer_markdown_parser::{EmojiContext, MarkdownParser, ParserFlags};

#[test]
fn unclosed_masked_links_parse_in_linear_time() {
    let line = "[a](".repeat(1024);
    let input = format!("{line}\n").repeat(400);
    let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::default());
    let started = Instant::now();
    let nodes = parser.parse(&input).expect("parse succeeds");
    let elapsed = started.elapsed();
    assert_eq!(nodes.len(), 1);
    assert!(
        elapsed < Duration::from_secs(20),
        "parsing {} bytes took {elapsed:?}",
        input.len()
    );
}
