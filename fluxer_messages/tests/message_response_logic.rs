// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_messages::mention_extractor::extract_mentions_from_markdown;
use fluxer_messages::types::ExtractedMentionsResponse;

#[test]
fn markdown_extractor_keeps_link_text_mentions_but_not_url_mentions() {
    let mentions =
        extract_mentions_from_markdown(Some("[<@101>](https://example.com/<@202>) <#303>"));
    assert!(mentions.users.contains(&101));
    assert!(!mentions.users.contains(&202));
    assert!(mentions.channels.contains(&303));
}

#[test]
fn extracted_mentions_response_serializes_ids_as_strings() {
    let mentions = extract_mentions_from_markdown(Some("<@123456789012345678> <@&999> <#888>"));
    let response = ExtractedMentionsResponse::from(mentions);
    assert!(response.users.contains(&"123456789012345678".to_owned()));
    assert!(response.roles.contains(&"999".to_owned()));
    assert!(response.channels.contains(&"888".to_owned()));

    let json = serde_json::to_string(&response).unwrap();
    assert!(json.contains("\"123456789012345678\""));
    assert!(!json.contains("123456789012345678,"));
}

#[test]
fn everyone_and_here_survive_response_conversion() {
    let mentions = extract_mentions_from_markdown(Some("@everyone @here <@1>"));
    assert!(mentions.everyone);
    assert!(mentions.here);
    let response = ExtractedMentionsResponse::from(mentions);
    assert!(response.everyone);
    assert!(response.here);
}
