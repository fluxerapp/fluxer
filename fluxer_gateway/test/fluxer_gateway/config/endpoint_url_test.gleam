// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/endpoint_url
import gleam/option.{None, Some}

fn normalize(url: String) -> String {
  endpoint_url.normalize(
    url,
    base_domain: Some("Example.com"),
    public_port: Some(8443),
  )
}

pub fn inserts_port_for_the_base_domain_test() {
  assert normalize("https://example.com/media")
    == "https://example.com:8443/media"
  assert normalize("wss://user@example.com?x=1")
    == "wss://user@example.com:8443?x=1"
}

pub fn keeps_other_urls_test() {
  assert normalize("https://other.com/media") == "https://other.com/media"
  assert normalize("https://example.com:9000/media")
    == "https://example.com:9000/media"
  assert normalize("ftp://example.com/media") == "ftp://example.com/media"
  assert normalize("https:///media") == "https:///media"
  assert normalize("example.com/media") == "example.com/media"
}

pub fn default_port_is_not_written_test() {
  assert endpoint_url.normalize(
      "https://example.com",
      base_domain: Some("example.com"),
      public_port: Some(443),
    )
    == "https://example.com"
}

pub fn needs_domain_and_positive_port_test() {
  assert endpoint_url.normalize(
      "https://example.com",
      base_domain: None,
      public_port: Some(8443),
    )
    == "https://example.com"
  assert endpoint_url.normalize(
      "https://example.com",
      base_domain: Some("example.com"),
      public_port: Some(0),
    )
    == "https://example.com"
}

pub fn leading_int_test() {
  assert endpoint_url.leading_int("8443") == Ok(8443)
  assert endpoint_url.leading_int("8443abc") == Ok(8443)
  assert endpoint_url.leading_int("-5") == Ok(-5)
  assert endpoint_url.leading_int("abc") == Error(Nil)
  assert endpoint_url.leading_int(" 80") == Error(Nil)
}
