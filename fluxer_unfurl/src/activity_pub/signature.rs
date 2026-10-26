// SPDX-License-Identifier: AGPL-3.0-or-later

use base64::prelude::*;
use chrono::{DateTime, Utc};
use reqwest::header::{DATE, HOST, HeaderMap, HeaderName, HeaderValue};
use ring::rand::SystemRandom;
use ring::signature::{RSA_PKCS1_SHA256, RsaKeyPair};
use std::sync::Arc;
use std::time::SystemTime;
use url::Url;

const SIGNATURE: HeaderName = HeaderName::from_static("signature");

pub fn parse_signing_key(encoded: &str) -> anyhow::Result<RsaKeyPair> {
    let compact: String = encoded
        .chars()
        .filter(|c| !c.is_ascii_whitespace())
        .collect();
    let der = BASE64_STANDARD.decode(compact)?;
    RsaKeyPair::from_pkcs8(&der)
        .or_else(|_| RsaKeyPair::from_der(&der))
        .map_err(|err| anyhow::anyhow!("invalid RSA private key: {err}"))
}

pub fn public_key_base64(key: &RsaKeyPair) -> String {
    BASE64_STANDARD.encode(key.public().as_ref())
}

#[derive(Clone)]
pub struct RequestSigner {
    key: Arc<RsaKeyPair>,
    key_id: String,
}

impl RequestSigner {
    pub fn new(key: Arc<RsaKeyPair>, key_id: String) -> Self {
        Self { key, key_id }
    }

    pub fn headers(&self, url: &Url, at: SystemTime) -> anyhow::Result<HeaderMap> {
        let host_name = url
            .host_str()
            .ok_or_else(|| anyhow::anyhow!("signed request URL has no host"))?;
        let host = match url.port() {
            Some(port) => format!("{host_name}:{port}"),
            None => host_name.to_owned(),
        };
        let date = DateTime::<Utc>::from(at)
            .format("%a, %d %b %Y %H:%M:%S GMT")
            .to_string();
        let target = match url.query() {
            Some(query) => format!("{}?{query}", url.path()),
            None => url.path().to_owned(),
        };
        let signing_string = format!("(request-target): get {target}\nhost: {host}\ndate: {date}");
        let mut signature = vec![0; self.key.public().modulus_len()];
        self.key
            .sign(
                &RSA_PKCS1_SHA256,
                &SystemRandom::new(),
                signing_string.as_bytes(),
                &mut signature,
            )
            .map_err(|_| anyhow::anyhow!("failed to sign ActivityPub request"))?;
        let signature_header = format!(
            "keyId=\"{}\",algorithm=\"rsa-sha256\",headers=\"(request-target) host date\",signature=\"{}\"",
            self.key_id,
            BASE64_STANDARD.encode(signature)
        );

        let mut headers = HeaderMap::new();
        headers.insert(HOST, HeaderValue::from_str(&host)?);
        headers.insert(DATE, HeaderValue::from_str(&date)?);
        headers.insert(SIGNATURE, HeaderValue::from_str(&signature_header)?);
        Ok(headers)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::http_fetch::{DEFAULT_MAX_BYTES, fetch_url_with_headers};
    use serde::Deserialize;
    use std::io::{BufRead, BufReader, Write};
    use std::net::TcpListener;
    use std::time::{Duration, UNIX_EPOCH};

    #[derive(Deserialize)]
    struct Vectors {
        key_id: String,
        private_key_pkcs8_base64: String,
        private_key_pkcs1_base64: String,
        public_key_pkcs1_base64: String,
        vectors: Vec<Vector>,
    }

    #[derive(Deserialize)]
    struct Vector {
        name: String,
        url: String,
        unix_seconds: u64,
        host: String,
        date: String,
        signature: String,
    }

    #[test]
    fn signs_known_answer_vectors_with_pkcs8_and_pkcs1_keys() {
        let raw = include_str!("testdata/http_signature_vectors.json");
        let vectors: Vectors = serde_json::from_str(raw).expect("valid vector file");
        for encoded in [
            &vectors.private_key_pkcs8_base64,
            &vectors.private_key_pkcs1_base64,
        ] {
            let key = parse_signing_key(encoded).expect("test key parses");
            assert_eq!(public_key_base64(&key), vectors.public_key_pkcs1_base64);
            let signer = RequestSigner::new(Arc::new(key), vectors.key_id.clone());
            for vector in &vectors.vectors {
                let url = Url::parse(&vector.url).unwrap();
                let at = UNIX_EPOCH + Duration::from_secs(vector.unix_seconds);
                let headers = signer.headers(&url, at).unwrap();
                assert_eq!(headers[HOST], vector.host.as_str(), "{}", vector.name);
                assert_eq!(headers[DATE], vector.date.as_str(), "{}", vector.name);
                assert_eq!(
                    headers[SIGNATURE],
                    vector.signature.as_str(),
                    "{}",
                    vector.name
                );
            }
        }
    }

    #[tokio::test]
    async fn signs_every_redirect_hop_for_its_own_target_and_host() {
        let raw = include_str!("testdata/http_signature_vectors.json");
        let vectors: Vectors = serde_json::from_str(raw).expect("valid vector file");
        let key = parse_signing_key(&vectors.private_key_pkcs8_base64).expect("test key parses");
        let signer = RequestSigner::new(Arc::new(key), vectors.key_id.clone());
        let at = UNIX_EPOCH + Duration::from_secs(1_790_000_000);

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let proxy = format!("http://{}", listener.local_addr().unwrap());
        let server = std::thread::spawn(move || {
            let mut hops = Vec::new();
            for response in [
                "HTTP/1.1 301 Moved Permanently\r\nLocation: http://8.8.8.8:8080/users/alice/statuses/1\r\nContent-Length: 0\r\nConnection: close\r\n\r\n",
                "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}",
            ] {
                let (mut stream, _) = listener.accept().unwrap();
                let mut reader = BufReader::new(stream.try_clone().unwrap());
                let mut request = Vec::new();
                loop {
                    let mut line = String::new();
                    reader.read_line(&mut line).unwrap();
                    if line.trim_end().is_empty() {
                        break;
                    }
                    request.push(line.trim_end().to_owned());
                }
                stream.write_all(response.as_bytes()).unwrap();
                hops.push(request);
            }
            hops
        });

        let client = reqwest::Client::builder()
            .redirect(reqwest::redirect::Policy::none())
            .proxy(reqwest::Proxy::http(proxy).unwrap())
            .build()
            .unwrap();
        let result = fetch_url_with_headers(
            &client,
            "http://1.1.1.1/@alice/1?x=1",
            |url| signer.headers(url, at),
            DEFAULT_MAX_BYTES,
            Duration::from_secs(5),
        )
        .await
        .unwrap();
        assert_eq!(result.status, 200);

        let hops = server.join().unwrap();
        for (request, url) in hops.iter().zip([
            "http://1.1.1.1/@alice/1?x=1",
            "http://8.8.8.8:8080/users/alice/statuses/1",
        ]) {
            let expected = signer.headers(&Url::parse(url).unwrap(), at).unwrap();
            let header = |name: &str| {
                request
                    .iter()
                    .find_map(|line| {
                        let (key, value) = line.split_once(':')?;
                        key.eq_ignore_ascii_case(name)
                            .then(|| value.trim().to_owned())
                    })
                    .unwrap_or_default()
            };
            assert_eq!(request[0], format!("GET {url} HTTP/1.1"));
            assert_eq!(header("host"), expected[HOST].to_str().unwrap(), "{url}");
            assert_eq!(
                header("signature"),
                expected[SIGNATURE].to_str().unwrap(),
                "{url}"
            );
        }
        assert_ne!(hops[0], hops[1]);
    }
}
