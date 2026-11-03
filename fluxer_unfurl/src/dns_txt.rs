// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::public_ip::is_valid_public_hostname;
use hickory_resolver::TokioResolver;
use hickory_resolver::config::{NameServerConfig, ResolveHosts, ResolverConfig, ResolverOpts};
use hickory_resolver::net::runtime::TokioRuntimeProvider;
use hickory_resolver::proto::rr::{RData, RecordType};
use std::net::{IpAddr, Ipv4Addr};
use std::time::Duration;
use tokio::task::JoinSet;
use tokio::time::timeout;

const DNS_TIMEOUT: Duration = Duration::from_millis(1500);
const DNS_SERVERS: [Ipv4Addr; 8] = [
    Ipv4Addr::new(1, 1, 1, 1),
    Ipv4Addr::new(1, 0, 0, 1),
    Ipv4Addr::new(8, 8, 8, 8),
    Ipv4Addr::new(8, 8, 4, 4),
    Ipv4Addr::new(9, 9, 9, 9),
    Ipv4Addr::new(149, 112, 112, 112),
    Ipv4Addr::new(208, 67, 222, 222),
    Ipv4Addr::new(208, 67, 220, 220),
];

pub fn validate_record_name(name: &str) -> anyhow::Result<()> {
    let host = match name.split_once('.') {
        Some((label, rest)) if label.len() > 1 && label.starts_with('_') => {
            if !label[1..]
                .bytes()
                .all(|b| b.is_ascii_lowercase() || b.is_ascii_digit() || b == b'-')
            {
                anyhow::bail!("invalid record name");
            }
            rest
        }
        _ => name,
    };
    validate_hostname(host)
}

pub fn validate_hostname(host: &str) -> anyhow::Result<()> {
    if !is_valid_public_hostname(host) || host.bytes().any(|b| b.is_ascii_uppercase()) {
        anyhow::bail!("invalid hostname");
    }
    Ok(())
}

pub async fn first_answer<F>(name: &str, accept: F) -> Option<Vec<String>>
where
    F: Fn(&[String]) -> bool,
{
    let record_name = format!("{name}.");
    let mut lookups = JoinSet::new();
    for server in DNS_SERVERS {
        let record_name = record_name.clone();
        lookups.spawn(async move { lookup_txt(IpAddr::V4(server), &record_name).await });
    }
    while let Some(result) = lookups.join_next().await {
        match result {
            Ok(Ok(records)) if accept(&records) => {
                lookups.abort_all();
                return Some(records);
            }
            Ok(Ok(_)) => {}
            Ok(Err(err)) => {
                tracing::debug!(error = %err, name, "DNS TXT lookup failed");
            }
            Err(err) => {
                tracing::debug!(error = %err, name, "DNS TXT lookup task failed");
            }
        }
    }
    None
}

async fn lookup_txt(server: IpAddr, record_name: &str) -> anyhow::Result<Vec<String>> {
    let mut options = ResolverOpts::default();
    options.timeout = DNS_TIMEOUT;
    options.cache_size = 0;
    options.use_hosts_file = ResolveHosts::Never;
    let config = ResolverConfig::from_name_servers(vec![NameServerConfig::udp_and_tcp(server)]);
    let resolver: TokioResolver =
        TokioResolver::builder_with_config(config, TokioRuntimeProvider::default())
            .with_options(options)
            .build()?;
    let lookup = timeout(DNS_TIMEOUT, resolver.lookup(record_name, RecordType::TXT))
        .await
        .map_err(|_| anyhow::anyhow!("DNS lookup timed out"))??;
    Ok(lookup
        .answers()
        .iter()
        .filter_map(|record| match &record.data {
            RData::TXT(txt) => Some(join_txt_strings(&txt.txt_data)),
            _ => None,
        })
        .collect())
}

fn join_txt_strings(strings: &[Box<[u8]>]) -> String {
    let joined: Vec<u8> = strings.iter().flat_map(|s| s.iter().copied()).collect();
    String::from_utf8_lossy(&joined).into_owned()
}
