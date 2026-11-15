// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context as _, bail};
use fluxer_common::config::EnvMap;
use std::collections::BTreeMap;

pub const SERVICES_VAR: &str = "FLUXER_SERVER_SERVICES";
const PREFIX: &str = "FLUXER_SERVER_";
const SEPARATOR: &str = "__";
const FILE_SUFFIX: &str = "_FILE";

pub struct Source {
    common: BTreeMap<String, String>,
    overrides: BTreeMap<&'static str, BTreeMap<String, String>>,
    services: Option<String>,
}

impl Source {
    pub fn from_process(known: &[&'static str]) -> anyhow::Result<Self> {
        Self::new(
            std::env::vars_os().filter_map(|(key, value)| {
                Some((key.into_string().ok()?, value.into_string().ok()?))
            }),
            known,
        )
    }

    pub fn new<I>(vars: I, known: &[&'static str]) -> anyhow::Result<Self>
    where
        I: IntoIterator<Item = (String, String)>,
    {
        let mut source = Self {
            common: BTreeMap::new(),
            overrides: BTreeMap::new(),
            services: None,
        };
        for (key, value) in vars {
            let Some(rest) = key.strip_prefix(PREFIX) else {
                source.common.insert(key, value);
                continue;
            };
            if key == SERVICES_VAR {
                source.services = Some(value);
                continue;
            }
            let Some((svc, setting)) = rest.split_once(SEPARATOR) else {
                bail!(
                    "{key} is not a setting fluxer-server knows, overrides look like {PREFIX}<SERVICE>{SEPARATOR}<KEY>"
                );
            };
            let Some(svc) = known.iter().copied().find(|name| env_name(name) == svc) else {
                bail!(
                    "{key} refers to the service {svc}, which fluxer-server does not run. Known services: {}",
                    known.join(", ")
                );
            };
            if setting.is_empty() {
                bail!("{key} has no setting after {SEPARATOR}");
            }
            source
                .overrides
                .entry(svc)
                .or_default()
                .insert(setting.to_owned(), value);
        }
        Ok(source)
    }

    pub fn services(&self, known: &[&'static str]) -> anyhow::Result<Vec<&'static str>> {
        let Some(list) = self
            .services
            .as_deref()
            .filter(|list| !list.trim().is_empty())
        else {
            return Ok(known.to_vec());
        };
        let mut picked = Vec::new();
        for name in list
            .split(',')
            .map(str::trim)
            .filter(|name| !name.is_empty())
        {
            let Some(svc) = known.iter().copied().find(|known| *known == name) else {
                bail!(
                    "{SERVICES_VAR} lists {name}, which fluxer-server does not run. Known services: {}",
                    known.join(", ")
                );
            };
            if !picked.contains(&svc) {
                picked.push(svc);
            }
        }
        Ok(known
            .iter()
            .copied()
            .filter(|svc| picked.contains(svc))
            .collect())
    }

    pub fn process(&self) -> EnvMap {
        EnvMap::from_pairs(
            self.common
                .iter()
                .filter(|(_, value)| !value.is_empty())
                .map(|(key, value)| (key.clone(), value.clone())),
        )
    }

    pub fn env(&self, svc: &str) -> anyhow::Result<EnvMap> {
        let mut env = self.common.clone();
        if let Some(own) = self.overrides.get(svc) {
            for key in own.keys() {
                env.remove(&sibling(key));
            }
            for (key, value) in own {
                if value.is_empty() {
                    env.remove(key);
                } else {
                    env.insert(key.clone(), value.clone());
                }
            }
        }
        env.retain(|_, value| !value.is_empty());
        EnvMap::resolved(env).with_context(|| format!("{svc} settings"))
    }
}

fn env_name(svc: &str) -> String {
    svc.to_ascii_uppercase().replace('-', "_")
}

fn sibling(key: &str) -> String {
    match key.strip_suffix(FILE_SUFFIX) {
        Some(base) => base.to_owned(),
        None => format!("{key}{FILE_SUFFIX}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    const KNOWN: &[&str] = &["snowflakes", "push", "app-proxy"];

    fn source(vars: &[(&str, &str)]) -> anyhow::Result<Source> {
        Source::new(
            vars.iter().map(|(k, v)| ((*k).to_owned(), (*v).to_owned())),
            KNOWN,
        )
    }

    fn map(env: EnvMap) -> BTreeMap<String, String> {
        env.iter()
            .map(|(k, v)| (k.to_owned(), v.to_owned()))
            .collect()
    }

    #[test]
    fn service_maps_follow_the_override_rules() {
        let dir = std::env::temp_dir().join(format!("fluxer-server-env-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let common_secret = dir.join("common");
        let push_secret = dir.join("push");
        std::fs::write(&common_secret, "from-common-file\n").unwrap();
        std::fs::write(&push_secret, "from-push-file\n").unwrap();
        let common_secret = common_secret.to_str().unwrap();
        let push_secret = push_secret.to_str().unwrap();

        let src = source(&[
            ("SHARED", "one"),
            ("FLUXER_SVC_PORT", "8090"),
            ("FLUXER_TOKEN_FILE", common_secret),
            ("FLUXER_PLAIN", "common-plain"),
            ("BLANK", ""),
            ("FLUXER_SERVER_SERVICES", "push,snowflakes"),
            ("FLUXER_SERVER_SNOWFLAKES__FLUXER_SVC_PORT", "18090"),
            ("FLUXER_SERVER_SNOWFLAKES__SHARED", ""),
            ("FLUXER_SERVER_SNOWFLAKES__FLUXER_TOKEN", "inline"),
            ("FLUXER_SERVER_PUSH__FLUXER_PLAIN_FILE", push_secret),
            ("FLUXER_SERVER_APP_PROXY__ONLY_HERE", "yes"),
        ])
        .unwrap();

        let cases: [(&str, &[(&str, &str)]); 3] = [
            (
                "snowflakes",
                &[
                    ("FLUXER_PLAIN", "common-plain"),
                    ("FLUXER_SVC_PORT", "18090"),
                    ("FLUXER_TOKEN", "inline"),
                ],
            ),
            (
                "push",
                &[
                    ("FLUXER_PLAIN", "from-push-file"),
                    ("FLUXER_SVC_PORT", "8090"),
                    ("FLUXER_TOKEN", "from-common-file"),
                    ("SHARED", "one"),
                ],
            ),
            (
                "app-proxy",
                &[
                    ("FLUXER_PLAIN", "common-plain"),
                    ("FLUXER_SVC_PORT", "8090"),
                    ("FLUXER_TOKEN", "from-common-file"),
                    ("ONLY_HERE", "yes"),
                    ("SHARED", "one"),
                ],
            ),
        ];
        for (svc, want) in cases {
            let want: BTreeMap<String, String> = want
                .iter()
                .map(|(k, v)| ((*k).to_owned(), (*v).to_owned()))
                .collect();
            assert_eq!(map(src.env(svc).unwrap()), want, "{svc}");
        }
        assert_eq!(src.services(KNOWN).unwrap(), vec!["snowflakes", "push"]);
        assert!(src.process().get("FLUXER_SERVER_SERVICES").is_none());

        assert_eq!(source(&[]).unwrap().services(KNOWN).unwrap(), KNOWN);
        let unknown = source(&[("FLUXER_SERVER_SERVICES", "push,media-proxy")])
            .unwrap()
            .services(KNOWN)
            .unwrap_err();
        assert!(unknown.to_string().contains("media-proxy"), "{unknown}");
        for bad in [
            "FLUXER_SERVER_MEDIA_PROXY__X",
            "FLUXER_SERVER_PUSH__",
            "FLUXER_SERVER_PUSH",
        ] {
            assert!(source(&[(bad, "1")]).is_err(), "{bad}");
        }

        let both = source(&[
            ("FLUXER_TOKEN", "inline"),
            ("FLUXER_TOKEN_FILE", common_secret),
        ])
        .unwrap();
        assert!(both.env("push").is_err());

        std::fs::remove_dir_all(&dir).unwrap();
    }
}
