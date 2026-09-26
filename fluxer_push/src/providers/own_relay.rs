// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::ProviderEnvironment;
use crate::subscription::Subscription;
use url::Url;

const APNS_SEGMENT: &str = "apns";
const APNS_VOIP_SEGMENT: &str = "apns-voip";
const FCM_SEGMENT: &str = "fcm";
const RELAY_PREFIX: [&str; 2] = ["relay", "v1"];

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Leg {
    Apns,
    ApnsVoip,
    Fcm,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Hop {
    pub leg: Leg,
    pub app_id: String,
    pub environment: Option<ProviderEnvironment>,
    pub device_token: String,
}

impl Hop {
    pub fn as_subscription(&self, sub: &Subscription) -> Subscription {
        Subscription {
            subscription_id: sub.subscription_id.clone(),
            endpoint: self.device_token.clone(),
            p256dh_key: None,
            auth_key: None,
            platform: Some(
                match self.leg {
                    Leg::Apns => "ios_apns",
                    Leg::ApnsVoip => "ios_apns_voip",
                    Leg::Fcm => "android_fcm",
                }
                .to_owned(),
            ),
            app_id: Some(self.app_id.clone()),
            provider_environment: self.environment.map(|environment| {
                match environment {
                    ProviderEnvironment::Production => "production",
                    ProviderEnvironment::Development => "development",
                }
                .to_owned()
            }),
        }
    }
}

pub fn parse(endpoint: &str, hosts: &[String]) -> Option<Hop> {
    if hosts.is_empty() {
        return None;
    }
    let url = Url::parse(endpoint).ok()?;
    if url.scheme() != "https" {
        return None;
    }
    let host = url.host_str()?.to_ascii_lowercase();
    if !hosts.iter().any(|allowed| allowed == &host) {
        return None;
    }
    let mut segments = url.path_segments()?;
    for expected in RELAY_PREFIX {
        if segments.next()? != expected {
            return None;
        }
    }
    let leg = match segments.next()? {
        APNS_SEGMENT => Leg::Apns,
        APNS_VOIP_SEGMENT => Leg::ApnsVoip,
        FCM_SEGMENT => Leg::Fcm,
        _ => return None,
    };
    let app_id = decode(segments.next()?)?;
    let (environment, device_token) = match leg {
        Leg::Fcm => (None, decode(segments.next()?)?),
        _ => (
            ProviderEnvironment::from_label(&decode(segments.next()?)?),
            decode(segments.next()?)?,
        ),
    };
    if segments.next().is_some() || app_id.is_empty() || device_token.is_empty() {
        return None;
    }
    if !matches!(leg, Leg::Fcm) && environment.is_none() {
        return None;
    }
    Some(Hop {
        leg,
        app_id,
        environment,
        device_token,
    })
}

fn decode(segment: &str) -> Option<String> {
    percent_encoding::percent_decode_str(segment)
        .decode_utf8()
        .ok()
        .map(|value| value.into_owned())
        .filter(|value| !value.contains('/'))
}

#[cfg(test)]
mod tests {
    use super::*;

    const TOKEN: &str = "3dbc5a5ef1a1c1666afc26f466e1b3ebaaf4c66d92dddeb0fd1b69c49641d4cd";

    fn ours() -> Vec<String> {
        vec!["push.fluxer.com".to_owned()]
    }

    #[test]
    fn an_apns_endpoint_on_our_own_relay_is_taken_in_process() {
        let hop = parse(
            &format!("https://push.fluxer.com/relay/v1/apns/canary/production/{TOKEN}"),
            &ours(),
        )
        .expect("our own relay endpoint parses");
        assert_eq!(hop.leg, Leg::Apns);
        assert_eq!(hop.app_id, "canary");
        assert_eq!(hop.environment, Some(ProviderEnvironment::Production));
        assert_eq!(hop.device_token, TOKEN);
    }

    #[test]
    fn an_fcm_endpoint_keeps_its_percent_encoded_token() {
        let hop = parse(
            "https://push.fluxer.com/relay/v1/fcm/canary/dYC_x9gXTjyyrG8_Aw3nUM%3AAPA91bExample",
            &ours(),
        )
        .expect("an fcm endpoint parses");
        assert_eq!(hop.leg, Leg::Fcm);
        assert_eq!(hop.environment, None);
        assert_eq!(hop.device_token, "dYC_x9gXTjyyrG8_Aw3nUM:APA91bExample");
    }

    #[test]
    fn a_voip_endpoint_on_our_own_relay_keeps_its_own_leg() {
        let hop = parse(
            &format!("https://push.fluxer.com/relay/v1/apns-voip/canary/production/{TOKEN}"),
            &ours(),
        )
        .expect("a voip endpoint parses");
        assert_eq!(hop.leg, Leg::ApnsVoip);
    }

    #[test]
    fn only_the_apns_alert_leg_is_taken_in_process() {
        let shortcut = |path: &str| {
            parse(&format!("https://push.fluxer.com/relay/v1/{path}"), &ours())
                .filter(|hop| matches!(hop.leg, Leg::Apns))
        };
        assert!(shortcut(&format!("apns/canary/production/{TOKEN}")).is_some());
        assert!(shortcut(&format!("apns-voip/canary/production/{TOKEN}")).is_none());
        assert!(shortcut("fcm/canary/dYC_x9gXTjyyrG8_Aw3nUM%3AAPA91bExample").is_none());
    }

    #[test]
    fn a_relay_we_do_not_operate_is_left_on_the_network_path() {
        let endpoint = format!("https://push.example.org/relay/v1/apns/canary/production/{TOKEN}");
        assert!(parse(&endpoint, &ours()).is_none());
    }

    #[test]
    fn a_self_hosted_deployment_configures_no_hosts_and_never_shortcuts() {
        let endpoint = format!("https://push.fluxer.com/relay/v1/apns/canary/production/{TOKEN}");
        assert!(parse(&endpoint, &[]).is_none());
    }

    #[test]
    fn a_third_party_web_push_endpoint_is_not_mistaken_for_a_relay_hop() {
        assert!(
            parse(
                "https://updates.push.services.mozilla.com/wpush/v2/gAAAAA",
                &ours()
            )
            .is_none()
        );
        assert!(parse("https://ntfy.sh/upZzH87cT9jJCc?up=1", &ours()).is_none());
    }

    #[test]
    fn a_malformed_relay_path_is_refused() {
        for endpoint in [
            "https://push.fluxer.com/relay/v1/apns/canary/production",
            "https://push.fluxer.com/relay/v1/apns/canary/production/tok/extra",
            "https://push.fluxer.com/relay/v1/sms/canary/production/tok",
            "https://push.fluxer.com/relay/v2/apns/canary/production/tok",
            "http://push.fluxer.com/relay/v1/apns/canary/production/tok",
        ] {
            assert!(
                parse(endpoint, &ours()).is_none(),
                "{endpoint} must not parse"
            );
        }
    }
}
