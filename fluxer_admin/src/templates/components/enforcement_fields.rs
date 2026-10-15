// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    admin_flags::{
        self, COPYRIGHT_REASON_CODE, ENFORCEMENT_REASONS, EnforcementHandling, user_flag_bits,
    },
    api::{
        client::ApiResult,
        enforcement::DecisionSubmission,
        generated::{snowflake, types as generated_types},
    },
    templates::components::form::checkbox,
    utils::{forms::MultiValueForm, plural::count_noun, timestamps::format_admin_timestamp},
};
use maud::{Markup, html};

pub const FIELD_LABEL_CLASS: &str = "block text-sm font-medium text-neutral-700";
pub const FIELD_HELP_CLASS: &str = "text-xs text-neutral-500";
pub const FIELD_INPUT_CLASS: &str = "block w-full rounded-md border border-neutral-300 \
     px-3 py-2 text-sm shadow-sm focus:border-brand-primary focus:outline-none \
     focus:ring-1 focus:ring-brand-primary";
pub const INTERNAL_NOTE_MAX_CHARS: usize = 512;
const STATEMENT_MAX_CHARS: usize = 500;
const ACCOUNT_DELETION_MIN_DAYS: u32 = 14;
const ENFORCEMENT_CLOSURE_MIN_DAYS: u32 = 74;
const RIGHTS_HOLDER_MAX_CHARS: usize = 200;

const AUTOMATED_LIMIT_FLAGS: &[(u64, &str)] = &[
    (user_flag_bits::ACCOUNT_LIMITED, "Account limited"),
    (user_flag_bits::PROFILE_HIDDEN, "Profile hidden"),
];

const HANDLING_GROUPS: &[(EnforcementHandling, &str)] = &[
    (EnforcementHandling::Standard, "Standard"),
    (
        EnforcementHandling::Protected,
        "Protected: fewer details in the notice",
    ),
    (
        EnforcementHandling::Restricted,
        "Restricted: no details in the notice",
    ),
];

pub struct DecisionFields<'a> {
    pub id_prefix: &'a str,
    pub kind: generated_types::EnforcementKindType,
    pub report_ids: &'a [String],
    pub messages: Option<&'a [String]>,
    pub user_flags: Option<u64>,
    pub allow_restricted: bool,
}

pub struct BasisFields<'a> {
    pub decision: DecisionFields<'a>,
    pub choices: &'a [(&'a str, &'a str)],
    pub plain_note_help: &'a str,
    pub enforcement_extra: Markup,
}

pub enum BasisSubmission {
    Enforcement(DecisionSubmission),
    HolderRequest(Option<String>),
    Housekeeping(Option<String>),
}

impl BasisSubmission {
    pub fn internal_note(&self) -> Option<&str> {
        match self {
            Self::Enforcement(submission) => Some(&submission.internal_note),
            Self::HolderRequest(note) | Self::Housekeeping(note) => note.as_deref(),
        }
    }

    pub fn moderation_basis(&self) -> generated_types::ModerationBasis {
        match self {
            Self::Enforcement(submission) => {
                generated_types::ModerationBasis::Enforcement(submission.decision.clone())
            }
            Self::HolderRequest(_) => generated_types::ModerationBasis::HolderRequest,
            Self::Housekeeping(_) => generated_types::ModerationBasis::Housekeeping,
        }
    }
}

pub fn automated_limits(flags: u64) -> Vec<&'static str> {
    AUTOMATED_LIMIT_FLAGS
        .iter()
        .filter(|(bit, _)| flags & bit != 0)
        .map(|(_, label)| *label)
        .collect()
}

pub fn field_label(for_id: &str, text: &str) -> Markup {
    html! {
        label for=(for_id) class=(FIELD_LABEL_CLASS) { (text) }
    }
}

pub fn internal_note_field(id_prefix: &str, help: &str) -> Markup {
    let id = format!("{id_prefix}-internal-note");
    html! {
        div class="space-y-1" {
            (field_label(&id, "Internal note"))
            input type="text" id=(id) name="internal_note" required
                maxlength=(INTERNAL_NOTE_MAX_CHARS) class=(FIELD_INPUT_CLASS);
            p class=(FIELD_HELP_CLASS) { (help) }
        }
    }
}

pub fn deletion_days_field(id: &str, self_hosted: bool) -> Markup {
    let (days, help) = if self_hosted {
        (
            ACCOUNT_DELETION_MIN_DAYS,
            format!("At least {ACCOUNT_DELETION_MIN_DAYS} days."),
        )
    } else {
        (
            ENFORCEMENT_CLOSURE_MIN_DAYS,
            format!(
                "At least {ENFORCEMENT_CLOSURE_MIN_DAYS} days when a rule was broken. At least {ACCOUNT_DELETION_MIN_DAYS} days otherwise."
            ),
        )
    };
    html! {
        div class="space-y-1" {
            (field_label(id, "Days until deletion"))
            input type="number" id=(id) name="days_until_deletion"
                value=(days) min="1" max="365" required class=(FIELD_INPUT_CLASS);
            p class=(FIELD_HELP_CLASS) { (help) }
        }
    }
}

pub fn statement_field(id_prefix: &str, help: &str) -> Markup {
    let id = format!("{id_prefix}-statement");
    html! {
        div class="space-y-1" {
            (field_label(&id, "Note to the user"))
            textarea id=(id) name="statement" rows="2" maxlength=(STATEMENT_MAX_CHARS)
                class=(FIELD_INPUT_CLASS) {}
            p class=(FIELD_HELP_CLASS) { (help) }
        }
    }
}

pub fn radio_group(legend: &str, name: &str, id_prefix: &str, options: &[(&str, &str)]) -> Markup {
    html! {
        fieldset class="space-y-1" {
            legend class=(FIELD_LABEL_CLASS) { (legend) }
            div class="flex flex-wrap gap-x-4 gap-y-1" {
                @for (value, label) in options {
                    @let id = format!("{id_prefix}-{name}-{value}");
                    div class="flex items-center gap-2" {
                        input type="radio" id=(id) name=(name) value=(value) required
                            class="h-4 w-4 accent-neutral-900";
                        label for=(id) class="text-sm text-neutral-900" { (label) }
                    }
                }
            }
        }
    }
}

fn codes_attribute(handling: EnforcementHandling) -> String {
    ENFORCEMENT_REASONS
        .iter()
        .filter(|reason| reason.handling == handling)
        .map(|reason| reason.code.to_string())
        .collect::<Vec<_>>()
        .join(" ")
}

pub fn reason_select(
    id_prefix: &str,
    kind: generated_types::EnforcementKindType,
    allow_restricted: bool,
) -> Markup {
    let id = format!("{id_prefix}-reason");
    html! {
        div class="space-y-1" {
            (field_label(&id, "Reason"))
            select id=(id) name="reason_code" required class=(FIELD_INPUT_CLASS) {
                option value="" disabled selected { "Choose a reason" }
                @for (handling, group) in HANDLING_GROUPS
                    .iter()
                    .filter(|(handling, _)| allow_restricted || *handling != EnforcementHandling::Restricted)
                {
                    @let reasons = ENFORCEMENT_REASONS
                        .iter()
                        .filter(|reason| reason.handling == *handling && reason.allows(kind))
                        .collect::<Vec<_>>();
                    @if !reasons.is_empty() {
                        optgroup label=(group) {
                            @for reason in reasons {
                                option value=(reason.code) {
                                    (admin_flags::deletion_reason_label(reason.code))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn automated_limits_hint(flags: u64) -> Markup {
    let limits = automated_limits(flags);
    html! {
        p class=(FIELD_HELP_CLASS) data-automated-limits="" {
            @if limits.is_empty() {
                "No automated limits on this account now."
            } @else {
                "Automated limits on this account now: " (limits.join(", ")) "."
            }
        }
    }
}

pub fn enforcement_fields(fields: &DecisionFields<'_>) -> Markup {
    let prefix = fields.id_prefix;
    let restricted_codes = codes_attribute(EnforcementHandling::Restricted);
    let reports_id = format!("{prefix}-report-ids");
    let messages_id = format!("{prefix}-evidence");
    let rights_holder_id = format!("{prefix}-rights-holder");
    html! {
        div class="space-y-4" data-enforcement-fields="" {
            (reason_select(prefix, fields.kind, fields.allow_restricted))
            (radio_group("How did we find this?", "source", prefix, &[
                ("report", "A report"),
                ("own_review", "Our own review"),
            ]))
            div class="space-y-1" {
                (radio_group("Automated tools", "automation", prefix, &[
                    ("none", "Not involved"),
                    ("flagged", "Flagged this for review"),
                ]))
                @if let Some(flags) = fields.user_flags {
                    (automated_limits_hint(flags))
                }
            }
            div class="space-y-1" {
                (field_label(&reports_id, "Reports"))
                textarea id=(reports_id) name="report_ids" rows="2"
                    placeholder="One report ID per line" class=(FIELD_INPUT_CLASS) {
                    (fields.report_ids.join("\n"))
                }
            }
            @if let Some(messages) = fields.messages {
                div class="space-y-1" {
                    (field_label(&messages_id, "Messages"))
                    textarea id=(messages_id) name="evidence" rows="3"
                        placeholder="One message link or channel_id/message_id per line"
                        class=(FIELD_INPUT_CLASS) {
                        (messages.join("\n"))
                    }
                }
            }
            div data-hide-for-codes=(restricted_codes) {
                (statement_field(prefix, "Shown to the user. Never name or describe other people. Not sent for child safety or violent extremism decisions."))
            }
            div class="space-y-1" data-show-for-codes=(COPYRIGHT_REASON_CODE) hidden {
                (field_label(&rights_holder_id, "Rights holder or right concerned"))
                input type="text" id=(rights_holder_id) name="rights_holder" required disabled
                    maxlength=(RIGHTS_HOLDER_MAX_CHARS) class=(FIELD_INPUT_CLASS);
                p class=(FIELD_HELP_CLASS) {
                    "For example Example Records, or trademark. This is the one place a name is shown to the user."
                }
            }
            div class="space-y-2" data-show-for-codes=(restricted_codes) hidden {
                (checkbox("confirmations", "evidence_preserved", "Evidence is preserved", false, true))
                (checkbox("confirmations", "reports_filed_or_not_required", "Any required report is filed, or none is required", false, true))
            }
            (internal_note_field(prefix, "Staff only. Saved as the first note on the decision."))
        }
    }
}

const NETWORK_BLOCK_DEFAULT_DAYS: u64 = 90;

fn network_note(network: &generated_types::NetworkBlockCandidatesResponseNetworksItem) -> String {
    use generated_types::NetworkBlockCandidatesResponseNetworksItemSkipReason as SkipReason;
    match network.skip_reason {
        Some(SkipReason::Exempt) => "Exempt, never blocked".to_owned(),
        Some(SkipReason::SpecialPurpose) => {
            "Private or special-purpose range, never blocked".to_owned()
        }
        None => {
            let others = u64::try_from(*network.other_unrestricted_accounts).unwrap_or(0);
            format!(
                "{} last active here",
                count_noun(
                    others,
                    "other unrestricted account",
                    "other unrestricted accounts"
                )
            )
        }
    }
}

fn is_blockable(network: &generated_types::NetworkBlockCandidatesResponseNetworksItem) -> bool {
    network.skip_reason.is_none() && *network.other_unrestricted_accounts == 0
}

pub fn network_block_fields(
    id_prefix: &str,
    candidates: &ApiResult<generated_types::NetworkBlockCandidatesResponse>,
) -> Markup {
    let days_id = format!("{id_prefix}-network-block-days");
    html! {
        fieldset class="space-y-2" data-network-blocks="" {
            legend class=(FIELD_LABEL_CLASS) { "Block networks this account used" }
            @match candidates {
                Err(_) => {
                    p class="text-sm text-red-700" { "Failed to load the networks this account used." }
                }
                Ok(response) if response.networks.is_empty() => {
                    p class=(FIELD_HELP_CLASS) { "This account used no networks in the last 30 days." }
                }
                Ok(response) => {
                    p class=(FIELD_HELP_CLASS) {
                        "Networks from the last 30 days. A network other unrestricted accounts use can't be blocked."
                    }
                    @for network in &response.networks {
                        div {
                            (checkbox("block_networks", &network.ip, &network.ip, false, is_blockable(network)))
                            p class={(FIELD_HELP_CLASS) " pl-7"} {
                                "Last used " (format_admin_timestamp(&network.last_seen_at)) ". " (network_note(network)) "."
                            }
                        }
                    }
                    div class="space-y-1" {
                        (field_label(&days_id, "Block length in days"))
                        input type="number" id=(days_id) name="network_block_days"
                            value=(NETWORK_BLOCK_DEFAULT_DAYS) min="1" max="365" class=(FIELD_INPUT_CLASS);
                        p class=(FIELD_HELP_CLASS) { "Ending the decision or deleting the account removes the blocks sooner." }
                    }
                }
            }
        }
    }
}

pub fn network_blocks_from_form(
    form: &MultiValueForm,
) -> Result<Option<generated_types::NetworkBlockRequest>, String> {
    let ips = form
        .values("block_networks")
        .iter()
        .map(|ip| ip.trim().to_owned())
        .filter(|ip| !ip.is_empty())
        .collect::<Vec<_>>();
    if ips.is_empty() {
        return Ok(None);
    }
    let days = match form.parse_value::<u64>("network_block_days") {
        Ok(Some(days)) => days,
        Ok(None) => NETWORK_BLOCK_DEFAULT_DAYS,
        Err(_) => return Err("Enter the block length as a number of days".to_owned()),
    };
    let days =
        std::num::NonZeroU64::new(days).ok_or("The block length must be at least one day")?;
    Ok(Some(generated_types::NetworkBlockRequest { days, ips }))
}

pub fn skipped_networks_summary(
    skipped: &[generated_types::SkippedNetworkBlock],
) -> Option<String> {
    use generated_types::SkippedNetworkBlockReason as Reason;
    if skipped.is_empty() {
        return None;
    }
    let entries = skipped
        .iter()
        .map(|entry| {
            let reason = match entry.reason {
                Reason::Exempt => "exempt",
                Reason::SpecialPurpose => "special-purpose range",
                Reason::AlreadyBlocked => "already blocked",
            };
            format!("{} ({reason})", entry.ip)
        })
        .collect::<Vec<_>>();
    Some(format!("Not blocked: {}.", entries.join(", ")))
}

pub fn basis_fields(fields: &BasisFields<'_>) -> Markup {
    let prefix = fields.decision.id_prefix;
    let plain = fields
        .choices
        .iter()
        .map(|(value, _)| *value)
        .filter(|value| *value != "enforcement")
        .collect::<Vec<_>>()
        .join(" ");
    html! {
        (radio_group("Basis", "basis", prefix, fields.choices))
        fieldset class="space-y-4" data-show-for-basis="enforcement" hidden disabled {
            (enforcement_fields(&fields.decision))
            (fields.enforcement_extra)
        }
        fieldset data-show-for-basis=(plain) hidden disabled {
            (internal_note_field(&format!("{prefix}-plain"), fields.plain_note_help))
        }
    }
}

pub fn basis_from_form(form: &MultiValueForm) -> Result<BasisSubmission, String> {
    match form.first("basis") {
        Some("enforcement") => decision_from_form(form).map(BasisSubmission::Enforcement),
        Some("holder_request") => Ok(BasisSubmission::HolderRequest(form.clean("internal_note"))),
        Some("housekeeping") => Ok(BasisSubmission::Housekeeping(form.clean("internal_note"))),
        _ => Err("Choose a basis".to_owned()),
    }
}

pub fn decision_from_form(form: &MultiValueForm) -> Result<DecisionSubmission, String> {
    let reason_code = form
        .first("reason_code")
        .and_then(|value| value.trim().parse::<i32>().ok())
        .and_then(|code| generated_types::EnforcementReasonCodeType::try_from(code).ok())
        .ok_or("Choose a reason")?;
    let source = form
        .first("source")
        .and_then(|value| generated_types::EnforcementDecisionInputSource::try_from(value).ok())
        .ok_or("Choose how we found this")?;
    let automation = form
        .first("automation")
        .and_then(|value| generated_types::EnforcementDecisionInputAutomation::try_from(value).ok())
        .ok_or("Choose whether automated tools were involved")?;
    let report_ids = lines(form.first("report_ids"))
        .map(|line| {
            is_snowflake(line)
                .then(|| snowflake(line))
                .ok_or_else(|| format!("Not a report ID: {line}"))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let evidence = lines(form.first("evidence"))
        .map(|line| {
            parse_message_reference(line)
                .map(|(channel_id, message_id)| {
                    generated_types::EnforcementDecisionInputEvidenceItem {
                        channel_id: snowflake(&channel_id),
                        message_id: snowflake(&message_id),
                    }
                })
                .ok_or_else(|| format!("Not a message link or channel_id/message_id: {line}"))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let confirmations = form
        .values("confirmations")
        .iter()
        .map(|value| {
            generated_types::EnforcementDecisionInputConfirmationsItem::try_from(value.as_str())
                .map_err(|_| format!("Unknown confirmation: {value}"))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let internal_note = form.clean("internal_note").ok_or("Add an internal note")?;
    Ok(DecisionSubmission {
        decision: generated_types::EnforcementDecisionInput {
            automation,
            confirmations,
            evidence,
            reason_code,
            report_ids,
            rights_holder: form.clean("rights_holder"),
            source,
            statement: form.clean("statement"),
        },
        internal_note,
    })
}

pub fn bulk_decision_from_form(
    form: &MultiValueForm,
) -> Result<generated_types::BulkEnforcementDecisionInput, String> {
    let reason_code = form
        .first("reason_code")
        .and_then(|value| value.trim().parse::<i32>().ok())
        .and_then(|code| generated_types::EnforcementReasonCodeType::try_from(code).ok())
        .ok_or("Choose a reason")?;
    let automation = form
        .first("automation")
        .and_then(|value| {
            generated_types::BulkEnforcementDecisionInputAutomation::try_from(value).ok()
        })
        .ok_or("Choose whether automated tools were involved")?;
    let statement = form.clean("statement").ok_or("Add a note to the user")?;
    Ok(generated_types::BulkEnforcementDecisionInput {
        automation,
        reason_code,
        source: generated_types::BulkEnforcementDecisionInputSource::OwnReview,
        statement,
    })
}

fn lines(value: Option<&str>) -> impl Iterator<Item = &str> {
    value
        .unwrap_or_default()
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
}

pub(crate) fn is_snowflake(value: &str) -> bool {
    (1..=20).contains(&value.len()) && value.bytes().all(|byte| byte.is_ascii_digit())
}

fn snowflake_pair(channel_id: &str, message_id: &str) -> Option<(String, String)> {
    (is_snowflake(channel_id) && is_snowflake(message_id))
        .then(|| (channel_id.to_owned(), message_id.to_owned()))
}

pub fn parse_message_reference(line: &str) -> Option<(String, String)> {
    let line = line.trim();
    if let Some((channel_id, message_id)) = line.split_once(['/', ','])
        && let Some(pair) = snowflake_pair(channel_id.trim(), message_id.trim())
    {
        return Some(pair);
    }
    let url = url::Url::parse(line)
        .or_else(|_| url::Url::parse("https://admin.invalid/")?.join(line))
        .ok()?;
    let query_value = |key: &str| {
        url.query_pairs()
            .find(|(name, _)| name == key)
            .map(|(_, value)| value.into_owned())
    };
    if let (Some(channel_id), Some(message_id)) =
        (query_value("channel_id"), query_value("message_id"))
    {
        return snowflake_pair(&channel_id, &message_id);
    }
    let segments: Vec<&str> = url.path_segments()?.filter(|s| !s.is_empty()).collect();
    match segments.as_slice() {
        [.., "channels", _, channel_id, message_id] => snowflake_pair(channel_id, message_id),
        _ => None,
    }
}

pub struct TakeAction<'a> {
    pub author_id: Option<&'a str>,
    pub report_id: Option<&'a str>,
}

pub fn can_take_action(admin_acls: &[String]) -> bool {
    acl::has_any_permission(admin_acls, &[acl::USER_TEMP_BAN, acl::USER_UPDATE_FLAGS])
}

pub fn take_action_href(
    base: &str,
    user_id: &str,
    report_id: Option<&str>,
    message: Option<(&str, &str)>,
) -> String {
    let mut href = format!("{base}/users/{user_id}?tab=moderation");
    if let Some(report_id) = report_id {
        href.push_str("&report_id=");
        href.push_str(&urlencoding::encode(report_id));
    }
    if let Some((channel_id, message_id)) = message {
        href.push_str("&message=");
        href.push_str(&urlencoding::encode(&format!("{channel_id}/{message_id}")));
    }
    href
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::generated::types::EnforcementKindType;

    fn form(body: &str) -> MultiValueForm {
        MultiValueForm::parse(body.as_bytes())
    }

    fn render(kind: EnforcementKindType, flags: u64) -> String {
        enforcement_fields(&DecisionFields {
            id_prefix: "suspend",
            kind,
            report_ids: &["1600000000000000001".to_owned()],
            messages: Some(&["1700000000000000001/1700000000000000002".to_owned()]),
            user_flags: Some(flags),
            allow_restricted: true,
        })
        .into_string()
    }

    #[test]
    fn deletion_days_default_to_the_closure_floor_of_each_mode() {
        let hosted = deletion_days_field("close-days", false).into_string();
        assert!(hosted.contains(r#"name="days_until_deletion" value="74""#));
        assert!(
            hosted.contains("At least 74 days when a rule was broken. At least 14 days otherwise.")
        );
        let self_hosted = deletion_days_field("close-days", true).into_string();
        assert!(self_hosted.contains(r#"name="days_until_deletion" value="14""#));
        assert!(self_hosted.contains(">At least 14 days.</p>"));
        assert!(!self_hosted.contains("74"));
    }

    #[test]
    fn fieldset_renders_every_input_with_prefills() {
        let html = render(EnforcementKindType::Suspension, 0);
        for marker in [
            r#"name="reason_code""#,
            r#"name="source" value="report""#,
            r#"name="source" value="own_review""#,
            r#"name="automation" value="none""#,
            r#"name="automation" value="flagged""#,
            r#"name="report_ids""#,
            r#"name="evidence""#,
            r#"name="statement""#,
            r#"name="rights_holder""#,
            r#"name="confirmations" value="evidence_preserved""#,
            r#"name="confirmations" value="reports_filed_or_not_required""#,
            r#"name="internal_note""#,
            ">1600000000000000001</textarea>",
            ">1700000000000000001/1700000000000000002</textarea>",
            "No automated limits on this account now.",
        ] {
            assert!(html.contains(marker), "missing {marker}\n{html}");
        }
        assert!(!html.contains(r#"name="automation" value="automated""#));
        assert!(!html.contains(r#"checked"#));
    }

    #[test]
    fn rights_holder_and_confirmations_start_hidden_and_follow_their_codes() {
        let html = render(EnforcementKindType::Suspension, 0);
        assert!(html.contains(r#"data-show-for-codes="31" hidden"#));
        assert!(html.contains(r#"data-show-for-codes="9 13 24" hidden"#));
        assert!(html.contains(r#"data-hide-for-codes="9 13 24""#));
        assert!(html.contains(r#"name="rights_holder" required disabled"#));
    }

    #[test]
    fn reasons_are_grouped_by_handling_and_filtered_by_kind() {
        let suspend = render(EnforcementKindType::Suspension, 0);
        assert!(suspend.contains(
            r#"<optgroup label="Restricted: no details in the notice"><option value="9">"#
        ));
        assert!(suspend.contains(
            r#"<optgroup label="Protected: fewer details in the notice"><option value="7">"#
        ));
        for omitted in [1, 2, 19, 30, 31] {
            assert!(
                !suspend.contains(&format!(r#"<option value="{omitted}">"#)),
                "suspension offers {omitted}"
            );
        }
        let warn = render(EnforcementKindType::Warning, 0);
        assert!(!warn.contains("Restricted: no details"));
        assert!(warn.contains(r#"<option value="31">Copyright</option>"#));
        assert!(warn.contains(r#"<option value="29">"#));
        assert!(!warn.contains(r#"<option value="30">"#));
    }

    #[test]
    fn automated_limits_list_the_neutral_hook_flags() {
        let html = render(
            EnforcementKindType::Warning,
            user_flag_bits::ACCOUNT_LIMITED
                | user_flag_bits::PROFILE_HIDDEN
                | user_flag_bits::SPAMMER,
        );
        assert!(
            html.contains("Automated limits on this account now: Account limited, Profile hidden.")
        );
    }

    #[test]
    fn decision_from_form_builds_the_request() {
        let submission = decision_from_form(&form(
            "reason_code=31&source=report&automation=flagged\
             &report_ids=1600000000000000001%0D%0A%0D%0A1600000000000000002\
             &evidence=1700000000000000001%2F1700000000000000002%0Ahttps%3A%2F%2Fweb.example%2Fchannels%2F%40me%2F1700000000000000003%2F1700000000000000004\
             &statement=+Please+stop+&rights_holder=Example+Records\
             &internal_note=Ticket+42",
        ))
        .expect("valid form");
        assert_eq!(submission.internal_note, "Ticket 42");
        let body = serde_json::to_value(&submission.decision).expect("serializable decision");
        assert_eq!(
            body,
            serde_json::json!({
                "reason_code": 31,
                "source": "report",
                "automation": "flagged",
                "report_ids": ["1600000000000000001", "1600000000000000002"],
                "evidence": [
                    {"channel_id": "1700000000000000001", "message_id": "1700000000000000002"},
                    {"channel_id": "1700000000000000003", "message_id": "1700000000000000004"}
                ],
                "statement": "Please stop",
                "rights_holder": "Example Records"
            })
        );
    }

    #[test]
    fn decision_from_form_sends_restricted_confirmations_and_omits_empty_fields() {
        let submission = decision_from_form(&form(
            "reason_code=13&source=own_review&automation=none&report_ids=&evidence=1%2F2\
             &statement=&confirmations=evidence_preserved\
             &confirmations=reports_filed_or_not_required&internal_note=Filed",
        ))
        .expect("valid form");
        let body = serde_json::to_value(&submission.decision).expect("serializable decision");
        assert_eq!(
            body,
            serde_json::json!({
                "reason_code": 13,
                "source": "own_review",
                "automation": "none",
                "evidence": [{"channel_id": "1", "message_id": "2"}],
                "confirmations": ["evidence_preserved", "reports_filed_or_not_required"]
            })
        );
    }

    #[test]
    fn decision_from_form_refuses_incomplete_input() {
        let cases = [
            (
                "source=report&automation=none&internal_note=x",
                "Choose a reason",
            ),
            (
                "reason_code=2&source=report&automation=none&internal_note=x",
                "Choose a reason",
            ),
            (
                "reason_code=3&automation=none&internal_note=x",
                "Choose how we found this",
            ),
            (
                "reason_code=3&source=report&automation=automated&internal_note=x",
                "Choose whether automated tools were involved",
            ),
            (
                "reason_code=3&source=report&automation=none&internal_note=+",
                "Add an internal note",
            ),
            (
                "reason_code=3&source=report&automation=none&internal_note=x&report_ids=abc",
                "Not a report ID: abc",
            ),
            (
                "reason_code=3&source=report&automation=none&internal_note=x&evidence=hello",
                "Not a message link or channel_id/message_id: hello",
            ),
        ];
        for (body, expected) in cases {
            let error = decision_from_form(&form(body)).err();
            assert_eq!(error.as_deref(), Some(expected), "{body}");
        }
    }

    #[test]
    fn basis_fields_hide_the_decision_until_a_rule_was_broken() {
        let html = basis_fields(&BasisFields {
            decision: DecisionFields {
                id_prefix: "shred",
                kind: EnforcementKindType::ContentRemoval,
                report_ids: &[],
                messages: None,
                user_flags: None,
                allow_restricted: false,
            },
            choices: &[
                ("enforcement", "A rule was broken"),
                ("holder_request", "The account holder asked"),
                ("housekeeping", "Housekeeping"),
            ],
            plain_note_help: "Staff only.",
            enforcement_extra: html! {},
        })
        .into_string();
        assert!(html.contains(r#"data-show-for-basis="enforcement" hidden disabled"#));
        assert!(
            html.contains(r#"data-show-for-basis="holder_request housekeeping" hidden disabled"#)
        );
        assert!(!html.contains(r#"name="evidence""#));
        assert!(!html.contains("Restricted: no details"));
        assert!(!html.contains("data-automated-limits"));
    }

    #[test]
    fn basis_from_form_reads_each_basis() {
        assert!(matches!(
            basis_from_form(&form("basis=holder_request&internal_note=Asked+by+email")),
            Ok(BasisSubmission::HolderRequest(Some(note))) if note == "Asked by email"
        ));
        assert!(matches!(
            basis_from_form(&form("basis=housekeeping")),
            Ok(BasisSubmission::Housekeeping(None))
        ));
        let enforcement = basis_from_form(&form(
            "basis=enforcement&reason_code=3&source=own_review&automation=none&internal_note=x",
        ))
        .expect("valid enforcement basis");
        assert_eq!(enforcement.internal_note(), Some("x"));
        assert!(matches!(
            enforcement.moderation_basis(),
            generated_types::ModerationBasis::Enforcement(_)
        ));
        assert_eq!(
            basis_from_form(&form("internal_note=x")).err().as_deref(),
            Some("Choose a basis")
        );
    }

    #[test]
    fn message_references_accept_ids_admin_links_and_app_links() {
        let expected = Some(("111".to_owned(), "222".to_owned()));
        for line in [
            "111/222",
            " 111 / 222 ",
            "111,222",
            "https://admin.example/messages?channel_id=111&message_id=222&context_limit=50",
            "/messages?channel_id=111&message_id=222",
            "https://web.example/channels/@me/111/222",
            "https://web.example/channels/999/111/222",
        ] {
            assert_eq!(parse_message_reference(line), expected, "{line}");
        }
        for line in [
            "111",
            "abc/222",
            "https://web.example/channels/999/111",
            "https://web.example/invite/111/222",
            "/messages?channel_id=111",
        ] {
            assert_eq!(parse_message_reference(line), None, "{line}");
        }
    }

    #[test]
    fn take_action_links_open_the_moderation_tab_with_prefills() {
        assert_eq!(
            take_action_href("/admin", "1", Some("2"), Some(("3", "4"))),
            "/admin/users/1?tab=moderation&report_id=2&message=3%2F4"
        );
        assert_eq!(
            take_action_href("", "1", None, None),
            "/users/1?tab=moderation"
        );
    }
}
