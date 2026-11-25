// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            alert::alert_info, page_container::page_header, section_card::section_card_simple,
        },
        layout::admin_layout,
    },
    updates::{self, DOCS_URL, Report},
};
use maud::{Markup, html};
use time::OffsetDateTime;

pub const UPDATES_KEY: &str = "updates";

const LINK_CLASS: &str = "font-medium text-blue-700 underline hover:text-blue-900";

pub fn update_strip(config: &AdminConfig, auth: &AuthContext) -> Markup {
    strip(
        &config.base_path,
        updates::visible(config, auth.acls()).as_ref(),
    )
}

pub fn strip(base: &str, report: Option<&Report>) -> Markup {
    let Some(report) = report.filter(|r| r.available()) else {
        return html! {};
    };
    let text = match report.newest() {
        Some(top) if !report.outdated => format!(
            "Fluxer {} is out. This instance runs {}.",
            top.version, report.running
        ),
        _ => "A newer Fluxer is out.".to_owned(),
    };
    html! {
        div class="rounded-lg border border-blue-200 bg-blue-50 px-4 py-3 text-blue-800 text-sm" role="status" {
            (text) " "
            a href={(base) "/updates"} class=(LINK_CLASS) { "See how to upgrade" }
        }
    }
}

pub fn has_new_release(config: &AdminConfig, acls: &[String]) -> bool {
    updates::visible(config, acls).is_some_and(|r| r.available())
}

pub fn updates_page(config: &AdminConfig, auth: &AuthContext, now: OffsetDateTime) -> Markup {
    let content = if !config.self_hosted {
        html! {
            (page_header("Updates", None))
            (alert_info(html! { "This page is for instances you run yourself." }))
        }
    } else if !acl::has_any_permission(auth.acls(), updates::REQUIRED_ACLS) {
        html! {
            (page_header("Updates", None))
            (alert_info(html! { "Seeing updates needs the " code { (acl::INSTANCE_CONFIG_VIEW) } " permission." }))
        }
    } else {
        updates_content(updates::visible(config, auth.acls()).as_ref(), now)
    };
    admin_layout(config, auth, "Updates", UPDATES_KEY, None, content)
}

pub fn updates_content(report: Option<&Report>, now: OffsetDateTime) -> Markup {
    let Some(report) = report else {
        return html! {
            div class="space-y-6" {
                (page_header("Updates", None))
                (alert_info(html! {
                    "Fluxer doesn't look for new releases by itself until you turn the check on with FLUXER_UPDATE_CHECK. "
                    a href=(DOCS_URL) class=(LINK_CLASS) { "Upgrades and rollbacks" }
                    " has the steps for every way of running Fluxer."
                }))
            }
        };
    };
    html! {
        div class="space-y-6" {
            (page_header("Updates", None))
            (section_card_simple("Release", status(report)))
            @if report.available() {
                (section_card_simple("Upgrade", upgrade(report)))
            }
            p class="text-neutral-500 text-xs" { (footer(report, now)) }
        }
    }
}

fn status(report: &Report) -> Markup {
    html! {
        div class="space-y-2 text-neutral-700 text-sm" {
            @if report.outdated {
                p {
                    "This instance runs Fluxer " (report.running) ". A newer release is out, and it lists releases in a format this one doesn't read."
                }
            } @else if let Some(top) = report.newest() {
                p {
                    "This instance runs Fluxer " (report.running) ". Fluxer " (top.version)
                    @if let Some(day) = updates::day(&top.date) { " came out on " (day) }
                    @else { " is out" }
                    "."
                }
                @if report.newer.len() > 1 {
                    p { "There are " (report.newer.len()) " releases since this one, and one upgrade takes you straight to the newest." }
                }
            } @else if !report.latest.is_empty() {
                p { "This instance runs Fluxer " (report.running) ", the newest release." }
            } @else {
                p { "This instance runs Fluxer " (report.running) "." }
            }
        }
    }
}

fn upgrade(report: &Report) -> Markup {
    html! {
        div class="space-y-3 text-neutral-700 text-sm" {
            @if !report.command.is_empty() {
                pre class="overflow-x-auto whitespace-pre-wrap break-words rounded bg-neutral-900 p-3 text-sm text-white" {
                    code { (report.command) }
                }
                button type="button"
                    class="cursor-pointer text-blue-600 hover:underline"
                    data-copy-value=(report.command)
                    onclick="window.__adminCopyToClipboard && window.__adminCopyToClipboard(this.dataset.copyValue, this, 'Copied')" {
                    "Copy command"
                }
            }
            @if !report.then.is_empty() { p { (report.then) } }
            @if !report.backup.is_empty() { p { (report.backup) } }
            p { a href=(DOCS_URL) class=(LINK_CLASS) { "Upgrades and rollbacks" } }
        }
    }
}

fn footer(report: &Report, now: OffsetDateTime) -> String {
    if !report.enabled {
        let start = match report.method.as_str() {
            "rexctl" => "run rexctl config set FLUXER_UPDATE_CHECK true",
            "helm" => "set config.FLUXER_UPDATE_CHECK to true in the chart values",
            "compose" | "docker" | "systemd" => "set FLUXER_UPDATE_CHECK=true in fluxer.env",
            _ => "set FLUXER_UPDATE_CHECK to true",
        };
        let off = format!(
            "Fluxer doesn't look for new releases by itself. To have it check once a day, {start}."
        );
        return match updates::ago(&report.checked_at, now) {
            Some(ago) => format!("Fluxer last checked {ago}. {off}"),
            None => off,
        };
    }
    if report.failures > 0 {
        let span = updates::span(&report.failed_at, now).unwrap_or_else(|| "a while".to_owned());
        return format!(
            "Fluxer couldn't get the list of releases for the last {span} ({}). It tries again by itself.",
            report.error
        );
    }
    let host = url::Url::parse(&report.url)
        .ok()
        .and_then(|u| u.host_str().map(str::to_owned))
        .unwrap_or_else(|| "fluxer.dev".to_owned());
    let privacy = format!(
        "Once a day it asks {host} for the list of releases, and the request holds nothing about this instance."
    );
    match updates::ago(&report.checked_at, now) {
        Some(ago) => format!("Fluxer last checked {ago}. {privacy}"),
        None => format!("Fluxer checks a few minutes after it starts. {privacy}"),
    }
}
