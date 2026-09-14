// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::{
        client::{ApiError, ApiResult},
        types::{AdminUser, MessageShredStatusResponse},
    },
    config::AdminConfig,
    templates::components::{
        form::{csrf_input, danger_button, form_actions, submit_button},
        page_container::card_with_header,
    },
};
use maud::{Markup, html};

const TEMP_BAN_DURATIONS: &[(&str, &str)] = &[
    ("1", "1 hour"),
    ("12", "12 hours"),
    ("24", "1 day"),
    ("72", "3 days"),
    ("120", "5 days"),
    ("168", "1 week"),
    ("336", "2 weeks"),
    ("720", "30 days"),
    ("0", "Permanent"),
];

const DELETION_REASONS: &[(&str, &str)] = &[
    ("1", "User requested"),
    ("2", "Other"),
    ("3", "Spam"),
    ("4", "Cheating or exploitation"),
    ("5", "Coordinated raiding or manipulation"),
    ("6", "Automation or self-bot usage"),
    ("7", "Nonconsensual sexual content"),
    ("8", "Scam or social engineering"),
    ("9", "Child sexual content"),
    ("10", "Privacy violation or doxxing"),
    ("11", "Harassment or bullying"),
    ("12", "Payment fraud"),
    ("13", "Child safety violation"),
    ("14", "Billing dispute or abuse"),
    ("15", "Unsolicited explicit content"),
    ("16", "Graphic violence"),
    ("17", "Ban evasion"),
    ("18", "Token or credential scam"),
    ("19", "Inactivity"),
    ("20", "Hate speech or extremist content"),
    ("21", "Malicious links or malware distribution"),
    ("22", "Impersonation or fake identity"),
];

pub fn moderation_tab(
    config: &AdminConfig,
    user: &AdminUser,
    csrf_token: &str,
    admin_acls: &[String],
    message_shred_job_id: Option<&str>,
    message_shred_status: Option<&ApiResult<MessageShredStatusResponse>>,
    delete_all_messages_dry_run: Option<(u64, u64)>,
) -> Markup {
    let base = &config.base_path;
    let can_delete_all_messages = acl::has_permission(admin_acls, acl::MESSAGE_DELETE_ALL);
    let can_shred_messages = acl::has_permission(admin_acls, acl::MESSAGE_SHRED);
    html! {
        div class="space-y-6" {
            div class="grid grid-cols-1 gap-6 md:grid-cols-2" {
                (ban_actions_card(base, user, csrf_token))
                (deletion_card(base, user, csrf_token))
            }
            @if can_delete_all_messages {
                (delete_all_messages_card(base, user, csrf_token, delete_all_messages_dry_run))
            }
            @if can_shred_messages {
                (message_shred_card(base, user, csrf_token, message_shred_job_id, message_shred_status))
            }
        }
    }
}

fn ban_actions_card(base: &str, user: &AdminUser, csrf_token: &str) -> Markup {
    html! {
        (card_with_header("Ban Actions", html! {
            @if user.temp_banned_until.is_some() {
                form method="post"
                    action={(base) "/users/" (user.id) "?action=unban&tab=moderation"} {
                    (csrf_input(csrf_token))
                    (form_actions(html! {
                        (submit_button("Unban User"))
                    }))
                }
            } @else {
                form method="post"
                    action={(base) "/users/" (user.id) "?action=temp_ban&tab=moderation"} {
                    (csrf_input(csrf_token))
                    div class="space-y-3" {
                        (form_label("Duration"))
                        select name="duration"
                            class="block w-full rounded-md border border-neutral-300 \
                                   px-3 py-2 text-sm shadow-sm \
                                   focus:border-brand-primary focus:outline-none \
                                   focus:ring-1 focus:ring-brand-primary" {
                            @for &(value, label) in TEMP_BAN_DURATIONS {
                                option value=(value) { (label) }
                            }
                        }
                        (form_label("Public Reason (optional)"))
                        input type="text" name="reason"
                            placeholder="Enter public ban reason..."
                            class="block w-full rounded-md border border-neutral-300 \
                                   px-3 py-2 text-sm shadow-sm \
                                   focus:border-brand-primary focus:outline-none \
                                   focus:ring-1 focus:ring-brand-primary";
                        (form_label("Private Reason (optional)"))
                        input type="text" name="private_reason"
                            placeholder="Enter private ban reason (audit log)..."
                            class="block w-full rounded-md border border-neutral-300 \
                                   px-3 py-2 text-sm shadow-sm \
                                   focus:border-brand-primary focus:outline-none \
                                   focus:ring-1 focus:ring-brand-primary";
                        (form_actions(html! {
                            (submit_button("Ban/Suspend User"))
                        }))
                    }
                }
            }
        }))
    }
}

fn deletion_card(base: &str, user: &AdminUser, csrf_token: &str) -> Markup {
    html! {
        (card_with_header("Account Deletion", html! {
            @if user.pending_deletion_at.is_some() {
                form method="post"
                    action={(base) "/users/" (user.id) "?action=cancel_deletion&tab=moderation"} {
                    (csrf_input(csrf_token))
                    (form_actions(html! {
                        (submit_button("Cancel Deletion"))
                    }))
                }
            } @else {
                form method="post"
                    action={(base) "/users/" (user.id) "?action=schedule_deletion&tab=moderation"} {
                    (csrf_input(csrf_token))
                    div class="space-y-3" {
                        (form_label("Days until deletion"))
                        input type="number" name="days_until_deletion" value="60"
                            min="60" max="365"
                            class="block w-full rounded-md border border-neutral-300 \
                                   px-3 py-2 text-sm shadow-sm \
                                   focus:border-brand-primary focus:outline-none \
                                   focus:ring-1 focus:ring-brand-primary";
                        (form_label("Reason"))
                        select name="reason_code"
                            class="block w-full rounded-md border border-neutral-300 \
                                   px-3 py-2 text-sm shadow-sm \
                                   focus:border-brand-primary focus:outline-none \
                                   focus:ring-1 focus:ring-brand-primary" {
                            @for &(value, label) in DELETION_REASONS {
                                option value=(value) { (label) }
                            }
                        }
                        (form_label("Public Reason (optional)"))
                        input type="text" name="public_reason"
                            placeholder="Enter public reason..."
                            class="block w-full rounded-md border border-neutral-300 \
                                   px-3 py-2 text-sm shadow-sm \
                                   focus:border-brand-primary focus:outline-none \
                                   focus:ring-1 focus:ring-brand-primary";
                        (form_label("Private Reason (optional)"))
                        input type="text" name="private_reason"
                            placeholder="Enter private reason (audit log)..."
                            class="block w-full rounded-md border border-neutral-300 \
                                   px-3 py-2 text-sm shadow-sm \
                                   focus:border-brand-primary focus:outline-none \
                                   focus:ring-1 focus:ring-brand-primary";
                        (form_actions(html! {
                            (submit_button("Schedule Deletion"))
                        }))
                    }
                }
            }
        }))
    }
}

fn delete_all_messages_card(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    dry_run: Option<(u64, u64)>,
) -> Markup {
    html! {
        (card_with_header("Delete All Messages", html! {
            div class="space-y-4" {
                p class="text-sm text-neutral-600" {
                    "Locate every message this user has ever sent and permanently remove them. First run a dry run to see how many channels and messages will be affected."
                }
                form method="post"
                    action={(base) "/users/" (user.id) "?action=delete_all_messages&tab=moderation"}
                    data-admin-result-form="true" {
                    (csrf_input(csrf_token))
                    input type="hidden" name="dry_run" value="true";
                    (form_actions(html! {
                        (submit_button("Preview Deletion"))
                    }))
                }
                @if let Some((channel_count, message_count)) = dry_run {
                    div class="space-y-3 rounded-lg border border-neutral-200 bg-neutral-50 p-4" {
                        p class="text-sm text-neutral-700" {
                            "Channels: " (channel_count) " · Messages: " (message_count)
                        }
                        form method="post"
                            action={(base) "/users/" (user.id) "?action=delete_all_messages&tab=moderation"} {
                            (csrf_input(csrf_token))
                            input type="hidden" name="dry_run" value="false";
                            (form_actions(html! {
                                (danger_button("Delete All Messages"))
                            }))
                        }
                    }
                }
            }
        }))
    }
}

fn message_shred_card(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    job_id: Option<&str>,
    status: Option<&ApiResult<MessageShredStatusResponse>>,
) -> Markup {
    html! {
        (card_with_header("Message Shredder", html! {
            div class="space-y-4" {
                p class="text-sm text-neutral-600" {
                    "Upload a CSV file where each row includes the channel_id and message_id separated by a comma. Large files are chunked server-side automatically."
                }
                form method="post"
                    action={(base) "/users/" (user.id) "?action=message_shred&tab=moderation"}
                    id="message-shred-form" {
                    (csrf_input(csrf_token))
                    input type="hidden" name="csv_data" id="message-shred-csv-data";
                    div class="space-y-3" {
                        div class="space-y-2" {
                            (form_label("CSV File"))
                            input id="message-shred-file" type="file" accept=".csv" class="hidden";
                            div class="flex items-center gap-2" {
                                button type="button"
                                    class="inline-flex items-center justify-center gap-2 rounded-lg border border-neutral-300 bg-neutral-50 px-3 py-1.5 text-sm font-medium text-neutral-700 transition-all duration-150 hover:border-neutral-400 hover:text-neutral-900 focus:outline-none focus:ring-2 focus:ring-brand-primary/20"
                                    onclick="document.getElementById('message-shred-file').click()" {
                                    span { "Choose file" }
                                }
                                span class="text-sm text-neutral-500" id="message-shred-file-name" {
                                    "No file chosen"
                                }
                            }
                        }
                        button type="submit"
                            id="message-shred-submit"
                            class="inline-flex w-full items-center justify-center gap-2 rounded-lg bg-neutral-900 px-4 py-2 text-base font-medium text-white transition-all duration-150 hover:bg-neutral-800 focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:ring-offset-2 focus:ring-offset-white disabled:cursor-not-allowed disabled:opacity-50" {
                            span { "Shred Messages" }
                        }
                    }
                }
                (message_shred_status_section(base, user, job_id, status))
                (message_shred_form_script())
            }
        }))
    }
}

fn message_shred_status_section(
    base: &str,
    user: &AdminUser,
    job_id: Option<&str>,
    status: Option<&ApiResult<MessageShredStatusResponse>>,
) -> Markup {
    let Some(_) = job_id.filter(|job_id| !job_id.trim().is_empty()) else {
        return html! {};
    };
    html! {
        div class="space-y-3 rounded-lg border border-neutral-200 bg-white p-4" {
            div class="flex items-center justify-between gap-3" {
                h3 class="text-sm font-medium text-neutral-900" { "Message Shred Status" }
                a href={(base) "/users/" (user.id) "?tab=moderation"}
                    class="text-sm text-neutral-500 hover:text-neutral-900" {
                    "Clear"
                }
            }
            @match status {
                Some(Ok(response)) => {
                    (message_shred_status_content(&response.data))
                }
                Some(Err(ApiError::Http { status: 404, .. })) | None => {
                    p class="text-sm text-neutral-700" { "Preparing job... check back in a moment." }
                }
                Some(Err(error)) => {
                    (message_shred_status_error(error))
                }
            }
        }
    }
}

fn message_shred_status_content(status: &serde_json::Value) -> Markup {
    let status_value = value_str(status, "status").unwrap_or("not_found");
    if status_value == "not_found" {
        return html! {
            div class="space-y-3" {
                p class="text-sm text-neutral-700" {
                    "Status: " (message_shred_status_label(status_value))
                }
            }
        };
    }
    let requested = value_u64(status, "requested").unwrap_or(0);
    let total = value_u64(status, "total").unwrap_or(0);
    let processed = value_u64(status, "processed").unwrap_or(0);
    let skipped = value_u64(status, "skipped").unwrap_or(0);
    let percentage = (u128::from(processed) * 100)
        .checked_div(u128::from(total))
        .unwrap_or(0)
        .min(100);
    html! {
        div class="space-y-3" {
            p class="text-sm text-neutral-700" {
                "Status: " (message_shred_status_label(status_value))
            }
            p class="text-sm text-neutral-700" {
                "Requested " (requested) " entries, skipped " (skipped) " entries"
            }
            @if status_value == "in_progress" {
                div class="space-y-2" {
                    div class="text-sm text-neutral-700" {
                        span { (processed) " / " (total) " (" (percentage) "%)" }
                    }
                    div class="h-2 w-full overflow-hidden rounded-full bg-neutral-200" {
                        div class="h-2 bg-neutral-900 transition-[width] duration-300"
                            style={ "width: " (percentage) "%;" } {}
                    }
                }
            }
            @if status_value == "completed" {
                p class="text-sm text-neutral-700" {
                    "Deleted " (processed) " / " (total) " entries"
                }
            }
            @if let Some(started_at) = value_str(status, "started_at") {
                p class="text-xs text-neutral-500" { "Started " (started_at) }
            }
            @if let Some(completed_at) = value_str(status, "completed_at") {
                p class="text-xs text-neutral-500" { "Completed " (completed_at) }
            }
            @if let Some(failed_at) = value_str(status, "failed_at") {
                p class="text-xs text-red-600" { "Failed " (failed_at) }
            }
            @if let Some(error) = value_str(status, "error") {
                p class="text-sm text-red-600" { (error) }
            }
        }
    }
}

fn message_shred_status_error(error: &ApiError) -> Markup {
    let message = match error {
        ApiError::Network(message) | ApiError::Parse(message) => message.as_str(),
        ApiError::Http { message, .. } => message.as_str(),
    };
    html! {
        div class="rounded-lg border border-red-200 bg-red-50 p-4 text-sm text-red-800" {
            "Failed to load message shred status"
            @if !message.is_empty() {
                div class="mt-1 text-xs" { (message) }
            }
        }
    }
}

fn message_shred_status_label(status: &str) -> &'static str {
    match status {
        "in_progress" => "In progress",
        "completed" => "Completed",
        "failed" => "Failed",
        "not_found" => "Preparing",
        _ => "Unknown",
    }
}

fn value_str<'a>(value: &'a serde_json::Value, key: &str) -> Option<&'a str> {
    value.get(key).and_then(serde_json::Value::as_str)
}

fn value_u64(value: &serde_json::Value, key: &str) -> Option<u64> {
    value.get(key).and_then(|value| {
        value
            .as_u64()
            .or_else(|| value.as_i64().and_then(|n| n.try_into().ok()))
    })
}

fn message_shred_form_script() -> Markup {
    html! {
        script defer {
            (maud::PreEscaped(MESSAGE_SHRED_FORM_SCRIPT))
        }
    }
}

fn form_label(text: &str) -> Markup {
    maud::html! {
        label class="block text-sm font-medium text-neutral-700" { (text) }
    }
}

const MESSAGE_SHRED_FORM_SCRIPT: &str = r#"
(function () {
	var form = document.getElementById('message-shred-form');
	if (!form) return;
	var file = document.getElementById('message-shred-file');
	var csvInput = document.getElementById('message-shred-csv-data');
	var submitButton = document.getElementById('message-shred-submit');
	var fileName = document.getElementById('message-shred-file-name');
	if (!file || !csvInput || !submitButton) return;
	var processing = false;
	function toast(level, message) {
		document.body.dispatchEvent(new CustomEvent('showFlash', {detail: {level: level, message: message}}));
	}
	function toastFromResponse(response) {
		var raw = response.headers.get('X-Fluxer-Admin-Toast');
		if (!raw) return null;
		try {
			var parsed = JSON.parse(raw);
			return {
				level: parsed && parsed.level ? String(parsed.level) : 'info',
				message: parsed && parsed.message ? String(parsed.message) : ''
			};
		} catch (error) {
			return null;
		}
	}
	function csrfToken() {
		var input = form.querySelector('input[name="_csrf"]');
		return input ? input.value || '' : '';
	}
	function resetSubmit(label) {
		processing = false;
		submitButton.disabled = false;
		submitButton.querySelector('span').textContent = label || 'Shred Messages';
	}
	file.addEventListener('change', function () {
		if (fileName) {
			fileName.textContent = file.files && file.files[0]
				? file.files[0].name
				: 'No file chosen';
		}
	});
	form.addEventListener('submit', function (event) {
		if (processing) {
			event.preventDefault();
			return;
		}
		var selected = file.files && file.files[0];
		if (!selected) {
			event.preventDefault();
			toast('error', 'Please select a CSV file to continue.');
			return;
		}
		event.preventDefault();
		processing = true;
		submitButton.disabled = true;
		submitButton.querySelector('span').textContent = 'Processing...';
		var reader = new FileReader();
		reader.onload = function () {
			csvInput.value = reader.result || '';
			toast('info', 'Queueing message shred...');
			fetch(form.getAttribute('action') || window.location.href, {
				method: 'POST',
				credentials: 'same-origin',
				headers: {
					'Content-Type': 'application/x-www-form-urlencoded',
					'HX-Request': 'true',
					'HX-Target': 'flash-container',
					'X-CSRF-Token': csrfToken()
				},
				body: new URLSearchParams(new FormData(form)).toString()
			}).then(function (response) {
				var parsed = toastFromResponse(response);
				if (!response.ok && !parsed) throw new Error('Failed to queue message shred.');
				toast(
					parsed ? parsed.level : response.ok ? 'success' : 'error',
					parsed ? parsed.message : response.ok ? 'Message shred queued.' : 'Failed to queue message shred.'
				);
				resetSubmit();
				if (response.ok) {
					file.value = '';
					if (fileName) fileName.textContent = 'No file chosen';
				}
			}).catch(function (error) {
				toast('error', error && error.message ? error.message : 'Failed to queue message shred.');
				resetSubmit();
			});
		};
		reader.onerror = function () {
			resetSubmit();
			toast('error', 'Failed to read the CSV file. Please try again.');
		};
		reader.readAsText(selected);
	});
})();
"#;
