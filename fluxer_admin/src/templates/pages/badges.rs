// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::{Badge, FlashMessage, ListBadgesResponse},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            error_display::error_alert,
            form::{
                FORM_INPUT_CLASS, FORM_LABEL_CLASS, csrf_input, danger_button, form_actions,
                submit_button,
            },
            page_container::page_header,
            section_card::section_card,
        },
        layout::admin_layout,
    },
};
use maud::{Markup, PreEscaped, html};

const BADGE_TYPES: [(&str, &str); 2] = [
    (
        "user",
        "User badges"
    ),
    (
        "guild",
        "Community badges"
    ),
];
const BUILTIN_BADGES: [(&str, &str); 5] = [
    ("staff", "Staff"),
    ("premium", "Premium"),
    ("verified", "Verified"),
    ("partnered", "Partnered"),
    ("discoverable", "Discoverable"),
];
const ICON_PREVIEW_CLASS: &str = "h-10 w-10 shrink-0 [&>svg]:h-full [&>svg]:w-full";

struct BadgePermissions {
    create: bool,
    update: bool,
    delete: bool,
}

pub fn badges_page(
    config: &AdminConfig,
    auth: &AuthContext,
    badges: Option<&ListBadgesResponse>,
    error: Option<&str>,
    flash: Option<&FlashMessage>,
    csrf_token: &str,
) -> Markup {
    let acls = auth
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let permissions = BadgePermissions {
        create: acl::has_permission(acls, acl::BADGE_CREATE),
        update: acl::has_permission(acls, acl::BADGE_UPDATE),
        delete: acl::has_permission(acls, acl::BADGE_DELETE),
    };
    let action = format!("{}/badges", config.base_path);
    let content = html! {
        div class="space-y-6" {
            (page_header("Badges", Some("Manage badges shown next to community names and on user profiles")))
            @if let Some(err) = error {
                (error_alert(err))
            }
            @if let Some(badges) = badges {
                @for (badge_type, title) in BADGE_TYPES {
                    (badge_type_section(&action, badges, badge_type, title, &permissions, csrf_token))
                }
                (builtin_section(&action, badges, permissions.update, csrf_token))
                script { (PreEscaped(BADGE_ICON_SCRIPT)) }
            }
        }
    };
    admin_layout(config, auth, "Badges", "badges", flash, content)
}

fn badge_type_section(
    action: &str,
    badges: &ListBadgesResponse,
    badge_type: &str,
    title: &str,
    permissions: &BadgePermissions,
    csrf_token: &str,
) -> Markup {
    let mut entries: Vec<&Badge> = badges
        .badges
        .iter()
        .filter(|badge| badge.badge_type == badge_type)
        .collect();
    entries.sort_by_key(|badge| badge.position);
    section_card(
        Some(title),
        None,
        None,
        html! {
            div class="flex flex-col gap-3" {
                @if entries.is_empty() {
                    p class="text-sm text-neutral-500" { "No badges defined yet." }
                }
                @for badge in entries {
                    (badge_row(action, badge, permissions, csrf_token))
                }
                @if permissions.create {
                    details {
                        summary class="cursor-pointer text-sm font-medium text-neutral-700" { "Add badge" }
                        form method="post" action={(action) "?action=create"} data-admin-result-form="true" class="mt-3 flex flex-col gap-4" {
                            (csrf_input(csrf_token))
                            input type="hidden" name="type" value=(badge_type);
                            (badge_fields(&format!("new-{badge_type}"), None))
                            (form_actions(submit_button("Create badge")))
                        }
                    }
                }
            }
        },
    )
}

fn badge_row(
    action: &str,
    badge: &Badge,
    permissions: &BadgePermissions,
    csrf_token: &str,
) -> Markup {
    html! {
        div class="flex flex-col gap-3 rounded-lg border border-neutral-200 p-4" {
            div class="flex items-start gap-3" {
                span class=(ICON_PREVIEW_CLASS) { (PreEscaped(&badge.icon)) }
                div class="flex min-w-0 flex-1 flex-col gap-0.5" {
                    p class="font-medium text-sm text-neutral-900" { (badge.name) }
                    p class="text-sm text-neutral-500" { (badge.tooltip) }
                    @if let Some(url) = &badge.url {
                        a href=(url) target="_blank" rel="noreferrer noopener"
                            class="break-all text-xs text-blue-600 hover:underline" { (url) }
                    }
                    p class="text-xs text-neutral-400" { "Position " (badge.position) " · " (badge.id) }
                }
                @if permissions.delete {
                    form method="post" action={(action) "?action=delete"} data-admin-result-form="true" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="id" value=(badge.id);
                        (danger_button("Delete"))
                    }
                }
            }
            @if permissions.update {
                details {
                    summary class="cursor-pointer text-sm font-medium text-neutral-700" { "Edit badge" }
                    form method="post" action={(action) "?action=update"} data-admin-result-form="true" class="mt-3 flex flex-col gap-4" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="id" value=(badge.id);
                        (badge_fields(&badge.id, Some(badge)))
                        (form_actions(submit_button("Save badge")))
                    }
                }
            }
        }
    }
}

fn badge_fields(id_prefix: &str, badge: Option<&Badge>) -> Markup {
    let position = badge.map(|b| b.position.to_string());
    html! {
        div class="grid grid-cols-1 gap-4 md:grid-cols-2" {
            (field(id_prefix, "name", "Name", "text", badge.map(|b| b.name.as_str()), true))
            (field(id_prefix, "tooltip", "Tooltip", "text", badge.map(|b| b.tooltip.as_str()), true))
            (field(id_prefix, "url", "Link URL", "url", badge.and_then(|b| b.url.as_deref()), false))
            (field(id_prefix, "position", "Position", "number", position.as_deref(), false))
        }
        (icon_input(id_prefix, badge.is_none()))
    }
}

fn field(
    id_prefix: &str,
    name: &str,
    label: &str,
    input_type: &str,
    value: Option<&str>,
    required: bool,
) -> Markup {
    let id = format!("badge-{id_prefix}-{name}");
    html! {
        div class="flex flex-col gap-2" {
            label for=(id) class=(FORM_LABEL_CLASS) { (label) }
            input type=(input_type) id=(id) name=(name) value=[value] required[required]
                min=[(input_type == "number").then_some("0")] class=(FORM_INPUT_CLASS);
        }
    }
}

fn icon_input(id_prefix: &str, required: bool) -> Markup {
    let id = format!("badge-{id_prefix}-icon");
    html! {
        div class="flex flex-col gap-2" {
            label for=(id) class=(FORM_LABEL_CLASS) { "Icon (SVG)" }
            div class="flex items-center gap-3" {
                input type="file" id=(id) accept=".svg,image/svg+xml" required[required]
                    data-badge-icon-file class="text-sm text-neutral-700";
                img data-badge-icon-preview alt="" class="hidden h-10 w-10";
            }
            input type="hidden" name="icon";
            @if !required {
                p class="text-xs text-neutral-500" { "Leave empty to keep the current icon." }
            }
        }
    }
}

fn builtin_section(
    action: &str,
    badges: &ListBadgesResponse,
    can_update: bool,
    csrf_token: &str,
) -> Markup {
    let icons = &badges.builtin_icons;
    section_card(
        Some("Built-in badge icons"),
        Some("Replace the icon of a built-in badge, or restore its default icon."),
        None,
        html! {
            div class="grid grid-cols-1 gap-3 md:grid-cols-2" {
                @for (key, label) in BUILTIN_BADGES {
                    @let icon = match key {
                        "staff" => icons.staff.as_deref(),
                        "premium" => icons.premium.as_deref(),
                        "verified" => icons.verified.as_deref(),
                        "partnered" => icons.partnered.as_deref(),
                        _ => icons.discoverable.as_deref(),
                    };
                    div class="flex flex-col gap-3 rounded-lg border border-neutral-200 p-4" {
                        div class="flex items-center gap-3" {
                            @if let Some(svg) = icon {
                                span class=(ICON_PREVIEW_CLASS) { (PreEscaped(svg)) }
                            }
                            div class="flex flex-col gap-0.5" {
                                p class="font-medium text-sm text-neutral-900" { (label) }
                                p class="text-xs text-neutral-500" {
                                    @if icon.is_some() { "Custom icon" } @else { "Default icon" }
                                }
                            }
                        }
                        @if can_update {
                            form method="post" action={(action) "?action=update_builtin"} data-admin-result-form="true" class="flex flex-col gap-3" {
                                (csrf_input(csrf_token))
                                input type="hidden" name="badge" value=(key);
                                (icon_input(&format!("builtin-{key}"), true))
                                (form_actions(submit_button("Upload icon")))
                            }
                            @if icon.is_some() {
                                form method="post" action={(action) "?action=reset_builtin"} data-admin-result-form="true" {
                                    (csrf_input(csrf_token))
                                    input type="hidden" name="badge" value=(key);
                                    button type="submit" class="text-sm font-medium text-red-600 hover:text-red-700" {
                                        "Restore default icon"
                                    }
                                }
                            }
                        }
                    }
                }
            }
        },
    )
}

const BADGE_ICON_SCRIPT: &str = r#"
document.querySelectorAll('[data-badge-icon-file]').forEach(function (input) {
	input.addEventListener('change', function () {
		var form = input.form;
		var preview = form.querySelector('[data-badge-icon-preview]');
		var file = input.files && input.files[0];
		form.elements.icon.value = '';
		preview.classList.toggle('hidden', !file);
		if (!file) return;
		preview.src = URL.createObjectURL(file);
		file.text().then(function (text) {
			form.elements.icon.value = text;
		});
	});
});
"#;
