// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::{
        AdminCustomBadge, AdminCustomBadgeList, BADGE_IMAGE_MAX_BYTES, BADGE_IMAGE_TYPES,
        CustomBadgeType,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            empty_state::empty_state_full,
            error_display::error_alert,
            form::{
                FORM_INPUT_CLASS, FORM_LABEL_CLASS, FORM_SELECT_CLASS, csrf_input, danger_button,
                submit_button,
            },
            page_container::{card_with_header, page_header},
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

struct BadgePermissions {
    create: bool,
    update: bool,
    delete: bool,
}

pub fn badges_page(
    config: &AdminConfig,
    auth: &AuthContext,
    badges: Option<&AdminCustomBadgeList>,
    csrf_token: &str,
) -> Markup {
    let acls = auth.acls();
    let permissions = BadgePermissions {
        create: acl::has_permission(acls, acl::BADGE_CREATE),
        update: acl::has_permission(acls, acl::BADGE_UPDATE),
        delete: acl::has_permission(acls, acl::BADGE_DELETE),
    };
    let content = html! {
        (page_header(
            "Badges",
            Some("Badges that you give to users and guilds. Users show them on their profile, and guilds show them next to their name. You can make up to 10 badges of each type."),
        ))
        @match badges {
            None => (error_alert("Failed to load badges.")),
            Some(list) => {
                div class="space-y-6" {
                    (badge_section(config, list, CustomBadgeType::User, &permissions, csrf_token))
                    (badge_section(config, list, CustomBadgeType::Guild, &permissions, csrf_token))
                    @if permissions.create {
                        (create_form(config, csrf_token))
                    }
                }
            }
        }
    };
    admin_layout(config, auth, "Badges", "badges", None, content)
}

fn section_title(badge_type: CustomBadgeType) -> &'static str {
    match badge_type {
        CustomBadgeType::User => "User badges",
        CustomBadgeType::Guild => "Guild badges",
    }
}

fn badge_section(
    config: &AdminConfig,
    list: &AdminCustomBadgeList,
    badge_type: CustomBadgeType,
    permissions: &BadgePermissions,
    csrf_token: &str,
) -> Markup {
    let badges: Vec<&AdminCustomBadge> = list.of_type(badge_type).collect();
    card_with_header(
        section_title(badge_type),
        html! {
            @if badges.is_empty() {
                (empty_state_full(None, "No badges of this type yet.", None))
            } @else {
                div class="divide-y divide-neutral-100" {
                    @for badge in badges {
                        (badge_row(config, badge, permissions, csrf_token))
                    }
                }
            }
        },
    )
}

fn badge_row(
    config: &AdminConfig,
    badge: &AdminCustomBadge,
    permissions: &BadgePermissions,
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    html! {
        div class="flex flex-col gap-4 py-4 first:pt-0 last:pb-0" {
            div class="flex items-start gap-3" {
                img src=(badge.image_url()) alt=""
                    class="h-10 w-10 shrink-0 rounded object-contain";
                div class="flex min-w-0 flex-col gap-1" {
                    span class="font-medium text-neutral-900" { (badge.name) }
                    @if let Some(description) = &badge.description {
                        span class="text-sm text-neutral-600" { (description) }
                    }
                    @if let Some(href) = &badge.href {
                        a href=(href) target="_blank" rel="noopener noreferrer"
                            class="break-all text-sm text-blue-600 hover:underline" { (href) }
                    }
                    span class="font-mono text-neutral-500 text-xs" { (badge.id) }
                }
            }
            @if permissions.update || permissions.delete {
                div class="flex flex-wrap items-start gap-2" {
                    @if permissions.update {
                        details class="rounded-lg border border-neutral-200 open:basis-full" {
                            summary class="cursor-pointer rounded-lg px-3 py-2 font-medium text-sm" { "Edit" }
                            form method="post" enctype="multipart/form-data"
                                action={(base) "/badges/" (badge.id) "?_csrf=" (csrf_token)}
                                class="flex flex-col gap-3 border-neutral-200 border-t p-3" {
                                (badge_fields(&badge.id, Some(badge)))
                                (image_field(&badge.id, false))
                                div { (submit_button("Save changes")) }
                            }
                        }
                    }
                    @if permissions.delete {
                        form method="post" action={(base) "/badges/" (badge.id) "/delete"}
                            onsubmit="return confirm('Delete this badge? Everyone it was given to stops showing it.')" {
                            (csrf_input(csrf_token))
                            (danger_button("Delete badge"))
                        }
                    }
                }
            }
        }
    }
}

fn badge_fields(prefix: &str, badge: Option<&AdminCustomBadge>) -> Markup {
    let name = badge.map_or("", |badge| badge.name.as_str());
    let description = badge
        .and_then(|badge| badge.description.as_deref())
        .unwrap_or("");
    let href = badge.and_then(|badge| badge.href.as_deref()).unwrap_or("");
    html! {
        label class="flex flex-col gap-1" for={"badge-" (prefix) "-name"} {
            span class=(FORM_LABEL_CLASS) { "Name" }
            input type="text" id={"badge-" (prefix) "-name"} name="name" value=(name)
                required maxlength="64" class=(FORM_INPUT_CLASS);
        }
        label class="flex flex-col gap-1" for={"badge-" (prefix) "-description"} {
            span class=(FORM_LABEL_CLASS) { "Description" }
            input type="text" id={"badge-" (prefix) "-description"} name="description"
                value=(description) maxlength="256" class=(FORM_INPUT_CLASS);
            span class="text-neutral-500 text-xs" {
                "Shown under the name when someone hovers over the badge."
            }
        }
        label class="flex flex-col gap-1" for={"badge-" (prefix) "-href"} {
            span class=(FORM_LABEL_CLASS) { "Link" }
            input type="url" id={"badge-" (prefix) "-href"} name="href" value=(href)
                placeholder="https://" class=(FORM_INPUT_CLASS);
            span class="text-neutral-500 text-xs" {
                "Opens when someone clicks the badge. It must start with https://."
            }
        }
    }
}

fn image_field(prefix: &str, required: bool) -> Markup {
    html! {
        label class="flex flex-col gap-1" for={"badge-" (prefix) "-image"} {
            span class=(FORM_LABEL_CLASS) { (if required { "Image" } else { "Replace the image" }) }
            input type="file" id={"badge-" (prefix) "-image"} name="image" accept=(BADGE_IMAGE_TYPES.join(","))
                required[required] class="text-sm"
                onchange=(format!("this.setCustomValidity(this.files[0] && this.files[0].size > {BADGE_IMAGE_MAX_BYTES} ? 'The image must be under 10 MB' : '')"));
            span class="text-neutral-500 text-xs" {
                "Use a square PNG, JPEG, GIF, WebP or SVG image of at least 128 by 128 pixels, under 10 MB. A GIF plays as an animation."
            }
        }
    }
}

fn create_form(config: &AdminConfig, csrf_token: &str) -> Markup {
    card_with_header(
        "Create a badge",
        html! {
            form method="post" enctype="multipart/form-data"
                action={(config.base_path) "/badges?_csrf=" (csrf_token)}
                class="flex flex-col gap-3" {
                label class="flex flex-col gap-1" for="badge-new-type" {
                    span class=(FORM_LABEL_CLASS) { "Given to" }
                    select id="badge-new-type" name="type" class=(FORM_SELECT_CLASS) {
                        option value=(CustomBadgeType::User.as_str()) { "Users" }
                        option value=(CustomBadgeType::Guild.as_str()) { "Guilds" }
                    }
                }
                (badge_fields("new", None))
                (image_field("new", true))
                div { (submit_button("Create badge")) }
            }
        },
    )
}

pub fn custom_badge_list(badges: &[&AdminCustomBadge]) -> Markup {
    html! {
        @if badges.is_empty() {
            p class="text-sm text-neutral-500" { "No badges given." }
        } @else {
            div class="flex flex-wrap gap-3" {
                @for badge in badges {
                    span class="inline-flex items-center gap-2 text-sm text-neutral-800" {
                        img src=(badge.image_url()) alt="" class="h-5 w-5 object-contain";
                        (badge.name)
                    }
                }
            }
        }
    }
}

pub fn custom_badge_picker(badges: &[&AdminCustomBadge], assigned: &[String]) -> Markup {
    html! {
        div class="grid grid-cols-1 gap-2 sm:grid-cols-2" {
            @for badge in badges {
                label class="flex items-center gap-2 text-sm text-neutral-800" {
                    input type="checkbox" name="badge_ids[]" value=(badge.id)
                        checked[assigned.contains(&badge.id)]
                        class="h-4 w-4 rounded border-neutral-300";
                    img src=(badge.image_url()) alt="" class="h-5 w-5 object-contain";
                    (badge.name)
                }
            }
        }
    }
}

pub fn no_badges_defined(config: &AdminConfig, label: &str) -> Markup {
    html! {
        p class="text-sm text-neutral-500" {
            "No " (label) " are defined yet. "
            a href={(config.base_path) "/badges"} class="text-blue-600 underline" { "Open badges" }
        }
    }
}
