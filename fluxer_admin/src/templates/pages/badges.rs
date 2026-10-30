// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::{
        AdminCustomBadge, AdminCustomBadgeList, BADGE_IMAGE_MAX_BYTES, BADGE_IMAGE_TYPES,
        BadgeMove, BuiltInBadgeImage, CustomBadgeType,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            empty_state::empty_state_full,
            error_display::error_alert,
            form::{
                FORM_INPUT_CLASS, FORM_LABEL_CLASS, FORM_SELECT_CLASS, csrf_input, danger_button,
                secondary_button, submit_button,
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
            Some("Badges that you give to users and guilds. Users show them on their profile, and guilds show them next to their name. You can make up to 100 badges of each type."),
        ))
        @match badges {
            None => (error_alert("Failed to load badges.")),
            Some(list) => {
                div class="space-y-6" {
                    (badge_section(config, list, CustomBadgeType::User, &permissions, csrf_token))
                    (badge_section(config, list, CustomBadgeType::Guild, &permissions, csrf_token))
                    (built_in_section(config, list, CustomBadgeType::User, &permissions, csrf_token))
                    (built_in_section(config, list, CustomBadgeType::Guild, &permissions, csrf_token))
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
                    @for (index, badge) in badges.iter().enumerate() {
                        (badge_row(config, badge, permissions, csrf_token, BadgePosition {
                            first: index == 0,
                            last: index + 1 == badges.len(),
                        }))
                    }
                }
            }
        },
    )
}

const BADGE_ACTION_BUTTON_CLASS: &str = "rounded-lg border border-neutral-200 px-3 py-2 font-medium text-sm cursor-pointer hover:border-neutral-300";

const GUILD_BADGE_SEAL_PATH: &str = "M240 128C240 138.44 232.49 146.27 225.86 153.18C222.09 157.12 218.19 161.18 216.72 164.75C215.36 168.02 215.28 173.44 215.2 178.69C215.05 188.45 214.89 199.51 207.2 207.2C199.51 214.89 188.45 215.05 178.69 215.2C173.44 215.28 168.02 215.36 164.75 216.72C161.18 218.19 157.12 222.09 153.18 225.86C146.27 232.49 138.44 240 128 240C117.56 240 109.73 232.49 102.82 225.86C98.88 222.09 94.82 218.19 91.25 216.72C87.98 215.36 82.56 215.28 77.31 215.2C67.55 215.05 56.49 214.89 48.8 207.2C41.11 199.51 40.95 188.45 40.8 178.69C40.72 173.44 40.64 168.02 39.28 164.75C37.81 161.18 33.91 157.12 30.14 153.18C23.51 146.27 16 138.44 16 128C16 117.56 23.51 109.73 30.14 102.82C33.91 98.88 37.81 94.82 39.28 91.25C40.64 87.98 40.72 82.56 40.8 77.31C40.95 67.55 41.11 56.49 48.8 48.8C56.49 41.11 67.55 40.95 77.31 40.8C82.56 40.72 87.98 40.64 91.25 39.28C94.82 37.81 98.88 33.91 102.82 30.14C109.73 23.51 117.56 16 128 16C138.44 16 146.27 23.51 153.18 30.14C157.12 33.91 161.18 37.81 164.75 39.28C168.02 40.64 173.44 40.72 178.69 40.8C188.45 40.95 199.51 41.11 207.2 48.8C214.89 56.49 215.05 67.55 215.2 77.31C215.28 82.56 215.36 87.98 216.72 91.25C218.19 94.82 222.09 98.88 225.86 102.82C232.49 109.73 240 117.56 240 128Z";

const VERIFIED_MARK_PATH: &str = "M185.655 109.455L121.455 173.655C119.955 175.154 117.921 175.996 115.8 175.996C113.679 175.996 111.645 175.154 110.145 173.655L74.3349 137.655C72.8373 136.155 71.9961 134.122 71.9961 132.003C71.9961 129.883 72.8373 127.85 74.3349 126.35L84.3349 116.35C85.8342 114.857 87.864 114.019 89.9799 114.019C92.0958 114.019 94.1256 114.857 95.6249 116.35L116 136.11L164.38 88.325C165.879 86.8304 167.91 85.9912 170.027 85.9912C172.145 85.9912 174.175 86.8304 175.675 88.325L185.65 98.095C186.4 98.8384 186.996 99.7229 187.402 100.698C187.809 101.672 188.018 102.718 188.019 103.774C188.019 104.83 187.811 105.876 187.405 106.851C186.999 107.826 186.404 108.711 185.655 109.455Z";

const PARTNERED_MARK_PATH: &str = "M112.732 144.785L110.823 146.945C106.987 150.791 102.1 153.41 96.7796 154.471C91.4591 155.533 85.9441 154.988 80.9322 152.907C75.9203 150.826 71.6365 147.301 68.6226 142.778C65.6087 138.256 64 132.939 64 127.5C64 122.061 65.6087 116.744 68.6226 112.222C71.6365 107.699 75.9203 104.175 80.9322 102.093C85.9441 100.012 91.4591 99.4673 96.7796 100.529C102.1 101.59 106.987 104.209 110.823 108.055L145.177 146.945C149.013 150.791 153.9 153.41 159.22 154.471C164.541 155.533 170.056 154.988 175.068 152.907C180.08 150.826 184.363 147.301 187.377 142.778C190.391 138.256 192 132.939 192 127.5C192 122.061 190.391 116.744 187.377 112.222C184.363 107.699 180.08 104.175 175.068 102.093C170.056 100.012 164.541 99.4673 159.22 100.529C153.9 101.59 149.013 104.209 145.177 108.055L143.268 110.215";

const DISCOVERABLE_MARK_PATH: &str = "M184 168H176V132L177.17 133.17C177.922 133.921 178.941 134.342 180.004 134.341C181.066 134.34 182.084 133.917 182.835 133.165C183.586 132.413 184.007 131.394 184.006 130.332C184.005 129.269 183.582 128.251 182.83 127.5L133.655 78.3401C132.155 76.8409 130.121 75.9988 128 75.9988C125.879 75.9988 123.845 76.8409 122.345 78.3401L73.17 127.5C72.4201 128.251 71.9991 129.268 71.9995 130.329C72 131.39 72.4219 132.408 73.1725 133.158C73.9231 133.907 74.9408 134.328 76.0018 134.328C77.0628 134.328 78.0801 133.906 78.83 133.155L80 132V168H72C70.9391 168 69.9217 168.421 69.1716 169.172C68.4214 169.922 68 170.939 68 172C68 173.061 68.4214 174.078 69.1716 174.828C69.9217 175.579 70.9391 176 72 176H184C185.061 176 186.078 175.579 186.828 174.828C187.579 174.078 188 173.061 188 172C188 170.939 187.579 169.922 186.828 169.172C186.078 168.421 185.061 168 184 168ZM140 168H116V144C116 143.47 116.211 142.961 116.586 142.586C116.961 142.211 117.47 142 118 142H138C138.53 142 139.039 142.211 139.414 142.586C139.789 142.961 140 143.47 140 144V168Z";

fn guild_badge_svg(kind: &str) -> Markup {
    let mark = match kind {
        "guild_partnered" => html! {
            path d=(PARTNERED_MARK_PATH)
                style="fill: none; stroke: var(--guild-badge-icon); stroke-width: 16; stroke-linecap: round; stroke-linejoin: round" {}
        },
        "guild_discoverable" => {
            html! { path d=(DISCOVERABLE_MARK_PATH) style="fill: var(--guild-badge-icon)" {} }
        }
        _ => html! { path d=(VERIFIED_MARK_PATH) style="fill: var(--guild-badge-icon)" {} },
    };
    html! {
        svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" fill="none" aria-hidden="true"
            class="built-in-guild-badge h-10 w-10 shrink-0" {
            path d=(GUILD_BADGE_SEAL_PATH) style="fill: var(--guild-badge-seal)" {}
            (mark)
        }
    }
}

fn built_in_image(config: &AdminConfig, badge: &BuiltInBadgeImage) -> Markup {
    let class = "h-10 w-10 shrink-0 rounded object-contain";
    if let Some(url) = badge.image_url() {
        return html! { img src=(url) alt=(badge.label()) title=(badge.label()) class=(class); };
    }
    match &badge.built_in_asset {
        Some(asset) => html! {
            img src={(config.static_cdn_endpoint.trim_end_matches('/')) "/badges/" (asset) ".webp"}
                alt=(badge.label()) title=(badge.label()) class=(class);
        },
        None => html! { span title=(badge.label()) { (guild_badge_svg(&badge.kind)) } },
    }
}

fn built_in_section(
    config: &AdminConfig,
    list: &AdminCustomBadgeList,
    badge_type: CustomBadgeType,
    permissions: &BadgePermissions,
    csrf_token: &str,
) -> Markup {
    let title = match badge_type {
        CustomBadgeType::User => "Built-in user badges",
        CustomBadgeType::Guild => "Built-in guild badges",
    };
    card_with_header(
        title,
        html! {
            p class="mb-4 text-sm text-neutral-600" {
                "Upload an image to show it everywhere this badge appears. The badge keeps its name and meaning."
            }
            div class="divide-y divide-neutral-100" {
                @for badge in list.built_in_of_type(badge_type) {
                    (built_in_row(config, badge, permissions, csrf_token))
                }
            }
        },
    )
}

fn built_in_row(
    config: &AdminConfig,
    badge: &BuiltInBadgeImage,
    permissions: &BadgePermissions,
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    let prefix = format!("built-in-{}", badge.kind);
    html! {
        div class="flex flex-col gap-3 py-4 first:pt-0 last:pb-0" {
            div class="flex items-center gap-3" {
                (built_in_image(config, badge))
                div class="flex min-w-0 flex-col gap-1" {
                    span class="font-medium text-neutral-900" { (badge.label()) }
                    span class="text-sm text-neutral-600" {
                        (if badge.asset_url.is_some() { "Shows your image" } else { "Shows the built-in image" })
                    }
                }
            }
            @if permissions.update {
                div class="flex flex-wrap items-start gap-2" {
                    details class="rounded-lg border border-neutral-200 open:basis-full" {
                        summary class="cursor-pointer rounded-lg px-3 py-2 font-medium text-sm" { "Replace image" }
                        form method="post" enctype="multipart/form-data"
                            action={(base) "/badges/built-in/" (badge.kind) "?_csrf=" (csrf_token)}
                            class="flex flex-col gap-3 border-neutral-200 border-t p-3" {
                            (image_field(&prefix, true))
                            div { (submit_button("Replace image")) }
                        }
                    }
                    @if badge.asset_url.is_some() {
                        form method="post" action={(base) "/badges/built-in/" (badge.kind) "/reset"} {
                            (csrf_input(csrf_token))
                            button type="submit" class=(BADGE_ACTION_BUTTON_CLASS) { "Use the built-in image" }
                        }
                    }
                }
            }
        }
    }
}

struct BadgePosition {
    first: bool,
    last: bool,
}

fn move_form(
    config: &AdminConfig,
    badge: &AdminCustomBadge,
    direction: BadgeMove,
    csrf_token: &str,
) -> Markup {
    let label = match direction {
        BadgeMove::Up => "Move up",
        BadgeMove::Down => "Move down",
    };
    html! {
        form method="post"
            action={(config.base_path) "/badges/" (badge.id) "/move/" (direction.as_str())} {
            (csrf_input(csrf_token))
            (secondary_button(label))
        }
    }
}

fn badge_row(
    config: &AdminConfig,
    badge: &AdminCustomBadge,
    permissions: &BadgePermissions,
    csrf_token: &str,
    position: BadgePosition,
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
                    @if permissions.update && !position.first {
                        (move_form(config, badge, BadgeMove::Up, csrf_token))
                    }
                    @if permissions.update && !position.last {
                        (move_form(config, badge, BadgeMove::Down, csrf_token))
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
