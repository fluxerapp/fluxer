// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{api::types::AdminUser, utils::user_tag::user_tag};
use maud::{Markup, html};

pub const SYSTEM_USER_ID: &str = "0";

pub fn format_user_display(
    global_name: Option<&str>,
    username: Option<&str>,
    discriminator: Option<&str>,
    is_bot: bool,
) -> String {
    match (global_name, username, discriminator) {
        (Some(gn), Some(un), Some("0")) => format!("{gn} (@{un})"),
        (Some(gn), _, _) => gn.to_owned(),
        (None, Some(un), Some(d)) if d != "0" => user_tag(un, d, is_bot),
        (None, Some(un), _) => format!("@{un}"),
        _ => "Unknown".to_owned(),
    }
}

pub fn user_name(users: &[AdminUser], id: &str) -> String {
    if id == SYSTEM_USER_ID {
        return "System".to_owned();
    }
    users.iter().find(|user| user.id == id).map_or_else(
        || id.to_owned(),
        |user| {
            format_user_display(
                user.global_name.as_deref(),
                Some(&user.username),
                Some(&user.discriminator),
                user.bot,
            )
        },
    )
}

pub fn user_link(base: &str, users: &[AdminUser], id: &str) -> Markup {
    if id == SYSTEM_USER_ID {
        return html! { "System" };
    }
    html! {
        a href={(base) "/users/" (id)} class="text-blue-600 hover:underline" { (user_name(users, id)) }
    }
}
