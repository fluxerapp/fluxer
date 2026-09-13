// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

#[derive(Clone, Copy)]
pub enum ResourceType {
    User,
    Guild,
}

const RESOURCE_LINK_CLASS: &str = "text-neutral-900 underline decoration-neutral-300 \
                      hover:text-neutral-600 hover:decoration-neutral-500 text-sm";
const NAV_LINK_CLASS: &str = "label rounded-lg border border-neutral-300 bg-white \
                      px-3 py-2 text-neutral-700 transition-colors hover:bg-neutral-50";

impl ResourceType {
    fn path_segment(self) -> &'static str {
        match self {
            ResourceType::User => "users",
            ResourceType::Guild => "guilds",
        }
    }

    fn peek_drawer_id(self) -> &'static str {
        match self {
            ResourceType::User => "user-peek",
            ResourceType::Guild => "guild-peek",
        }
    }

    fn label(self) -> &'static str {
        match self {
            ResourceType::User => "User",
            ResourceType::Guild => "Guild",
        }
    }
}

pub fn resource_link(
    base_path: &str,
    resource_type: ResourceType,
    resource_id: &str,
    display: Markup,
) -> Markup {
    let href = format!(
        "{}/{}/{}",
        base_path,
        resource_type.path_segment(),
        resource_id,
    );
    html! {
        a href=(href) class=(RESOURCE_LINK_CLASS) { (display) }
    }
}

pub fn resource_link_peek(
    base_path: &str,
    resource_type: ResourceType,
    resource_id: &str,
    display: Markup,
    peek_title: Option<&str>,
) -> Markup {
    let href = format!(
        "{}/{}/{}",
        base_path,
        resource_type.path_segment(),
        resource_id,
    );
    let fragment_href = format!("{}/fragment", href);
    let title = peek_title
        .map(|t| t.to_owned())
        .unwrap_or_else(|| format!("{} {}", resource_type.label(), resource_id));
    html! {
        a href=(href)
          class=(RESOURCE_LINK_CLASS)
          data-drawer-open=(resource_type.peek_drawer_id())
          data-drawer-href=(fragment_href)
          hx-get=(fragment_href)
          hx-target={"#" (resource_type.peek_drawer_id()) "-body"}
          hx-swap="innerHTML"
          data-drawer-title=(title) {
            (display)
        }
    }
}

pub fn nav_link(href: &str, content: Markup) -> Markup {
    html! {
        a href=(href) class=(NAV_LINK_CLASS) { (content) }
    }
}
