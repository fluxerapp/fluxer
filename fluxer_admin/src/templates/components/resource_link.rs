// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

#[derive(Clone, Copy)]
pub enum ResourceType {
    User,
    Guild,
}

const RESOURCE_LINK_CLASS: &str = "text-neutral-900 underline decoration-neutral-300 \
                      hover:text-neutral-600 hover:decoration-neutral-500 text-sm";

impl ResourceType {
    fn path_segment(self) -> &'static str {
        match self {
            ResourceType::User => "users",
            ResourceType::Guild => "guilds",
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
