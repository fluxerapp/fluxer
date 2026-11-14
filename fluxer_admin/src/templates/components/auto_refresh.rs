// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, PreEscaped, html};

pub fn auto_refresh(enabled: bool, interval_ms: u32) -> Markup {
    if !enabled {
        return html! {};
    }
    let safe_interval = interval_ms.max(1000);
    let script = format!("setTimeout(function(){{location.reload();}},{safe_interval});");
    html! {
        script defer { (PreEscaped(script)) }
    }
}
