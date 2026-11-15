// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

pub fn error_alert(error: &str) -> Markup {
    html! {
        div class="rounded-lg border border-red-200 bg-red-50 px-4 py-3 text-sm text-red-800" role="alert" {
            (error)
        }
    }
}
