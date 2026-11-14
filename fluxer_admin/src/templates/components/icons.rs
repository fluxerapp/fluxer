// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, PreEscaped, html};

pub fn paperclip_icon(color: &str) -> Markup {
    html! {
        svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256"
            class={"inline-block h-3 w-3 " (color)}
        {
            (PreEscaped(concat!(
                r#"<rect width="256" height="256" fill="none"/>"#,
                r#"<path d="M108.71,197.23l-5.11,5.11a46.63,46.63,0,0,1-66-.05h0"#,
                r#"a46.63,46.63,0,0,1,.06-65.89L72.4,101.66a46.62,46.62,0,0,1,65.94,0"#,
                r#"h0A46.34,46.34,0,0,1,150.78,124" fill="none" stroke="currentColor""#,
                r#" stroke-linecap="round" stroke-linejoin="round" stroke-width="24"/>"#,
                r#"<path d="M147.29,58.77l5.11-5.11a46.62,46.62,0,0,1,65.94,0h0"#,
                r#"a46.62,46.62,0,0,1,0,65.94L193.94,144,183.6,154.34"#,
                r#"a46.63,46.63,0,0,1-66-.05h0A46.46,46.46,0,0,1,105.22,132""#,
                r#" fill="none" stroke="currentColor" stroke-linecap="round""#,
                r#" stroke-linejoin="round" stroke-width="24"/>"#,
            )))
        }
    }
}

pub fn close_icon() -> Markup {
    html! {
        svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"
            class="h-5 w-5" fill="none" stroke="currentColor"
            stroke-width="2" stroke-linecap="round" stroke-linejoin="round"
            aria-hidden="true"
        {
            (PreEscaped(concat!(
                r#"<line x1="18" y1="6" x2="6" y2="18"/>"#,
                r#"<line x1="6" y1="6" x2="18" y2="18"/>"#,
            )))
        }
    }
}

pub fn spinner_icon() -> Markup {
    html! {
        svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"
            fill="none" stroke="currentColor" stroke-width="2"
            stroke-linecap="round" stroke-linejoin="round"
            class="h-6 w-6 animate-spin" aria-hidden="true"
        {
            (PreEscaped(r#"<path d="M21 12a9 9 0 1 1-6.219-8.56"/>"#))
        }
    }
}
