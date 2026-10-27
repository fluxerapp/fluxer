// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::templates::pages::instance_config::ConfigForm;
use crate::{
    api::types::{
        AppPublicConfigResponse, BRANDING_ASSET_ACCEPT, BRANDING_ASSET_MAX_IMAGE_CHARS,
        BrandingAssetKind, CUSTOM_CSS_MAX_CHARS,
    },
    config::AdminConfig,
    templates::components::form::{
        Checkbox, FORM_LABEL_CLASS, FORM_TEXTAREA_CLASS, form_actions, submit_button, text_input,
        textarea_input,
    },
    utils::plural::group_digits,
};
use maud::{Markup, PreEscaped, html};

pub const CUSTOM_CSS_FIELD: &str = "app_custom_css";
pub const UPLOAD_LIMIT_MESSAGE_FIELD: &str = "app_upload_limit_message";
pub const TRUSTED_DOMAINS_FIELD: &str = "app_trusted_domains";

const FLUXER_LOGO_SVG: &str = include_str!("../../../static/fluxer-logo-color.svg");
const FLUXER_WORDMARK_SVG: &str = include_str!("../../../static/fluxer-wordmark.svg");

const ASSET_ORDER: [BrandingAssetKind; 7] = [
    BrandingAssetKind::Logo,
    BrandingAssetKind::Wordmark,
    BrandingAssetKind::SignInBackdrop,
    BrandingAssetKind::Icon,
    BrandingAssetKind::Symbol,
    BrandingAssetKind::Favicon,
    BrandingAssetKind::FaviconDark,
];

struct AssetGuidance {
    help: &'static str,
    empty: &'static str,
}

fn asset_guidance(kind: BrandingAssetKind) -> AssetGuidance {
    match kind {
        BrandingAssetKind::Logo => AssetGuidance {
            help: "The large image next to the sign-in form, which is always dark. On a phone it only shows next to the name, when there is no wordmark and the product name is not Fluxer. Use a square or wide PNG or SVG in light colors with a transparent background, at least 256 pixels tall and up to 10 MB.",
            empty: "If you leave it empty, the sign-in page uses the app icon or symbol. Without those, it shows the Fluxer logo while the product name is Fluxer, and no image for any other name.",
        },
        BrandingAssetKind::Wordmark => AssetGuidance {
            help: "The name in your own lettering. It shows under the logo on the sign-in page, at the top of the sign-in page on phones and in the desktop app title bar. Use a wide PNG or SVG with a transparent background, at least 64 pixels tall and up to 10 MB, in a color that works on both light and dark title bars.",
            empty: "If you leave it empty, the product name shows as plain text, or as the Fluxer wordmark while the product name is Fluxer.",
        },
        BrandingAssetKind::SignInBackdrop => AssetGuidance {
            help: "Fills the sign-in page behind the form, darkened so the form stays easy to read. Use a landscape JPEG, PNG, WebP or AVIF, at least 1920 by 1080 pixels and up to 4 MB.",
            empty: "If you leave it empty, the sign-in page has the dark starry background.",
        },
        BrandingAssetKind::Icon => AssetGuidance {
            help: "Shows when someone adds the web app to their home screen and next to the join date on profiles. It also fills in for the logo, symbol and favicon when those are empty. With an app icon or a symbol set, people without a profile picture get a plain default avatar instead of the Fluxer one. Use a square PNG, at least 512 by 512 pixels and up to 10 MB.",
            empty: "If you leave it empty, those places show the Fluxer icon.",
        },
        BrandingAssetKind::Symbol => AssetGuidance {
            help: "A small mark next to the instance name in the account switcher and the instance list. With a symbol or an app icon set, people without a profile picture get a plain default avatar instead of the Fluxer one. Use a square PNG or SVG with a transparent background that is still clear at 16 pixels, up to 10 MB.",
            empty: "If you leave it empty, the lists use the app icon or logo, and a globe if both are empty.",
        },
        BrandingAssetKind::Favicon => AssetGuidance {
            help: "The small image in the browser tab. Use a square PNG, at least 64 by 64 pixels and up to 10 MB.",
            empty: "If you leave it empty, the tab uses the app icon, or the Fluxer icon if that is empty too.",
        },
        BrandingAssetKind::FaviconDark => AssetGuidance {
            help: "Browsers set to a dark theme show this instead of the favicon. Add one when your favicon is too dark to see on a dark tab. Use a square PNG, at least 64 by 64 pixels and up to 10 MB.",
            empty: "If you leave it empty, the tab shows the same favicon in light and dark themes.",
        },
    }
}

pub(super) fn branding_section(
    config: &AdminConfig,
    config_form: ConfigForm<'_>,
    app_public: &AppPublicConfigResponse,
    host: &str,
) -> Markup {
    let base = &config.base_path;
    html! {
        form method="post" action={(base) "/instance-config?action=update_app_public"} data-branding-form {
            (config_form.fields())
            div class="grid grid-cols-1 gap-8 xl:grid-cols-[minmax(0,27rem)_minmax(0,1fr)] xl:items-start" {
                div class="min-w-0 xl:sticky xl:top-6 xl:col-start-2 xl:row-start-1" {
                    (branding_preview(host, &config.image_origins()))
                }
                div class="min-w-0 space-y-6 xl:col-start-1 xl:row-start-1" {
                    div class="flex flex-col gap-2" {
                        (text_input(
                            "app_product_name",
                            "Product Name",
                            &app_public.branding.product_name,
                            "Fluxer",
                        ))
                        p class="text-xs leading-5 text-neutral-500" {
                            "Shows in the browser tab, the instance list and across the app. Without a wordmark, it also shows on the sign-in page and in the desktop app title bar."
                        }
                    }
                    div class="flex flex-col gap-2" {
                        (text_input(
                            "app_theme_color",
                            "Theme Color (hex)",
                            app_public.branding.theme_color.as_deref().unwrap_or(""),
                            "#4641d9",
                        ))
                        p class="text-xs leading-5 text-neutral-500" {
                            "Buttons, links and highlights across the app use this color, and phone browsers use it for their toolbar. Leave it empty to keep Fluxer's indigo."
                        }
                    }
                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        h3 class="text-sm font-semibold text-neutral-900" { "Images" }
                        @for kind in ASSET_ORDER {
                            (branding_asset_field(config, kind, kind.url(&app_public.branding)))
                        }
                    }
                    div class="flex flex-col gap-2 border-t border-neutral-200 pt-6" {
                        (custom_css_field(app_public.branding.custom_css.as_deref().unwrap_or("")))
                        p class="text-xs leading-5 text-neutral-500" {
                            "Applies to every page of the web and desktop app, the sign-in pages included, for everyone on this instance. Each person's own theme and custom CSS load after it and can override it. Class names can change between releases, so prefer the color variables on :root."
                        }
                    }
                    div class="flex flex-col gap-2 border-t border-neutral-200 pt-6" {
                        (textarea_input(
                            UPLOAD_LIMIT_MESSAGE_FIELD,
                            "Upload limit message",
                            "Upload large files to https://files.example.com instead.",
                            app_public.branding.upload_limit_message.as_deref().unwrap_or(""),
                            3,
                            false,
                        ))
                        p class="text-xs leading-5 text-neutral-500" {
                            "Shows when someone picks a file over their upload limit, in place of \"This limit is configured by your instance administrator.\" Markdown links work. Up to 500 characters. Leave it empty to keep the default sentence."
                        }
                    }
                    div class="flex flex-col gap-2 border-t border-neutral-200 pt-6" {
                        (textarea_input(
                            TRUSTED_DOMAINS_FIELD,
                            "Trusted link domains",
                            "example.com\n*.example.com",
                            &app_public.branding.trusted_domains.join("\n"),
                            3,
                            false,
                        ))
                        p class="text-xs leading-5 text-neutral-500" {
                            "Links to these domains open without the external link warning for everyone on this instance. Put one domain on each line, up to 50. Start a line with *. to also cover its subdomains, as in *.example.com. Your instance's own addresses are always trusted."
                        }
                    }
                    div class="grid grid-cols-1 gap-4 border-t border-neutral-200 pt-6 sm:grid-cols-2" {
                        (text_input(
                            "app_status_page_url",
                            "Status page URL",
                            app_public.branding.status_page_url.as_deref().unwrap_or(""),
                            "https://fluxerstatus.com",
                        ))
                        (text_input(
                            "app_status_page_incident_history_url",
                            "Status page history URL",
                            app_public.branding.status_page_incident_history_url.as_deref().unwrap_or(""),
                            "https://fluxerstatus.com/history",
                        ))
                    }
                    div class="space-y-2" {
                        (Checkbox {
                            name: "app_setup_configured",
                            value: "true",
                            label: "Setup complete",
                            checked: app_public.setup.configured,
                            enabled: true,
                        }.render())
                        p class="text-xs text-neutral-500" { "When this is unchecked, the web app shows the self-host setup wizard." }
                    }
                    p class="text-xs text-neutral-500" {
                        "Uploads and removals save right away. Use https URLs for images you link to, and save to apply the name, color, links and setup state."
                    }
                    (form_actions(html! {
                        (submit_button("Save Public App Identity"))
                    }))
                }
            }
        }
        style { (PreEscaped(BRANDING_PREVIEW_STYLE)) }
        script { (PreEscaped(BRANDING_ASSET_SCRIPT)) }
        script { (PreEscaped(BRANDING_PREVIEW_SCRIPT)) }
        script { (PreEscaped(CUSTOM_CSS_COUNT_SCRIPT)) }
    }
}

fn custom_css_count_text(length: usize) -> String {
    format!(
        "{} of {} characters",
        group_digits(length),
        group_digits(CUSTOM_CSS_MAX_CHARS)
    )
}

fn custom_css_field(css: &str) -> Markup {
    let length = css.replace("\r\n", "\n").encode_utf16().count();
    let count_id = format!("{CUSTOM_CSS_FIELD}-count");
    let error_id = format!("{CUSTOM_CSS_FIELD}-error");
    html! {
        div class="flex flex-col gap-2" {
            label for=(CUSTOM_CSS_FIELD) class=(FORM_LABEL_CLASS) { "Custom CSS" }
            textarea id=(CUSTOM_CSS_FIELD) name=(CUSTOM_CSS_FIELD)
                placeholder=":root { --background-primary: #10141c; }" rows="8"
                maxlength=(CUSTOM_CSS_MAX_CHARS)
                aria-describedby={(count_id) " " (error_id)}
                data-custom-css-limit=(CUSTOM_CSS_MAX_CHARS)
                class=(FORM_TEXTAREA_CLASS) {
                (css)
            }
            p id=(count_id) class="text-xs leading-5 text-neutral-500 tabular-nums"
                data-custom-css-count {
                (custom_css_count_text(length))
            }
            p id=(error_id) class="text-xs leading-5 text-red-600"
                data-field-error=(CUSTOM_CSS_FIELD) hidden {}
        }
    }
}

pub fn branding_asset_field_id(kind: BrandingAssetKind) -> String {
    format!("branding-asset-{}", kind.as_str())
}

pub fn branding_asset_field(
    config: &AdminConfig,
    kind: BrandingAssetKind,
    url: Option<&str>,
) -> Markup {
    let name = format!("app_{}_url", kind.as_str());
    let placeholder = match kind {
        BrandingAssetKind::Favicon => "https://example.com/favicon.ico".to_owned(),
        BrandingAssetKind::FaviconDark => "https://example.com/favicon-dark.png".to_owned(),
        _ => format!("https://example.com/{}.png", kind.as_str()),
    };
    let guidance = asset_guidance(kind);
    let failed_note = match url {
        Some(url) if config.shows_image(url) => UNLOADABLE_IMAGE_NOTE,
        _ => UNPREVIEWABLE_IMAGE_NOTE,
    };
    let thumbnail_class = if kind == BrandingAssetKind::SignInBackdrop {
        "h-16 w-28"
    } else {
        "h-16 w-16"
    };
    html! {
        div id=(branding_asset_field_id(kind))
            class="flex flex-col gap-3 rounded-lg border border-neutral-200 p-4"
            data-branding-asset=(kind.as_str())
            data-branding-asset-max-chars=(BRANDING_ASSET_MAX_IMAGE_CHARS) {
            div class="flex flex-col gap-1" {
                h4 class="text-sm font-semibold text-neutral-900" { (kind.label()) }
                p class="text-xs leading-5 text-neutral-500" { (guidance.help) }
            }
            div class="flex items-center gap-3" {
                div class={"flex shrink-0 items-center justify-center overflow-hidden rounded-md border border-neutral-200 " (thumbnail_class)}
                    style=(CHECKERBOARD_STYLE) {
                    @if let Some(url) = url {
                        img src=(url) alt={(kind.label()) " preview"}
                            onerror=(THUMBNAIL_ONERROR)
                            class={"max-h-full max-w-full " (if kind == BrandingAssetKind::SignInBackdrop { "h-full w-full object-cover" } else { "object-contain" })};
                        span class="h-6 w-6 text-neutral-400" data-branding-asset-placeholder hidden {
                            (PreEscaped(IMAGE_PLACEHOLDER_SVG))
                        }
                    }
                }
                div class="flex flex-wrap items-center gap-2" {
                    input type="file" accept=(BRANDING_ASSET_ACCEPT) class="hidden"
                        data-branding-asset-file
                        aria-label={"Upload " (kind.label())};
                    button type="button" data-branding-asset-upload
                        class=(BRANDING_ASSET_BUTTON_CLASS) {
                        @if url.is_some() { "Replace" } @else { "Upload image" }
                    }
                    @if url.is_some() {
                        button type="button" data-branding-asset-remove
                            class=(BRANDING_ASSET_BUTTON_CLASS) {
                            "Remove"
                        }
                    }
                }
            }
            @if url.is_none() {
                p class="text-xs leading-5 text-neutral-500" { (guidance.empty) }
            } @else {
                p class="text-xs leading-5 text-neutral-500" data-branding-asset-unpreviewable hidden {
                    (failed_note)
                }
            }
            (text_input(&name, &format!("{} URL", kind.label()), url.unwrap_or(""), &placeholder))
        }
    }
}

fn branding_preview(host: &str, image_origins: &[String]) -> Markup {
    let image_origins = serde_json::to_string(image_origins).unwrap_or_else(|_| "[]".to_owned());
    html! {
        div class="bp-root" data-branding-preview data-bp-image-origins=(image_origins) {
            div class="bp-tabs" role="tablist" aria-label="Preview" {
                button type="button" role="tab" class="bp-tab" aria-selected="true" data-bp-tab="sign_in" { "Sign-in page" }
                button type="button" role="tab" class="bp-tab" aria-selected="false" tabindex="-1" data-bp-tab="phone" { "Phone" }
                button type="button" role="tab" class="bp-tab" aria-selected="false" tabindex="-1" data-bp-tab="desktop" { "Desktop app" }
                button type="button" role="tab" class="bp-tab" aria-selected="false" tabindex="-1" data-bp-tab="instances" { "Instance list" }
            }
            p class="bp-note" data-bp="unpreviewable-note" hidden { (UNPREVIEWABLE_PREVIEW_NOTE) }
            p class="bp-note" data-bp="unloadable-note" hidden { (UNLOADABLE_PREVIEW_NOTE) }
            div class="bp-stage" {
                div role="tabpanel" data-bp-panel="sign_in" {
                    div class="bp-computer" aria-hidden="true" {
                        div class="bp-chrome" {
                            div class="bp-tabstrip" {
                                div class="bp-browser-tab" {
                                    span class="bp-favicon" data-bp-target="favicon" {
                                        img alt="" data-bp="favicon-img" hidden;
                                        span class="bp-fill" data-bp="favicon-default" { (PreEscaped(FLUXER_LOGO_SVG)) }
                                    }
                                    span class="bp-tab-title" dir="auto" data-bp="name" data-bp-target="name" {}
                                }
                            }
                            div class="bp-address" { span dir="ltr" { (host) } }
                        }
                        div class="bp-page" {
                            div class="bp-bg" data-bp-target="sign_in_backdrop" {
                                img class="bp-bg-img" alt="" data-bp="backdrop-img" hidden;
                                div class="bp-scrim" data-bp="backdrop-scrim" hidden {}
                            }
                            div class="bp-card" data-bp="card" {
                                div class="bp-card-brand" {
                                    span class="bp-card-logo" data-bp="logo-box" data-bp-target="logo" {
                                        img alt="" data-bp="logo-img" hidden;
                                        span class="bp-fill" data-bp="logo-default" { (PreEscaped(FLUXER_LOGO_SVG)) }
                                    }
                                    span class="bp-card-wordmark" data-bp-target="wordmark" {
                                        (wordmark_slots())
                                    }
                                    span class="bp-host" dir="ltr" { (host) }
                                }
                                div class="bp-card-form" { (form_skeleton()) }
                            }
                        }
                    }
                    p class="bp-caption" { "The sign-in page in a browser on a computer. The desktop app shows the same page." }
                }
                div role="tabpanel" data-bp-panel="phone" hidden {
                    div class="bp-phone-stage" {
                        div class="bp-phone" aria-hidden="true" {
                            div class="bp-phone-toolbar" data-bp="toolbar" data-bp-target="theme_color" {
                                span class="bp-phone-address" dir="ltr" { (host) }
                            }
                            div class="bp-phone-page" {
                                div class="bp-bg" data-bp-target="sign_in_backdrop" {
                                    img class="bp-bg-img" alt="" data-bp="backdrop-img" hidden;
                                    div class="bp-scrim bp-scrim-phone" data-bp="backdrop-scrim" hidden {}
                                }
                                div class="bp-phone-content" {
                                    div class="bp-phone-identity" {
                                        span class="bp-phone-logo" data-bp="phone-logo-box" data-bp-target="logo" hidden {
                                            img alt="" data-bp="logo-img" hidden;
                                        }
                                        span class="bp-phone-wordmark" data-bp-target="wordmark" {
                                            (wordmark_slots())
                                        }
                                    }
                                    span class="bp-phone-host" dir="ltr" { (host) }
                                    (form_skeleton())
                                }
                            }
                        }
                    }
                    p class="bp-caption" { "The sign-in page on a phone, in a mobile browser or the Fluxer mobile app." }
                }
                div role="tabpanel" data-bp-panel="desktop" hidden {
                    div class="bp-titlebar-groups" {
                        (titlebar_group("Windows and Linux", false))
                        (titlebar_group("macOS", true))
                    }
                    p class="bp-caption" { "The title bar of the desktop app. The wordmark is on the left on Windows and Linux, and on the right on macOS. The title bar follows the theme each person picks." }
                }
                div role="tabpanel" data-bp-panel="instances" hidden {
                    div class="bp-instance-stage" aria-hidden="true" {
                        div class="bp-instance-row" {
                            span class="bp-instance-mark" data-bp-target="symbol" {
                                img alt="" data-bp="symbol-img" hidden;
                                span class="bp-fill" data-bp="symbol-default" { (PreEscaped(GLOBE_SVG)) }
                            }
                            span class="bp-instance-meta" {
                                span class="bp-instance-name" dir="auto" data-bp="name" data-bp-target="name" {}
                                span class="bp-instance-host" dir="ltr" { (host) }
                            }
                        }
                    }
                    p class="bp-caption" { "People who use more than one instance see this in the account switcher and when they pick an instance to sign in to." }
                }
            }
        }
    }
}

fn wordmark_slots() -> Markup {
    html! {
        img class="bp-wordmark-img" alt="" data-bp="wordmark-img" hidden;
        span class="bp-wordmark-text" dir="auto" data-bp="wordmark-text" hidden {}
        span class="bp-wordmark-svg" data-bp="wordmark-default" { (PreEscaped(FLUXER_WORDMARK_SVG)) }
    }
}

fn form_skeleton() -> Markup {
    html! {
        div class="bp-form" {
            div class="bp-sk-title" {}
            div class="bp-sk-line" {}
            div class="bp-sk-input" {}
            div class="bp-sk-input" {}
            div class="bp-sk-button" data-bp="accent" data-bp-target="theme_color" {}
        }
    }
}

fn titlebar_group(title: &str, macos: bool) -> Markup {
    html! {
        section class="bp-titlebar-group" {
            h4 class="bp-titlebar-title" { (title) }
            div class="bp-titlebar-grid" {
                @for (theme_label, theme_class) in [("Dark theme", "bp-window-dark"), ("Light theme", "bp-window-light")] {
                    figure class="bp-titlebar-figure" {
                        figcaption class="bp-titlebar-label" { (theme_label) }
                        div class={"bp-window " (theme_class)} aria-hidden="true" {
                            div class="bp-titlebar" {
                                @if macos {
                                    span class="bp-lights" { span {} span {} span {} }
                                    span class="bp-spacer" {}
                                    span class="bp-titlebar-brand" data-bp-target="wordmark" { (wordmark_slots()) }
                                } @else {
                                    span class="bp-titlebar-brand" data-bp-target="wordmark" { (wordmark_slots()) }
                                    span class="bp-spacer" {}
                                    span class="bp-controls" { span { "–" } span { "□" } span { "✕" } }
                                }
                            }
                            div class="bp-window-body" { span class="bp-rail" {} span class="bp-sidebar" {} }
                        }
                    }
                }
            }
        }
    }
}

const UNPREVIEWABLE_IMAGE_NOTE: &str = "This image can't be previewed here. The admin panel only shows images stored on this instance.";

const UNLOADABLE_IMAGE_NOTE: &str =
    "This image could not be loaded from this instance. Try uploading it again.";

const UNLOADABLE_PREVIEW_NOTE: &str =
    "Some images could not be loaded from this instance, so a gray square stands in for them.";

const UNPREVIEWABLE_PREVIEW_NOTE: &str = "Some images can't be previewed here. The admin panel only shows images stored on this instance, so a gray square stands in for them.";

const THUMBNAIL_ONERROR: &str = "this.hidden=true;this.closest('[data-branding-asset]').querySelectorAll('[data-branding-asset-placeholder],[data-branding-asset-unpreviewable]').forEach(function(el){el.hidden=false})";

const IMAGE_PLACEHOLDER_SVG: &str = r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" width="24" height="24" fill="currentColor" aria-hidden="true"><path d="M216,40H40A16,16,0,0,0,24,56V200a16,16,0,0,0,16,16H216a16,16,0,0,0,16-16V56A16,16,0,0,0,216,40Zm0,16V158.75l-26.07-26.06a16,16,0,0,0-22.63,0l-20,20-44-44a16,16,0,0,0-22.62,0L40,149.37V56ZM40,172l52-52,80,80H40Zm176,28H194.63l-36-36,20-20L216,181.38V200ZM144,100a12,12,0,1,1,12,12A12,12,0,0,1,144,100Z"/></svg>"#;

const GLOBE_SVG: &str = r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" fill="currentColor"><path d="M128,24h0A104,104,0,1,0,232,128,104.12,104.12,0,0,0,128,24Zm87.62,96H175.79C174,83.49,159.94,57.67,148.41,42.4A88.19,88.19,0,0,1,215.63,120ZM96.23,136h63.54c-2.31,41.61-22.23,67.11-31.77,77C118.45,203.1,98.54,177.6,96.23,136Zm0-16C98.54,78.39,118.46,52.89,128,43c9.55,9.93,29.46,35.43,31.77,77Zm11.36-77.6C96.06,57.67,82,83.49,80.21,120H40.37A88.19,88.19,0,0,1,107.59,42.4ZM40.37,136H80.21c1.82,36.51,15.85,62.33,27.38,77.6A88.19,88.19,0,0,1,40.37,136Zm108,77.6c11.53-15.27,25.56-41.09,27.38-77.6h39.84A88.19,88.19,0,0,1,148.41,213.6Z"/></svg>"#;

const CHECKERBOARD_STYLE: &str = "background-color:#2a2a31;background-image:linear-gradient(45deg,#34343c 25%,transparent 25%),linear-gradient(-45deg,#34343c 25%,transparent 25%),linear-gradient(45deg,transparent 75%,#34343c 75%),linear-gradient(-45deg,transparent 75%,#34343c 75%);background-size:12px 12px;background-position:0 0,0 6px,6px -6px,-6px 0";

const BRANDING_ASSET_BUTTON_CLASS: &str = "inline-flex h-8 shrink-0 items-center justify-center \
     rounded-lg border border-neutral-300 bg-neutral-50 px-3 text-xs font-medium \
     text-neutral-700 transition-all hover:border-neutral-400 hover:text-neutral-900 \
     focus:outline-none focus:ring-2 focus:ring-brand-primary/20 \
     disabled:cursor-not-allowed disabled:opacity-50";

const BRANDING_PREVIEW_STYLE: &str = r#"
.bp-root{display:flex;flex-direction:column;gap:12px;min-width:0}
.bp-tabs{display:flex;gap:4px;padding:4px;border-radius:10px;background:#f4f4f5;border:1px solid #e4e4e7}
.bp-tab{flex:1 1 0;min-width:0;padding:6px 8px;border-radius:7px;font-size:13px;font-weight:600;color:#52525b;line-height:1.3}
.bp-tab:hover{color:#18181b}
.bp-tab[aria-selected="true"]{background:#fff;color:#18181b;box-shadow:0 1px 2px rgb(0 0 0/.08)}
.bp-tab:focus-visible{outline:2px solid #4641d9;outline-offset:1px}
[data-theme="dark"] .bp-tabs{background:#27272a;border-color:#3f3f46}[data-theme="dark"] .bp-tab{color:#a1a1aa}[data-theme="dark"] .bp-tab:hover{color:#fafafa}[data-theme="dark"] .bp-tab[aria-selected="true"]{background:#3f3f46;color:#fafafa;box-shadow:none}[data-theme="dark"] .bp-tab:focus-visible{outline-color:#8b87ff}[data-theme="dark"] .bp-note,[data-theme="dark"] .bp-caption{color:#a1a1aa}[data-theme="dark"] .bp-computer{border-color:#3f3f46}
.bp-stage{min-width:0}
.bp-note{margin:0;font-size:12px;line-height:1.5;color:#71717a}
.bp-caption{margin:10px 0 0;font-size:12px;line-height:1.5;color:#71717a}
.bp-hl{outline:calc(2*var(--u,1px)) dashed #8b87ff;outline-offset:calc(4*var(--u,1px))}
.bp-fill,.bp-fill svg{display:block;width:100%;height:100%}
.bp-computer{--u:calc(100cqw/1280);container-type:inline-size;display:flex;flex-direction:column;overflow:hidden;border:1px solid #d4d4d8;border-radius:10px;background:#202024}
.bp-chrome{background:#202024;color:#e3e3e8}
.bp-tabstrip{display:flex;align-items:flex-end;height:calc(40*var(--u));padding-inline:calc(84*var(--u)) calc(16*var(--u))}
.bp-browser-tab{display:flex;align-items:center;gap:calc(10*var(--u));width:calc(260*var(--u));height:calc(32*var(--u));padding-inline:calc(12*var(--u));border-radius:calc(10*var(--u)) calc(10*var(--u)) 0 0;background:#35353b;font-size:calc(13*var(--u))}
.bp-favicon{display:flex;flex:0 0 auto;width:calc(16*var(--u));height:calc(16*var(--u))}
.bp-favicon img{width:100%;height:100%;object-fit:contain}
.bp-tab-title{flex:1 1 auto;min-width:0;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
.bp-address{display:flex;align-items:center;height:calc(30*var(--u));margin:calc(6*var(--u)) calc(12*var(--u)) calc(8*var(--u));padding-inline:calc(14*var(--u));border-radius:calc(15*var(--u));background:#35353b;color:#c8c8d0;font-size:calc(13*var(--u))}
.bp-page{position:relative;isolation:isolate;display:flex;align-items:center;justify-content:center;height:calc(720*var(--u));overflow:hidden}
.bp-bg{position:absolute;inset:0;z-index:-1;background:radial-gradient(circle at 18% 22%,rgb(226 218 255/.5) 0 1px,transparent 1.5px),radial-gradient(circle at 72% 14%,rgb(255 255 255/.45) 0 1px,transparent 1.5px),radial-gradient(circle at 86% 68%,rgb(213 202 255/.4) 0 1px,transparent 1.5px),radial-gradient(circle at 34% 82%,rgb(255 255 255/.35) 0 1px,transparent 1.5px),linear-gradient(145deg,#05040d 0%,#080616 48%,#0d0a1c 100%)}
.bp-bg.bp-hl{outline-offset:calc(-6*var(--u))}
.bp-bg-img{position:absolute;inset:0;width:100%;height:100%;object-fit:cover}
.bp-scrim{position:absolute;inset:0;background:rgb(5 4 13/.55)}
.bp-scrim-phone{background:rgb(5 4 13/.75)}
.bp-card{display:flex;flex-direction:column;width:calc(512*var(--u));overflow:hidden;border:calc(1*var(--u)) solid rgb(255 255 255/.08);border-radius:calc(8*var(--u));background:#1c1b21;color:#f2f2f5;box-shadow:0 calc(20*var(--u)) calc(64*var(--u)) calc(-24*var(--u)) rgb(0 0 0/.7)}
.bp-card.bp-split{flex-direction:row;width:calc(800*var(--u));min-height:calc(448*var(--u))}
.bp-card-brand{display:flex;flex-direction:column;align-items:center;justify-content:center;gap:calc(10*var(--u));padding:calc(40*var(--u)) calc(56*var(--u)) 0;text-align:center}
.bp-split .bp-card-brand{flex:0 0 calc(288*var(--u));gap:calc(24*var(--u));padding:calc(40*var(--u)) calc(24*var(--u));border-inline-end:calc(1*var(--u)) solid rgb(255 255 255/.08)}
.bp-card-logo{display:flex;align-items:center;justify-content:center;height:calc(128*var(--u));max-width:calc(208*var(--u))}
.bp-card-logo img{height:100%;width:auto;max-width:100%;object-fit:contain}
.bp-card-logo .bp-fill{width:calc(128*var(--u))}
.bp-card-wordmark,.bp-phone-wordmark{display:flex;justify-content:center;max-width:calc(208*var(--u))}
.bp-wordmark-img{display:block;height:calc(32*var(--u));width:auto;max-width:100%;object-fit:contain}
.bp-wordmark-text{font-weight:800;font-size:calc(24*var(--u));line-height:1.1;overflow-wrap:anywhere}
.bp-wordmark-svg,.bp-wordmark-svg svg{display:block;height:calc(32*var(--u));width:auto;color:currentColor}
.bp-host{color:#949ba4;font-size:calc(13*var(--u))}
.bp-split .bp-host{margin-top:calc(-12*var(--u))}
.bp-card-form{flex:1 1 auto;display:flex;flex-direction:column;justify-content:center;padding:calc(40*var(--u))}
.bp-form{display:flex;flex-direction:column;gap:calc(14*var(--u))}
.bp-sk-title{width:55%;height:calc(22*var(--u));border-radius:calc(6*var(--u));background:rgb(242 242 245/.22)}
.bp-sk-line{width:80%;height:calc(12*var(--u));margin-bottom:calc(8*var(--u));border-radius:calc(6*var(--u));background:rgb(242 242 245/.12)}
.bp-sk-input{height:calc(40*var(--u));border-radius:calc(6*var(--u));border:calc(1*var(--u)) solid rgb(255 255 255/.08);background:#121116}
.bp-sk-button{height:calc(40*var(--u));margin-top:calc(6*var(--u));border-radius:calc(6*var(--u));background:#4641d9}
.bp-phone-stage{display:flex;justify-content:center}
.bp-phone{--u:calc(100cqw/390);container-type:inline-size;display:flex;flex-direction:column;width:min(100%,15rem);overflow:hidden;border:6px solid #232327;border-radius:28px;background:#080616}
.bp-phone-toolbar{display:flex;align-items:flex-end;justify-content:center;height:calc(92*var(--u));padding-bottom:calc(10*var(--u));background:#18171f}
.bp-phone-address{padding:calc(6*var(--u)) calc(18*var(--u));border-radius:calc(16*var(--u));background:rgb(255 255 255/.14);color:#f2f2f5;font-size:calc(14*var(--u))}
.bp-phone-page{position:relative;isolation:isolate;height:calc(752*var(--u));overflow:hidden}
.bp-phone-content{display:flex;flex-direction:column;padding:calc(40*var(--u)) calc(24*var(--u));color:#f2f2f5}
.bp-phone-identity{display:flex;align-items:center;justify-content:center;gap:calc(10*var(--u))}
.bp-phone-logo{display:flex;height:calc(28*var(--u));max-width:calc(64*var(--u))}
.bp-phone-logo[hidden]{display:none}
.bp-phone-logo img{height:100%;width:auto;object-fit:contain}
.bp-phone-wordmark{max-width:calc(224*var(--u))}
.bp-phone-host{margin:calc(6*var(--u)) 0 calc(32*var(--u));color:#949ba4;font-size:calc(13*var(--u));text-align:center}
.bp-titlebar-groups{display:flex;flex-direction:column;gap:16px}
.bp-titlebar-group{display:flex;flex-direction:column;gap:8px}
.bp-titlebar-title{margin:0;font-size:13px;font-weight:600;color:#18181b}
.bp-titlebar-grid{display:grid;grid-template-columns:minmax(0,1fr);gap:12px}
.bp-titlebar-figure{display:flex;flex-direction:column;gap:6px;margin:0;min-width:0}
.bp-titlebar-label{font-size:12px;color:#71717a}
.bp-window{--u:calc(100cqw/720);container-type:inline-size;overflow:hidden;border:1px solid #d4d4d8;border-radius:8px}
.bp-window-dark{--tb-bg:#121214;--tb-fg:#b9b9c4;--w-rail:#0c0c0e;--w-side:#18181b;--w-body:#1f1f23}
.bp-window-light{--tb-bg:#e3e5e8;--tb-fg:#4e5058;--w-rail:#d7d9dc;--w-side:#f2f3f5;--w-body:#fff}
.bp-titlebar{display:flex;align-items:center;gap:calc(8*var(--u));height:calc(32*var(--u));padding-inline:calc(12*var(--u));background:var(--tb-bg);color:var(--tb-fg)}
.bp-titlebar-brand{display:flex;align-items:center;opacity:.8}
.bp-titlebar .bp-wordmark-img,.bp-titlebar .bp-wordmark-svg,.bp-titlebar .bp-wordmark-svg svg{height:calc(13*var(--u))}
.bp-titlebar .bp-wordmark-text{font-size:calc(13*var(--u));line-height:1}
.bp-spacer{flex:1 1 auto}
.bp-lights{display:flex;gap:calc(8*var(--u))}
.bp-lights span{width:calc(12*var(--u));height:calc(12*var(--u));border-radius:50%;background:#ff5f57}
.bp-lights span:nth-child(2){background:#febc2e}
.bp-lights span:nth-child(3){background:#28c840}
.bp-controls{display:flex;margin-inline-end:calc(-12*var(--u))}
.bp-controls span{width:calc(46*var(--u));text-align:center;font-size:calc(12*var(--u))}
.bp-window-body{display:flex;height:calc(28*var(--u));background:var(--w-body)}
.bp-rail{width:calc(56*var(--u));background:var(--w-rail)}
.bp-sidebar{width:calc(160*var(--u));background:var(--w-side)}
.bp-instance-stage{display:flex;flex-direction:column;max-width:24rem;padding:8px;border-radius:8px;border:1px solid #2c2b33;background:#1c1b21}
.bp-instance-row{display:flex;align-items:center;gap:12px;padding:10px 12px;border-radius:6px;background:rgb(255 255 255/.05)}
.bp-instance-mark{display:flex;align-items:center;justify-content:center;flex:0 0 auto;width:36px;height:36px;border-radius:8px;background:#121116;color:#b5bac1}
.bp-instance-mark img,.bp-instance-mark .bp-fill{width:20px;height:20px;object-fit:contain}
.bp-instance-meta{display:flex;flex-direction:column;min-width:0}
.bp-instance-name{overflow:hidden;text-overflow:ellipsis;white-space:nowrap;color:#f2f2f5;font-size:15px;font-weight:600}
.bp-instance-host{color:#949ba4;font-size:13px}
.bp-instance-stage .bp-hl{outline-width:2px;outline-offset:2px}
[data-branding-preview] [hidden]{display:none!important}
"#;

const CUSTOM_CSS_COUNT_SCRIPT: &str = r#"
(function () {
	function update(field) {
		var limit = Number(field.getAttribute('data-custom-css-limit'));
		var count = document.getElementById(field.id + '-count');
		var length = field.value.length;
		var over = length > limit;
		var text = length.toLocaleString('en-US') + ' of ' + limit.toLocaleString('en-US') + ' characters';
		if (count) {
			count.textContent = over ? text + '. Shorten it to save.' : text;
			count.classList.toggle('text-red-600', over);
			count.classList.toggle('text-neutral-500', !over);
		}
		field.setCustomValidity(over ? 'Custom CSS can be up to ' + limit.toLocaleString('en-US') + ' characters.' : '');
	}
	document.querySelectorAll('textarea[data-custom-css-limit]').forEach(update);
	if (window.__adminCustomCssCountBound) return;
	window.__adminCustomCssCountBound = true;
	document.addEventListener('input', function (event) {
		if (event.target instanceof HTMLTextAreaElement && event.target.hasAttribute('data-custom-css-limit')) update(event.target);
	});
})();
"#;

const BRANDING_PREVIEW_SCRIPT: &str = r#"
(function () {
	var DEFAULT_NAME = 'Fluxer';
	var PLACEHOLDER = 'data:image/svg+xml,' + encodeURIComponent('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64"><rect width="64" height="64" fill="rgb(82,82,91)"/><path d="M18 44l10-13 8 9 5-6 7 10z" fill="rgb(161,161,170)"/><circle cx="41" cy="23" r="4" fill="rgb(161,161,170)"/></svg>');
	var KINDS = ['logo', 'wordmark', 'sign_in_backdrop', 'icon', 'symbol', 'favicon', 'favicon_dark'];
	var TAB_FOR_TARGET = {name: 'sign_in', theme_color: 'phone', logo: 'sign_in', wordmark: 'sign_in', sign_in_backdrop: 'sign_in', icon: 'sign_in', symbol: 'instances', favicon: 'sign_in', favicon_dark: 'sign_in'};
	function form() {
		return document.querySelector('[data-branding-form]');
	}
	function value(name) {
		var root = form();
		var input = root ? root.querySelector('input[name="' + name + '"]') : null;
		return input ? input.value.trim() : '';
	}
	function each(selector, fn) {
		var root = document.querySelector('[data-branding-preview]');
		if (root) root.querySelectorAll(selector).forEach(fn);
	}
	function setImage(selector, url) {
		each(selector, function (img) {
			if (url) {
				if (img.getAttribute('data-bp-src') !== url) {
					img.setAttribute('data-bp-src', url);
					img.removeAttribute('data-bp-unpreviewable');
					img.removeAttribute('data-bp-unloadable');
					img.setAttribute('src', url);
				}
				img.hidden = false;
			} else {
				img.removeAttribute('data-bp-src');
				img.removeAttribute('data-bp-unpreviewable');
				img.removeAttribute('data-bp-unloadable');
				img.removeAttribute('src');
				img.hidden = true;
			}
		});
	}
	function showUnpreviewableNote() {
		var root = document.querySelector('[data-branding-preview]');
		show('[data-bp="unpreviewable-note"]', Boolean(root && root.querySelector('img[data-bp-unpreviewable]:not([hidden])')));
		show('[data-bp="unloadable-note"]', Boolean(root && root.querySelector('img[data-bp-unloadable]:not([hidden])')));
	}
	function isInstanceImage(url) {
		var root = document.querySelector('[data-branding-preview]');
		var origins = [];
		try {
			origins = JSON.parse((root && root.getAttribute('data-bp-image-origins')) || '[]');
		} catch (error) {
			origins = [];
		}
		try {
			var parsed = new URL(url, window.location.href);
			if (parsed.protocol === 'data:' || parsed.protocol === 'blob:') return true;
			return parsed.origin === window.location.origin || origins.indexOf(parsed.origin) !== -1;
		} catch (error) {
			return false;
		}
	}
	function markUnpreviewable(event) {
		var img = event.target;
		if (!(img instanceof HTMLImageElement) || !img.hasAttribute('data-bp-src') || !img.closest('[data-branding-preview]')) return;
		if (img.hasAttribute('data-bp-unpreviewable') || img.hasAttribute('data-bp-unloadable')) return;
		img.setAttribute(isInstanceImage(img.getAttribute('data-bp-src')) ? 'data-bp-unloadable' : 'data-bp-unpreviewable', '');
		img.setAttribute('src', PLACEHOLDER);
		showUnpreviewableNote();
	}
	function show(selector, visible) {
		each(selector, function (el) {
			el.hidden = !visible;
		});
	}
	function render() {
		if (!document.querySelector('[data-branding-preview]')) return;
		var urls = {};
		KINDS.forEach(function (kind) {
			urls[kind] = value('app_' + kind + '_url');
		});
		var name = value('app_product_name') || DEFAULT_NAME;
		var usesDefaultName = name === DEFAULT_NAME;
		var color = value('app_theme_color');
		var validColor = /^#?[0-9a-fA-F]{6}$/.test(color) ? (color.charAt(0) === '#' ? color : '#' + color) : '';
		var logo = urls.logo || urls.icon || urls.symbol;
		var showBundledLogo = !logo && usesDefaultName;
		var favicon = urls.favicon_dark || urls.favicon || urls.icon;
		var symbol = urls.symbol || urls.icon || urls.logo;
		each('[data-bp="name"]', function (el) {
			if (el.textContent !== name) el.textContent = name;
		});
		setImage('[data-bp="favicon-img"]', favicon);
		show('[data-bp="favicon-default"]', !favicon);
		setImage('[data-bp="logo-img"]', logo);
		show('[data-bp="logo-default"]', showBundledLogo);
		show('[data-bp="logo-box"]', Boolean(logo) || showBundledLogo);
		each('[data-bp="card"]', function (el) {
			el.classList.toggle('bp-split', Boolean(logo) || showBundledLogo);
		});
		show('[data-bp="phone-logo-box"]', !urls.wordmark && !usesDefaultName && Boolean(logo));
		setImage('[data-bp="wordmark-img"]', urls.wordmark);
		each('[data-bp="wordmark-text"]', function (el) {
			if (el.textContent !== name) el.textContent = name;
		});
		show('[data-bp="wordmark-text"]', !urls.wordmark && !usesDefaultName);
		show('[data-bp="wordmark-default"]', !urls.wordmark && usesDefaultName);
		setImage('[data-bp="backdrop-img"]', urls.sign_in_backdrop);
		show('[data-bp="backdrop-scrim"]', Boolean(urls.sign_in_backdrop));
		setImage('[data-bp="symbol-img"]', symbol);
		show('[data-bp="symbol-default"]', !symbol);
		each('[data-bp="accent"]', function (el) {
			el.style.backgroundColor = validColor || '';
		});
		each('[data-bp="toolbar"]', function (el) {
			el.style.backgroundColor = validColor || '';
		});
		showUnpreviewableNote();
	}
	function selectTab(tab) {
		each('[data-bp-tab]', function (button) {
			var selected = button.getAttribute('data-bp-tab') === tab;
			button.setAttribute('aria-selected', selected ? 'true' : 'false');
			button.tabIndex = selected ? 0 : -1;
		});
		each('[data-bp-panel]', function (panel) {
			panel.hidden = panel.getAttribute('data-bp-panel') !== tab;
		});
	}
	function highlight(target) {
		each('[data-bp-target]', function (el) {
			var targets = [el.getAttribute('data-bp-target')];
			if (el.getAttribute('data-bp-target') === 'logo' || el.getAttribute('data-bp-target') === 'favicon') targets.push('icon');
			if (el.getAttribute('data-bp-target') === 'favicon') targets.push('favicon_dark');
			if (el.getAttribute('data-bp-target') === 'wordmark') targets.push('name');
			el.classList.toggle('bp-hl', target !== null && targets.indexOf(target) !== -1);
		});
	}
	function targetOf(element) {
		if (!(element instanceof Element) || !element.closest('[data-branding-form]')) return null;
		var field = element.closest('[data-branding-asset]');
		if (field) return field.getAttribute('data-branding-asset');
		var input = element.closest('input[name="app_product_name"], input[name="app_theme_color"]');
		if (!input) return null;
		return input.name === 'app_product_name' ? 'name' : 'theme_color';
	}
	render();
	if (window.__adminBrandingPreviewBound) return;
	window.__adminBrandingPreviewBound = true;
	document.addEventListener('error', markUnpreviewable, true);
	document.addEventListener('input', function (event) {
		if (event.target instanceof Element && event.target.closest('[data-branding-form]')) render();
	});
	document.addEventListener('focusin', function (event) {
		var target = targetOf(event.target);
		if (target === null) return;
		var preview = document.querySelector('[data-branding-preview]');
		if (preview && preview.parentElement && getComputedStyle(preview.parentElement).position === 'sticky') {
			selectTab(TAB_FOR_TARGET[target] || 'sign_in');
		}
		highlight(target);
	});
	document.addEventListener('focusout', function (event) {
		if (targetOf(event.target) !== null) highlight(null);
	});
	document.addEventListener('click', function (event) {
		var button = event.target instanceof Element ? event.target.closest('[data-bp-tab]') : null;
		if (button) selectTab(button.getAttribute('data-bp-tab'));
	});
	document.addEventListener('keydown', function (event) {
		var button = event.target instanceof Element ? event.target.closest('[data-bp-tab]') : null;
		if (!button || (event.key !== 'ArrowLeft' && event.key !== 'ArrowRight' && event.key !== 'Home' && event.key !== 'End')) return;
		var tabs = Array.prototype.slice.call(button.parentElement.querySelectorAll('[data-bp-tab]'));
		var index = tabs.indexOf(button);
		var next = event.key === 'Home' ? 0 : event.key === 'End' ? tabs.length - 1 : (index + (event.key === 'ArrowRight' ? 1 : -1) + tabs.length) % tabs.length;
		event.preventDefault();
		selectTab(tabs[next].getAttribute('data-bp-tab'));
		tabs[next].focus();
	});
	new MutationObserver(function (records) {
		var fieldChanged = records.some(function (record) {
			return record.target instanceof Element && !record.target.closest('[data-branding-preview]');
		});
		if (fieldChanged) render();
	}).observe(document.body, {childList: true, subtree: true});
})();
"#;

const BRANDING_ASSET_SCRIPT: &str = r#"
(function () {
	if (window.__adminBrandingAssetBound) return;
	window.__adminBrandingAssetBound = true;
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
	function fieldOf(element) {
		return element.closest('[data-branding-asset]');
	}
	function setBusy(field, busy) {
		field.setAttribute('aria-busy', busy ? 'true' : 'false');
		field.querySelectorAll('button').forEach(function (button) {
			button.disabled = busy;
		});
	}
	function send(field, action, image, failure) {
		var form = field.closest('form');
		var csrfInput = form ? form.querySelector('input[name="_csrf"]') : null;
		var token = csrfInput ? csrfInput.value || '' : '';
		var url = new URL(form ? form.getAttribute('action') || window.location.href : window.location.href, window.location.href);
		url.searchParams.set('action', action);
		var body = new URLSearchParams();
		body.set('_csrf', token);
		body.set('kind', field.getAttribute('data-branding-asset') || '');
		if (image !== null) body.set('image', image);
		setBusy(field, true);
		fetch(url.toString(), {
			method: 'POST',
			credentials: 'same-origin',
			headers: {
				'Content-Type': 'application/x-www-form-urlencoded',
				'HX-Request': 'true',
				'HX-Target': field.id,
				'X-CSRF-Token': token
			},
			body: body.toString()
		}).then(function (response) {
			var parsed = toastFromResponse(response);
			if (response.status !== 200) {
				setBusy(field, false);
				toast(parsed ? parsed.level : 'error', parsed ? parsed.message : failure);
				return;
			}
			return response.text().then(function (html) {
				var template = document.createElement('template');
				template.innerHTML = html.trim();
				var replacement = template.content.firstElementChild;
				if (replacement && replacement.id === field.id) {
					field.replaceWith(replacement);
				} else {
					setBusy(field, false);
				}
				if (parsed) toast(parsed.level, parsed.message);
			});
		}).catch(function () {
			setBusy(field, false);
			toast('error', failure);
		});
	}
	document.addEventListener('click', function (event) {
		var target = event.target;
		if (!(target instanceof Element)) return;
		var upload = target.closest('[data-branding-asset-upload]');
		if (upload) {
			var uploadField = fieldOf(upload);
			var file = uploadField ? uploadField.querySelector('[data-branding-asset-file]') : null;
			if (file) file.click();
			return;
		}
		var remove = target.closest('[data-branding-asset-remove]');
		if (remove) {
			var removeField = fieldOf(remove);
			if (removeField) send(removeField, 'remove_branding_asset', null, 'Failed to remove the image');
		}
	});
	document.addEventListener('change', function (event) {
		var input = event.target;
		if (!(input instanceof HTMLInputElement) || !input.hasAttribute('data-branding-asset-file')) return;
		var field = fieldOf(input);
		var selected = input.files && input.files[0];
		input.value = '';
		if (!field || !selected) return;
		var maxChars = Number(field.getAttribute('data-branding-asset-max-chars')) || 0;
		setBusy(field, true);
		var reader = new FileReader();
		reader.onload = function () {
			var dataUrl = typeof reader.result === 'string' ? reader.result : '';
			if (!dataUrl) {
				setBusy(field, false);
				toast('error', 'Failed to read the image');
				return;
			}
			if (maxChars > 0 && dataUrl.length > maxChars) {
				setBusy(field, false);
				toast('error', 'This image is too large to upload');
				return;
			}
			send(field, 'upload_branding_asset', dataUrl, 'Failed to upload the image');
		};
		reader.onerror = function () {
			setBusy(field, false);
			toast('error', 'Failed to read the image');
		};
		reader.readAsDataURL(selected);
	});
})();
"#;
