// SPDX-License-Identifier: AGPL-3.0-or-later

use super::tr;
use crate::{
    i18n::{MarketingI18n, descriptors::*},
    request_context::RequestContext,
};
use maud::{Markup, html};

pub fn pwa_install_trigger(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    html! {
        a
            href="#pwa-modal-backdrop"
            class="body-sm text-gray-500 underline decoration-gray-300 underline-offset-2 transition hover:text-[#4641D9]" {
            (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_LINK_DESCRIPTOR))
        }
    }
}

pub fn pwa_install_modal(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    html! {
        div id="pwa-modal-backdrop" class="pwa-modal-backdrop" {
            div class="pwa-modal" role="dialog" aria-modal="true" aria-labelledby="pwa-modal-title" {
                div class="contents" {
                    div class="flex items-center justify-between p-6 pb-4" {
                        h2 id="pwa-modal-title" class="font-bold text-gray-900 text-xl" {
                            (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_INSTALL_FLUXER_AS_APP_DESCRIPTOR))
                        }
                        a href="#" class="rounded-lg p-2 text-gray-600 hover:bg-gray-100 hover:text-gray-900" id="pwa-close" aria-label=(tr(i18n, ctx, NAVIGATION_CLOSE_DESCRIPTOR)) {
                            svg class="h-5 w-5" fill="none" stroke="currentColor" viewBox="0 0 24 24" {
                                path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" {}
                            }
                        }
                    }
                    fieldset class="contents" {
                        legend class="sr-only" {
                            (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_TITLE_DESCRIPTOR))
                        }
                        input class="pwa-tab-input" type="radio" name="pwa-tab" id="pwa-tab-android" aria-controls="pwa-panel-android" checked;
                        input class="pwa-tab-input" type="radio" name="pwa-tab" id="pwa-tab-ios" aria-controls="pwa-panel-ios";
                        input class="pwa-tab-input" type="radio" name="pwa-tab" id="pwa-tab-desktop" aria-controls="pwa-panel-desktop";
                        div class="pwa-tab-strip px-6" {
                            div class="flex flex-col gap-1 rounded-xl bg-gray-100 p-1 sm:flex-row" {
                                (tab_label("android", tr(i18n, ctx, PLATFORM_SUPPORT_PLATFORMS_ANDROID_NAME_DESCRIPTOR)))
                                (tab_label("ios", tr(i18n, ctx, PLATFORM_SUPPORT_PLATFORMS_IOS_IOS_IPADOS_DESCRIPTOR)))
                                (tab_label("desktop", tr(i18n, ctx, PLATFORM_SUPPORT_DESKTOP_LABEL_DESCRIPTOR)))
                            }
                        }
                        div class="pwa-panels-container flex-1 overflow-y-auto p-6 pt-4" {
                            div id="pwa-panel-android" class="pwa-panel pwa-panel-android" { (android_steps(i18n, ctx)) }
                            div id="pwa-panel-ios" class="pwa-panel pwa-panel-ios" { (ios_steps(i18n, ctx)) }
                            div id="pwa-panel-desktop" class="pwa-panel pwa-panel-desktop" { (desktop_steps(i18n, ctx)) }
                        }
                    }
                    div class="border-gray-100 border-t px-6 py-4 text-center" {
                        p class="text-gray-400 text-xs" {
                            (tr(i18n, ctx, DOWNLOAD_SCREENSHOTS_COURTESY_OF_DESCRIPTOR))
                            a href="https://installpwa.com/" target="_blank" rel="noopener noreferrer" class="text-blue-500 underline hover:text-blue-600" {
                                "installpwa.com"
                            }
                        }
                    }
                }
            }
        }
    }
}

fn tab_label(id: &str, label: String) -> Markup {
    html! {
        label for=(format!("pwa-tab-{id}")) class="pwa-tab flex-1 cursor-pointer rounded-lg px-4 py-2 text-center font-medium text-gray-600 text-sm whitespace-nowrap transition-colors hover:text-gray-900" {
            (label)
        }
    }
}

fn android_steps(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    html! {
        div class="flex flex-col gap-6 md:flex-row" {
            div class="flex justify-center md:w-1/3" { (pwa_image(i18n, ctx, "android", "240", "320", "480")) }
            div class="md:w-2/3" {
                ol class="space-y-4" {
                    (step("1", html! {
                        span {
                            a href="https://web.fluxer.app" target="_blank" rel="noopener noreferrer" class="text-gray-900 underline hover:text-gray-700" {
                                (tr(i18n, ctx, APP_OPEN_OPEN_WEB_APP_DESCRIPTOR))
                            }
                            (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_IN_CHROME_DESCRIPTOR))
                        }
                    }))
                    (step("2", html! { (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_MORE_MENU_DESCRIPTOR)) }))
                    (step("3", html! { (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_INSTALL_APP_DESCRIPTOR)) }))
                    (step("4", html! { (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_DONE_MOBILE_DESCRIPTOR)) }))
                }
            }
        }
    }
}

fn ios_steps(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    html! {
        div class="flex flex-col gap-6 md:flex-row" {
            div class="flex justify-center md:w-1/2" { (pwa_image(i18n, ctx, "ios", "320", "480", "640")) }
            div class="md:w-1/2" {
                ol class="space-y-4" {
                    (step("1", html! {
                        span {
                            a href="https://web.fluxer.app" target="_blank" rel="noopener noreferrer" class="text-gray-900 underline hover:text-gray-700" {
                                (tr(i18n, ctx, APP_OPEN_OPEN_WEB_APP_DESCRIPTOR))
                            }
                            (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_IN_SAFARI_DESCRIPTOR))
                        }
                    }))
                    (step("2", html! { (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_SHARE_BUTTON_DESCRIPTOR)) }))
                    (step("3", html! { (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_ADD_TO_HOME_SCREEN_DESCRIPTOR)) }))
                    (step("4", html! { (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_ADD_UPPER_RIGHT_DESCRIPTOR)) }))
                    (step("5", html! { (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_DONE_MOBILE_DESCRIPTOR)) }))
                }
            }
        }
    }
}

fn desktop_steps(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    html! {
        div class="flex flex-col gap-6 md:flex-row" {
            div class="flex justify-center md:w-1/2" { (pwa_image(i18n, ctx, "desktop", "320", "480", "640")) }
            div class="md:w-1/2" {
                ol class="space-y-4" {
                    (step("1", html! {
                        span {
                            a href="https://web.fluxer.app" target="_blank" rel="noopener noreferrer" class="text-gray-900 underline hover:text-gray-700" {
                                (tr(i18n, ctx, APP_OPEN_OPEN_WEB_APP_DESCRIPTOR))
                            }
                            (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_IN_CHROME_OR_ANOTHER_BROWSER_DESCRIPTOR))
                        }
                    }))
                    (step("2", html! { (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_INSTALL_BUTTON_ADDRESS_BAR_DESCRIPTOR)) }))
                    (step("3", html! { (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_STEPS_PRESS_INSTALL_IN_POPUP_DESCRIPTOR)) }))
                    (step("4", html! { (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_DONE_DESKTOP_DESCRIPTOR)) }))
                }
            }
        }
    }
}

fn step(number: &str, content: Markup) -> Markup {
    html! {
        li class="flex items-start gap-4" {
            div class="flex h-8 w-8 flex-shrink-0 items-center justify-center rounded-full bg-gray-100 font-semibold text-gray-600 text-sm" { (number) }
            div class="pt-1.5 text-left text-gray-700" { (content) }
        }
    }
}

fn pwa_image(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    name: &str,
    small: &str,
    medium: &str,
    large: &str,
) -> Markup {
    let base_path = format!("{}/marketing/pwa-install/{name}", ctx.static_cdn_endpoint);
    let srcset_avif = format!(
        "{base_path}-{small}w.avif 1x, {base_path}-{medium}w.avif 1.5x, {base_path}-{large}w.avif 2x"
    );
    let srcset_webp = format!(
        "{base_path}-{small}w.webp 1x, {base_path}-{medium}w.webp 1.5x, {base_path}-{large}w.webp 2x"
    );
    let srcset_png = format!(
        "{base_path}-{small}w.png 1x, {base_path}-{medium}w.png 1.5x, {base_path}-{large}w.png 2x"
    );
    let alt = i18n.text_with(
        ctx.locale,
        PLATFORM_SUPPORT_MOBILE_INSTALL_AS_APP_GUIDES_PWA_INSTALLATION_GUIDE_DESCRIPTOR,
        &[("name", name)],
    );
    html! {
        picture {
            source type="image/avif" srcset=(srcset_avif);
            source type="image/webp" srcset=(srcset_webp);
            img src=(format!("{base_path}-{medium}w.png")) srcset=(srcset_png) alt=(alt) class="h-auto max-w-full rounded-lg border border-gray-200 shadow-lg";
        }
    }
}
