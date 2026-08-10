// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    LinkReplacement, PageMeta, final_cta, hero_base,
    icons::{Icon, icon},
    layout, message_with_links, tr,
};
use crate::{
    i18n::{MarketingI18n, MarketingMessageDescriptor, descriptors::*},
    pricing::{
        PricingTier, format_price_minor, get_base_currency, get_currency, get_price_minor,
        has_localized_pricing_choice,
    },
    request_context::RequestContext,
};
use maud::{Markup, html};

pub fn plutonium_page(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    let currency = get_currency(&ctx.country_code);
    let base_currency = get_base_currency(&ctx.country_code);
    let monthly_price =
        format_price_minor(get_price_minor(PricingTier::Monthly, currency), currency);
    let yearly_price = format_price_minor(get_price_minor(PricingTier::Yearly, currency), currency);
    let free_price = format_price_minor(0, currency);
    let operator_price =
        format_price_minor(get_price_minor(PricingTier::Operator, currency), currency);
    let base_monthly_price = format_price_minor(
        get_price_minor(PricingTier::Monthly, base_currency),
        base_currency,
    );
    let base_yearly_price = format_price_minor(
        get_price_minor(PricingTier::Yearly, base_currency),
        base_currency,
    );
    let base_monthly_price_with_cadence = format!(
        "{}{}",
        base_monthly_price,
        tr(i18n, ctx, PRICING_AND_TIERS_BILLING_PER_MONTH_DESCRIPTOR)
    );
    let base_yearly_price_with_cadence = format!(
        "{}{}",
        base_yearly_price,
        tr(
            i18n,
            ctx,
            PRICING_AND_TIERS_BILLING_PER_YEAR_FULL_DESCRIPTOR
        )
    );
    let standard_pricing = i18n.text_with(
        ctx.locale,
        PRICING_AND_TIERS_PLUTONIUM_STANDARD_PRICING_AVAILABLE_DESCRIPTOR,
        &[
            ("currency", base_currency.code()),
            ("monthly_price", &base_monthly_price_with_cadence),
            ("yearly_price", &base_yearly_price_with_cadence),
        ],
    );
    layout(
        i18n,
        ctx,
        PageMeta {
            title: tr(i18n, ctx, PRICING_AND_TIERS_PLUTONIUM_TIER_NAME_DESCRIPTOR),
            description: tr(
                i18n,
                ctx,
                PRICING_AND_TIERS_PLUTONIUM_HIGHER_LIMITS_AND_EARLY_ACCESS_DESCRIPTOR,
            ),
            ..Default::default()
        },
        html! {
            (hero_base(
                html! {
                    span style="--fluxer-premium-inner: #4641D9;" {
                        (icon(Icon::FluxerPremium, "h-14 w-14 text-white md:h-18 md:w-18"))
                    }
                },
                tr(i18n, ctx, PRICING_AND_TIERS_PLUTONIUM_TIER_NAME_DESCRIPTOR),
                tr(i18n, ctx, PRICING_AND_TIERS_PLUTONIUM_HIGHER_LIMITS_AND_EARLY_ACCESS_DESCRIPTOR),
                html! {
                    p class="body-lg mx-auto mt-6 mb-10 max-w-3xl text-white/70 md:mb-12" {
                        (tr(i18n, ctx, PRICING_AND_TIERS_PLUTONIUM_BENEFITS_NOTE_OFFICIAL_INSTANCE_ONLY_DESCRIPTOR))
                        @if has_localized_pricing_choice(&ctx.country_code) && base_currency != currency {
                            " " (standard_pricing.clone())
                        }
                    }
                    div class="mb-8 flex flex-col items-center justify-center gap-3 sm:flex-row sm:gap-4" {
                        span class="font-bold text-3xl md:text-4xl" { (monthly_price.clone()) (tr(i18n, ctx, PRICING_AND_TIERS_BILLING_PER_MONTH_DESCRIPTOR)) }
                        span class="text-lg text-white/80" { (tr(i18n, ctx, GENERAL_OR_DESCRIPTOR)) }
                        span class="font-bold text-3xl md:text-4xl" { (yearly_price.clone()) (tr(i18n, ctx, PRICING_AND_TIERS_BILLING_PER_YEAR_SHORT_DESCRIPTOR)) }
                        span class="inline-flex items-center rounded-xl bg-white/20 px-4 py-2 font-semibold text-base backdrop-blur-sm md:text-lg" {
                            (tr(i18n, ctx, PRICING_AND_TIERS_BILLING_SAVE_PERCENT_DESCRIPTOR))
                        }
                    }
                }
            ))
            section class="bg-gradient-to-b from-white to-gray-50 px-6 py-24 text-gray-950 sm:px-8 md:px-12 md:py-40 lg:px-16 xl:px-20" {
                div class="mx-auto max-w-5xl" {
                    h2 class="display mb-16 text-center text-4xl text-black md:mb-20 md:text-5xl lg:text-6xl" {
                        (tr(i18n, ctx, PRICING_AND_TIERS_FREE_COMPARISON_LABEL_DESCRIPTOR))
                    }
                    div class="mx-auto mb-16 grid max-w-4xl grid-cols-1 gap-8 md:mb-20 md:grid-cols-2 md:gap-10" {
                        (free_pricing_card(i18n, ctx, &free_price))
                        (plutonium_pricing_card(i18n, ctx, &monthly_price, &yearly_price))
                    }
                    div class="overflow-x-auto" {
                        table class="w-full border-collapse rounded-lg border border-gray-200" style="table-layout: fixed" {
                            thead class="bg-gray-50" {
                                tr {
                                    th class="label w-1/2 border-gray-200 border-b px-4 py-3 text-left text-black" scope="col" {
                                        (tr(i18n, ctx, MISC_LABELS_FEATURE_DESCRIPTOR))
                                    }
                                    th class="label w-1/4 border-gray-200 border-b px-2 py-3 text-center text-black text-xs sm:px-3 sm:text-sm" scope="col" {
                                        (tr(i18n, ctx, PRICING_AND_TIERS_FREE_LABEL_DESCRIPTOR))
                                    }
                                    th class="label w-1/4 border-gray-200 border-b px-2 py-3 text-center text-[#4641D9] text-xs sm:px-3 sm:text-sm" scope="col" {
                                        (tr(i18n, ctx, PRICING_AND_TIERS_PLUTONIUM_TIER_NAME_DESCRIPTOR))
                                    }
                                }
                            }
                            tbody {
                                @for perk in plutonium_perks() {
                                    (plutonium_perk_row(i18n, ctx, perk))
                                }
                            }
                        }
                    }
                    div class="mt-12 text-center md:mt-16" {
                        a class="label inline-block rounded-xl bg-[#4641D9] px-10 py-5 text-lg text-white shadow-lg transition hover:bg-[#3d38c7] md:px-12 md:py-6 md:text-xl" href=(ctx.app_url("/channels/@me")) {
                            (tr(i18n, ctx, PRICING_AND_TIERS_PLUTONIUM_GET_PLUTONIUM_DESCRIPTOR))
                        }
                    }
                }
            }
            section class="bg-white px-6 py-24 text-gray-950 sm:px-8 md:px-12 md:py-40 lg:px-16 xl:px-20" {
                div class="mx-auto max-w-7xl" {
                    h2 class="display mb-16 text-center text-4xl text-black md:mb-20 md:text-5xl lg:text-6xl" {
                        (tr(i18n, ctx, PRICING_AND_TIERS_PLUTONIUM_GET_MORE_WITH_PLUTONIUM_DESCRIPTOR))
                    }
                    div class="grid grid-cols-1 gap-8 md:grid-cols-2 md:gap-10 lg:grid-cols-3" {
                        @for (feature_icon, title, description, badge) in plutonium_feature_cards() {
                            (plutonium_feature_card(i18n, ctx, feature_icon, title, description, badge))
                        }
                    }
                }
            }
            (self_hosting_section(i18n, ctx, &free_price, &operator_price))
            (final_cta(i18n, ctx))
        },
    )
}

pub fn partners_page(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    layout(
        i18n,
        ctx,
        PageMeta {
            title: tr(i18n, ctx, PARTNER_PROGRAM_BECOME_PARTNER_HEADING_DESCRIPTOR),
            description: tr(i18n, ctx, PARTNER_PROGRAM_DESCRIPTION_DESCRIPTOR),
            ..Default::default()
        },
        html! {
            (hero_base(
                icon(Icon::FluxerPartner, "h-14 w-14 text-white md:h-18 md:w-18"),
                tr(i18n, ctx, PARTNER_PROGRAM_BECOME_PARTNER_HEADING_DESCRIPTOR),
                tr(i18n, ctx, PARTNER_PROGRAM_DESCRIPTION_DESCRIPTOR),
                html! {}
            ))
            section class="bg-white px-6 py-24 text-gray-950 sm:px-8 md:px-12 md:py-40 lg:px-16 xl:px-20" {
                div class="mx-auto max-w-6xl" {
                    div class="mb-12 text-center md:mb-16" {
                        h2 class="display mb-6 text-4xl text-black md:mb-8 md:text-5xl lg:text-6xl" {
                            (tr(i18n, ctx, PARTNER_PROGRAM_PERKS_HEADING_DESCRIPTOR))
                        }
                        p class="lead mx-auto max-w-3xl text-gray-700" {
                            (tr(i18n, ctx, PARTNER_PROGRAM_WHO_ITS_FOR_DESCRIPTOR))
                        }
                    }
                    div class="grid gap-6 md:grid-cols-2 md:gap-8 lg:grid-cols-3" {
                        @for (perk_icon, title, description, coming_soon, href) in partner_perks() {
                            (partner_perk_card(i18n, ctx, perk_icon, title, description, coming_soon, href))
                        }
                    }
                }
            }
            section id="apply" class="gradient-light" {
                div class="gradient-cta rounded-t-3xl" {
                    div class="mx-auto max-w-4xl px-6 py-16 text-center sm:px-8 md:px-12 md:py-24 lg:px-16 xl:px-20" {
                        h2 class="display mb-6 text-4xl md:mb-8 md:text-5xl lg:text-6xl" {
                            (tr(i18n, ctx, PARTNER_PROGRAM_BECOME_PARTNER_READY_PROMPT_DESCRIPTOR))
                        }
                        p class="body-lg mx-auto mb-8 max-w-3xl text-white/90 md:mb-10" {
                            (tr(i18n, ctx, PARTNER_PROGRAM_APPLY_GUIDE_PROMPT_DESCRIPTOR))
                        }
                        a class="inline-flex w-fit items-center justify-center gap-2 rounded-full bg-white px-6 py-3 font-semibold text-[#4641D9] shadow-lg transition-opacity hover:opacity-90" href=(ctx.href("/help/community-programmes")) {
                            (tr(i18n, ctx, PARTNER_PROGRAM_BECOME_PARTNER_CALL_TO_ACTION_DESCRIPTOR))
                            (icon(Icon::ArrowRight, "h-4 w-4 shrink-0"))
                        }
                    }
                }
            }
        },
    )
}

pub fn press_page(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    layout(
        i18n,
        ctx,
        PageMeta {
            title: tr(
                i18n,
                ctx,
                NAVIGATION_PRESS_PRESS_AND_BRAND_ASSETS_DESCRIPTOR,
            ),
            description: tr(i18n, ctx, NAVIGATION_PRESS_DOWNLOAD_ASSETS_INTRO_DESCRIPTOR),
            ..Default::default()
        },
        html! {
            (hero_base(
                icon(Icon::Newspaper, "h-14 w-14 text-white md:h-18 md:w-18"),
                tr(i18n, ctx, NAVIGATION_PRESS_PRESS_AND_BRAND_ASSETS_DESCRIPTOR),
                tr(i18n, ctx, NAVIGATION_PRESS_DOWNLOAD_ASSETS_INTRO_DESCRIPTOR),
                html! {}
            ))
            section class="gradient-light px-6 py-24 text-gray-950 sm:px-8 md:px-12 md:py-40 lg:px-16 xl:px-20" {
                div class="mx-auto max-w-6xl" {
                    div class="mb-16 text-center md:mb-20" {
                        h2 class="display mb-6 text-4xl text-black md:mb-8 md:text-5xl lg:text-6xl" {
                            (tr(i18n, ctx, PRESS_BRANDING_ASSETS_LABEL_DESCRIPTOR))
                        }
                        p class="body-lg mx-auto max-w-3xl text-gray-600" {
                            (tr(i18n, ctx, PRESS_BRANDING_ASSETS_USAGE_GUIDANCE_FULL_LOGO_DESCRIPTION_DESCRIPTOR))
                        }
                    }
                    div class="grid gap-6 md:grid-cols-3 md:gap-8" {
                        (press_asset_card(i18n, ctx, "logo-white", "/marketing/branding/logo-white.svg", PRESS_BRANDING_ASSETS_LOGO_VARIANTS_WHITE_LOGO_DESCRIPTOR, PRESS_BRANDING_ASSETS_USAGE_GUIDANCE_FOR_DARK_BACKGROUNDS_DESCRIPTOR, "bg-[#1a1a1a]"))
                        (press_asset_card(i18n, ctx, "logo-black", "/marketing/branding/logo-black.svg", PRESS_BRANDING_ASSETS_LOGO_VARIANTS_BLACK_LOGO_DESCRIPTOR, PRESS_BRANDING_ASSETS_USAGE_GUIDANCE_FOR_LIGHT_BACKGROUNDS_DESCRIPTOR, "bg-gray-50"))
                        (press_asset_card(i18n, ctx, "logo-color", "/marketing/branding/logo-color.svg", PRESS_BRANDING_ASSETS_LOGO_VARIANTS_COLOR_LOGO_DESCRIPTOR, PRESS_BRANDING_ASSETS_FULL_COLOR_DESCRIPTOR, "bg-gray-50"))
                    }
                }
            }
            section class="bg-white px-6 py-24 text-gray-950 sm:px-8 md:px-12 md:py-40 lg:px-16 xl:px-20" {
                div class="mx-auto max-w-6xl" {
                    div class="mb-16 text-center md:mb-20" {
                        h2 class="display mb-6 text-4xl text-black md:mb-8 md:text-5xl lg:text-6xl" {
                            (tr(i18n, ctx, PRESS_BRANDING_ASSETS_SYMBOL_VARIANTS_LABEL_DESCRIPTOR))
                        }
                        p class="body-lg mx-auto max-w-3xl text-gray-600" {
                            (tr(i18n, ctx, PRESS_BRANDING_ASSETS_USAGE_GUIDANCE_SYMBOL_DESCRIPTION_DESCRIPTOR))
                        }
                    }
                    div class="grid gap-6 md:grid-cols-3 md:gap-8" {
                        (press_asset_card(i18n, ctx, "symbol-white", "/marketing/branding/symbol-white.svg", PRESS_BRANDING_ASSETS_SYMBOL_VARIANTS_WHITE_SYMBOL_DESCRIPTOR, PRESS_BRANDING_ASSETS_USAGE_GUIDANCE_FOR_DARK_BACKGROUNDS_DESCRIPTOR, "bg-[#1a1a1a]"))
                        (press_asset_card(i18n, ctx, "symbol-black", "/marketing/branding/symbol-black.svg", PRESS_BRANDING_ASSETS_SYMBOL_VARIANTS_BLACK_SYMBOL_DESCRIPTOR, PRESS_BRANDING_ASSETS_USAGE_GUIDANCE_FOR_LIGHT_BACKGROUNDS_DESCRIPTOR, "bg-gray-50"))
                        (press_asset_card(i18n, ctx, "symbol-color", "/marketing/branding/symbol-color.svg", PRESS_BRANDING_ASSETS_SYMBOL_VARIANTS_COLOR_SYMBOL_DESCRIPTOR, PRESS_BRANDING_ASSETS_FULL_COLOR_DESCRIPTOR, "bg-gray-50"))
                    }
                }
            }
            section class="bg-gray-50 px-6 py-24 text-gray-950 sm:px-8 md:px-12 md:py-40 lg:px-16 xl:px-20" {
                div class="mx-auto max-w-6xl" {
                    div class="mb-16 text-center md:mb-20" {
                        h2 class="display mb-6 text-4xl text-black md:mb-8 md:text-5xl lg:text-6xl" {
                            (tr(i18n, ctx, PRESS_BRANDING_ASSETS_BRAND_COLORS_HEADING_DESCRIPTOR))
                        }
                        p class="body-lg mx-auto max-w-3xl text-gray-600" {
                            (tr(i18n, ctx, PRESS_BRANDING_ASSETS_PALETTE_DESCRIPTION_DESCRIPTOR))
                        }
                    }
                    div class="grid gap-6 md:grid-cols-3 md:gap-8" {
                    (color_card(i18n, ctx, "#4641D9", PRESS_BRANDING_COLORS_BLUE_DA_BA_DEE_DESCRIPTOR, PRESS_BRANDING_ASSETS_PRIMARY_BRAND_COLOR_DESCRIPTION_DESCRIPTOR))
                        (color_card(i18n, ctx, "#FFFFFF", PRESS_BRANDING_COLORS_WHITE_DESCRIPTOR, PRESS_BRANDING_ASSETS_USAGE_GUIDANCE_DARK_SURFACE_GUIDANCE_DESCRIPTOR))
                        (color_card(i18n, ctx, "#000000", PRESS_BRANDING_COLORS_BLACK_DESCRIPTOR, PRESS_BRANDING_ASSETS_USAGE_GUIDANCE_LIGHT_SURFACE_GUIDANCE_DESCRIPTOR))
                    }
                }
            }
            section class="gradient-light" {
                div class="gradient-cta rounded-t-3xl" {
                    div class="mx-auto max-w-3xl px-6 py-16 text-center sm:px-8 md:px-12 md:py-24 lg:px-16 xl:px-20" {
                        h2 class="display mb-6 text-4xl md:mb-8 md:text-5xl lg:text-6xl" {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_PRESS_PRESS_CONTACT_DESCRIPTOR))
                        }
                        p class="body-lg mx-auto mb-6 max-w-3xl text-white/90 md:mb-8" {
                            @let press_template = i18n.template(ctx.locale, PRESS_BRANDING_CONTACT_STORY_PROMPT_MESSAGE_DESCRIPTOR);
                            (message_with_links(&press_template, &[
                                LinkReplacement {
                                    variable: "email",
                                    text: "press@fluxer.app",
                                    href: "mailto:press@fluxer.app",
                                    class: "text-white underline decoration-white/50 hover:decoration-white",
                                },
                            ]))
                        }
                        p class="body-sm text-white/80" { (tr(i18n, ctx, PRESS_BRANDING_CONTACT_RESPONSE_TIME_DESCRIPTOR)) }
                    }
                }
            }
        },
    )
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PerkStatus {
    Available,
    ComingSoon,
    Beta,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PerkUnit {
    Count,
    Bytes,
    Characters,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PerkValue {
    Boolean(bool),
    Numeric(u64, PerkUnit),
    Text(MarketingMessageDescriptor),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct PlutoniumPerk {
    label: MarketingMessageDescriptor,
    status: PerkStatus,
    free: PerkValue,
    plutonium: PerkValue,
}

const PLUTONIUM_PERKS: &[PlutoniumPerk] = &[
    PlutoniumPerk {
        label: PRICING_AND_TIERS_PLUTONIUM_FEATURES_CUSTOM_4_DIGIT_USERNAME_TAG_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Boolean(false),
        plutonium: PerkValue::Boolean(true),
    },
    PlutoniumPerk {
        label: PRICING_AND_TIERS_PLUTONIUM_FEATURES_PER_COMMUNITY_PROFILES_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Boolean(false),
        plutonium: PerkValue::Boolean(true),
    },
    PlutoniumPerk {
        label: PRICING_AND_TIERS_PLUTONIUM_FEATURES_MESSAGE_SCHEDULING_DESCRIPTOR,
        status: PerkStatus::ComingSoon,
        free: PerkValue::Boolean(false),
        plutonium: PerkValue::Boolean(true),
    },
    PlutoniumPerk {
        label: MISC_LABELS_PROFILE_BADGE_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Boolean(false),
        plutonium: PerkValue::Boolean(true),
    },
    PlutoniumPerk {
        label: PRICING_AND_TIERS_PLUTONIUM_FEATURES_CUSTOM_VIDEO_BACKGROUNDS_DESCRIPTOR,
        status: PerkStatus::Beta,
        free: PerkValue::Numeric(1, PerkUnit::Count),
        plutonium: PerkValue::Numeric(15, PerkUnit::Count),
    },
    PlutoniumPerk {
        label: APP_CUSTOMIZATION_CUSTOM_SOUNDS_ENTRANCE_SOUNDS_DESCRIPTOR,
        status: PerkStatus::Beta,
        free: PerkValue::Boolean(false),
        plutonium: PerkValue::Boolean(true),
    },
    PlutoniumPerk {
        label: APP_COMMUNITIES_TITLE_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Numeric(100, PerkUnit::Count),
        plutonium: PerkValue::Numeric(200, PerkUnit::Count),
    },
    PlutoniumPerk {
        label: PRICING_AND_TIERS_PLUTONIUM_FEATURES_MESSAGE_CHARACTER_LIMIT_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Numeric(2_000, PerkUnit::Characters),
        plutonium: PerkValue::Numeric(4_000, PerkUnit::Characters),
    },
    PlutoniumPerk {
        label: APP_MESSAGING_FEATURES_BOOKMARKED_MESSAGES_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Numeric(50, PerkUnit::Count),
        plutonium: PerkValue::Numeric(300, PerkUnit::Count),
    },
    PlutoniumPerk {
        label: PRICING_AND_TIERS_PLUTONIUM_FEATURES_FILE_UPLOAD_SIZE_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Numeric(25 * 1024 * 1024, PerkUnit::Bytes),
        plutonium: PerkValue::Numeric(500 * 1024 * 1024, PerkUnit::Bytes),
    },
    PlutoniumPerk {
        label: PRICING_AND_TIERS_PLUTONIUM_FEATURES_EMOJI_STICKER_PACKS_DESCRIPTOR,
        status: PerkStatus::ComingSoon,
        free: PerkValue::Boolean(false),
        plutonium: PerkValue::Boolean(true),
    },
    PlutoniumPerk {
        label: PRICING_AND_TIERS_PLUTONIUM_FEATURES_SAVED_MEDIA_DESCRIPTOR,
        status: PerkStatus::Beta,
        free: PerkValue::Numeric(50, PerkUnit::Count),
        plutonium: PerkValue::Numeric(500, PerkUnit::Count),
    },
    PlutoniumPerk {
        label: APP_CUSTOMIZATION_USE_ANIMATED_EMOJIS_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Boolean(true),
        plutonium: PerkValue::Boolean(true),
    },
    PlutoniumPerk {
        label: PRICING_AND_TIERS_PLUTONIUM_FEATURES_GLOBAL_EMOJI_STICKER_ACCESS_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Boolean(false),
        plutonium: PerkValue::Boolean(true),
    },
    PlutoniumPerk {
        label: APP_VOICE_AND_VIDEO_FEATURES_VIDEO_QUALITY_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Text(APP_VOICE_AND_VIDEO_FEATURES_VIDEO_QUALITY_FREE_DESCRIPTOR),
        plutonium: PerkValue::Text(APP_VOICE_AND_VIDEO_FEATURES_VIDEO_QUALITY_PREMIUM_DESCRIPTOR),
    },
    PlutoniumPerk {
        label: APP_CUSTOMIZATION_ANIMATED_PROFILE_ANIMATED_AVATARS_AND_BANNERS_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Boolean(false),
        plutonium: PerkValue::Boolean(true),
    },
    PlutoniumPerk {
        label: BETA_AND_ACCESS_EARLY_ACCESS_LABEL_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Boolean(false),
        plutonium: PerkValue::Boolean(true),
    },
    PlutoniumPerk {
        label: APP_CUSTOMIZATION_CUSTOM_THEMES_DESCRIPTOR,
        status: PerkStatus::Available,
        free: PerkValue::Boolean(true),
        plutonium: PerkValue::Boolean(true),
    },
];

fn free_pricing_card(i18n: &MarketingI18n, ctx: &RequestContext, free_price: &str) -> Markup {
    html! {
        div class="rounded-3xl border-2 border-gray-200 bg-white p-10 text-center shadow-lg md:p-12" {
            h3 class="title mb-4 text-2xl text-black md:text-3xl" {
                (tr(i18n, ctx, PRICING_AND_TIERS_FREE_LABEL_DESCRIPTOR))
            }
            p class="mb-3 font-bold text-4xl text-gray-900 md:text-5xl" { (free_price) }
            p class="body-lg text-gray-600" { (tr(i18n, ctx, PRICING_AND_TIERS_BILLING_FOREVER_DESCRIPTOR)) }
        }
    }
}

fn plutonium_pricing_card(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    monthly_price: &str,
    yearly_price: &str,
) -> Markup {
    html! {
        div class="relative rounded-3xl border-2 border-[#4641D9] bg-gradient-to-br from-[#4641D9]/5 to-[#6b5ce7]/5 p-10 text-center shadow-xl md:p-12" {
            div class="label absolute -top-4 left-1/2 -translate-x-1/2 rounded-xl bg-[#4641D9] px-4 py-2 text-white shadow-md" {
                (tr(i18n, ctx, PRICING_AND_TIERS_BILLING_MOST_POPULAR_DESCRIPTOR))
            }
            h3 class="title mb-4 text-2xl text-black md:text-3xl" {
                (tr(i18n, ctx, PRICING_AND_TIERS_PLUTONIUM_TIER_NAME_DESCRIPTOR))
            }
            p class="mb-3 font-bold text-4xl text-[#4641D9] md:text-5xl" {
                (monthly_price)(tr(i18n, ctx, PRICING_AND_TIERS_BILLING_PER_MONTH_DESCRIPTOR))
            }
            p class="body-lg text-gray-700" {
                (tr(i18n, ctx, GENERAL_OR_DESCRIPTOR)) " " (yearly_price)
                (tr(i18n, ctx, PRICING_AND_TIERS_BILLING_PER_YEAR_FULL_DESCRIPTOR))
            }
        }
    }
}

fn plutonium_perks() -> &'static [PlutoniumPerk] {
    PLUTONIUM_PERKS
}

fn plutonium_perk_row(i18n: &MarketingI18n, ctx: &RequestContext, perk: &PlutoniumPerk) -> Markup {
    let badge = perk_status_badge(perk.status);
    match (perk.free, perk.plutonium) {
        (PerkValue::Boolean(_free_has), PerkValue::Boolean(_plutonium_has))
            if perk.status == PerkStatus::ComingSoon =>
        {
            comparison_value_row(
                i18n,
                ctx,
                perk.label,
                badge,
                tr(i18n, ctx, GENERAL_NOT_AVAILABLE_DESCRIPTOR),
                tr(i18n, ctx, GENERAL_COMING_SOON_LABEL_DESCRIPTOR),
            )
        }
        (PerkValue::Boolean(free_has), PerkValue::Boolean(plutonium_has)) => {
            comparison_check_row(i18n, ctx, perk.label, badge, free_has, plutonium_has)
        }
        (PerkValue::Numeric(free, free_unit), PerkValue::Numeric(plutonium, plutonium_unit)) => {
            comparison_value_row(
                i18n,
                ctx,
                perk.label,
                badge,
                format_perk_numeric_value(free, free_unit),
                format_perk_numeric_value(plutonium, plutonium_unit),
            )
        }
        (PerkValue::Text(free), PerkValue::Text(plutonium)) => comparison_value_row(
            i18n,
            ctx,
            perk.label,
            badge,
            tr(i18n, ctx, free),
            tr(i18n, ctx, plutonium),
        ),
        _ => comparison_value_row(
            i18n,
            ctx,
            perk.label,
            badge,
            tr(i18n, ctx, GENERAL_NOT_AVAILABLE_DESCRIPTOR),
            tr(i18n, ctx, GENERAL_NOT_AVAILABLE_DESCRIPTOR),
        ),
    }
}

fn perk_status_badge(status: PerkStatus) -> Option<MarketingMessageDescriptor> {
    match status {
        PerkStatus::Available => None,
        PerkStatus::ComingSoon => Some(GENERAL_COMING_SOON_LABEL_DESCRIPTOR),
        PerkStatus::Beta => Some(BETA_AND_ACCESS_BETA_LABEL_DESCRIPTOR),
    }
}

fn comparison_value_row(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    feature: MarketingMessageDescriptor,
    badge: Option<MarketingMessageDescriptor>,
    free_value: String,
    plutonium_value: String,
) -> Markup {
    html! {
        tr class="border-gray-100 border-b" {
            th scope="row" class="body px-4 py-3 text-left font-normal text-gray-900" {
                (comparison_feature_label(i18n, ctx, feature, badge))
            }
            td class="body px-2 py-3 text-center text-gray-600 text-xs sm:px-3 sm:text-sm" { (free_value) }
            td class="label px-2 py-3 text-center text-[#4641D9] text-xs sm:px-3 sm:text-sm" { (plutonium_value) }
        }
    }
}

fn comparison_check_row(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    feature: MarketingMessageDescriptor,
    badge: Option<MarketingMessageDescriptor>,
    free_has: bool,
    plutonium_has: bool,
) -> Markup {
    html! {
        tr class="border-gray-100 border-b" {
            th scope="row" class="body px-4 py-3 text-left font-normal text-gray-900" {
                (comparison_feature_label(i18n, ctx, feature, badge))
            }
            td class="px-3 py-3 text-center" {
                (boolean_perk_icon(free_has, "green"))
            }
            td class="px-3 py-3 text-center" {
                (boolean_perk_icon(plutonium_has, "plutonium"))
            }
        }
    }
}

fn comparison_feature_label(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    feature: MarketingMessageDescriptor,
    badge: Option<MarketingMessageDescriptor>,
) -> Markup {
    html! {
        div class="flex flex-wrap items-center gap-2" {
            span { (tr(i18n, ctx, feature)) }
            @if let Some(badge) = badge {
                span class="caption inline-flex items-center rounded-full border border-[#4641D9] px-3 py-1 font-semibold text-[#4641D9] text-xs uppercase" {
                    (tr(i18n, ctx, badge))
                }
            }
        }
    }
}

fn boolean_perk_icon(value: bool, tone: &str) -> Markup {
    let class = if value {
        match tone {
            "plutonium" => "mx-auto h-5 w-5 text-[#4641D9]",
            _ => "mx-auto h-5 w-5 text-green-600",
        }
    } else {
        "mx-auto h-5 w-5 text-gray-400"
    };
    icon(if value { Icon::Check } else { Icon::Cross }, class)
}

fn format_perk_numeric_value(value: u64, unit: PerkUnit) -> String {
    match unit {
        PerkUnit::Bytes => format!("{} MB", value / (1024 * 1024)),
        PerkUnit::Count | PerkUnit::Characters => format_number(value),
    }
}

fn format_number(value: u64) -> String {
    let raw = value.to_string();
    let mut output = String::with_capacity(raw.len() + raw.len() / 3);
    for (index, ch) in raw.chars().rev().enumerate() {
        if index != 0 && index % 3 == 0 {
            output.push(',');
        }
        output.push(ch);
    }
    output.chars().rev().collect()
}

fn plutonium_feature_cards() -> [(
    Icon,
    MarketingMessageDescriptor,
    MarketingMessageDescriptor,
    Option<MarketingMessageDescriptor>,
); 11] {
    [
        (
            Icon::Hash,
            PRICING_AND_TIERS_PLUTONIUM_FEATURES_CUSTOM_USERNAME_TAG_DESCRIPTOR,
            PRICING_AND_TIERS_PLUTONIUM_FEATURES_CHOOSE_CUSTOM_4_DIGIT_TAG_DESCRIPTOR,
            None,
        ),
        (
            Icon::UserCircle,
            PRICING_AND_TIERS_PLUTONIUM_FEATURES_PER_COMMUNITY_PROFILES_DESCRIPTOR,
            APP_PROFILES_IDENTITY_CUSTOMISE_PER_COMMUNITY_DESCRIPTOR,
            None,
        ),
        (
            Icon::CalendarCheck,
            PRICING_AND_TIERS_PLUTONIUM_FEATURES_MESSAGE_SCHEDULING_DESCRIPTOR,
            APP_MESSAGING_FEATURES_MESSAGE_SCHEDULING_DESCRIPTION_DESCRIPTOR,
            Some(GENERAL_COMING_SOON_LABEL_DESCRIPTOR),
        ),
        (
            Icon::Gif,
            APP_CUSTOMIZATION_ANIMATED_PROFILE_ANIMATED_AVATARS_AND_BANNERS_DESCRIPTOR,
            APP_CUSTOMIZATION_ANIMATED_PROFILE_STAND_OUT_ANIMATED_PROFILE_DESCRIPTOR,
            None,
        ),
        (
            Icon::Smiley,
            PRICING_AND_TIERS_PLUTONIUM_FEATURES_GLOBAL_EMOJI_STICKER_ACCESS_DESCRIPTOR,
            APP_CUSTOMIZATION_GLOBAL_EMOJI_AND_STICKER_ACCESS_DESCRIPTOR,
            None,
        ),
        (
            Icon::VideoCamera,
            APP_VOICE_AND_VIDEO_FEATURES_UP_TO_4K_VIDEO_QUALITY_DESCRIPTOR,
            APP_VOICE_AND_VIDEO_FEATURES_STREAM_4K_60FPS_DESCRIPTOR,
            None,
        ),
        (
            Icon::Video,
            PRICING_AND_TIERS_PLUTONIUM_FEATURES_CUSTOM_VIDEO_BACKGROUNDS_DESCRIPTOR,
            APP_VOICE_AND_VIDEO_FEATURES_VIDEO_BACKGROUNDS_DESCRIPTION_DESCRIPTOR,
            Some(BETA_AND_ACCESS_BETA_LABEL_DESCRIPTOR),
        ),
        (
            Icon::UserPlus,
            APP_CUSTOMIZATION_CUSTOM_SOUNDS_ENTRANCE_SOUNDS_DESCRIPTOR,
            APP_CUSTOMIZATION_CUSTOM_SOUNDS_SET_PERSONALIZED_JOIN_SOUNDS_DESCRIPTOR,
            Some(BETA_AND_ACCESS_BETA_LABEL_DESCRIPTOR),
        ),
        (
            Icon::ArrowUp,
            PRICING_AND_TIERS_PLUTONIUM_HIGHER_LIMITS_EVERYWHERE_DESCRIPTOR,
            PRICING_AND_TIERS_PLUTONIUM_FEATURE_HIGHLIGHTS_DESCRIPTOR,
            None,
        ),
        (
            Icon::FluxerPremium,
            MISC_LABELS_PROFILE_BADGE_DESCRIPTOR,
            PRICING_AND_TIERS_PLUTONIUM_SHOW_OFF_STATUS_BADGE_DESCRIPTOR,
            None,
        ),
        (
            Icon::Rocket,
            BETA_AND_ACCESS_EARLY_ACCESS_LABEL_DESCRIPTOR,
            BETA_AND_ACCESS_EARLY_ACCESS_BE_FIRST_TO_TRY_DESCRIPTOR,
            None,
        ),
    ]
}

fn plutonium_feature_card(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    feature_icon: Icon,
    title: MarketingMessageDescriptor,
    description: MarketingMessageDescriptor,
    badge: Option<MarketingMessageDescriptor>,
) -> Markup {
    html! {
        div class="relative rounded-3xl border border-gray-100 bg-gray-50 p-8 shadow-md md:p-10" {
            @if let Some(badge) = badge {
                div class="caption absolute top-4 right-4 rounded-full bg-[#4641D9] px-3 py-1 font-semibold text-xs text-white uppercase shadow-lg" {
                    (tr(i18n, ctx, badge))
                }
            }
            div class="mb-5 inline-flex h-16 w-16 items-center justify-center rounded-2xl bg-gradient-to-br from-[#4641D9]/10 to-[#4641D9]/5 md:h-20 md:w-20" {
                (icon(feature_icon, "h-8 w-8 text-[#4641D9] md:h-10 md:w-10"))
            }
            h3 class="title mb-3 text-xl text-black md:text-2xl" { (tr(i18n, ctx, title)) }
            p class="body-lg text-gray-700 leading-relaxed" { (tr(i18n, ctx, description)) }
        }
    }
}

fn self_hosting_section(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    free_price: &str,
    operator_price: &str,
) -> Markup {
    html! {
        section id="self-hosting" class="bg-gradient-to-b from-gray-50 to-white px-6 py-24 text-gray-950 sm:px-8 md:px-12 md:py-40 lg:px-16 xl:px-20" style="scroll-margin-top: 8rem" {
            div class="mx-auto max-w-7xl" {
                div class="mb-16 text-center md:mb-20" {
                    h2 class="display mb-6 text-4xl text-black md:mb-8 md:text-5xl lg:text-6xl" {
                        (tr(i18n, ctx, PRODUCT_POSITIONING_SELF_HOSTING_LABEL_DESCRIPTOR))
                    }
                    p class="lead mx-auto mb-3 max-w-3xl text-gray-700 text-xl md:text-2xl" {
                        (tr(i18n, ctx, PRODUCT_POSITIONING_FREE_AND_OPEN_SOURCE_DESCRIPTOR))
                    }
                    p class="body-lg mx-auto max-w-3xl text-gray-600" {
                        (tr(i18n, ctx, PRODUCT_POSITIONING_SELF_HOSTING_OPERATOR_PASS_EXPECTATIONS_DESCRIPTOR))
                    }
                }
                div class="mx-auto mb-16 grid max-w-5xl grid-cols-1 gap-10 md:mb-20 md:grid-cols-2 md:gap-12" {
                    div class="rounded-3xl border-2 border-gray-200 bg-white p-10 shadow-lg md:p-12" {
                        div class="mb-4 flex justify-center" {
                            (icon(Icon::Globe, "h-16 w-16 text-gray-400"))
                        }
                        h3 class="title mb-2 text-center text-black text-xl md:text-2xl" {
                            (tr(i18n, ctx, PRODUCT_POSITIONING_SELF_HOSTING_FREE_SELF_HOSTING_DESCRIPTOR)) " "
                        }
                        div class="mb-6 text-center" {
                            span class="display text-4xl text-black md:text-5xl" { (free_price) }
                            span class="body-lg text-gray-600" {
                                (tr(i18n, ctx, PRICING_AND_TIERS_BILLING_PER_FOREVER_DESCRIPTOR))
                            }
                        }
                        div class="mb-6 space-y-3" {
                            (self_hosting_benefit_item(i18n, ctx, MISC_LABELS_UNLIMITED_USERS_DESCRIPTOR))
                            (self_hosting_benefit_item(i18n, ctx, PRICING_AND_TIERS_FREE_FULL_ACCESS_TO_ALL_FEATURES_DESCRIPTOR))
                            (self_hosting_benefit_item(i18n, ctx, PRODUCT_POSITIONING_SELF_HOSTING_CONNECT_FROM_ANY_CLIENT_DESCRIPTOR))
                            (self_hosting_benefit_item(i18n, ctx, PRODUCT_POSITIONING_OPEN_SOURCE_LICENSE_DESCRIPTOR))
                            (self_hosting_benefit_item(i18n, ctx, APP_COMMUNITIES_COMMUNITY_SUPPORT_DESCRIPTOR))
                        }
                    }
                    div class="relative rounded-3xl border-2 border-[#4641D9] bg-white p-10 shadow-xl md:p-12" {
                        div class="label absolute -top-4 left-1/2 -translate-x-1/2 rounded-xl bg-[#4641D9] px-4 py-2 text-white shadow-md" {
                            (tr(i18n, ctx, GENERAL_COMING_SOON_LABEL_DESCRIPTOR))
                        }
                        div class="mb-4 flex justify-center" {
                            (icon(Icon::Globe, "h-16 w-16 text-[#4641D9]"))
                        }
                        h3 class="title mb-2 text-center text-black text-xl md:text-2xl" {
                            (tr(i18n, ctx, PRODUCT_POSITIONING_SELF_HOSTING_OPERATOR_PASS_LABEL_DESCRIPTOR)) " "
                        }
                        div class="mb-6 text-center" {
                            span class="display text-4xl text-black md:text-5xl" { (operator_price) }
                            span class="body-lg block text-gray-600" {
                                (tr(i18n, ctx, PRICING_AND_TIERS_VISIONARY_ONE_TIME_PURCHASE_LABEL_DESCRIPTOR))
                            }
                        }
                        div class="mb-6 space-y-3" {
                            (self_hosting_benefit_item(i18n, ctx, PRICING_AND_TIERS_FREE_EVERYTHING_IN_FREE_PLUS_DESCRIPTOR))
                            (self_hosting_benefit_item(i18n, ctx, PRODUCT_POSITIONING_SELF_HOSTING_OPERATOR_PASS_COMMUNITY_FEEDBACK_DESCRIPTOR))
                            (self_hosting_benefit_item(i18n, ctx, MISC_LABELS_DONATING_WITH_PERKS_DESCRIPTOR))
                            (self_hosting_benefit_item(i18n, ctx, DONATIONS_SUPPORT_FUTURE_DEVELOPMENT_DESCRIPTOR))
                        }
                    }
                }
            }
        }
    }
}

fn self_hosting_benefit_item(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    text: MarketingMessageDescriptor,
) -> Markup {
    html! {
        div class="flex items-start gap-3" {
            (icon(Icon::Check, "mt-0.5 h-5 w-5 shrink-0 text-[#4641D9]"))
            span class="body text-gray-700" { (tr(i18n, ctx, text)) }
        }
    }
}

fn partner_perks() -> [(
    Icon,
    MarketingMessageDescriptor,
    MarketingMessageDescriptor,
    bool,
    Option<&'static str>,
); 12] {
    [
        (
            Icon::FluxerPremium,
            PARTNER_PROGRAM_PERKS_FREE_PLUTONIUM_LABEL_DESCRIPTOR,
            PARTNER_PROGRAM_PERKS_FREE_PLUTONIUM_DESCRIPTION_DESCRIPTOR,
            false,
            Some("/plutonium"),
        ),
        (
            Icon::FluxerPartner,
            PARTNER_PROGRAM_PERKS_PARTNER_BADGE_LABEL_DESCRIPTOR,
            PARTNER_PROGRAM_PERKS_PARTNER_BADGE_DESCRIPTION_DESCRIPTOR,
            false,
            None,
        ),
        (
            Icon::SealCheck,
            APP_COMMUNITIES_VERIFICATION_LABEL_DESCRIPTOR,
            APP_COMMUNITIES_VERIFICATION_VALUE_STATEMENT_DESCRIPTOR,
            false,
            None,
        ),
        (
            Icon::Link,
            PARTNER_PROGRAM_PERKS_CUSTOM_VANITY_URL_LABEL_DESCRIPTOR,
            PARTNER_PROGRAM_PERKS_CUSTOM_VANITY_URL_DESCRIPTION_DESCRIPTOR,
            false,
            None,
        ),
        (
            Icon::FluxerStaff,
            PARTNER_PROGRAM_PERKS_DIRECT_TEAM_ACCESS_LABEL_DESCRIPTOR,
            PARTNER_PROGRAM_PERKS_DIRECT_TEAM_ACCESS_DESCRIPTION_DESCRIPTOR,
            false,
            None,
        ),
        (
            Icon::MagnifyingGlass,
            APP_COMMUNITIES_FEATURED_IN_DISCOVERY_DESCRIPTOR,
            PARTNER_PROGRAM_PERKS_DISCOVERY_VISIBILITY_DESCRIPTOR,
            true,
            None,
        ),
        (
            Icon::Gif,
            APP_CUSTOMIZATION_ANIMATED_PROFILE_ANIMATED_AVATARS_AND_BANNERS_DESCRIPTOR,
            APP_CUSTOMIZATION_ANIMATED_PROFILE_STAND_OUT_ANIMATED_PROFILE_DESCRIPTOR,
            false,
            None,
        ),
        (
            Icon::ArrowUp,
            PARTNER_PROGRAM_PERKS_INCREASED_LIMITS_LABEL_DESCRIPTOR,
            PARTNER_PROGRAM_PERKS_INCREASED_LIMITS_DESCRIPTION_DESCRIPTOR,
            false,
            None,
        ),
        (
            Icon::Rocket,
            BETA_AND_ACCESS_EARLY_ACCESS_LABEL_DESCRIPTOR,
            BETA_AND_ACCESS_EARLY_ACCESS_BE_FIRST_TO_TRY_DESCRIPTOR,
            false,
            None,
        ),
        (
            Icon::Coins,
            PARTNER_PROGRAM_PERKS_CREATOR_MONETIZATION_LABEL_DESCRIPTOR,
            PARTNER_PROGRAM_PERKS_CREATOR_MONETIZATION_DESCRIPTION_DESCRIPTOR,
            true,
            None,
        ),
        (
            Icon::Microphone,
            PARTNER_PROGRAM_PERKS_VIP_VOICE_SERVERS_LABEL_DESCRIPTOR,
            PARTNER_PROGRAM_PERKS_VIP_VOICE_SERVERS_DESCRIPTION_DESCRIPTOR,
            true,
            None,
        ),
        (
            Icon::Tshirt,
            PARTNER_PROGRAM_PERKS_EXCLUSIVE_MERCH_LABEL_DESCRIPTOR,
            PARTNER_PROGRAM_PERKS_EXCLUSIVE_MERCH_DESCRIPTION_DESCRIPTOR,
            true,
            None,
        ),
    ]
}

fn partner_perk_card(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    perk_icon: Icon,
    title: MarketingMessageDescriptor,
    description: MarketingMessageDescriptor,
    coming_soon: bool,
    href: Option<&str>,
) -> Markup {
    html! {
        div class="relative flex h-full flex-col rounded-2xl border border-gray-200/80 bg-white p-6 shadow-lg md:p-7" {
            @if coming_soon {
                div class="caption absolute -top-2 -right-2 rounded-full bg-[#4641D9] px-3 py-1 text-white" {
                    (tr(i18n, ctx, GENERAL_COMING_SOON_LABEL_DESCRIPTOR))
                }
            }
            @if let Some(href) = href {
                a class="caption absolute top-2 right-2 flex items-center gap-1 rounded-full bg-[#4641D9] px-3 py-1 text-white transition hover:bg-[#3832B8]" href=(ctx.href(href)) {
                    (tr(i18n, ctx, PARTNER_PROGRAM_PERKS_SEE_PERKS_DESCRIPTOR))
                    (icon(Icon::ArrowRight, "h-3 w-3"))
                }
            }
            div class="mb-4 inline-flex h-12 w-12 items-center justify-center rounded-xl bg-[#4641D9]/10" {
                (icon(perk_icon, "h-8 w-8 text-[#4641D9]"))
            }
            h3 class="title-sm mb-2 text-black" { (tr(i18n, ctx, title)) }
            p class="body text-gray-600" { (tr(i18n, ctx, description)) }
        }
    }
}

fn press_asset_card(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    asset_id: &str,
    path: &str,
    label: MarketingMessageDescriptor,
    description: MarketingMessageDescriptor,
    background_class: &str,
) -> Markup {
    html! {
        div class="overflow-hidden rounded-2xl border border-gray-200/80 bg-white shadow-lg" {
            div class=(format!("flex aspect-video items-center justify-center p-12 {background_class}")) {
                img class="max-h-32 w-auto" src=(format!("{}{}", ctx.static_cdn_endpoint, path)) alt=(tr(i18n, ctx, label));
            }
            div class="h-px bg-gray-200" {}
            div class="flex items-start justify-between bg-white p-6" {
                div class="flex-1" {
                    h3 class="subtitle mb-2 text-black" { (tr(i18n, ctx, label)) }
                    p class="body-sm text-gray-600" { (tr(i18n, ctx, description)) }
                }
                a class="flex items-center justify-center rounded-lg bg-[#4641D9] p-3 text-white hover:bg-[#3832B8]" href=(ctx.href(&format!("/press/download/{asset_id}"))) download aria-label=(tr(i18n, ctx, DOWNLOAD_DOWNLOAD_DESCRIPTOR)) {
                    (icon(Icon::Download, "h-5 w-5"))
                }
            }
        }
    }
}

fn color_card(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    hex: &str,
    label: MarketingMessageDescriptor,
    description: MarketingMessageDescriptor,
) -> Markup {
    let border = if hex == "#FFFFFF" {
        "border border-gray-200"
    } else {
        ""
    };
    html! {
        div class="overflow-hidden rounded-2xl border border-gray-200/80 bg-white shadow-lg" {
            div class=(format!("h-32 {border}")) style=(format!("background-color: {hex};")) {}
            div class="h-px bg-gray-200" {}
            div class="bg-white p-6" {
                h3 class="title-sm mb-1 text-black" { (tr(i18n, ctx, label)) }
                p class="caption mb-3 font-mono text-gray-500" { (hex) }
                p class="body-sm text-gray-600" { (tr(i18n, ctx, description)) }
            }
        }
    }
}
