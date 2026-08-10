// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    content::{
        HELP_ARTICLES, HELP_CATEGORIES, HeadingEntry, HelpArticle, HelpCategory, JOBS, JobListing,
        POLICIES, Policy, get_help_category, render_markdown_with_copy_label,
        render_markdown_with_headings_and_copy_label,
    },
    downloads::{LatestDesktopVersions, LatestVersionInfo, format_latest_version_line},
    i18n::{MarketingI18n, MarketingMessageDescriptor, descriptors::*},
    request_context::{Architecture, Platform, RequestContext},
};
use maud::{DOCTYPE, Markup, PreEscaped, html};

mod blog;
mod donations;
mod icons;
mod marketing_pages;
mod pwa;
mod voice_regions;

pub use blog::{blog_page, blog_post_page};
pub use donations::{
    DonatePageOptions, DonationAudience, DonationCurrency, donate_manage_page, donate_page,
    donate_success_page, donation_amount_fieldset_fragment, donation_amount_from_form,
    donation_checkout_error_fragment, manage_message_fragment, swish_payment_fragment,
};
pub use marketing_pages::{partners_page, plutonium_page, press_page};
use voice_regions::{languages_section, voice_region_flag_preload_hrefs, voice_regions_section};

use icons::{Icon, icon};

#[derive(Default)]
pub struct PageMeta {
    pub title: String,
    pub title_format: PageTitleFormat,
    pub description: String,
    pub og_type: OgType,
    pub og_image_url: Option<String>,
    pub published_time: Option<String>,
    pub modified_time: Option<String>,
    pub author: Option<String>,
    pub article_tags: Vec<String>,
    pub json_ld: Vec<String>,
    pub preload_images: Vec<String>,
    pub enable_htmx: bool,
}

#[derive(Default, Clone, Copy)]
pub enum PageTitleFormat {
    BrandTagline,
    #[default]
    Page,
}

#[derive(Default, Clone, Copy)]
pub enum OgType {
    #[default]
    Website,
    Article,
}

impl OgType {
    pub fn as_str(self) -> &'static str {
        match self {
            OgType::Website => "website",
            OgType::Article => "article",
        }
    }
}

impl PageMeta {
    pub fn new(title: String, description: String) -> Self {
        Self {
            title,
            description,
            ..Default::default()
        }
    }

    pub fn article(title: String, description: String) -> Self {
        Self {
            title,
            description,
            og_type: OgType::Article,
            ..Default::default()
        }
    }

    fn formatted_title(&self) -> String {
        if self.title.is_empty() {
            return "Fluxer".to_owned();
        }

        match self.title_format {
            PageTitleFormat::BrandTagline => format!("Fluxer - {}", self.title),
            PageTitleFormat::Page => format!("{} | Fluxer", self.title),
        }
    }
}

pub fn home_page(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    layout(
        i18n,
        ctx,
        PageMeta {
            title: tr(i18n, ctx, GENERAL_TAGLINE_DESCRIPTOR),
            title_format: PageTitleFormat::BrandTagline,
            description: tr(i18n, ctx, PRODUCT_POSITIONING_INTRO_DESCRIPTOR),
            preload_images: voice_region_flag_preload_hrefs(ctx),
            ..Default::default()
        },
        html! {
            (hero(i18n, ctx))
            (features_section(i18n, ctx))
            (voice_regions_section(i18n, ctx))
            (languages_section(i18n, ctx))
            (get_involved_section(i18n, ctx))
            (final_cta(i18n, ctx))
        },
    )
}

pub fn download_page(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    latest_versions: &LatestDesktopVersions,
) -> Markup {
    layout(
        i18n,
        ctx,
        PageMeta {
            title: tr(i18n, ctx, DOWNLOAD_DOWNLOAD_FLUXER_DESCRIPTOR),
            description: tr(
                i18n,
                ctx,
                PLATFORM_SUPPORT_AVAILABILITY_META_DESCRIPTION_DESCRIPTOR,
            ),
            ..Default::default()
        },
        html! {
            main class="bg-white px-6 pt-32 pb-20 text-gray-950 sm:px-8 md:px-12 md:pt-40 md:pb-28 lg:px-16 xl:px-20" {
                div class="mx-auto max-w-4xl" {
                    header class="mb-6 md:mb-8" {
                        h1 class="display text-4xl text-gray-950 md:text-5xl" { (tr(i18n, ctx, DOWNLOAD_DOWNLOAD_FLUXER_DESCRIPTOR)) }
                        p class="lead mt-4 max-w-2xl text-gray-600" {
                            (tr(i18n, ctx, PLATFORM_SUPPORT_AVAILABILITY_SUMMARY_DESCRIPTOR))
                        }
                    }
                    ul class="border-gray-200 border-t" {
                        (download_strip(i18n, ctx, Platform::Windows, latest_versions.windows.as_ref()))
                        (download_strip(i18n, ctx, Platform::Macos, latest_versions.macos.as_ref()))
                        (download_strip(i18n, ctx, Platform::Linux, latest_versions.linux.as_ref()))
                        (mobile_download_row(i18n, ctx, MobileDownload::WebApp))
                        (mobile_download_row(i18n, ctx, MobileDownload::Ios))
                        (mobile_download_row(i18n, ctx, MobileDownload::Android))
                    }
                }
                (pwa::pwa_install_modal(i18n, ctx))
            }
            (final_cta(i18n, ctx))
        },
    )
}

pub fn careers_page(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    layout(
        i18n,
        ctx,
        PageMeta {
            title: tr(
                i18n,
                ctx,
                COMPANY_AND_RESOURCES_CAREERS_CAREERS_AT_FLUXER_DESCRIPTOR,
            ),
            description: tr(
                i18n,
                ctx,
                COMPANY_AND_RESOURCES_CAREERS_HERO_DESCRIPTION_DESCRIPTOR,
            ),
            ..Default::default()
        },
        html! {
            section class="px-6 pt-44 pb-14 text-white sm:px-8 md:px-12 md:pt-56 md:pb-16 lg:px-16 xl:px-20" {
                div class="mx-auto max-w-7xl" {
                    h1 class="display max-w-3xl text-5xl md:text-6xl lg:text-7xl" {
                        (tr(i18n, ctx, COMPANY_AND_RESOURCES_CAREERS_CAREERS_AT_FLUXER_DESCRIPTOR))
                    }
                    p class="mt-5 max-w-2xl text-lg leading-relaxed text-white/80 md:text-xl" {
                        (tr(i18n, ctx, COMPANY_AND_RESOURCES_CAREERS_HERO_DESCRIPTION_DESCRIPTOR))
                    }
                }
            }
            section id="future-roles" class="bg-white px-6 py-16 text-gray-950 sm:px-8 md:px-12 md:py-24 lg:px-16 xl:px-20" {
                div class="mx-auto max-w-7xl" {
                    div class="mb-8 max-w-3xl" {
                        h2 class="display text-3xl text-black md:text-4xl" {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_CAREERS_OPEN_POSITIONS_DESCRIPTOR))
                        }
                        p class="mt-3 text-base leading-relaxed text-gray-600 md:text-lg" {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_CAREERS_OPEN_POSITIONS_DESCRIPTION_DESCRIPTOR))
                        }
                        p class="mt-2 text-sm leading-relaxed text-gray-500" {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_CAREERS_NOT_CURRENTLY_OPEN_NOTICE_DESCRIPTOR))
                        }
                    }
                    div class="grid gap-4 md:grid-cols-2 xl:grid-cols-3" {
                        @for job in JOBS {
                            (job_listing_card(i18n, ctx, *job))
                        }
                    }
                }
            }
        },
    )
}

fn hero_base(icon_markup: Markup, title: String, description: String, extra: Markup) -> Markup {
    html! {
        section class="px-6 pt-48 pb-16 text-white sm:px-8 md:px-12 md:pt-60 md:pb-20 lg:px-16 lg:pb-24 xl:px-20" {
            div class="mx-auto max-w-5xl text-center" {
                div class="mb-8 flex justify-center" {
                    div class="inline-flex h-28 w-28 items-center justify-center rounded-3xl bg-white/10 backdrop-blur-sm md:h-36 md:w-36" {
                        (icon_markup)
                    }
                }
                h1 class="hero mb-8 font-bold text-5xl md:mb-10 md:text-6xl lg:text-7xl" { (title) }
                p class="lead mx-auto max-w-4xl text-xl text-white/90 md:text-2xl" { (description) }
                (extra)
            }
        }
    }
}

fn job_listing_card(i18n: &MarketingI18n, ctx: &RequestContext, job: JobListing) -> Markup {
    html! {
        a class="group flex h-full flex-col rounded-lg border border-gray-200 bg-white p-5 transition hover:border-[#4641D9]/50 focus:outline-none focus-visible:ring-2 focus-visible:ring-[#4641D9] focus-visible:ring-offset-2" href=(ctx.href(&format!("/careers/{}", job.slug))) {
            div class="mb-4 flex items-center justify-between gap-3" {
                span class="text-sm font-medium text-gray-500" { (tr(i18n, ctx, job.department)) }
                span class="text-gray-400 transition group-hover:text-[#4641D9]" aria-hidden="true" { "→" }
            }
            h3 class="font-semibold text-lg text-gray-950 group-hover:text-[#4641D9]" { (tr(i18n, ctx, job.title)) }
            p class="mt-2 flex-1 text-sm leading-relaxed text-gray-600" { (tr(i18n, ctx, job.description)) }
            div class="mt-5 flex items-center gap-2 text-sm text-gray-500" {
                span { (tr(i18n, ctx, job.location)) }
                (inline_separator())
                span { (tr(i18n, ctx, job.employment_type)) }
            }
        }
    }
}

pub fn job_page(i18n: &MarketingI18n, ctx: &RequestContext, job: JobListing) -> Markup {
    let title = tr(i18n, ctx, job.title);
    let description = tr(i18n, ctx, job.description);
    let department = tr(i18n, ctx, job.department);
    let location = tr(i18n, ctx, job.location);
    let employment_type = tr(i18n, ctx, job.employment_type);
    let body = job.body;
    let local_markdown_base = ctx.href("/");
    content_layout(
        i18n,
        ctx,
        PageMeta {
            title: title.clone(),
            description: description.clone(),
            og_type: OgType::Article,
            modified_time: Some(job.posted_date.to_owned()),
            ..Default::default()
        },
        html! {
            section class="mx-auto max-w-4xl" {
                div class="mb-6" {
                    a href=(ctx.href("/careers")) class="text-muted-foreground text-sm transition-colors hover:text-foreground" {
                        "← " (tr(i18n, ctx, COMPANY_AND_RESOURCES_CAREERS_ALL_OPEN_POSITIONS_DESCRIPTOR))
                    }
                }
                header class="mb-10 max-w-3xl space-y-4" {
                    div class="flex flex-wrap items-center gap-2" {
                        span class="rounded-full bg-indigo-50 px-3 py-1 font-medium text-indigo-700 text-xs" { (department) }
                        span class="rounded-full bg-gray-100 px-3 py-1 font-medium text-gray-600 text-xs" { (employment_type) }
                        span class="rounded-full bg-gray-100 px-3 py-1 font-medium text-gray-600 text-xs" { (location) }
                    }
                    h1 class="font-bold text-4xl text-foreground md:text-5xl" { (title) }
                    p class="text-lg leading-relaxed text-muted-foreground" { (description) }
                    p class="text-muted-foreground text-sm" {
                        (tr(i18n, ctx, COMPANY_AND_RESOURCES_CAREERS_POSTED_DESCRIPTOR)) " " (format_long_date(job.posted_date, ctx.locale))
                    }
                    div class="rounded-lg border border-gray-200 bg-gray-50 p-4 text-sm leading-relaxed text-gray-600" {
                        (tr(i18n, ctx, COMPANY_AND_RESOURCES_CAREERS_NOT_CURRENTLY_OPEN_NOTICE_DESCRIPTOR))
                    }
                }
                div class="policy-prose" {
                    (render_markdown_with_copy_label(
                        body,
                        &local_markdown_base,
                        &tr(i18n, ctx, NAVIGATION_COPY_LINK_TO_SECTION_DESCRIPTOR),
                    ))
                }
            }
            (heading_anchor_script())
        },
    )
}

pub(crate) struct LinkReplacement<'a> {
    pub variable: &'a str,
    pub text: &'a str,
    pub href: &'a str,
    pub class: &'a str,
}

pub(crate) fn message_with_links(template: &str, replacements: &[LinkReplacement<'_>]) -> Markup {
    let mut hits: Vec<(usize, usize, &LinkReplacement<'_>)> = Vec::new();
    for replacement in replacements {
        let needle = format!("{{{}}}", replacement.variable);
        if let Some(index) = template.find(&needle) {
            hits.push((index, needle.len(), replacement));
        }
    }
    hits.sort_by_key(|entry| entry.0);

    let mut cursor = 0usize;
    let mut segments: Vec<Markup> = Vec::with_capacity(hits.len() * 2 + 1);
    for (index, length, replacement) in &hits {
        let before = &template[cursor..*index];
        segments.push(html! { (before) });
        segments.push(html! {
            a href=(replacement.href) class=(replacement.class) { (replacement.text) }
        });
        cursor = *index + *length;
    }
    let tail = &template[cursor..];
    segments.push(html! { (tail) });
    html! {
        @for segment in &segments { (segment) }
    }
}

pub fn policy_page(i18n: &MarketingI18n, ctx: &RequestContext, policy: Policy) -> Markup {
    let title = tr(i18n, ctx, policy.title);
    let description = tr(i18n, ctx, policy.description);
    let _category = tr(i18n, ctx, policy.category);
    let copy_link_label = tr(i18n, ctx, NAVIGATION_COPY_LINK_TO_SECTION_DESCRIPTOR);
    let local_markdown_base = ctx.href("/");
    let (rendered, headings) = render_markdown_with_headings_and_copy_label(
        policy.body,
        &local_markdown_base,
        &copy_link_label,
    );
    let related = collect_related_policies(policy);
    let toc_title = tr(i18n, ctx, NAVIGATION_ON_THIS_PAGE_DESCRIPTOR);
    content_layout(
        i18n,
        ctx,
        PageMeta {
            title: title.clone(),
            description: description.clone(),
            og_type: OgType::Article,
            modified_time: policy.last_updated.map(ToOwned::to_owned),
            ..Default::default()
        },
        html! {
            section class="mx-auto max-w-5xl" {
                header class="mb-10 space-y-3" {
                    h1 class="font-bold text-4xl text-foreground" { (title) }
                    @if !description.is_empty() {
                        p class="text-lg text-muted-foreground" { (description) }
                    }
                    @if let Some(last_updated) = policy.last_updated {
                        p class="text-muted-foreground text-sm" {
                            (tr(i18n, ctx, GENERAL_LAST_UPDATED_DESCRIPTOR)) " " (format_long_date(last_updated, ctx.locale))
                        }
                    }
                }
                div class="grid gap-10 lg:grid-cols-[minmax(0,1fr)_220px]" {
                    div id="policy-content" class="policy-prose" { (rendered) }
                    aside id="policy-toc" class="hidden lg:block" {
                        (render_toc(&toc_title, &headings))
                    }
                }
                @if !related.is_empty() {
                    div class="mt-12 border-gray-200/60 border-t pt-8" {
                        h2 class="mb-4 font-semibold text-foreground text-lg" {
                            (tr(i18n, ctx, MISC_LABELS_RELATED_POLICIES_DESCRIPTOR))
                        }
                        div class="grid gap-3 md:grid-cols-2" {
                            @for entry in &related {
                                (render_related_policy(i18n, ctx, *entry))
                            }
                        }
                    }
                }
                (policy_page_script())
            }
        },
    )
}

pub(crate) fn render_toc(title: &str, headings: &[HeadingEntry]) -> Markup {
    let filtered: Vec<&HeadingEntry> = headings.iter().filter(|h| h.level <= 3).collect();
    if filtered.is_empty() {
        return html! {};
    }
    let min_level = filtered.iter().map(|h| h.level).min().unwrap_or(2);
    html! {
        nav class="space-y-2" {
            h2 class="font-semibold text-gray-900 text-sm" { (title) }
            ul class="space-y-1 text-sm" {
                @for heading in &filtered {
                    li style=(format!("margin-left: {}px", (heading.level - min_level) * 12)) {
                        a href=(format!("#{}", heading.id)) data-toc-link=(heading.id) class="toc-link" {
                            (heading.title)
                        }
                    }
                }
            }
        }
    }
}

fn render_related_policy(i18n: &MarketingI18n, ctx: &RequestContext, policy: Policy) -> Markup {
    let url = ctx.href(&format!("/{}", policy.slug));
    let title = tr(i18n, ctx, policy.title);
    let description = tr(i18n, ctx, policy.description);
    html! {
        a href=(url) class="group block py-2 text-muted-foreground text-sm hover:text-foreground" {
            div class="font-medium text-foreground group-hover:text-primary" { (title) }
            @if !description.is_empty() {
                div class="mt-0.5 text-muted-foreground text-sm" { (description) }
            }
        }
    }
}

fn collect_related_policies(policy: Policy) -> Vec<Policy> {
    let others: Vec<Policy> = POLICIES
        .iter()
        .copied()
        .filter(|entry| entry.slug != policy.slug)
        .collect();
    let mut same_category: Vec<Policy> = others
        .iter()
        .copied()
        .filter(|entry| entry.category.key() == policy.category.key())
        .collect();
    let mut fallback: Vec<Policy> = others
        .into_iter()
        .filter(|entry| entry.category.key() != policy.category.key())
        .collect();
    same_category.append(&mut fallback);
    same_category.truncate(4);
    same_category
}

fn policy_page_script() -> Markup {
    html! {
        (heading_anchor_script())
    }
}

pub fn help_page(i18n: &MarketingI18n, ctx: &RequestContext, query: Option<&str>) -> Markup {
    let query = query.unwrap_or_default().trim().to_owned();
    let search_results = if query.is_empty() {
        Vec::new()
    } else {
        HELP_ARTICLES
            .iter()
            .copied()
            .filter(|article| help_article_matches(i18n, ctx, *article, &query))
            .collect::<Vec<_>>()
    };
    content_layout(
        i18n,
        ctx,
        PageMeta {
            title: tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_HELP_CENTER_DESCRIPTOR),
            description: tr(
                i18n,
                ctx,
                COMPANY_AND_RESOURCES_HELP_HELP_CENTER_DESCRIPTION_DESCRIPTOR,
            ),
            ..Default::default()
        },
        html! {
            section class="mx-auto max-w-6xl" {
                header class="mb-12" {
                    div class="max-w-3xl" {
                        h1 class="display text-4xl text-gray-950 md:text-5xl lg:text-6xl" {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_HELP_CENTER_DESCRIPTOR))
                        }
                        p class="mt-5 text-lg leading-relaxed text-muted-foreground" {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_HELP_CENTER_DESCRIPTION_DESCRIPTOR))
                        }
                    }
                    form role="search" action=(ctx.href("/help")) method="get" class="mt-8 max-w-3xl" {
                        label for="help-search-input" class="sr-only" {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_SEARCH_DESCRIPTOR))
                        }
                        div class="flex min-h-16 items-stretch overflow-hidden rounded-2xl border border-gray-200 bg-white p-2 shadow-xl shadow-gray-950/5 ring-1 ring-gray-950/5 transition-colors focus-within:border-gray-300 sm:min-h-[4.5rem]" {
                            div class="pointer-events-none flex shrink-0 items-center justify-center pr-3 pl-4 text-primary sm:pr-4 sm:pl-5" {
                                (icon(Icon::MagnifyingGlassBold, "h-6 w-6 sm:h-7 sm:w-7"))
                            }
                            input
                                id="help-search-input"
                                name="q"
                                type="search"
                                value=(query)
                                placeholder=(tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_SEARCH_PLACEHOLDER_DESCRIPTOR))
                                class="min-w-0 flex-1 bg-transparent py-3 pr-3 text-base font-medium text-gray-950 outline-none placeholder:text-gray-400 sm:py-4 sm:text-lg";
                            button type="submit" class="inline-flex min-w-24 shrink-0 items-center justify-center rounded-xl bg-primary px-5 py-3 font-bold text-base text-white transition hover:bg-primary-600 focus:outline-none sm:min-w-32 sm:px-8 sm:text-lg" {
                                (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_SEARCH_BUTTON_DESCRIPTOR))
                            }
                        }
                    }
                }

                @if !query.is_empty() {
                    section class="mb-14" aria-labelledby="help-search-results-heading" {
                        h2 id="help-search-results-heading" class="display mb-5 text-2xl text-gray-950 md:text-3xl" {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_SEARCH_RESULTS_DESCRIPTOR))
                        }
                        @if search_results.is_empty() {
                            div class="rounded-lg border border-gray-200 bg-gray-50 p-6" {
                                h3 class="font-semibold text-gray-950 text-lg" {
                                    (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_NO_SEARCH_RESULTS_TITLE_DESCRIPTOR))
                                }
                                p class="mt-2 text-gray-600" {
                                    (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_NO_SEARCH_RESULTS_DESCRIPTION_DESCRIPTOR))
                                }
                            }
                        } @else {
                            div class="grid gap-4 md:grid-cols-2" {
                                @for article in &search_results {
                                    (help_result_card(i18n, ctx, *article))
                                }
                            }
                        }
                    }
                }

                section aria-labelledby="help-topics-heading" {
                    h2 id="help-topics-heading" class="display mb-6 text-3xl text-gray-950 md:text-4xl" {
                        @if query.is_empty() {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_ALL_ARTICLES_DESCRIPTOR))
                        } @else {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_BROWSE_BY_TOPIC_DESCRIPTOR))
                        }
                    }
                    div class="grid gap-5 lg:grid-cols-2" {
                        @for category in HELP_CATEGORIES {
                            (help_category_section(i18n, ctx, *category))
                        }
                    }
                }
            }
        },
    )
}

pub fn help_article_page(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    article: HelpArticle,
) -> Markup {
    let copy_link_label = tr(i18n, ctx, NAVIGATION_COPY_LINK_TO_SECTION_DESCRIPTOR);
    let local_markdown_base = ctx.href("/");
    let (rendered, headings) = render_markdown_with_headings_and_copy_label(
        article.body,
        &local_markdown_base,
        &copy_link_label,
    );
    let category = get_help_category(article.category_slug);
    let category_title = category.map(|c| tr(i18n, ctx, c.title)).unwrap_or_default();
    let article_title = tr(i18n, ctx, article.title);
    let article_description = tr(i18n, ctx, article.description);
    let related = collect_related_help_articles(article);
    let toc_title = tr(i18n, ctx, NAVIGATION_ON_THIS_PAGE_DESCRIPTOR);
    content_layout(
        i18n,
        ctx,
        PageMeta {
            title: article_title.clone(),
            description: article_description.clone(),
            og_type: OgType::Article,
            modified_time: Some(article.last_updated.to_owned()),
            ..Default::default()
        },
        html! {
            section class="mx-auto max-w-6xl" {
                div class="mb-6" {
                    a href=(ctx.href("/help")) class="text-muted-foreground text-sm transition-colors hover:text-foreground" {
                        "← " (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_BACK_TO_HELP_CENTER_DESCRIPTOR))
                    }
                }
                header class="mb-10 max-w-3xl space-y-4" {
                    div class="flex flex-wrap items-center gap-2" {
                        @if let Some(category) = category {
                            a href=(ctx.href(&format!("/help#{}", category.slug))) class="rounded-full bg-indigo-50 px-3 py-1 font-medium text-indigo-700 text-xs transition hover:bg-indigo-100" {
                                (category_title)
                            }
                        }
                    }
                    h1 class="font-bold text-4xl text-foreground md:text-5xl" {
                        (article_title)
                    }
                    p class="text-lg leading-relaxed text-muted-foreground" {
                        (article_description)
                    }
                    p class="text-muted-foreground text-sm" {
                        (tr(i18n, ctx, GENERAL_LAST_UPDATED_DESCRIPTOR)) " " (format_long_date(article.last_updated, ctx.locale))
                    }
                }
                div class="grid gap-10 lg:grid-cols-[minmax(0,1fr)_240px]" {
                    article id="policy-content" class="min-w-0" { (rendered) }
                    aside id="policy-toc" class="hidden lg:block" {
                        (render_toc(&toc_title, &headings))
                    }
                }
                @if !related.is_empty() {
                    div class="mt-12 border-gray-200/60 border-t pt-8" {
                        h2 class="mb-4 font-semibold text-foreground text-lg" {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_RELATED_ARTICLES_DESCRIPTOR))
                        }
                        div class="grid gap-3 md:grid-cols-2" {
                            @for entry in &related {
                                (render_related_help_article(i18n, ctx, *entry))
                            }
                        }
                    }
                }
                (policy_page_script())
            }
        },
    )
}

fn help_category_section(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    category: HelpCategory,
) -> Markup {
    let articles = HELP_ARTICLES
        .iter()
        .copied()
        .filter(|article| article.category_slug == category.slug)
        .collect::<Vec<_>>();
    html! {
        section id=(category.slug) class="rounded-lg border border-gray-200 bg-white p-6 shadow-sm" {
            div class="mb-5 flex items-start gap-4" {
                div class="flex h-11 w-11 shrink-0 items-center justify-center rounded-lg bg-primary/10 text-primary" {
                    (icon(help_category_icon(category.slug), "h-5 w-5"))
                }
                div class="min-w-0" {
                    h3 class="font-semibold text-gray-950 text-xl" { (tr(i18n, ctx, category.title)) }
                    p class="mt-1 text-sm leading-relaxed text-gray-600" { (tr(i18n, ctx, category.description)) }
                }
            }
            div class="border-gray-200 border-t" {
                @for article in &articles {
                    (help_article_row(i18n, ctx, *article))
                }
            }
        }
    }
}

fn help_article_row(i18n: &MarketingI18n, ctx: &RequestContext, article: HelpArticle) -> Markup {
    html! {
        a href=(ctx.href(&format!("/help/{}", article.slug))) class="group block border-gray-100 border-b py-4 last:border-b-0 focus:outline-none focus-visible:ring-2 focus-visible:ring-primary focus-visible:ring-offset-2" {
            div class="flex items-start justify-between gap-4" {
                div class="min-w-0" {
                    h4 class="font-semibold text-gray-950 transition group-hover:text-primary" {
                        (tr(i18n, ctx, article.title))
                    }
                    p class="mt-1 text-sm leading-relaxed text-gray-600" {
                        (tr(i18n, ctx, article.description))
                    }
                    p class="mt-2 text-xs text-gray-500" {
                        (tr(i18n, ctx, GENERAL_LAST_UPDATED_DESCRIPTOR)) " " (format_long_date(article.last_updated, ctx.locale))
                    }
                }
                span class="mt-1 text-gray-300 transition group-hover:text-primary" aria-hidden="true" {
                    "→"
                }
            }
        }
    }
}

fn help_result_card(i18n: &MarketingI18n, ctx: &RequestContext, article: HelpArticle) -> Markup {
    let category = get_help_category(article.category_slug);
    html! {
        a href=(ctx.href(&format!("/help/{}", article.slug))) class="group block rounded-lg border border-gray-200 bg-white p-5 shadow-sm transition hover:border-primary/40 hover:shadow-md focus:outline-none focus-visible:ring-2 focus-visible:ring-primary focus-visible:ring-offset-2" {
            @if let Some(category) = category {
                p class="mb-2 font-semibold text-primary text-xs uppercase tracking-normal" {
                    (tr(i18n, ctx, category.title))
                }
            }
            h3 class="font-semibold text-gray-950 transition group-hover:text-primary" {
                (tr(i18n, ctx, article.title))
            }
            p class="mt-2 text-sm leading-relaxed text-gray-600" {
                (tr(i18n, ctx, article.description))
            }
        }
    }
}

fn render_related_help_article(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    article: HelpArticle,
) -> Markup {
    html! {
        a href=(ctx.href(&format!("/help/{}", article.slug))) class="group block py-2 text-muted-foreground text-sm hover:text-foreground" {
            div class="font-medium text-foreground group-hover:text-primary" { (tr(i18n, ctx, article.title)) }
            div class="mt-0.5 text-muted-foreground text-sm" { (tr(i18n, ctx, article.description)) }
        }
    }
}

fn collect_related_help_articles(article: HelpArticle) -> Vec<HelpArticle> {
    let others = HELP_ARTICLES
        .iter()
        .copied()
        .filter(|entry| entry.slug != article.slug)
        .collect::<Vec<_>>();
    let mut same_category = others
        .iter()
        .copied()
        .filter(|entry| entry.category_slug == article.category_slug)
        .collect::<Vec<_>>();
    let mut fallback = others
        .into_iter()
        .filter(|entry| entry.category_slug != article.category_slug)
        .collect::<Vec<_>>();
    same_category.append(&mut fallback);
    same_category.truncate(4);
    same_category
}

fn help_article_matches(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    article: HelpArticle,
    query: &str,
) -> bool {
    let query = query.to_lowercase();
    tr(i18n, ctx, article.title).to_lowercase().contains(&query)
        || tr(i18n, ctx, article.description)
            .to_lowercase()
            .contains(&query)
        || article.category_slug.to_lowercase().contains(&query)
        || article.body.to_lowercase().contains(&query)
}

fn help_category_icon(slug: &str) -> Icon {
    match slug {
        "premium" => Icon::Coins,
        "faqs" => Icon::ChatCenteredText,
        "account" => Icon::UserCircle,
        "legal-policy" => Icon::ShieldCheck,
        _ => Icon::ChatsCircle,
    }
}

pub(crate) fn inline_separator() -> Markup {
    html! {
        span class="inline-separator" aria-hidden="true" {}
    }
}

pub(crate) fn heading_anchor_script() -> Markup {
    html! {
        script { (PreEscaped(HEADING_ANCHOR_SCRIPT)) }
    }
}

pub(crate) const HEADING_ANCHOR_SCRIPT: &str = r#"
(function () {
  function initHeadingAnchorLinks() {
    var anchorButtons = document.querySelectorAll('[data-anchor-link]');
    anchorButtons.forEach(function (button) {
      if (button.dataset.anchorBound === 'true') return;
      button.dataset.anchorBound = 'true';
      button.addEventListener('click', function (event) {
        event.preventDefault();
        event.stopPropagation();
        var slug = button.dataset.anchorLink;
        if (!slug) return;
        var url = window.location.origin + window.location.pathname + '#' + slug;
        if (!navigator.clipboard || !navigator.clipboard.writeText) return;
        navigator.clipboard.writeText(url).then(function () {
          button.classList.add('copied');
          window.setTimeout(function () {
            button.classList.remove('copied');
          }, 2000);
        }).catch(function () {});
      });
    });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initHeadingAnchorLinks);
  } else {
    initHeadingAnchorLinks();
  }
})();
"#;

pub fn generic_page(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    title: MarketingMessageDescriptor,
    description: MarketingMessageDescriptor,
    body: Markup,
) -> Markup {
    content_layout(
        i18n,
        ctx,
        PageMeta {
            title: tr(i18n, ctx, title),
            description: tr(i18n, ctx, description),
            ..Default::default()
        },
        html! {
            header class="mx-auto max-w-4xl text-center" {
                h1 class="display text-5xl text-gray-950" { (tr(i18n, ctx, title)) }
                p class="lead mt-5 text-gray-600" { (tr(i18n, ctx, description)) }
            }
            (body)
        },
    )
}

pub fn not_found_page(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    let title = tr(i18n, ctx, NAVIGATION_PAGE_NOT_FOUND_TITLE_DESCRIPTOR);
    let subtitle = tr(i18n, ctx, NAVIGATION_PAGE_NOT_FOUND_DESCRIPTION_DESCRIPTOR);
    layout(
        i18n,
        ctx,
        PageMeta {
            title: title.clone(),
            description: subtitle.clone(),
            ..Default::default()
        },
        html! {
            div class="h-28 shrink-0 md:h-36" {}
            main class="flex flex-1 flex-col items-center justify-center px-6 pt-36 pb-12 text-center sm:px-8 md:px-12 md:pt-44 md:pb-16 lg:px-16 xl:px-20" {
                div class="mx-auto max-w-2xl" {
                    div class="mb-8" {
                        (icon(Icon::FluxerLogoWordmark, "mx-auto h-16 opacity-80"))
                    }
                    div class="mb-6" {
                        h1 class="hero text-white/90" { "404" }
                    }
                    div class="mb-8" {
                        h2 class="display mb-4 text-2xl text-white md:text-3xl lg:text-4xl" { (title) }
                        p class="body-lg text-white/80 md:text-lg" { (subtitle) }
                    }
                    div class="flex flex-col items-center gap-4 sm:flex-row sm:justify-center" {
                        a class="rounded-lg border border-white bg-white px-6 py-3 text-[#4641D9] transition-opacity hover:opacity-90" href=(ctx.href("/")) {
                            (tr(i18n, ctx, NAVIGATION_GO_HOME_DESCRIPTOR))
                        }
                        a class="rounded-lg border border-white/30 px-6 py-3 text-white transition-colors hover:border-white/50 hover:bg-white/10" href=(ctx.href("/help")) {
                            (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_GET_HELP_DESCRIPTOR))
                        }
                    }
                }
            }
            div class="h-28 shrink-0 md:h-36" {}
        },
    )
}

fn layout(i18n: &MarketingI18n, ctx: &RequestContext, meta: PageMeta, body: Markup) -> Markup {
    base_document(
        i18n,
        ctx,
        meta,
        "flex min-h-screen flex-col bg-[#4641D9] font-sans text-white",
        html! {
            (nav(i18n, ctx))
            div class="flex grow flex-col" { (body) }
            (footer(i18n, ctx))
            (locale_modal(i18n, ctx))
        },
    )
}

pub(crate) fn content_layout(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    meta: PageMeta,
    body: Markup,
) -> Markup {
    content_layout_with_footer_class(i18n, ctx, meta, body, "")
}

pub(crate) fn content_layout_with_footer_class(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    meta: PageMeta,
    body: Markup,
    footer_class_name: &str,
) -> Markup {
    base_document(
        i18n,
        ctx,
        meta,
        "bg-white",
        html! {
            (nav(i18n, ctx))
            main class="min-h-screen bg-white px-6 pt-48 pb-16 sm:px-8 md:px-12 md:pt-60 lg:px-16 xl:px-20" { (body) }
            (footer_with_class(i18n, ctx, footer_class_name))
            (locale_modal(i18n, ctx))
        },
    )
}

fn base_document(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    meta: PageMeta,
    body_class: &str,
    body: Markup,
) -> Markup {
    let page_url = if ctx.current_path == "/" {
        ctx.base_url.clone()
    } else {
        format!("{}{}", ctx.base_url, ctx.current_path)
    };
    let default_og_image = format!("{}/web/og-image-default.png", ctx.static_cdn_endpoint);
    let og_image = meta.og_image_url.clone().unwrap_or(default_og_image);
    let og_locale = og_locale_from_code(ctx.locale.code());
    let page_title = meta.formatted_title();
    let rss_title = tr(i18n, ctx, SOCIAL_AND_FEEDS_RSS_FLUXER_BLOG_RSS_DESCRIPTOR);
    let rss_url = ctx.absolute_href("/blog/rss.xml");
    let atom_url = ctx.absolute_href("/blog/atom.xml");
    let author = meta
        .author
        .clone()
        .unwrap_or_else(|| tr(i18n, ctx, GENERAL_FLUXER_TEAM_DESCRIPTOR));
    let cdn = &ctx.static_cdn_endpoint;
    html! {
        (DOCTYPE)
        html lang=(ctx.locale.code()) dir=(if ctx.locale.is_rtl() { "rtl" } else { "ltr" }) {
            head {
                meta charset="utf-8";
                meta name="viewport" content="width=device-width, initial-scale=1.0";
                meta name="description" content=(meta.description);
                meta property="og:site_name" content="Fluxer";
                meta property="og:locale" content=(og_locale);
                meta property="og:title" content=(page_title.as_str());
                meta property="og:description" content=(meta.description);
                meta property="og:image" content=(og_image);
                meta property="og:url" content=(page_url);
                meta property="og:type" content=(meta.og_type.as_str());
                meta name="twitter:card" content="summary_large_image";
                meta name="twitter:title" content=(page_title.as_str());
                meta name="twitter:description" content=(meta.description);
                meta name="twitter:image" content=(og_image);
                meta name="robots" content="index,follow";
                meta name="theme-color" content="#4641D9";
                meta name="author" content=(author.as_str());
                link rel="canonical" href=(page_url);
                link rel="alternate" type="application/rss+xml" title=(rss_title.clone()) href=(rss_url);
                link rel="alternate" type="application/atom+xml" title=(rss_title) href=(atom_url);
                @for preload_image in &meta.preload_images {
                    link rel="preload" as="image" href=(preload_image);
                }
                @if let Some(published) = &meta.published_time {
                    meta property="article:published_time" content=(published);
                }
                @if meta.author.is_some() {
                    meta property="article:author" content=(author.as_str());
                }
                @for tag in &meta.article_tags {
                    meta property="article:tag" content=(tag);
                }
                @if let Some(modified) = &meta.modified_time {
                    meta property="article:modified_time" content=(modified);
                    meta property="og:updated_time" content=(modified);
                }
                @for json_ld in &meta.json_ld {
                    script type="application/ld+json" { (PreEscaped(json_ld)) }
                }
                title { (page_title.as_str()) }
                link rel="preconnect" href=(cdn);
                link rel="stylesheet" href=(format!("{cdn}/fonts/ibm-plex.css?v=3"));
                link rel="stylesheet" href=(format!("{cdn}/fonts/bricolage.css?v=3"));
                link rel="stylesheet" href=(format!("{}/static/app.css?v={}", ctx.base_path, ctx.asset_version));
                link rel="icon" type="image/x-icon" href=(format!("{cdn}/web/favicon.ico"));
                link rel="apple-touch-icon" href=(format!("{cdn}/web/apple-touch-icon.png"));
                link rel="icon" type="image/png" sizes="32x32" href=(format!("{cdn}/web/favicon-32x32.png"));
                link rel="icon" type="image/png" sizes="16x16" href=(format!("{cdn}/web/favicon-16x16.png"));
                @if meta.enable_htmx {
                    script src=(format!("{}/static/htmx.min.js?v={}", ctx.base_path, ctx.asset_version)) defer {}
                }
            }
            body class=(body_class) {
                (body)
            }
        }
    }
}

fn og_locale_from_code(code: &str) -> String {
    let mut parts = code.split('-');
    match (parts.next(), parts.next()) {
        (Some(lang), Some(region)) if !lang.is_empty() && !region.is_empty() => {
            format!("{lang}_{}", region.to_uppercase())
        }
        _ => code.replace('-', "_"),
    }
}

fn nav(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    let open_menu_label = tr(i18n, ctx, NAVIGATION_OPEN_NAVIGATION_MENU_DESCRIPTOR);
    let close_menu_label = tr(i18n, ctx, NAVIGATION_CLOSE_NAVIGATION_MENU_DESCRIPTOR);
    html! {
        nav id="navbar" class="fixed top-0 right-0 left-0 z-40" {
            input type="checkbox" id="nav-toggle" class="peer sr-only";
            div class="px-6 py-4 sm:px-8 md:px-12 md:py-5 lg:px-8 xl:px-16" {
                div class="mx-auto max-w-7xl rounded-2xl border border-gray-200/60 bg-white/95 px-3 py-2 shadow-lg backdrop-blur-lg md:px-5 md:py-2.5" {
                    div class="flex items-center justify-between" {
                        div class="flex items-center gap-4 xl:gap-6" {
                            a class="relative z-10 flex shrink-0 items-center transition-opacity hover:opacity-80" href=(ctx.href("/")) aria-label=(tr(i18n, ctx, NAVIGATION_GO_HOME_DESCRIPTOR)) {
                                (icon(Icon::FluxerLogoWordmark, "h-8 text-[#4641D9] md:h-9"))
                                span class="absolute right-0 -bottom-1.5 whitespace-nowrap rounded-full border border-white bg-[#4641D9] px-1.5 py-0.5 font-bold text-[8px] leading-none text-white" {
                                    (tr(i18n, ctx, BETA_AND_ACCESS_PUBLIC_BETA_DESCRIPTOR))
                                }
                            }
                            div class="marketing-nav-links hidden items-center gap-4 lg:flex xl:gap-6" {
                                (nav_link(i18n, ctx, "/download", DOWNLOAD_DOWNLOAD_DESCRIPTOR))
                                (nav_link(i18n, ctx, "/plutonium", PRICING_AND_TIERS_PLUTONIUM_TIER_NAME_DESCRIPTOR))
                                (nav_link(i18n, ctx, "/help", COMPANY_AND_RESOURCES_HELP_LABEL_DESCRIPTOR))
                                a class="body-lg text-gray-900/90 transition-colors hover:text-gray-900" href="https://docs.fluxer.app" {
                                    (tr(i18n, ctx, COMPANY_AND_RESOURCES_DOCS_DESCRIPTOR))
                                }
                                (nav_link(i18n, ctx, "/blog", COMPANY_AND_RESOURCES_BLOG_DESCRIPTOR))
                                (nav_link(i18n, ctx, "/donate", DONATIONS_DONATE_ACTION_DESCRIPTOR))
                            }
                        }
                        div class="flex items-center gap-1 xl:gap-2" {
                            (nav_icon_link("https://bsky.app/profile/fluxer.app", tr(i18n, ctx, SOCIAL_AND_FEEDS_BLUESKY_LABEL_DESCRIPTOR), Icon::Bluesky, "hidden lg:flex"))
                            (nav_icon_link("https://github.com/fluxerapp/fluxer", tr(i18n, ctx, SOCIAL_AND_FEEDS_GITHUB_DESCRIPTOR), Icon::Github, "hidden lg:flex"))
                            (nav_icon_link(&ctx.href("/blog/rss.xml"), tr(i18n, ctx, SOCIAL_AND_FEEDS_RSS_LABEL_DESCRIPTOR), Icon::Rss, "marketing-nav-rss hidden xl:flex"))
                            button
                                type="button"
                                class="locale-toggle hidden items-center rounded-lg p-2 text-[#4641D9] transition-colors hover:bg-gray-100 hover:text-[#3d38c7] lg:flex"
                                id="locale-button"
                                popovertarget="locale-modal-backdrop"
                                aria-label=(tr(i18n, ctx, LANGUAGES_CHANGE_LANGUAGE_DESCRIPTOR))
                            {
                                (icon(Icon::Translate, "h-5 w-5"))
                            }
                            a class="ml-2 hidden whitespace-nowrap rounded-xl bg-[#4641D9] px-4 py-2 font-semibold text-sm text-white shadow-lg transition hover:bg-[#3832B8] lg:inline-flex xl:px-6 xl:py-3 xl:text-base" href=(ctx.app_url("/channels/@me")) {
                                (tr(i18n, ctx, APP_OPEN_OPEN_FLUXER_DESCRIPTOR))
                            }
                            label for="nav-toggle" class="relative z-10 flex cursor-pointer items-center justify-center rounded-lg p-2 transition-colors hover:bg-gray-100 lg:hidden" {
                                span class="sr-only" { (open_menu_label) }
                                (icon(Icon::Menu, "h-6 w-6 text-gray-900"))
                            }
                        }
                    }
                }
            }
            div class="pointer-events-none fixed inset-0 z-50 bg-black/50 opacity-0 backdrop-blur-sm transition-opacity peer-checked:pointer-events-auto peer-checked:opacity-100 lg:hidden" {
                label for="nav-toggle" class="absolute inset-0 cursor-pointer" aria-label=(close_menu_label.clone()) {}
            }
            div class="fixed top-0 right-0 bottom-0 z-50 w-full translate-x-full transform overflow-y-auto rounded-none bg-white shadow-2xl transition-transform peer-checked:translate-x-0 sm:w-[420px] sm:max-w-[90vw] sm:rounded-l-3xl lg:hidden" {
                div class="flex h-full flex-col p-6" {
                    div class="mb-6 flex items-center justify-between" {
                        a class="flex items-center gap-3 rounded-xl px-2 py-1 transition-colors hover:bg-gray-50" href=(ctx.href("/")) aria-label=(tr(i18n, ctx, NAVIGATION_GO_HOME_DESCRIPTOR)) {
                            (icon(Icon::FluxerLogoWordmark, "h-7 text-[#4641D9]"))
                        }
                        label for="nav-toggle" class="cursor-pointer rounded-lg p-2 transition-colors hover:bg-gray-100" {
                            span class="sr-only" { (close_menu_label) }
                            (icon(Icon::X, "h-6 w-6 text-gray-900"))
                        }
                    }
                    div class="-mx-2 flex-1 overflow-y-auto px-2" {
                        div class="flex flex-col gap-6" {
                            (mobile_drawer_section(i18n, ctx, COMPANY_AND_RESOURCES_PRODUCT_DESCRIPTOR, &[
                                (ctx.href("/download"), DOWNLOAD_DOWNLOAD_DESCRIPTOR),
                                (ctx.href("/plutonium"), PRICING_AND_TIERS_PLUTONIUM_TIER_NAME_DESCRIPTOR),
                                (ctx.href("/partners"), PARTNER_PROGRAM_LABEL_DESCRIPTOR),
                            ]))
                            div {
                                p class="mb-2 font-semibold text-gray-500 text-xs uppercase" { (tr(i18n, ctx, COMPANY_AND_RESOURCES_RESOURCES_DESCRIPTOR)) }
                                div class="flex flex-col gap-1" {
                                    a class="rounded-lg py-2.5 pr-3 pl-0 text-base text-gray-900 transition-colors hover:bg-gray-100" href=(ctx.href("/help")) {
                                        (tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_HELP_CENTER_DESCRIPTOR))
                                    }
                                    a class="rounded-lg py-2.5 pr-3 pl-0 text-base text-gray-900 transition-colors hover:bg-gray-100" href="https://docs.fluxer.app" {
                                        (tr(i18n, ctx, COMPANY_AND_RESOURCES_DOCS_DESCRIPTOR))
                                    }
                                    a class="flex items-center gap-2 rounded-lg py-2.5 pr-3 pl-0 text-base text-gray-900 transition-colors hover:bg-gray-100" href=(ctx.href("/blog")) {
                                        (tr(i18n, ctx, COMPANY_AND_RESOURCES_BLOG_DESCRIPTOR))
                                        (icon(Icon::Rss, "h-4 w-4 text-gray-500"))
                                    }
                                    a class="rounded-lg py-2.5 pr-3 pl-0 text-base text-gray-900 transition-colors hover:bg-gray-100" href=(ctx.href("/press")) {
                                        (tr(i18n, ctx, COMPANY_AND_RESOURCES_PRESS_LABEL_DESCRIPTOR))
                                    }
                                }
                            }
                            (mobile_drawer_section(i18n, ctx, COMPANY_AND_RESOURCES_CONNECT_DESCRIPTOR, &[
                                ("https://bsky.app/profile/fluxer.app".to_owned(), SOCIAL_AND_FEEDS_BLUESKY_LABEL_DESCRIPTOR),
                                ("https://github.com/fluxerapp/fluxer".to_owned(), COMPANY_AND_RESOURCES_SOURCE_AND_CONTRIBUTION_SOURCE_CODE_DESCRIPTOR),
                            ]))
                            (mobile_drawer_section(i18n, ctx, COMPANY_AND_RESOURCES_COMPANY_DESCRIPTOR, &[
                                (ctx.href("/careers"), COMPANY_AND_RESOURCES_CAREERS_LABEL_DESCRIPTOR),
                                (ctx.href("/donate"), DONATIONS_DONATE_ACTION_DESCRIPTOR),
                                (ctx.href("/company-information"), COMPANY_AND_RESOURCES_COMPANY_INFO_DESCRIPTOR),
                            ]))
                        }
                        div class="mt-4 flex flex-col gap-3 border-t border-gray-200 pt-4" {
                            button
                                type="button"
                                class="flex w-full items-center justify-start gap-3 rounded-lg px-3 py-2.5 text-base text-gray-900 transition-colors hover:bg-gray-100 lg:hidden"
                                popovertarget="locale-modal-backdrop"
                                aria-label=(tr(i18n, ctx, LANGUAGES_CHANGE_LANGUAGE_DESCRIPTOR))
                            {
                                (icon(Icon::Translate, "h-5 w-5"))
                                span class="flex-1 text-left" { (tr(i18n, ctx, LANGUAGES_LANGUAGE_LABEL_DESCRIPTOR)) }
                            }
                            (mobile_drawer_button(&primary_download_url(ctx), tr(i18n, ctx, DOWNLOAD_DOWNLOAD_DESCRIPTOR), Some(icon(Icon::Download, "h-5 w-5")), None))
                        }
                    }
                    div class="pt-6" {
                        a class="flex w-full items-center justify-center rounded-xl bg-[#4641D9] px-5 py-3 font-semibold text-white shadow-lg transition hover:bg-[#3832B8]" href=(ctx.app_url("/channels/@me")) {
                            (tr(i18n, ctx, APP_OPEN_OPEN_FLUXER_DESCRIPTOR))
                        }
                    }
                }
            }
        }
    }
}

fn nav_link(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    href: &str,
    descriptor: MarketingMessageDescriptor,
) -> Markup {
    html! {
        a class="body-lg text-gray-900/90 transition-colors hover:text-gray-900" href=(ctx.href(href)) { (tr(i18n, ctx, descriptor)) }
    }
}

fn nav_icon_link(href: &str, label: String, icon_name: Icon, extra_class: &str) -> Markup {
    html! {
        a
            class=(format!("{extra_class} items-center rounded-lg p-2 text-[#4641D9] transition-colors hover:bg-gray-100 hover:text-[#3d38c7]"))
            href=(href)
            target="_blank"
            rel="noopener noreferrer"
            aria-label=(label) {
            (icon(icon_name, "h-5 w-5"))
        }
    }
}

fn mobile_drawer_section(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    title: MarketingMessageDescriptor,
    links: &[(String, MarketingMessageDescriptor)],
) -> Markup {
    html! {
        div {
            p class="mb-2 font-semibold text-gray-500 text-xs uppercase" { (tr(i18n, ctx, title)) }
            div class="flex flex-col gap-1" {
                @for (href, label) in links {
                    a class="rounded-lg py-2.5 pr-3 pl-0 text-base text-gray-900 transition-colors hover:bg-gray-100" href=(href) {
                        (tr(i18n, ctx, *label))
                    }
                }
            }
        }
    }
}

fn mobile_drawer_button(
    href: &str,
    label: String,
    icon_markup: Option<Markup>,
    aria_label: Option<String>,
) -> Markup {
    html! {
        a class="flex w-full items-center justify-start gap-3 rounded-lg px-3 py-2.5 text-base text-gray-900 transition-colors hover:bg-gray-100 lg:hidden" href=(href) aria-label=[aria_label] {
            @if let Some(icon_markup) = icon_markup {
                (icon_markup)
            }
            span class="flex-1 text-left" { (label) }
        }
    }
}

fn section_headered(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    variant: &str,
    id: Option<&str>,
    title: MarketingMessageDescriptor,
    description: MarketingMessageDescriptor,
    body: Markup,
) -> Markup {
    let gradient_class = match variant {
        "cta" => "gradient-cta",
        "light" => "gradient-light",
        "white" => "bg-white",
        _ => "gradient-purple",
    };
    let is_dark = matches!(variant, "dark" | "cta");
    let title_color = if is_dark { "text-white" } else { "text-black" };
    let description_color = if is_dark {
        "text-white/90"
    } else {
        "text-gray-700"
    };
    html! {
        section id=[id] class=(format!("{gradient_class} px-6 py-16 sm:px-8 md:px-12 md:py-24 lg:px-16 xl:px-20")) {
            div class="mx-auto max-w-7xl" {
                div class="mb-12 text-center md:mb-16" {
                    h2 class=(format!("display mb-6 text-4xl md:mb-8 md:text-5xl lg:text-6xl {title_color}")) {
                        (tr(i18n, ctx, title))
                    }
                    p class=(format!("lead mx-auto max-w-3xl {description_color}")) {
                        (tr(i18n, ctx, description))
                    }
                }
                (body)
            }
        }
    }
}

fn hero(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    html! {
        main class="flex flex-col items-center justify-center px-6 pt-36 pb-16 sm:px-8 md:px-12 md:pt-44 md:pb-20 lg:px-16 lg:pb-24 xl:px-20" {
            div class="max-w-4xl space-y-8 text-center" {
                @if ctx.locale.code() == "ja" {
                    div class="flex justify-center" {
                        span class="font-bold text-3xl text-white" { "Fluxer（フラクサー）" }
                    }
                }
                @if ctx.locale.code() == "ko" {
                    div class="flex justify-center" {
                        span class="font-bold text-3xl text-white" { "Fluxer (플럭서)" }
                    }
                }
                div class="flex flex-wrap items-center justify-center gap-3 pb-2" {
                    a class="inline-flex items-center gap-1.5 rounded-full bg-white/10 px-4 py-1.5 font-medium text-sm text-white transition-colors hover:bg-white/20" href=(ctx.href("/blog/how-i-built-fluxer-a-discord-like-chat-app")) {
                        (tr(i18n, ctx, LAUNCH_HEADING_DESCRIPTOR))
                        (icon(Icon::ArrowRight, "h-3.5 w-3.5"))
                    }
                    a class="inline-flex items-center gap-1.5 rounded-full bg-white/10 px-4 py-1.5 font-medium text-sm text-white transition-colors hover:bg-white/20" href=(ctx.href("/blog/roadmap-2026")) {
                        (tr(i18n, ctx, LAUNCH_VIEW_FULL_ROADMAP_DESCRIPTOR))
                        (icon(Icon::ArrowRight, "h-3.5 w-3.5"))
                    }
                }
                h1 class="hero" { (tr(i18n, ctx, GENERAL_TAGLINE_DESCRIPTOR)) }
                div class="-mt-4 flex items-center justify-center gap-2 font-medium text-sm text-white/80" {
                    span class="inline-flex items-center gap-1.5" {
                        (flag_svg(ctx, crate::i18n::Locale::SvSe, "h-3.5 w-3.5 rounded-sm"))
                        (tr(i18n, ctx, GENERAL_MADE_IN_SWEDEN_DESCRIPTOR))
                    }
                }
                p class="lead mx-auto max-w-2xl text-white/90" { (tr(i18n, ctx, PRODUCT_POSITIONING_INTRO_DESCRIPTOR)) }
                div class="flex flex-col items-center justify-center gap-4 pt-4 sm:flex-row sm:items-stretch" {
                    (primary_download_button(i18n, ctx))
                    a class="hidden items-center justify-center gap-2 rounded-2xl bg-white/10 px-5 py-3 font-semibold text-sm text-white shadow-lg ring-1 ring-inset ring-white/30 backdrop-blur-sm transition-colors hover:bg-white/20 sm:inline-flex md:px-6 md:py-3.5 md:text-base" href=(ctx.app_url("/channels/@me")) {
                        (tr(i18n, ctx, DOWNLOAD_OPEN_IN_BROWSER_DESCRIPTOR))
                    }
                }
                a class="mt-6 inline-flex items-center gap-2 rounded-full bg-white px-4 py-1.5 font-medium text-[#4641D9] text-sm transition-opacity hover:opacity-90" href="https://fluxer.gg/fluxer-hq" {
                    span { (tr(i18n, ctx, BETA_AND_ACCESS_TRY_WITHOUT_EMAIL_DESCRIPTOR)) }
                    (icon(Icon::ArrowRight, "h-4 w-4"))
                }
            }
            div class="mt-16 flex w-full max-w-6xl items-end justify-center gap-4 px-6 md:mt-24 md:gap-8" {
                div class="hidden w-full md:block md:w-4/5 lg:w-3/4" {
                    picture {
                        source type="image/avif" srcset=(screenshot_srcset(ctx, "desktop", "avif", &[480, 768, 1024, 1920, 2560])) sizes="(max-width: 768px) 100vw, 80vw";
                        source type="image/webp" srcset=(screenshot_srcset(ctx, "desktop", "webp", &[480, 768, 1024, 1920, 2560])) sizes="(max-width: 768px) 100vw, 80vw";
                        img
                            class="aspect-video w-full rounded-lg border-2 border-white/50"
                            src=(format!("{}/marketing/screenshots/desktop-1920w.png?v=5", ctx.static_cdn_endpoint))
                            srcset=(screenshot_srcset(ctx, "desktop", "png", &[480, 768, 1024, 1920, 2560]))
                            sizes="(max-width: 768px) 100vw, 80vw"
                            alt=(tr(i18n, ctx, PLATFORM_SUPPORT_DESKTOP_INTERFACE_LABEL_DESCRIPTOR));
                    }
                }
                div class="w-full max-w-[240px] md:w-1/6 md:max-w-none lg:w-1/6" {
                    picture {
                        source type="image/avif" srcset=(screenshot_srcset(ctx, "mobile", "avif", &[480, 768])) sizes="(max-width: 768px) 240px, 17vw";
                        source type="image/webp" srcset=(screenshot_srcset(ctx, "mobile", "webp", &[480, 768])) sizes="(max-width: 768px) 240px, 17vw";
                        img
                            class="aspect-[9/19] w-full rounded-xl border-2 border-white/50"
                            src=(format!("{}/marketing/screenshots/mobile-768w.png?v=5", ctx.static_cdn_endpoint))
                            srcset=(screenshot_srcset(ctx, "mobile", "png", &[480, 768]))
                            sizes="(max-width: 768px) 240px, 17vw"
                            alt=(tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_INTERFACE_LABEL_DESCRIPTOR));
                    }
                }
            }
        }
    }
}

fn features_section(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    let cards = [
        (
            Icon::Chats,
            APP_MESSAGING_TITLE_DESCRIPTOR,
            APP_MESSAGING_DESCRIPTION_DESCRIPTOR,
            [
                APP_MESSAGING_FEATURES_FULL_MARKDOWN_SUPPORT_DESCRIPTOR,
                APP_MESSAGING_FEATURES_PRIVATE_DMS_AND_GROUP_CHATS_DESCRIPTOR,
                APP_MESSAGING_FEATURES_ORGANISED_CHANNELS_FOR_COMMUNITIES_DESCRIPTOR,
                APP_MESSAGING_FEATURES_FILE_SHARING_DESCRIPTOR,
            ],
            None,
        ),
        (
            Icon::Microphone,
            APP_VOICE_AND_VIDEO_TITLE_DESCRIPTOR,
            APP_VOICE_AND_VIDEO_HOP_IN_A_CALL_DESCRIPTOR,
            [
                MISC_LABELS_JOIN_MULTIPLE_DEVICES_DESCRIPTOR,
                APP_VOICE_AND_VIDEO_FEATURES_SCREEN_SHARING_DESCRIPTOR,
                APP_VOICE_AND_VIDEO_FEATURES_NOISE_SUPPRESSION_DESCRIPTOR,
                APP_VOICE_AND_VIDEO_FEATURES_MUTE_CONTROLS_DESCRIPTOR,
            ],
            None,
        ),
        (
            Icon::Gear,
            APP_COMMUNITIES_MODERATION_TOOLS_DESCRIPTOR,
            APP_COMMUNITIES_ROLES_PERMISSIONS_AUDIT_KEEP_RUNNING_SMOOTHLY_DESCRIPTOR,
            [
                APP_COMMUNITIES_ROLES_PERMISSIONS_AUDIT_GRANULAR_ROLES_AND_PERMISSIONS_DESCRIPTOR,
                APP_COMMUNITIES_MODERATION_ACTIONS_AND_TOOLS_DESCRIPTOR,
                APP_COMMUNITIES_ROLES_PERMISSIONS_AUDIT_AUDIT_LOGS_DESCRIPTOR,
                PRICING_AND_TIERS_PLUTONIUM_FEATURES_WEBHOOKS_AND_BOT_SUPPORT_DESCRIPTOR,
            ],
            None,
        ),
        (
            Icon::MagnifyingGlass,
            APP_MESSAGING_FEATURES_SEARCH_SEARCH_AND_QUICK_SWITCHER_DESCRIPTOR,
            APP_MESSAGING_FEATURES_SEARCH_FIND_OLD_MESSAGES_DESCRIPTOR,
            [
                APP_MESSAGING_FEATURES_SEARCH_LABEL_DESCRIPTOR,
                APP_MESSAGING_FEATURES_SEARCH_FILTER_OPTIONS_DESCRIPTOR,
                APP_MESSAGING_FEATURES_SEARCH_QUICK_SWITCHER_SHORTCUTS_DESCRIPTOR,
                APP_PROFILES_IDENTITY_MANAGE_FRIENDS_AND_BLOCK_USERS_DESCRIPTOR,
            ],
            None,
        ),
        (
            Icon::Palette,
            APP_CUSTOMIZATION_TITLE_DESCRIPTOR,
            APP_CUSTOMIZATION_SAVED_MEDIA_AND_CSS_DESCRIPTOR,
            [
                APP_CUSTOMIZATION_UPLOAD_CUSTOM_EMOJIS_AND_STICKERS_DESCRIPTOR,
                APP_MESSAGING_FEATURES_SAVE_MEDIA_DESCRIPTOR,
                APP_CUSTOMIZATION_CUSTOM_CSS_THEMES_DESCRIPTOR,
                APP_CUSTOMIZATION_COMPACT_MODE_DESCRIPTOR,
            ],
            None,
        ),
        (
            Icon::Globe,
            PRODUCT_POSITIONING_SELF_HOSTING_LABEL_DESCRIPTOR,
            PRODUCT_POSITIONING_SELF_HOSTING_RUN_BACKEND_ON_YOUR_HARDWARE_DESCRIPTOR,
            [
                PRODUCT_POSITIONING_OPEN_SOURCE_FULLY_OPEN_SOURCE_AGPLV3_DESCRIPTOR,
                PRODUCT_POSITIONING_SELF_HOSTING_HOST_YOUR_OWN_INSTANCE_DESCRIPTOR,
                PLATFORM_SUPPORT_DESKTOP_USE_DESKTOP_CLIENT_MOBILE_SOON_DESCRIPTOR,
                PRODUCT_POSITIONING_SELF_HOSTING_SWITCH_BETWEEN_INSTANCES_DESCRIPTOR,
            ],
            Some("https://docs.fluxer.app/operator/get-started/"),
        ),
    ];
    html! {
        (section_headered(i18n, ctx, "dark", None, GENERAL_COMING_SOON_WHATS_AVAILABLE_TODAY_DESCRIPTOR, BETA_AND_ACCESS_FEATURED_BENEFIT_LINE_DESCRIPTOR, html! {
            div class="grid grid-cols-1 gap-6 md:gap-8 lg:grid-cols-2" {
                @for (icon_name, title, description, features, learn_more) in cards {
                    (feature_card(i18n, ctx, icon_name, title, description, &features, learn_more))
                }
            }
        }))
    }
}

fn feature_card(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    icon_name: Icon,
    title: MarketingMessageDescriptor,
    description: MarketingMessageDescriptor,
    features: &[MarketingMessageDescriptor; 4],
    learn_more: Option<&str>,
) -> Markup {
    html! {
        div class="relative flex h-full flex-col rounded-2xl border border-gray-200 bg-white p-8 shadow-md md:p-10" {
            @if let Some(link) = learn_more {
                a class="absolute top-4 right-4 inline-flex items-center gap-1.5 rounded-xl bg-[#4641D9] px-3 py-1.5 font-semibold text-sm text-white shadow-lg transition hover:bg-[#3832B8] md:top-6 md:right-6" href=(link) target="_blank" rel="noopener noreferrer" {
                    (tr(i18n, ctx, MISC_LABELS_LEARN_MORE_DESCRIPTOR))
                    (icon(Icon::ArrowRight, "h-3.5 w-3.5"))
                }
            }
            div class="mb-6" {
                div class="mb-5 inline-flex h-14 w-14 items-center justify-center rounded-2xl bg-gradient-to-br from-[#4641D9]/10 to-[#4641D9]/5 md:h-16 md:w-16" {
                    (icon(icon_name, "h-7 w-7 text-[#4641D9] md:h-8 md:w-8"))
                }
                h3 class="title mb-3 text-gray-900" { (tr(i18n, ctx, title)) }
                p class="body-lg text-gray-600" { (tr(i18n, ctx, description)) }
            }
            div class="mt-2 flex-1" {
                ul class="space-y-3" {
                    @for feature in features {
                        li class="flex items-start gap-3" {
                            span class="mt-[.7em] h-1.5 w-1.5 shrink-0 rounded-full bg-[#4641D9]" {}
                            span class="body-lg text-gray-900" { (tr(i18n, ctx, *feature)) }
                        }
                    }
                }
            }
        }
    }
}

fn get_involved_section(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    html! {
        section id="get-involved" class="gradient-light px-6 py-16 sm:px-8 md:px-12 md:py-28 lg:px-16 xl:px-20" {
            div class="mx-auto max-w-7xl" {
                div class="mb-12 text-center md:mb-16" {
                    h2 class="display mb-6 text-4xl text-black md:mb-8 md:text-5xl lg:text-6xl" {
                        (tr(i18n, ctx, COMPANY_AND_RESOURCES_SOURCE_AND_CONTRIBUTION_GET_INVOLVED_DESCRIPTOR))
                    }
                    p class="lead mx-auto max-w-3xl text-gray-700" {
                        (tr(i18n, ctx, COMPANY_AND_RESOURCES_SOURCE_AND_CONTRIBUTION_FLUXER_BUILT_IN_OPEN_DESCRIPTOR))
                    }
                }
                div class="grid grid-cols-1 gap-8 md:grid-cols-2 md:gap-10" {
                    (support_action_card(i18n, ctx, Icon::ChatCenteredText, MISC_LABELS_JOIN_FLUXER_HQ_DESCRIPTOR, MISC_LABELS_GET_UPDATES_DESCRIPTOR, MISC_LABELS_JOIN_FLUXER_HQ_DESCRIPTOR, "https://fluxer.gg/fluxer-hq"))
                    (bluesky_support_card(i18n, ctx))
                    (support_action_card(i18n, ctx, Icon::Bug, MISC_LABELS_REPORT_BUGS_DESCRIPTOR, SECURITY_TESTERS_ACCESS_FROM_REPORTS_DESCRIPTOR, MISC_LABELS_READ_THE_GUIDE_DESCRIPTOR, "/help/report-bug"))
                    (support_action_card(i18n, ctx, Icon::ShieldCheck, SECURITY_FOUND_SECURITY_ISSUE_DESCRIPTOR, SECURITY_RESPONSIBLE_DISCLOSURE_NOTE_DESCRIPTOR, SECURITY_SECURITY_BUG_BOUNTY_DESCRIPTOR, "/security"))
                }
            }
        }
    }
}

fn support_action_card(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    icon_name: Icon,
    title: MarketingMessageDescriptor,
    description: MarketingMessageDescriptor,
    button: MarketingMessageDescriptor,
    href: &str,
) -> Markup {
    let final_href = resolve_href(ctx, href);
    let external = is_external_href(href);
    html! {
        div class="flex h-full flex-col rounded-3xl border border-gray-200 bg-white p-8 shadow-md md:p-10" style="box-shadow: 0 0 0 1px rgba(0,0,0,0.03), 0 4px 12px rgba(0,0,0,0.08), 0 2px 6px rgba(0,0,0,0.05);" {
            div class="mb-8 text-center" {
                div class="mb-6 inline-flex h-20 w-20 items-center justify-center rounded-3xl bg-[#4641D9] md:h-24 md:w-24" {
                    (icon(icon_name, "h-10 w-10 text-white md:h-12 md:w-12"))
                }
                h3 class="title mb-4 text-xl text-gray-900 md:text-2xl" { (tr(i18n, ctx, title)) }
                p class="body-lg text-gray-700 leading-relaxed" { (tr(i18n, ctx, description)) }
            }
            div class="mt-auto flex flex-col items-center" {
                a class="label w-full rounded-xl bg-[#4641D9] px-6 py-3 text-center font-semibold text-base text-white shadow-md transition hover:bg-[#3832B8] md:text-lg" href=(final_href) target=[external.then_some("_blank")] rel=[external.then_some("noopener noreferrer")] {
                    (tr(i18n, ctx, button))
                }
            }
        }
    }
}

fn bluesky_support_card(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    html! {
        div class="flex h-full flex-col rounded-3xl border border-gray-200 bg-white p-8 shadow-md md:p-10" style="box-shadow: 0 0 0 1px rgba(0,0,0,0.03), 0 4px 12px rgba(0,0,0,0.08), 0 2px 6px rgba(0,0,0,0.05);" {
            div class="mb-8 text-center" {
                div class="mb-6 inline-flex h-20 w-20 items-center justify-center rounded-3xl bg-[#4641D9] md:h-24 md:w-24" {
                    (icon(Icon::Bluesky, "h-10 w-10 text-white md:h-12 md:w-12"))
                }
                h3 class="title mb-4 text-xl text-black md:text-2xl" { (tr(i18n, ctx, SOCIAL_AND_FEEDS_BLUESKY_FOLLOW_US_DESCRIPTOR)) }
                p class="body-lg text-gray-700 leading-relaxed" {
                    (tr(i18n, ctx, SOCIAL_AND_FEEDS_STAY_UPDATED_CTA_DESCRIPTOR)) " "
                    a class="underline hover:text-[#4641D9]" href="https://bsky.app/profile/fluxer.app/rss" target="_blank" rel="noopener noreferrer" {
                        (tr(i18n, ctx, SOCIAL_AND_FEEDS_BLUESKY_RSS_FEED_DESCRIPTOR))
                    }
                    " " (tr(i18n, ctx, GENERAL_OR_DESCRIPTOR)) " "
                    a class="underline hover:text-[#4641D9]" href=(ctx.href("/blog/rss.xml")) {
                        (tr(i18n, ctx, SOCIAL_AND_FEEDS_RSS_BLOG_RSS_FEED_DESCRIPTOR))
                    }
                    "."
                }
            }
            div class="mt-auto flex flex-col items-center" {
                a class="label w-full rounded-xl bg-[#4641D9] px-6 py-3 text-center font-semibold text-base text-white shadow-md transition hover:bg-[#3832B8] md:text-lg" href="https://bsky.app/profile/fluxer.app" target="_blank" rel="noopener noreferrer" {
                    (tr(i18n, ctx, SOCIAL_AND_FEEDS_FOLLOW_FLUXER_DESCRIPTOR))
                }
            }
        }
    }
}

fn resolve_href(ctx: &RequestContext, href: &str) -> String {
    if href.starts_with('/') {
        ctx.href(href)
    } else {
        href.to_owned()
    }
}

fn is_external_href(href: &str) -> bool {
    href.starts_with("https://") || href.starts_with("http://")
}

fn final_cta(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    html! {
        section class="bg-white" {
            div class="gradient-cta rounded-t-3xl" {
                div class="px-6 py-24 text-center sm:px-8 md:px-12 md:py-40 lg:px-16 xl:px-20" {
                    h2 class="display mb-8 font-bold text-5xl text-white md:mb-10 md:text-6xl lg:text-7xl" {
                        (tr(i18n, ctx, MISC_LABELS_READY_TO_GET_STARTED_DESCRIPTOR))
                    }
                    p class="lead mx-auto mb-12 max-w-3xl text-xl text-white/90 md:mb-14 md:text-2xl" {
                        (tr(i18n, ctx, DOWNLOAD_DOWNLOAD_APP_OR_OPEN_IN_BROWSER_DESCRIPTOR))
                    }
                    div class="flex flex-col items-center justify-center gap-4 sm:flex-row sm:items-stretch" {
                        (render_desktop_or_download_button(i18n, ctx))
                        a class="hidden items-center justify-center gap-2 rounded-2xl bg-white/10 px-5 py-3 font-semibold text-sm text-white shadow-lg ring-1 ring-inset ring-white/30 backdrop-blur-sm transition-colors hover:bg-white/20 sm:inline-flex md:px-6 md:py-3.5 md:text-base" href=(ctx.app_url("/channels/@me")) {
                            (tr(i18n, ctx, DOWNLOAD_OPEN_IN_BROWSER_DESCRIPTOR))
                        }
                    }
                }
            }
        }
    }
}

fn primary_download_button(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    render_desktop_or_download_button(i18n, ctx)
}

fn recommended_arch(ctx: &RequestContext, platform: Platform) -> &'static str {
    match ctx.architecture {
        Architecture::Arm64 => "arm64",
        Architecture::X64 => "x64",
        Architecture::Unknown => match platform {
            Platform::Macos => "arm64",
            _ => "x64",
        },
    }
}

fn recommended_desktop_url(ctx: &RequestContext, platform: Platform) -> String {
    match platform {
        Platform::Windows => desktop_url(ctx, "win32", recommended_arch(ctx, platform), "setup"),
        Platform::Macos => desktop_url(ctx, "darwin", recommended_arch(ctx, platform), "dmg"),
        Platform::Linux => desktop_url(ctx, "linux", recommended_arch(ctx, platform), "appimage"),
        Platform::Ios | Platform::Android | Platform::Unknown => ctx.href("/download"),
    }
}

fn primary_download_url(ctx: &RequestContext) -> String {
    recommended_desktop_url(ctx, ctx.platform)
}

fn screenshot_srcset(ctx: &RequestContext, family: &str, format: &str, widths: &[u32]) -> String {
    widths
        .iter()
        .map(|width| {
            format!(
                "{}/marketing/screenshots/{family}-{width}w.{format}?v=5 {width}w",
                ctx.static_cdn_endpoint
            )
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_desktop_or_download_button(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    match ctx.platform {
        Platform::Windows | Platform::Macos | Platform::Linux => {
            let name = platform_name(i18n, ctx, ctx.platform);
            let requirement = system_requirement(i18n, ctx, ctx.platform);
            let button_label = i18n.text_with(
                ctx.locale,
                DOWNLOAD_DOWNLOAD_FOR_PLATFORM_DESCRIPTOR,
                &[("platform", &name)],
            );
            html! {
                div class="relative" {
                    a class="inline-flex items-center justify-center gap-2 rounded-2xl bg-white px-5 py-3 font-semibold text-sm text-[#4641D9] shadow-lg transition hover:bg-white/90 md:px-6 md:py-3.5 md:text-base" href=(recommended_desktop_url(ctx, ctx.platform)) {
                        (icon(platform_icon(ctx.platform), "h-5 w-5 shrink-0 md:h-6 md:w-6"))
                        span { (button_label) }
                    }
                    div class="absolute top-full left-1/2 mt-2 flex -translate-x-1/2 flex-col items-center gap-0.5 whitespace-nowrap text-center" {
                        a class="text-white/70 text-xs underline decoration-white/30 underline-offset-2 transition hover:text-white" href=(ctx.href("/download")) {
                            (tr(i18n, ctx, DOWNLOAD_OTHER_DOWNLOADS_DESCRIPTOR))
                        }
                        @if !requirement.is_empty() {
                            span class="text-white/50 text-xs" { (requirement) }
                        }
                    }
                }
            }
        }
        Platform::Ios | Platform::Android | Platform::Unknown => html! {
            a class="inline-flex items-center justify-center gap-2 rounded-2xl bg-white px-5 py-3 font-semibold text-sm text-[#4641D9] shadow-lg transition hover:bg-white/90 md:px-6 md:py-3.5 md:text-base" href=(ctx.href("/download")) {
                (icon(Icon::Download, "h-5 w-5 shrink-0"))
                span { (tr(i18n, ctx, DOWNLOAD_DOWNLOAD_FLUXER_DESCRIPTOR)) }
            }
        },
    }
}

struct AltBuild {
    label: String,
    url: String,
    external: bool,
}

fn alt(label: String, url: String, external: bool) -> AltBuild {
    AltBuild {
        label,
        url,
        external,
    }
}

fn alternate_builds(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    platform: Platform,
    arch: &str,
) -> Vec<AltBuild> {
    let other_arch = if arch == "arm64" { "x64" } else { "arm64" };
    match platform {
        Platform::Windows => {
            let mut builds = vec![alt(
                other_arch.to_owned(),
                desktop_url(ctx, "win32", other_arch, "setup"),
                false,
            )];
            if ctx.release_channel.is_canary() {
                builds.push(alt(
                    tr(i18n, ctx, PLATFORM_SUPPORT_PLATFORMS_PORTABLE_DESCRIPTOR),
                    desktop_url(ctx, "win32", arch, "portable"),
                    false,
                ));
            }
            builds
        }
        Platform::Macos => {
            let label = if other_arch == "arm64" {
                tr(
                    i18n,
                    ctx,
                    PLATFORM_SUPPORT_PLATFORMS_MACOS_APPLE_SILICON_DESCRIPTOR,
                )
            } else {
                tr(i18n, ctx, PLATFORM_SUPPORT_PLATFORMS_MACOS_INTEL_DESCRIPTOR)
            };
            vec![alt(
                label,
                desktop_url(ctx, "darwin", other_arch, "dmg"),
                false,
            )]
        }
        Platform::Linux => {
            let mut builds = Vec::new();
            if !ctx.release_channel.is_canary() {
                builds.push(alt("Flatpak".to_owned(), FLATPAK_URL.to_owned(), true));
            }
            builds.push(alt(
                "DEB".to_owned(),
                desktop_url(ctx, "linux", arch, "deb"),
                false,
            ));
            builds.push(alt(
                "RPM".to_owned(),
                desktop_url(ctx, "linux", arch, "rpm"),
                false,
            ));
            builds.push(alt(
                "tar.gz".to_owned(),
                desktop_url(ctx, "linux", arch, "tar_gz"),
                false,
            ));
            builds.push(alt(
                other_arch.to_owned(),
                desktop_url(ctx, "linux", other_arch, "appimage"),
                false,
            ));
            builds
        }
        _ => Vec::new(),
    }
}

fn download_strip(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    platform: Platform,
    latest_version: Option<&LatestVersionInfo>,
) -> Markup {
    let name = platform_name(i18n, ctx, platform);
    let requirement = if platform == Platform::Macos {
        latest_version
            .and_then(|info| info.minimum_system_version.as_deref())
            .map(|version| format!("macOS {version}+"))
            .unwrap_or_else(|| system_requirement(i18n, ctx, platform))
    } else {
        system_requirement(i18n, ctx, platform)
    };
    let arch = recommended_arch(ctx, platform);
    let primary_url = recommended_desktop_url(ctx, platform);
    let alternates = alternate_builds(i18n, ctx, platform, arch);
    let button_label = i18n.text_with(
        ctx.locale,
        DOWNLOAD_DOWNLOAD_FOR_PLATFORM_DESCRIPTOR,
        &[("platform", &name)],
    );
    let description = if platform == Platform::Windows && ctx.release_channel.is_canary() {
        let warning = i18n.template(
            ctx.locale,
            PLATFORM_SUPPORT_DESKTOP_CANARY_WINDOWS_WARNING_DESCRIPTOR,
        );
        html! {
            p class="body-sm mt-2 max-w-xl text-amber-800" {
                (message_with_links(&warning, &[LinkReplacement {
                    variable: "issue_link",
                    text: "#1393",
                    href: "https://github.com/fluxerapp/fluxer/issues/1393",
                    class: "font-medium underline decoration-amber-400 underline-offset-2 hover:text-amber-950",
                }]))
            }
        }
    } else {
        html! {}
    };
    let details = html! {
        @if let Some(info) = latest_version {
            p class="caption mt-2 text-gray-500" { (format_latest_version_line(info)) }
        }
    };
    let actions = html! {
        a class="inline-flex w-full items-center justify-center gap-2 rounded-xl bg-[#4641D9] px-5 py-3 font-semibold text-sm text-white shadow-md transition hover:bg-[#3832B8] sm:w-auto" href=(primary_url) {
            (icon(Icon::Download, "h-5 w-5 shrink-0"))
            span { (button_label) }
        }
        @if !alternates.is_empty() {
            div class="flex flex-wrap items-center gap-x-3 gap-y-1 md:justify-end" {
                span class="body-sm text-gray-500" { (tr(i18n, ctx, DOWNLOAD_OTHER_DOWNLOADS_DESCRIPTOR)) }
                @for alt in &alternates {
                    a class="body-sm text-gray-600 underline decoration-gray-300 underline-offset-2 transition hover:text-[#4641D9]" href=(alt.url) target=[alt.external.then_some("_blank")] rel=[alt.external.then_some("noopener noreferrer")] {
                        (alt.label)
                    }
                }
            }
        }
    };
    download_row(DownloadRow {
        icon: platform_icon(platform),
        title: name,
        beta_label: None,
        requirement,
        description,
        details,
        actions,
    })
}

#[derive(Clone, Copy)]
enum MobileDownload {
    WebApp,
    Ios,
    Android,
}

struct DownloadRow {
    icon: Icon,
    title: String,
    beta_label: Option<String>,
    requirement: String,
    description: Markup,
    details: Markup,
    actions: Markup,
}

fn mobile_download_row(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    download: MobileDownload,
) -> Markup {
    match download {
        MobileDownload::WebApp => download_row(DownloadRow {
            icon: Icon::Devices,
            title: tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_WEB_APP_TITLE_DESCRIPTOR),
            beta_label: None,
            requirement: String::new(),
            description: html! {
                p class="body-sm mt-2 max-w-xl text-gray-600" {
                    (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_WEB_APP_BODY_DESCRIPTOR))
                }
            },
            details: html! {},
            actions: html! {
                a class="inline-flex w-full items-center justify-center rounded-xl bg-[#4641D9] px-5 py-3 font-semibold text-sm text-white shadow-md transition hover:bg-[#3832B8] sm:w-auto" href=(ctx.app_url("/channels/@me")) {
                    (tr(i18n, ctx, DOWNLOAD_OPEN_IN_BROWSER_DESCRIPTOR))
                }
                (pwa::pwa_install_trigger(i18n, ctx))
            },
        }),
        MobileDownload::Ios => download_row(DownloadRow {
            icon: Icon::Apple,
            title: tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_IOS_TITLE_DESCRIPTOR),
            beta_label: Some(tr(i18n, ctx, BETA_AND_ACCESS_BETA_LABEL_DESCRIPTOR)),
            requirement: system_requirement(i18n, ctx, Platform::Ios),
            description: html! {
                p class="body-sm mt-2 max-w-xl text-gray-600" {
                    (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_IOS_BODY_DESCRIPTOR))
                }
            },
            details: html! {},
            actions: html! {
                div class="flex flex-wrap items-center gap-x-3 gap-y-1 md:justify-end" {
                    span class="body-sm text-gray-500" { (tr(i18n, ctx, DOWNLOAD_OTHER_DOWNLOADS_DESCRIPTOR)) }
                    a class="body-sm text-gray-600 underline decoration-gray-300 underline-offset-2 transition hover:text-[#4641D9]" href=(ctx.app_url("/channels/@me")) {
                        (tr(i18n, ctx, DOWNLOAD_OPEN_IN_BROWSER_DESCRIPTOR))
                    }
                    (pwa::pwa_install_trigger(i18n, ctx))
                }
            },
        }),
        MobileDownload::Android => download_row(DownloadRow {
            icon: Icon::Android,
            title: tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_ANDROID_TITLE_DESCRIPTOR),
            beta_label: Some(tr(i18n, ctx, BETA_AND_ACCESS_BETA_LABEL_DESCRIPTOR)),
            requirement: system_requirement(i18n, ctx, Platform::Android),
            description: html! {
                p class="body-sm mt-2 max-w-xl text-gray-600" {
                    (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_ANDROID_BODY_DESCRIPTOR))
                }
            },
            details: html! {},
            actions: html! {
                a class="inline-flex w-full items-center justify-center gap-2 rounded-xl bg-[#4641D9] px-5 py-3 font-semibold text-sm text-white shadow-md transition hover:bg-[#3832B8] sm:w-auto" href="https://github.com/fluxerapp/flutter_client" target="_blank" rel="noopener noreferrer" {
                    (icon(Icon::Github, "h-5 w-5 shrink-0"))
                    (tr(i18n, ctx, PLATFORM_SUPPORT_MOBILE_ANDROID_CTA_DESCRIPTOR))
                }
            },
        }),
    }
}

fn download_row(row: DownloadRow) -> Markup {
    let DownloadRow {
        icon: row_icon,
        title,
        beta_label,
        requirement,
        description,
        details,
        actions,
    } = row;
    html! {
        li class="flex flex-col gap-5 border-gray-200 border-b py-7 last:border-b-0 md:flex-row md:items-start md:gap-10" {
            div class="flex min-w-0 flex-1 items-start gap-4" {
                div class="flex h-11 w-11 shrink-0 items-center justify-center rounded-xl bg-[#4641D9]/10" {
                    (icon(row_icon, "h-6 w-6 text-[#4641D9]"))
                }
                div class="min-w-0" {
                    div class="flex flex-wrap items-center gap-x-3 gap-y-1.5" {
                        div class="flex items-center gap-x-2" {
                            h2 class="title-sm text-gray-950" { (title) }
                            @if let Some(beta_label) = beta_label {
                                span class="caption rounded-full border border-amber-200 bg-amber-50 px-2.5 py-0.5 font-semibold text-amber-800 uppercase" {
                                    (beta_label)
                                }
                            }
                        }
                        @if !requirement.is_empty() {
                            span class="body-sm text-gray-500" { (requirement) }
                        }
                    }
                    (description)
                    (details)
                }
            }
            div class="flex shrink-0 flex-col gap-2.5 md:w-60 md:items-end" {
                (actions)
            }
        }
    }
}

const FLATPAK_URL: &str = "https://flathub.org/en/apps/app.fluxer.Fluxer";

fn platform_icon(platform: Platform) -> Icon {
    match platform {
        Platform::Windows => Icon::Windows,
        Platform::Macos | Platform::Ios => Icon::Apple,
        Platform::Linux => Icon::Linux,
        Platform::Android => Icon::Android,
        Platform::Unknown => Icon::Download,
    }
}

fn desktop_url(ctx: &RequestContext, platform: &str, arch: &str, format: &str) -> String {
    let channel = ctx.release_channel.segment();
    let path = format!("/dl/desktop/{channel}/{platform}/{arch}/latest/{format}");
    let final_path = desktop_path_with_query(path, ctx.test_build);
    ctx.api_url(&final_path)
}

fn desktop_path_with_query(mut path: String, test_build: bool) -> String {
    if test_build {
        path.push('?');
        path.push_str("test=1");
    }
    path
}

fn platform_name(i18n: &MarketingI18n, ctx: &RequestContext, platform: Platform) -> String {
    match platform {
        Platform::Windows => tr(
            i18n,
            ctx,
            PLATFORM_SUPPORT_PLATFORMS_WINDOWS_NAME_DESCRIPTOR,
        ),
        Platform::Macos => tr(i18n, ctx, PLATFORM_SUPPORT_PLATFORMS_MACOS_NAME_DESCRIPTOR),
        Platform::Linux => tr(i18n, ctx, PLATFORM_SUPPORT_PLATFORMS_LINUX_NAME_DESCRIPTOR),
        Platform::Ios => tr(i18n, ctx, PLATFORM_SUPPORT_PLATFORMS_IOS_NAME_DESCRIPTOR),
        Platform::Android => tr(
            i18n,
            ctx,
            PLATFORM_SUPPORT_PLATFORMS_ANDROID_NAME_DESCRIPTOR,
        ),
        Platform::Unknown => tr(i18n, ctx, DOWNLOAD_DOWNLOAD_DESCRIPTOR),
    }
}

fn system_requirement(i18n: &MarketingI18n, ctx: &RequestContext, platform: Platform) -> String {
    match platform {
        Platform::Windows => tr(
            i18n,
            ctx,
            PLATFORM_SUPPORT_PLATFORMS_WINDOWS_MIN_VERSION_DESCRIPTOR,
        ),
        Platform::Macos => tr(
            i18n,
            ctx,
            PLATFORM_SUPPORT_PLATFORMS_MACOS_MIN_VERSION_DESCRIPTOR,
        ),
        Platform::Ios => tr(
            i18n,
            ctx,
            PLATFORM_SUPPORT_PLATFORMS_IOS_MIN_VERSION_DESCRIPTOR,
        ),
        Platform::Android => tr(
            i18n,
            ctx,
            PLATFORM_SUPPORT_PLATFORMS_ANDROID_MIN_VERSION_DESCRIPTOR,
        ),
        Platform::Linux | Platform::Unknown => String::new(),
    }
}

fn footer(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    footer_with_class(i18n, ctx, "")
}

fn footer_with_class(i18n: &MarketingI18n, ctx: &RequestContext, class_name: &str) -> Markup {
    let link_class = "body-lg text-white/90 transition-colors hover:text-white hover:underline";
    let footer_class = if class_name.is_empty() {
        "gradient-purple px-6 py-20 text-white sm:px-8 md:px-12 md:py-24 lg:px-16 xl:px-20"
            .to_owned()
    } else {
        format!(
            "gradient-purple px-6 py-20 text-white sm:px-8 md:px-12 md:py-24 lg:px-16 xl:px-20 {class_name}"
        )
    };
    html! {
        footer class=(footer_class) {
            div class="mx-auto max-w-7xl" {
                div class="mb-10 md:mb-12" {
                    div class="flex flex-col items-start gap-4 lg:flex-row lg:items-center lg:justify-between lg:gap-8" {
                        div class="flex flex-col items-start gap-3" {
                            div class="flex shrink-0 items-center justify-center sm:h-12 sm:w-12 sm:rounded-full sm:bg-white/10" {
                                (icon(Icon::Heart, "h-8 w-8 text-white"))
                            }
                            p class="body-lg max-w-xl text-white/90" {
                                (tr(i18n, ctx, FOOTER_HELP_SUPPORT_AN_INDEPENDENT_COMMUNICATION_DESCRIPTOR))
                            }
                        }
                        a class="mt-2 inline-flex w-fit shrink-0 items-center justify-center gap-2 rounded-full bg-white px-6 py-3 font-semibold text-[#4641D9] shadow-lg transition-opacity hover:opacity-90 lg:mt-0" href=(ctx.href("/donate")) {
                            (tr(i18n, ctx, FOOTER_DONATE_DESCRIPTOR))
                            (icon(Icon::ArrowRight, "h-4 w-4 shrink-0"))
                        }
                    }
                }
                div class="mb-10" {
                    (icon(Icon::FluxerLogoWordmark, "h-8"))
                }
                div class="grid grid-cols-1 gap-8 sm:grid-cols-3 sm:gap-10 md:gap-x-12 md:gap-y-10 min-[480px]:grid-cols-2 min-[480px]:gap-x-6 min-[480px]:gap-y-8" {
                    div {
                        h3 class="title mb-4 text-white md:mb-6" { (tr(i18n, ctx, FOOTER_FLUXER_DESCRIPTOR)) }
                        ul class="space-y-3" {
                            (footer_link(ctx.href("/plutonium"), tr(i18n, ctx, FOOTER_PLUTONIUM_TIER_DESCRIPTOR), link_class))
                            (footer_link(ctx.href("/partners"), tr(i18n, ctx, FOOTER_PARTNERS_DESCRIPTOR), link_class))
                            (footer_link(ctx.href("/download"), tr(i18n, ctx, FOOTER_DOWNLOAD_DESCRIPTOR), link_class))
                            (footer_link("https://github.com/fluxerapp/fluxer".to_owned(), tr(i18n, ctx, FOOTER_SOURCE_CODE_DESCRIPTOR), link_class))
                            (footer_link("https://bsky.app/profile/fluxer.app".to_owned(), tr(i18n, ctx, FOOTER_BLUESKY_SOCIAL_MEDIA_DESCRIPTOR), link_class))
                            li {
                                div class="flex items-center gap-2" {
                                    a href=(ctx.href("/blog")) class=(link_class) {
                                        (tr(i18n, ctx, COMPANY_AND_RESOURCES_BLOG_DESCRIPTOR))
                                    }
                                    a href=(ctx.href("/blog/rss.xml")) title=(tr(i18n, ctx, FOOTER_RSS_FEED_DESCRIPTOR)) class="text-white/90 transition-colors hover:text-white" {
                                        (icon(Icon::Rss, "h-[1em] w-[1em]"))
                                    }
                                }
                            }
                            (footer_link(ctx.href("/blog/roadmap-2026"), tr(i18n, ctx, FOOTER_ROADMAP_DESCRIPTOR), link_class))
                            (footer_link(ctx.href("/help"), tr(i18n, ctx, COMPANY_AND_RESOURCES_HELP_HELP_CENTER_DESCRIPTOR), link_class))
                            (footer_link("https://fluxerstatus.com".to_owned(), tr(i18n, ctx, FOOTER_STATUS_DESCRIPTOR), link_class))
                            (footer_link(ctx.href("/press"), tr(i18n, ctx, FOOTER_PRESS_DESCRIPTOR), link_class))
                            (footer_link("https://docs.fluxer.app".to_owned(), tr(i18n, ctx, COMPANY_AND_RESOURCES_DOCS_DESCRIPTOR), link_class))
                            (footer_link(ctx.href("/careers"), tr(i18n, ctx, COMPANY_AND_RESOURCES_CAREERS_LABEL_DESCRIPTOR), link_class))
                        }
                    }
                    div {
                        h3 class="title mb-4 text-white md:mb-6" { (tr(i18n, ctx, FOOTER_POLICIES_DESCRIPTOR)) }
                        ul class="space-y-3" {
                            (footer_link(ctx.href("/terms"), tr(i18n, ctx, FOOTER_TERMS_OF_SERVICE_DESCRIPTOR), link_class))
                            (footer_link(ctx.href("/privacy"), tr(i18n, ctx, FOOTER_PRIVACY_POLICY_DESCRIPTOR), link_class))
                            (footer_link(ctx.href("/guidelines"), tr(i18n, ctx, FOOTER_COMMUNITY_GUIDELINES_DESCRIPTOR), link_class))
                            (footer_link(ctx.href("/security"), tr(i18n, ctx, FOOTER_SECURITY_BUG_BOUNTY_DESCRIPTOR), link_class))
                            (footer_link(ctx.href("/company-information"), tr(i18n, ctx, FOOTER_COMPANY_INFORMATION_DESCRIPTOR), link_class))
                        }
                    }
                    div class="sm:col-span-1 min-[480px]:col-span-2" {
                        h3 class="title mb-4 text-white md:mb-6" { (tr(i18n, ctx, FOOTER_CONNECT_DESCRIPTOR)) }
                        ul class="space-y-3" {
                            (footer_link("mailto:press@fluxer.app".to_owned(), "press@fluxer.app".to_owned(), link_class))
                            (footer_link("mailto:support@fluxer.app".to_owned(), "support@fluxer.app".to_owned(), link_class))
                            (footer_link(ctx.href("/help/report-bug"), tr(i18n, ctx, FOOTER_REPORT_A_BUG_DESCRIPTOR), link_class))
                        }
                    }
                }
                div class="mt-12 pt-8" {
                    p class="body-sm text-white/80" {
                        (tr(i18n, ctx, FOOTER_FLUXER_PLATFORM_AB_SWEDISH_LIMITED_DESCRIPTOR))
                    }
                }
            }
        }
    }
}

fn footer_link(href: String, label: String, class_name: &str) -> Markup {
    html! {
        li {
            a href=(href) class=(class_name) { (label) }
        }
    }
}

fn locale_modal(i18n: &MarketingI18n, ctx: &RequestContext) -> Markup {
    let title = tr(i18n, ctx, LANGUAGES_CHOOSE_YOUR_LANGUAGE_DESCRIPTOR);
    let notice = tr(
        i18n,
        ctx,
        COMPANY_AND_RESOURCES_SOURCE_AND_CONTRIBUTION_TRANSLATION_LLM_TRANSLATION_NOTE_DESCRIPTOR,
    );
    let close_label = tr(i18n, ctx, NAVIGATION_CLOSE_DESCRIPTOR);
    html! {
        div id="locale-modal-backdrop" class="locale-modal-backdrop" popover="auto" {
            button
                type="button"
                class="absolute inset-0 h-full w-full cursor-default"
                popovertarget="locale-modal-backdrop"
                popovertargetaction="hide"
                aria-label=(close_label.clone())
            {}
            div class="locale-modal relative" role="dialog" aria-modal="true" aria-labelledby="locale-modal-title" {
                div class="flex h-full flex-col" {
                    div class="flex items-center justify-between p-6 pb-0" {
                        h2 id="locale-modal-title" class="font-bold text-gray-900 text-xl" { (title) }
                        button
                            type="button"
                            class="rounded-lg p-2 text-gray-600 hover:bg-gray-100 hover:text-gray-900"
                            id="locale-close"
                            aria-label=(close_label)
                            popovertarget="locale-modal-backdrop"
                            popovertargetaction="hide"
                        {
                            svg class="h-5 w-5" fill="none" stroke="currentColor" viewBox="0 0 24 24" {
                                path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" {}
                            }
                        }
                    }
                    p class="px-6 pb-2 text-gray-500 text-xs leading-relaxed" { (notice) }
                    div class="flex-1 overflow-y-auto p-6 pt-4" {
                        div class="grid grid-cols-2 gap-3 md:grid-cols-4 lg:grid-cols-5" {
                            @for locale in crate::i18n::Locale::ALL {
                                (locale_card(i18n, ctx, *locale))
                            }
                        }
                    }
                }
            }
        }
    }
}

fn locale_card(i18n: &MarketingI18n, ctx: &RequestContext, locale: crate::i18n::Locale) -> Markup {
    let is_current = locale == ctx.locale;
    let native_name = locale_native_name(locale);
    let localized_name = tr(i18n, ctx, locale_label_descriptor(locale));
    let border_class = if is_current {
        "border-blue-500 bg-blue-50"
    } else {
        "border-gray-200"
    };
    html! {
        form action=(ctx.href("/_locale")) method="post" class="locale-form contents" {
            input type="hidden" name="locale" value=(locale.code());
            input type="hidden" name="redirect" value=(ctx.current_path);
            button
                type="submit"
                aria-current=[is_current.then_some("true")]
                class=(format!(
                    "relative flex min-h-[120px] flex-col items-center justify-center gap-3 rounded-xl border-2 p-4 text-center transition-colors hover:bg-gray-50 {border_class}"
                )) {
                @if is_current {
                    div class="absolute top-2 right-2 flex h-6 w-6 items-center justify-center rounded-full bg-blue-600" {
                        svg class="h-4 w-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24" stroke-width="2" {
                            path stroke-linecap="round" stroke-linejoin="round" d="M5 13l4 4L19 7" {}
                        }
                    }
                }
                (flag_svg(ctx, locale, "h-8 w-8 rounded"))
                div class="font-semibold text-gray-900 text-sm" { (native_name) }
                div class="text-gray-500 text-xs" { (localized_name) }
            }
        }
    }
}

pub(crate) fn flag_svg(
    ctx: &RequestContext,
    locale: crate::i18n::Locale,
    class_name: &str,
) -> Markup {
    let flag_url = format!(
        "{}/marketing/flags/{}.svg",
        ctx.static_cdn_endpoint,
        locale_flag_code(locale)
    );
    html! {
        img src=(flag_url) alt="" aria-hidden="true" loading="lazy" class=(class_name);
    }
}

pub(crate) fn locale_native_name(locale: crate::i18n::Locale) -> &'static str {
    match locale {
        crate::i18n::Locale::Ar => "العربية",
        crate::i18n::Locale::Bg => "Български",
        crate::i18n::Locale::Cs => "Čeština",
        crate::i18n::Locale::Da => "Dansk",
        crate::i18n::Locale::De => "Deutsch",
        crate::i18n::Locale::El => "Ελληνικά",
        crate::i18n::Locale::EnGb => "English",
        crate::i18n::Locale::EnUs => "English (US)",
        crate::i18n::Locale::EsEs => "Español (España)",
        crate::i18n::Locale::Es419 => "Español (Latinoamérica)",
        crate::i18n::Locale::Fi => "Suomi",
        crate::i18n::Locale::Fr => "Français",
        crate::i18n::Locale::He => "עברית",
        crate::i18n::Locale::Hi => "हिन्दी",
        crate::i18n::Locale::Hr => "Hrvatski",
        crate::i18n::Locale::Hu => "Magyar",
        crate::i18n::Locale::Id => "Bahasa Indonesia",
        crate::i18n::Locale::It => "Italiano",
        crate::i18n::Locale::Ja => "日本語",
        crate::i18n::Locale::Ko => "한국어",
        crate::i18n::Locale::Lt => "Lietuvių",
        crate::i18n::Locale::Nl => "Nederlands",
        crate::i18n::Locale::No => "Norsk",
        crate::i18n::Locale::Pl => "Polski",
        crate::i18n::Locale::PtBr => "Português (Brasil)",
        crate::i18n::Locale::Ro => "Română",
        crate::i18n::Locale::Ru => "Русский",
        crate::i18n::Locale::SvSe => "Svenska",
        crate::i18n::Locale::Th => "ไทย",
        crate::i18n::Locale::Tr => "Türkçe",
        crate::i18n::Locale::Uk => "Українська",
        crate::i18n::Locale::Vi => "Tiếng Việt",
        crate::i18n::Locale::ZhCn => "简体中文",
        crate::i18n::Locale::ZhTw => "繁體中文",
    }
}

pub(crate) fn locale_flag_code(locale: crate::i18n::Locale) -> &'static str {
    match locale {
        crate::i18n::Locale::Ar => "1f1f8-1f1e6",
        crate::i18n::Locale::Bg => "1f1e7-1f1ec",
        crate::i18n::Locale::Cs => "1f1e8-1f1ff",
        crate::i18n::Locale::Da => "1f1e9-1f1f0",
        crate::i18n::Locale::De => "1f1e9-1f1ea",
        crate::i18n::Locale::El => "1f1ec-1f1f7",
        crate::i18n::Locale::EnGb => "1f1ec-1f1e7",
        crate::i18n::Locale::EnUs => "1f1fa-1f1f8",
        crate::i18n::Locale::EsEs => "1f1ea-1f1f8",
        crate::i18n::Locale::Es419 => "1f30e",
        crate::i18n::Locale::Fi => "1f1eb-1f1ee",
        crate::i18n::Locale::Fr => "1f1eb-1f1f7",
        crate::i18n::Locale::He => "1f1ee-1f1f1",
        crate::i18n::Locale::Hi => "1f1ee-1f1f3",
        crate::i18n::Locale::Hr => "1f1ed-1f1f7",
        crate::i18n::Locale::Hu => "1f1ed-1f1fa",
        crate::i18n::Locale::Id => "1f1ee-1f1e9",
        crate::i18n::Locale::It => "1f1ee-1f1f9",
        crate::i18n::Locale::Ja => "1f1ef-1f1f5",
        crate::i18n::Locale::Ko => "1f1f0-1f1f7",
        crate::i18n::Locale::Lt => "1f1f1-1f1f9",
        crate::i18n::Locale::Nl => "1f1f3-1f1f1",
        crate::i18n::Locale::No => "1f1f3-1f1f4",
        crate::i18n::Locale::Pl => "1f1f5-1f1f1",
        crate::i18n::Locale::PtBr => "1f1e7-1f1f7",
        crate::i18n::Locale::Ro => "1f1f7-1f1f4",
        crate::i18n::Locale::Ru => "1f1f7-1f1fa",
        crate::i18n::Locale::SvSe => "1f1f8-1f1ea",
        crate::i18n::Locale::Th => "1f1f9-1f1ed",
        crate::i18n::Locale::Tr => "1f1f9-1f1f7",
        crate::i18n::Locale::Uk => "1f1fa-1f1e6",
        crate::i18n::Locale::Vi => "1f1fb-1f1f3",
        crate::i18n::Locale::ZhCn => "1f1e8-1f1f3",
        crate::i18n::Locale::ZhTw => "1f1f9-1f1fc",
    }
}

pub(crate) fn tr(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    descriptor: MarketingMessageDescriptor,
) -> String {
    i18n.text(ctx.locale, descriptor)
}

pub(crate) fn format_long_date(iso: &str, locale: crate::i18n::Locale) -> String {
    let parts: Vec<&str> = iso.split('-').collect();
    if parts.len() != 3 {
        return iso.to_owned();
    }
    let (Ok(year), Ok(month), Ok(day)) = (
        parts[0].parse::<i32>(),
        parts[1].parse::<u32>(),
        parts[2].parse::<u32>(),
    ) else {
        return iso.to_owned();
    };
    if !(1..=12).contains(&month) {
        return iso.to_owned();
    }
    let Ok(icu_locale) = locale.code().parse::<icu_locale::Locale>() else {
        return iso.to_owned();
    };
    let Ok(date) = icu_datetime::input::Date::try_new_iso(year, month as u8, day as u8) else {
        return iso.to_owned();
    };
    let Ok(formatter) = icu_datetime::DateTimeFormatter::try_new(
        icu_locale.into(),
        icu_datetime::fieldsets::YMD::long(),
    ) else {
        return iso.to_owned();
    };
    formatter.format(&date).to_string()
}

pub(crate) fn locale_label_descriptor(locale: crate::i18n::Locale) -> MarketingMessageDescriptor {
    match locale {
        crate::i18n::Locale::Ar => LANGUAGES_LIST_ARABIC_DESCRIPTOR,
        crate::i18n::Locale::Bg => LANGUAGES_LIST_BULGARIAN_DESCRIPTOR,
        crate::i18n::Locale::Cs => LANGUAGES_LIST_CZECH_DESCRIPTOR,
        crate::i18n::Locale::Da => LANGUAGES_LIST_DANISH_DESCRIPTOR,
        crate::i18n::Locale::De => LANGUAGES_LIST_GERMAN_DESCRIPTOR,
        crate::i18n::Locale::El => LANGUAGES_LIST_GREEK_DESCRIPTOR,
        crate::i18n::Locale::EnGb => LANGUAGES_LIST_ENGLISH_UK_DESCRIPTOR,
        crate::i18n::Locale::EnUs => LANGUAGES_LIST_ENGLISH_US_DESCRIPTOR,
        crate::i18n::Locale::Es419 => LANGUAGES_LIST_SPANISH_LATIN_AMERICA_DESCRIPTOR,
        crate::i18n::Locale::EsEs => LANGUAGES_LIST_SPANISH_SPAIN_DESCRIPTOR,
        crate::i18n::Locale::Fi => LANGUAGES_LIST_FINNISH_DESCRIPTOR,
        crate::i18n::Locale::Fr => LANGUAGES_LIST_FRENCH_DESCRIPTOR,
        crate::i18n::Locale::He => LANGUAGES_LIST_HEBREW_DESCRIPTOR,
        crate::i18n::Locale::Hi => LANGUAGES_LIST_HINDI_DESCRIPTOR,
        crate::i18n::Locale::Hr => LANGUAGES_LIST_CROATIAN_DESCRIPTOR,
        crate::i18n::Locale::Hu => LANGUAGES_LIST_HUNGARIAN_DESCRIPTOR,
        crate::i18n::Locale::Id => LANGUAGES_LIST_INDONESIAN_DESCRIPTOR,
        crate::i18n::Locale::It => LANGUAGES_LIST_ITALIAN_DESCRIPTOR,
        crate::i18n::Locale::Ja => LANGUAGES_LIST_JAPANESE_DESCRIPTOR,
        crate::i18n::Locale::Ko => LANGUAGES_LIST_KOREAN_DESCRIPTOR,
        crate::i18n::Locale::Lt => LANGUAGES_LIST_LITHUANIAN_DESCRIPTOR,
        crate::i18n::Locale::Nl => LANGUAGES_LIST_DUTCH_DESCRIPTOR,
        crate::i18n::Locale::No => LANGUAGES_LIST_NORWEGIAN_DESCRIPTOR,
        crate::i18n::Locale::Pl => LANGUAGES_LIST_POLISH_DESCRIPTOR,
        crate::i18n::Locale::PtBr => LANGUAGES_LIST_PORTUGUESE_BRAZIL_DESCRIPTOR,
        crate::i18n::Locale::Ro => LANGUAGES_LIST_ROMANIAN_DESCRIPTOR,
        crate::i18n::Locale::Ru => LANGUAGES_LIST_RUSSIAN_DESCRIPTOR,
        crate::i18n::Locale::SvSe => LANGUAGES_LIST_SWEDISH_DESCRIPTOR,
        crate::i18n::Locale::Th => LANGUAGES_LIST_THAI_DESCRIPTOR,
        crate::i18n::Locale::Tr => LANGUAGES_LIST_TURKISH_DESCRIPTOR,
        crate::i18n::Locale::Uk => LANGUAGES_LIST_UKRAINIAN_DESCRIPTOR,
        crate::i18n::Locale::Vi => LANGUAGES_LIST_VIETNAMESE_DESCRIPTOR,
        crate::i18n::Locale::ZhCn => LANGUAGES_LIST_CHINESE_SIMPLIFIED_DESCRIPTOR,
        crate::i18n::Locale::ZhTw => LANGUAGES_LIST_CHINESE_TRADITIONAL_DESCRIPTOR,
    }
}
