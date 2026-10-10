// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, bail};
use clap::{Parser, ValueEnum};
use regex::Regex;
use serde_json::{Map, Value, json};

use crate::config::{
    AUTO_I18N_REVIEWED_UNCHANGED_FILE, DEFAULT_LOCALE_CONCURRENCY, DEFAULT_OPENROUTER_APP_TITLE,
    DEFAULT_OPENROUTER_BASE_URL, DEFAULT_OPENROUTER_FALLBACK_MODELS,
    DEFAULT_OPENROUTER_HTTP_REFERER, DEFAULT_OPENROUTER_MODEL, DEFAULT_OPENROUTER_PROVIDER_SORT,
    DEFAULT_PROGRESS_INTERVAL_SECONDS, DEFAULT_REQUEST_TIMEOUT_SECONDS, DEFAULT_STRING_CONCURRENCY,
    EnvOverlay, SOURCE_LOCALE, default_app_dir, env_value, is_auto_i18n_unchanged_comment,
    locales_dir, positive_float_env, positive_int_env, trim_trailing_slash,
};
use crate::json_catalog::{
    StaticJsonCatalogConfig, read_static_json_entries, rebuild_static_json_allow_replacing,
    reset_static_json_translations,
};
use crate::llm::{LocalizationClient, base_options};
use crate::locales::{display_name, is_supported_locale};
use crate::openrouter::{
    OpenRouterClient, OpenRouterClientConfig, parse_openrouter_fallback_models,
};
use crate::po::{
    Entry, Translation, extract_placeholder_hints, extract_translator_comments, parse_po,
    rebuild_po_allow_replacing, reset_po_translations,
};
use crate::prompts::{build_system_prompt, build_target_locale_prompt, load_prompt_guidance};
use crate::reviewed_unchanged::ReviewedUnchangedStore;
use crate::tokens::{
    TokenAlias, build_masked_source, build_token_alias_context, extract_icu_controls,
    extract_preserved_tokens, has_icu_control, mask_text_with_aliases,
    normalize_localized_capitalization, restore_masked_tokens, should_keep_unchanged,
    validate_locale_specific_localization, validate_localization,
};
use crate::ts_catalog::StaticTsCatalogKind;

const FULL_TRANSLATION_ATTEMPTS: usize = 3;
const SEGMENT_TRANSLATION_ATTEMPTS: usize = 2;
const POLISH_TRANSLATION_ATTEMPTS: usize = 2;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TranslateArgs {
    pub all: bool,
    pub batch_size: usize,
    pub catalog: CatalogName,
    pub dry_run: bool,
    pub limit: Option<usize>,
    pub locales: Vec<String>,
    pub msgctxts: Vec<String>,
    pub refresh_source_equal: bool,
    pub reset: bool,
    pub string_concurrency: usize,
    pub locale_concurrency: usize,
    pub progress_interval: Duration,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CatalogLayout {
    NestedMessages,
    StaticJson(StaticJsonCatalogConfig),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
pub enum CatalogName {
    #[value(name = "app")]
    App,
    #[value(name = "errors")]
    Errors,
    #[value(name = "api-content", alias = "content")]
    ApiContent,
    #[value(name = "email", alias = "emails")]
    Email,
}

impl CatalogName {
    fn label(self) -> &'static str {
        match self {
            CatalogName::App => "app",
            CatalogName::Errors => "errors",
            CatalogName::ApiContent => "api-content",
            CatalogName::Email => "email",
        }
    }
}

#[derive(Clone, Debug)]
pub struct RuntimeConfig {
    pub app_dir: PathBuf,
    pub catalog_layout: CatalogLayout,
    pub locales_dir: PathBuf,
    pub reviewed_unchanged_path: PathBuf,
    pub openrouter_api_key: String,
    pub openrouter_app_title: String,
    pub openrouter_base_url: String,
    pub openrouter_fallback_models: Vec<String>,
    pub openrouter_http_referer: String,
    pub openrouter_model: String,
    pub openrouter_provider_sort: String,
    pub request_timeout_seconds: f64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CatalogPaths {
    pub locales_dir: PathBuf,
    pub catalog_layout: CatalogLayout,
    pub reviewed_unchanged_path: PathBuf,
}

pub fn catalog_paths(app_dir: &Path, catalog: CatalogName) -> CatalogPaths {
    let repo_root = app_dir
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));
    match catalog {
        CatalogName::App => {
            let locales_dir = locales_dir(app_dir);
            CatalogPaths {
                reviewed_unchanged_path: locales_dir.join(AUTO_I18N_REVIEWED_UNCHANGED_FILE),
                locales_dir,
                catalog_layout: CatalogLayout::NestedMessages,
            }
        }
        CatalogName::Errors => static_json_catalog_paths(
            repo_root
                .join("packages")
                .join("errors")
                .join("src")
                .join("i18n"),
            StaticTsCatalogKind::SimpleMessages,
        ),
        CatalogName::ApiContent => static_json_catalog_paths(
            repo_root
                .join("fluxer_api")
                .join("src")
                .join("api")
                .join("content_i18n"),
            StaticTsCatalogKind::SimpleMessages,
        ),
        CatalogName::Email => static_json_catalog_paths(
            repo_root
                .join("fluxer_api")
                .join("pkgs")
                .join("email")
                .join("src")
                .join("email_i18n"),
            StaticTsCatalogKind::EmailTemplates,
        ),
    }
}

fn static_json_catalog_paths(i18n_dir: PathBuf, kind: StaticTsCatalogKind) -> CatalogPaths {
    let weblate_dir = i18n_dir.join("weblate");
    CatalogPaths {
        locales_dir: weblate_dir.join("locales"),
        catalog_layout: CatalogLayout::StaticJson(StaticJsonCatalogConfig {
            kind,
            source_path: weblate_dir.join("messages.json"),
        }),
        reviewed_unchanged_path: i18n_dir
            .join("locales")
            .join(AUTO_I18N_REVIEWED_UNCHANGED_FILE),
    }
}

impl RuntimeConfig {
    pub fn from_env(env_overrides: &EnvOverlay, catalog: CatalogName) -> Self {
        let app_dir = default_app_dir();
        let CatalogPaths {
            locales_dir,
            catalog_layout,
            reviewed_unchanged_path,
        } = catalog_paths(&app_dir, catalog);
        let openrouter_base_url = trim_trailing_slash(&env_value(
            "OPENROUTER_BASE_URL",
            env_overrides,
            DEFAULT_OPENROUTER_BASE_URL,
        ));
        let openrouter_model = env_value("I18N_LLM_MODEL", env_overrides, "")
            .if_empty(|| env_value("OPENROUTER_MODEL", env_overrides, DEFAULT_OPENROUTER_MODEL));
        let openrouter_api_key = env_value("OPENROUTER_API_KEY", env_overrides, "");
        let openrouter_fallback_models = parse_openrouter_fallback_models(&env_value(
            "OPENROUTER_FALLBACK_MODELS",
            env_overrides,
            DEFAULT_OPENROUTER_FALLBACK_MODELS,
        ));
        let openrouter_provider_sort = env_value(
            "OPENROUTER_PROVIDER_SORT",
            env_overrides,
            DEFAULT_OPENROUTER_PROVIDER_SORT,
        );
        let openrouter_http_referer = env_value(
            "OPENROUTER_HTTP_REFERER",
            env_overrides,
            DEFAULT_OPENROUTER_HTTP_REFERER,
        );
        let openrouter_app_title = env_value(
            "OPENROUTER_APP_TITLE",
            env_overrides,
            DEFAULT_OPENROUTER_APP_TITLE,
        );
        let request_timeout_seconds = positive_float_env(
            "FLUXER_AUTO_I18N_REQUEST_TIMEOUT",
            DEFAULT_REQUEST_TIMEOUT_SECONDS,
            env_overrides,
        );
        Self {
            app_dir,
            catalog_layout,
            locales_dir,
            reviewed_unchanged_path,
            openrouter_api_key,
            openrouter_app_title,
            openrouter_base_url,
            openrouter_fallback_models,
            openrouter_http_referer,
            openrouter_model,
            openrouter_provider_sort,
            request_timeout_seconds,
        }
    }
}

trait EmptyFallback {
    fn if_empty(self, fallback: impl FnOnce() -> String) -> String;
}

impl EmptyFallback for String {
    fn if_empty(self, fallback: impl FnOnce() -> String) -> String {
        if self.is_empty() { fallback() } else { self }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LocaleResult {
    pub locale: String,
    pub translated: usize,
    pub errors: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LocalePlan {
    pub locale: String,
    pub pending: usize,
    pub selected: usize,
}

#[derive(Clone, Debug)]
pub struct Prompt {
    pub system_prompt: String,
    pub user_prompt: String,
    pub token_aliases: Vec<TokenAlias>,
}

#[derive(Parser, Debug)]
#[command(about = "Translate Lingui PO catalogs with an LLM provider.")]
struct RawTranslateArgs {
    #[arg(long, help = "Process all non-source locales")]
    all: bool,
    #[arg(long = "locale", help = "Process one locale; repeatable")]
    locales: Vec<String>,
    #[arg(
        long,
        value_enum,
        help = "Catalog target to translate: app, errors, api-content, or email"
    )]
    catalog: Option<CatalogName>,
    #[arg(
        long = "msgctxt",
        help = "Only translate entries with this PO msgctxt or static catalog key; repeatable. Email fields use <template>.<subject|body>."
    )]
    msgctxts: Vec<String>,
    #[arg(
        long,
        help = "Clear msgstr values before translating selected locale(s)"
    )]
    reset: bool,
    #[arg(
        long,
        help = "Clear msgstr values before translating every non-source locale"
    )]
    reset_all: bool,
    #[arg(
        long,
        help = "Translate at most n strings per locale for staged review"
    )]
    limit: Option<i64>,
    #[arg(
        long,
        help = "Translate up to n strings in one validated LLM request; falls back per string on batch failures"
    )]
    batch_size: Option<usize>,
    #[arg(
        long,
        help = "Re-translate existing non-English strings that still equal the source"
    )]
    refresh_source_equal: bool,
    #[arg(long)]
    string_concurrency: Option<usize>,
    #[arg(long)]
    locale_concurrency: Option<usize>,
    #[arg(long)]
    progress_interval: Option<f64>,
    #[arg(long)]
    dry_run: bool,
}

pub fn translate_main(argv: &[String], env_overrides: &EnvOverlay) -> Result<u8> {
    let raw = match RawTranslateArgs::try_parse_from(
        std::iter::once("i18n-auto").chain(argv.iter().map(String::as_str)),
    ) {
        Ok(raw) => raw,
        Err(error) => {
            error.print()?;
            return Ok(error.exit_code() as u8);
        }
    };
    let args = normalize_args(raw, env_overrides)?;
    let config = RuntimeConfig::from_env(env_overrides, args.catalog);
    run_translation(&config, &args)
}

fn normalize_args(raw: RawTranslateArgs, env_overrides: &EnvOverlay) -> Result<TranslateArgs> {
    if let Some(limit) = raw.limit
        && limit < 0
    {
        bail!("--limit must be a non-negative integer");
    }
    let progress_interval_seconds = raw.progress_interval.unwrap_or_else(|| {
        positive_float_env(
            "FLUXER_AUTO_I18N_PROGRESS_INTERVAL",
            DEFAULT_PROGRESS_INTERVAL_SECONDS,
            env_overrides,
        )
    });
    if progress_interval_seconds <= 0.0 {
        bail!("--progress-interval must be greater than zero");
    }
    let mut all = raw.all;
    let mut reset = raw.reset;
    if raw.reset_all {
        reset = true;
        all = true;
    }
    let catalog = raw.catalog.unwrap_or(CatalogName::App);
    if !all && raw.locales.is_empty() {
        all = true;
    }
    Ok(TranslateArgs {
        all,
        batch_size: raw.batch_size.unwrap_or(1).max(1),
        catalog,
        dry_run: raw.dry_run,
        limit: raw.limit.map(|limit| limit as usize),
        locales: raw.locales,
        msgctxts: raw.msgctxts,
        refresh_source_equal: raw.refresh_source_equal,
        reset,
        string_concurrency: raw
            .string_concurrency
            .unwrap_or_else(|| {
                positive_int_env(
                    "FLUXER_AUTO_I18N_STRING_CONCURRENCY",
                    DEFAULT_STRING_CONCURRENCY,
                    env_overrides,
                )
            })
            .max(1),
        locale_concurrency: raw
            .locale_concurrency
            .unwrap_or_else(|| {
                positive_int_env(
                    "FLUXER_AUTO_I18N_LOCALE_CONCURRENCY",
                    DEFAULT_LOCALE_CONCURRENCY,
                    env_overrides,
                )
            })
            .max(1),
        progress_interval: Duration::from_secs_f64(progress_interval_seconds),
    })
}

pub fn run_translation(config: &RuntimeConfig, args: &TranslateArgs) -> Result<u8> {
    let locales = select_locales(config, args)?;
    let client = new_runtime_client(config)?;
    log("Starting i18n translation...", false);
    log(&format!("Catalog target: {}", args.catalog.label()), false);
    log("LLM provider: openrouter", false);
    log(
        &format!("Locales directory: {}", config.locales_dir.display()),
        false,
    );
    log(
        &format!(
            "Catalog layout: {}",
            match &config.catalog_layout {
                CatalogLayout::NestedMessages => "nested messages.po",
                CatalogLayout::StaticJson(_) => "weblate JSON locale catalog",
            }
        ),
        false,
    );
    if let CatalogLayout::StaticJson(static_config) = &config.catalog_layout {
        log(
            &format!("Source catalog: {}", static_config.source_path.display()),
            false,
        );
    }
    log(&format!("Model: {}", client.model()), false);
    log(&format!("LLM endpoint: {}", client.base_url()), false);
    log(
        &format!(
            "LLM request timeout: {}s",
            format_float(client.request_timeout_seconds())
        ),
        false,
    );
    log(&format!("Locales: {}", locales.join(", ")), false);
    log(
        &format!(
            "Requested concurrency: {} locales, {} strings per locale",
            args.locale_concurrency, args.string_concurrency
        ),
        false,
    );
    log(
        &format!(
            "Progress interval: {}s",
            format_float(args.progress_interval.as_secs_f64())
        ),
        false,
    );
    if let Some(limit) = args.limit {
        log(&format!("Limit: {limit} strings per locale"), false);
    }
    if args.batch_size > 1 {
        log(
            &format!("Batch size: {} strings per LLM request", args.batch_size),
            false,
        );
    }
    if !args.msgctxts.is_empty() {
        log(
            &format!("Msgctxt filter: {}", args.msgctxts.join(", ")),
            false,
        );
    }
    if args.refresh_source_equal {
        log("Refresh source-equal strings: enabled", false);
    }
    if args.reset {
        log("Reset mode: enabled", false);
    }
    if args.dry_run {
        log("Dry run: enabled", false);
    }
    let plans = locales
        .iter()
        .map(|locale| build_locale_plan(config, locale, args))
        .collect::<Result<Vec<_>>>()?;
    log_translation_plan(&plans);
    let started = Instant::now();
    let mut results = Vec::new();
    let locale_concurrency = args.locale_concurrency.max(1);
    for chunk in locales.chunks(locale_concurrency) {
        if chunk.len() == 1 {
            results.push(process_locale(config, &client, &chunk[0], args)?);
            continue;
        }
        let chunk_results = std::thread::scope(|scope| {
            let handles = chunk
                .iter()
                .map(|locale| {
                    scope.spawn(move || {
                        let client = new_runtime_client(config)?;
                        process_locale(config, &client, locale, args)
                    })
                })
                .collect::<Vec<_>>();
            let mut chunk_results = Vec::with_capacity(handles.len());
            for handle in handles {
                chunk_results.push(
                    handle
                        .join()
                        .map_err(|_| anyhow::anyhow!("i18n locale worker panicked"))??,
                );
            }
            Ok::<_, anyhow::Error>(chunk_results)
        })?;
        results.extend(chunk_results);
    }
    let total_translated = results
        .iter()
        .map(|result| result.translated)
        .sum::<usize>();
    let total_errors = results.iter().map(|result| result.errors).sum::<usize>();
    log(
        &format!(
            "\nTranslation complete in {:.1}s",
            started.elapsed().as_secs_f64()
        ),
        false,
    );
    log(
        &format!("Total: {total_translated} strings translated, {total_errors} errors"),
        false,
    );
    Ok(if total_errors > 0 { 1 } else { 0 })
}

fn new_runtime_client(config: &RuntimeConfig) -> Result<OpenRouterClient> {
    OpenRouterClient::new(OpenRouterClientConfig {
        base_url: config.openrouter_base_url.clone(),
        model: config.openrouter_model.clone(),
        fallback_models: config.openrouter_fallback_models.clone(),
        provider_sort: config.openrouter_provider_sort.clone(),
        http_referer: config.openrouter_http_referer.clone(),
        app_title: config.openrouter_app_title.clone(),
        api_key: config.openrouter_api_key.clone(),
        request_timeout_seconds: config.request_timeout_seconds,
    })
}

pub fn select_locales(config: &RuntimeConfig, args: &TranslateArgs) -> Result<Vec<String>> {
    let available_locales = available_locales(config)?;
    let locales = if args.all {
        available_locales
    } else {
        args.locales.clone()
    };
    for locale in &locales {
        if locale != SOURCE_LOCALE && !is_supported_locale(locale) {
            bail!("Unsupported locale: {locale}");
        }
    }
    Ok(locales)
}

fn available_locales(config: &RuntimeConfig) -> Result<Vec<String>> {
    let mut locales = Vec::new();
    for entry in fs::read_dir(&config.locales_dir)
        .with_context(|| format!("failed to read {}", config.locales_dir.display()))?
    {
        let entry = entry?;
        let locale = match &config.catalog_layout {
            CatalogLayout::NestedMessages => {
                if !entry.file_type()?.is_dir() {
                    continue;
                }
                entry.file_name().to_string_lossy().to_string()
            }
            CatalogLayout::StaticJson(_) => {
                let path = entry.path();
                if path.extension().and_then(|ext| ext.to_str()) != Some("json") {
                    continue;
                }
                let Some(stem) = path.file_stem().and_then(|stem| stem.to_str()) else {
                    continue;
                };
                stem.to_string()
            }
        };
        if locale != SOURCE_LOCALE && is_supported_locale(&locale) {
            locales.push(locale);
        }
    }
    locales.sort();
    Ok(locales)
}

fn should_translate_entry_with_reviewed_unchanged(
    entry: &Entry,
    locale: &str,
    args: &TranslateArgs,
    reviewed_unchanged: bool,
) -> bool {
    if !args.msgctxts.is_empty()
        && !entry
            .msgctxt
            .as_ref()
            .is_some_and(|msgctxt| args.msgctxts.contains(msgctxt))
    {
        return false;
    }
    if entry.msgstr.is_empty() {
        return true;
    }
    if args.refresh_source_equal
        && refreshes_source_equal_locale(locale, args)
        && entry.msgstr == entry.msgid
        && !entry
            .comments
            .iter()
            .any(|comment| is_auto_i18n_unchanged_comment(comment))
        && !reviewed_unchanged
    {
        return !should_keep_unchanged(&entry.msgid, locale);
    }
    false
}

fn refreshes_source_equal_locale(locale: &str, args: &TranslateArgs) -> bool {
    locale != "en-GB" || (!args.all && args.locales.iter().any(|selected| selected == locale))
}

fn prune_reviewed_unchanged(
    store: &mut ReviewedUnchangedStore,
    locale: &str,
    entries: &[Entry],
) -> usize {
    let unchanged = entries
        .iter()
        .filter(|entry| entry.msgstr == entry.msgid)
        .map(|entry| (entry.msgctxt.as_deref(), entry.msgid.as_str()))
        .collect::<HashSet<_>>();
    store.retain_locale(locale, |entry| {
        unchanged.contains(&(entry.msgctxt.as_deref(), entry.msgid.as_str()))
    })
}

pub fn build_locale_plan(
    config: &RuntimeConfig,
    locale: &str,
    args: &TranslateArgs,
) -> Result<LocalePlan> {
    if locale == SOURCE_LOCALE {
        return Ok(LocalePlan {
            locale: locale.to_string(),
            pending: 0,
            selected: 0,
        });
    }
    let catalog_path = catalog_path(config, locale);
    let entries = read_catalog_entries(config, &catalog_path, args.reset)?;
    let reviewed_unchanged = ReviewedUnchangedStore::load(&config.reviewed_unchanged_path)?;
    let pending = entries
        .iter()
        .filter(|entry| {
            should_translate_entry_with_reviewed_unchanged(
                entry,
                locale,
                args,
                reviewed_unchanged.contains(locale, entry.msgctxt.as_deref(), &entry.msgid),
            )
        })
        .count();
    let selected = args.limit.map_or(pending, |limit| pending.min(limit));
    Ok(LocalePlan {
        locale: locale.to_string(),
        pending,
        selected,
    })
}

fn log_translation_plan(plans: &[LocalePlan]) {
    let total_pending = plans.iter().map(|plan| plan.pending).sum::<usize>();
    let total_selected = plans.iter().map(|plan| plan.selected).sum::<usize>();
    let active_locale_count = plans.iter().filter(|plan| plan.selected > 0).count();
    log(
        &format!(
            "Planned work: {total_selected} selected strings across {active_locale_count} locale(s) ({total_pending} pending before limit)"
        ),
        false,
    );
    let non_empty = plans
        .iter()
        .filter(|plan| plan.pending > 0)
        .map(|plan| format!("{}={}", plan.locale, plan.pending))
        .collect::<Vec<_>>();
    if !non_empty.is_empty() {
        log(
            &format!("Pending by locale: {}", non_empty.join(", ")),
            false,
        );
    }
}

pub fn process_locale<C: LocalizationClient>(
    config: &RuntimeConfig,
    client: &C,
    locale: &str,
    args: &TranslateArgs,
) -> Result<LocaleResult> {
    let catalog_path = catalog_path(config, locale);
    if locale == SOURCE_LOCALE {
        return sync_source_locale(config, &catalog_path, args);
    }
    log(
        &format!(
            "[{locale}] Starting with {} via {}...",
            client.model(),
            client.base_url()
        ),
        false,
    );
    let prompt_guidance = load_prompt_guidance(&config.app_dir, locale)?;
    let mut content = fs::read_to_string(&catalog_path)
        .with_context(|| format!("failed to read {}", catalog_path.display()))?;
    let mut reviewed_unchanged = ReviewedUnchangedStore::load(&config.reviewed_unchanged_path)?;
    if args.reset {
        content = reset_catalog_translations(config, &content)?;
        if !args.dry_run {
            write_file_atomic(&catalog_path, &content)?;
            reviewed_unchanged.clear_locale(locale);
            reviewed_unchanged.save_if_dirty().with_context(|| {
                format!(
                    "failed to update reviewed-unchanged sidecar {}",
                    reviewed_unchanged.path().display()
                )
            })?;
        }
        log(&format!("[{locale}] Reset catalog translations"), false);
    }
    let entries = read_catalog_entries_from_content(config, &content, false)?;
    if !args.dry_run {
        let pruned = prune_reviewed_unchanged(&mut reviewed_unchanged, locale, &entries);
        if pruned > 0 {
            reviewed_unchanged.save_if_dirty().with_context(|| {
                format!(
                    "failed to update reviewed-unchanged sidecar {}",
                    reviewed_unchanged.path().display()
                )
            })?;
            log(
                &format!("[{locale}] Pruned {pruned} stale reviewed-unchanged entries"),
                false,
            );
        }
    }
    let pending = entries
        .into_iter()
        .filter(|entry| {
            should_translate_entry_with_reviewed_unchanged(
                entry,
                locale,
                args,
                reviewed_unchanged.contains(locale, entry.msgctxt.as_deref(), &entry.msgid),
            )
        })
        .collect::<Vec<_>>();
    let limit = args.limit.unwrap_or(pending.len());
    let limited = pending.iter().take(limit).cloned().collect::<Vec<_>>();
    if limited.is_empty() {
        log(&format!("[{locale}] No strings to translate"), false);
        return Ok(LocaleResult {
            locale: locale.to_string(),
            translated: 0,
            errors: 0,
        });
    }
    log(
        &format!(
            "[{locale}] Translating {}/{} pending strings",
            limited.len(),
            pending.len()
        ),
        false,
    );
    let mut current_content = content;
    let process_context = ProcessContext {
        config,
        client,
        locale,
        prompt_guidance: &prompt_guidance,
        catalog_path: &catalog_path,
        args,
    };
    let (translated, errors) = if args.batch_size > 1 {
        process_batches(
            &process_context,
            &mut current_content,
            &limited,
            &mut reviewed_unchanged,
        )?
    } else {
        process_strings(
            &process_context,
            &mut current_content,
            &limited,
            &mut reviewed_unchanged,
        )?
    };
    Ok(LocaleResult {
        locale: locale.to_string(),
        translated,
        errors,
    })
}

struct StringOutcome {
    index: usize,
    entry: Entry,
    elapsed: Duration,
    result: Result<Translation>,
}

struct BatchOutcome {
    chunk_index: usize,
    start_index: usize,
    len: usize,
    elapsed: Duration,
    translations: Vec<Translation>,
    warnings: Vec<String>,
    errors: Vec<String>,
}

struct ProcessContext<'a, C: LocalizationClient> {
    config: &'a RuntimeConfig,
    client: &'a C,
    locale: &'a str,
    prompt_guidance: &'a [String],
    catalog_path: &'a Path,
    args: &'a TranslateArgs,
}

fn process_strings<C: LocalizationClient>(
    context: &ProcessContext<'_, C>,
    current_content: &mut String,
    entries: &[Entry],
    reviewed_unchanged: &mut ReviewedUnchangedStore,
) -> Result<(usize, usize)> {
    let mut translated = 0;
    let mut errors = 0;
    let concurrency = context.args.string_concurrency.max(1);
    for (window_index, window) in entries.chunks(concurrency).enumerate() {
        let window_start = window_index * concurrency;
        let mut outcomes = std::thread::scope(|scope| {
            let handles = window
                .iter()
                .enumerate()
                .map(|(offset, entry)| {
                    scope.spawn(move || {
                        let started = Instant::now();
                        let result = localize_string(
                            context.client,
                            entry,
                            context.locale,
                            context.prompt_guidance,
                        );
                        StringOutcome {
                            index: window_start + offset,
                            entry: entry.clone(),
                            elapsed: started.elapsed(),
                            result,
                        }
                    })
                })
                .collect::<Vec<_>>();
            let mut outcomes = Vec::with_capacity(handles.len());
            for handle in handles {
                outcomes.push(
                    handle
                        .join()
                        .map_err(|_| anyhow::anyhow!("i18n string worker panicked"))?,
                );
            }
            Ok::<_, anyhow::Error>(outcomes)
        })?;
        outcomes.sort_by_key(|outcome| outcome.index);
        let translations = outcomes
            .iter()
            .filter_map(|outcome| outcome.result.as_ref().ok())
            .cloned()
            .collect::<Vec<_>>();
        if !translations.is_empty() && !context.args.dry_run {
            *current_content =
                rebuild_catalog_allow_replacing(context.config, current_content, &translations)?;
            write_file_atomic(context.catalog_path, current_content)?;
            record_reviewed_unchanged_translations(
                reviewed_unchanged,
                context.locale,
                &translations,
            );
            reviewed_unchanged.save_if_dirty().with_context(|| {
                format!(
                    "failed to update reviewed-unchanged sidecar {}",
                    reviewed_unchanged.path().display()
                )
            })?;
        }
        for outcome in outcomes {
            match outcome.result {
                Ok(translation) => {
                    translated += 1;
                    if !translation.notes.is_empty() {
                        log(
                            &format!(
                                "[{}] Note for {:?}: {}",
                                context.locale, outcome.entry.msgid, translation.notes
                            ),
                            false,
                        );
                    }
                    let action = if context.args.dry_run {
                        "completed"
                    } else {
                        "saved"
                    };
                    let queued = entries.len().saturating_sub(outcome.index + 1);
                    let active = window
                        .len()
                        .saturating_sub(outcome.index.saturating_sub(window_start) + 1);
                    log(
                        &format!(
                            "[{}] {}/{} {action} in {} ({} done, {} errors, {} active, {} queued)",
                            context.locale,
                            outcome.index + 1,
                            entries.len(),
                            format_duration(outcome.elapsed),
                            translated,
                            errors,
                            active,
                            queued
                        ),
                        false,
                    );
                }
                Err(error) => {
                    errors += 1;
                    let queued = entries.len().saturating_sub(outcome.index + 1);
                    let active = window
                        .len()
                        .saturating_sub(outcome.index.saturating_sub(window_start) + 1);
                    log(
                        &format!(
                            "[{}] {:?} error after {}: {error} ({} done, {} errors, {} active, {} queued)",
                            context.locale,
                            outcome.entry.msgid,
                            format_duration(outcome.elapsed),
                            translated,
                            errors,
                            active,
                            queued
                        ),
                        true,
                    );
                }
            }
        }
    }
    Ok((translated, errors))
}

fn record_reviewed_unchanged_translations(
    store: &mut ReviewedUnchangedStore,
    locale: &str,
    translations: &[Translation],
) {
    for translation in translations {
        if translation.reviewed_unchanged {
            store.mark(locale, translation.msgctxt.as_deref(), &translation.msgid);
        } else {
            store.unmark(locale, translation.msgctxt.as_deref(), &translation.msgid);
        }
    }
}

fn process_batches<C: LocalizationClient>(
    context: &ProcessContext<'_, C>,
    current_content: &mut String,
    entries: &[Entry],
    reviewed_unchanged: &mut ReviewedUnchangedStore,
) -> Result<(usize, usize)> {
    let mut translated = 0;
    let mut errors = 0;
    let batch_concurrency = (context.args.string_concurrency / context.args.batch_size).max(1);
    let chunks = entries.chunks(context.args.batch_size).collect::<Vec<_>>();
    for (window_index, window) in chunks.chunks(batch_concurrency).enumerate() {
        let window_start = window_index * batch_concurrency;
        let mut outcomes = std::thread::scope(|scope| {
            let handles = window
                .iter()
                .enumerate()
                .map(|(offset, chunk)| {
                    let chunk_index = window_start + offset;
                    scope.spawn(move || {
                        localize_batch_chunk(
                            context.client,
                            chunk,
                            context.locale,
                            context.prompt_guidance,
                            chunk_index,
                            chunk_index * context.args.batch_size,
                        )
                    })
                })
                .collect::<Vec<_>>();
            let mut outcomes = Vec::with_capacity(handles.len());
            for handle in handles {
                outcomes.push(
                    handle
                        .join()
                        .map_err(|_| anyhow::anyhow!("i18n batch worker panicked"))?,
                );
            }
            Ok::<_, anyhow::Error>(outcomes)
        })?;
        outcomes.sort_by_key(|outcome| outcome.start_index);
        let translations = outcomes
            .iter()
            .flat_map(|outcome| outcome.translations.iter().cloned())
            .collect::<Vec<_>>();
        if !translations.is_empty() && !context.args.dry_run {
            *current_content =
                rebuild_catalog_allow_replacing(context.config, current_content, &translations)?;
            write_file_atomic(context.catalog_path, current_content)?;
            record_reviewed_unchanged_translations(
                reviewed_unchanged,
                context.locale,
                &translations,
            );
            reviewed_unchanged.save_if_dirty().with_context(|| {
                format!(
                    "failed to update reviewed-unchanged sidecar {}",
                    reviewed_unchanged.path().display()
                )
            })?;
        }
        for outcome in outcomes {
            for warning in &outcome.warnings {
                log(warning, true);
            }
            for error in &outcome.errors {
                log(error, true);
            }
            translated += outcome.translations.len();
            errors += outcome.errors.len();
            let action = if context.args.dry_run {
                "completed"
            } else {
                "saved"
            };
            let completed = outcome.start_index + outcome.len;
            let active = window
                .len()
                .saturating_sub(outcome.chunk_index.saturating_sub(window_start) + 1);
            log(
                &format!(
                    "[{}] {}/{} {action} in {} ({} done, {} errors, {} active, {} queued)",
                    context.locale,
                    completed,
                    entries.len(),
                    format_duration(outcome.elapsed),
                    translated,
                    errors,
                    active,
                    entries.len().saturating_sub(completed)
                ),
                false,
            );
        }
    }
    Ok((translated, errors))
}

fn localize_batch_chunk<C: LocalizationClient>(
    client: &C,
    chunk: &[Entry],
    locale: &str,
    prompt_guidance: &[String],
    chunk_index: usize,
    start_index: usize,
) -> BatchOutcome {
    let started = Instant::now();
    let mut translations = Vec::new();
    let mut warnings = Vec::new();
    let mut errors = Vec::new();
    let candidates = chunk
        .iter()
        .filter(|entry| {
            if should_keep_unchanged(&entry.msgid, locale) {
                translations.push(Translation {
                    msgctxt: entry.msgctxt.clone(),
                    msgid: entry.msgid.clone(),
                    msgstr: entry.msgid.clone(),
                    reviewed_unchanged: true,
                    notes: String::new(),
                });
                false
            } else {
                true
            }
        })
        .cloned()
        .collect::<Vec<_>>();
    if !candidates.is_empty() {
        match localize_batch_partial(client, &candidates, locale, prompt_guidance) {
            Ok(batch_outcome) => {
                translations.extend(batch_outcome.translations);
                if !batch_outcome.failed_entries.is_empty() {
                    warnings.push(format!(
                        "[{locale}] Batch {} fallback for {} string(s) after per-item validation errors",
                        chunk_index + 1,
                        batch_outcome.failed_entries.len()
                    ));
                }
                for (entry, reason) in batch_outcome.failed_entries {
                    match localize_string(client, &entry, locale, prompt_guidance) {
                        Ok(translation) => translations.push(translation),
                        Err(error) => {
                            errors.push(format!(
                                "[{locale}] {:?} error after batch item fallback ({reason}): {error}",
                                entry.msgid
                            ));
                        }
                    }
                }
            }
            Err(error) => {
                warnings.push(format!(
                    "[{locale}] Batch {} fallback after validation error: {error}",
                    chunk_index + 1
                ));
                for entry in &candidates {
                    match localize_string(client, entry, locale, prompt_guidance) {
                        Ok(translation) => translations.push(translation),
                        Err(error) => {
                            errors.push(format!(
                                "[{locale}] {:?} error after batch fallback: {error}",
                                entry.msgid
                            ));
                        }
                    }
                }
            }
        }
    }
    BatchOutcome {
        chunk_index,
        start_index,
        len: chunk.len(),
        elapsed: started.elapsed(),
        translations,
        warnings,
        errors,
    }
}

pub fn sync_source_locale(
    config: &RuntimeConfig,
    catalog_path: &Path,
    args: &TranslateArgs,
) -> Result<LocaleResult> {
    if matches!(&config.catalog_layout, CatalogLayout::StaticJson(_)) {
        log(
            &format!(
                "[{SOURCE_LOCALE}] Source strings live in the static source catalog; nothing to sync"
            ),
            false,
        );
        return Ok(LocaleResult {
            locale: SOURCE_LOCALE.to_string(),
            translated: 0,
            errors: 0,
        });
    }
    log(
        &format!("[{SOURCE_LOCALE}] Syncing source strings without LLM..."),
        false,
    );
    let content = fs::read_to_string(catalog_path)
        .with_context(|| format!("failed to read {}", catalog_path.display()))?;
    let translations = parse_po(&content)?
        .into_iter()
        .map(|entry| Translation::new(entry.msgctxt, entry.msgid.clone(), entry.msgid))
        .collect::<Vec<_>>();
    if !args.dry_run {
        write_file_atomic(
            catalog_path,
            &rebuild_po_allow_replacing(&content, &translations)?,
        )?;
    }
    log(
        &format!(
            "[{SOURCE_LOCALE}] Synced {} source strings",
            translations.len()
        ),
        false,
    );
    Ok(LocaleResult {
        locale: SOURCE_LOCALE.to_string(),
        translated: translations.len(),
        errors: 0,
    })
}

fn read_catalog_entries(
    config: &RuntimeConfig,
    catalog_path: &Path,
    reset: bool,
) -> Result<Vec<Entry>> {
    let content = fs::read_to_string(catalog_path)
        .with_context(|| format!("failed to read {}", catalog_path.display()))?;
    read_catalog_entries_from_content(config, &content, reset)
}

fn read_catalog_entries_from_content(
    config: &RuntimeConfig,
    content: &str,
    reset: bool,
) -> Result<Vec<Entry>> {
    match &config.catalog_layout {
        CatalogLayout::NestedMessages => {
            let content = if reset {
                reset_po_translations(content)?
            } else {
                content.to_string()
            };
            parse_po(&content)
        }
        CatalogLayout::StaticJson(static_config) => {
            let source_content = read_static_source_catalog(&static_config.source_path)?;
            read_static_json_entries(static_config, &source_content, content, reset)
        }
    }
}

fn reset_catalog_translations(config: &RuntimeConfig, content: &str) -> Result<String> {
    match &config.catalog_layout {
        CatalogLayout::NestedMessages => reset_po_translations(content),
        CatalogLayout::StaticJson(static_config) => {
            reset_static_json_translations(static_config, content)
        }
    }
}

fn rebuild_catalog_allow_replacing(
    config: &RuntimeConfig,
    content: &str,
    translations: &[Translation],
) -> Result<String> {
    match &config.catalog_layout {
        CatalogLayout::NestedMessages => rebuild_po_allow_replacing(content, translations),
        CatalogLayout::StaticJson(static_config) => {
            let source_content = read_static_source_catalog(&static_config.source_path)?;
            rebuild_static_json_allow_replacing(
                static_config,
                &source_content,
                content,
                translations,
            )
        }
    }
}

fn read_static_source_catalog(source_path: &Path) -> Result<String> {
    fs::read_to_string(source_path)
        .with_context(|| format!("failed to read {}", source_path.display()))
}

fn catalog_path(config: &RuntimeConfig, locale: &str) -> PathBuf {
    match &config.catalog_layout {
        CatalogLayout::NestedMessages => config.locales_dir.join(locale).join("messages.po"),
        CatalogLayout::StaticJson(_) => config.locales_dir.join(format!("{locale}.json")),
    }
}

pub fn write_file_atomic(file_path: &Path, content: &str) -> Result<()> {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let temp_path = file_path.with_file_name(format!(
        ".{}.{}.{}.tmp",
        file_path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("messages.po"),
        std::process::id(),
        timestamp
    ));
    fs::write(&temp_path, content)?;
    fs::rename(&temp_path, file_path)?;
    Ok(())
}

pub fn localize_string<C: LocalizationClient>(
    client: &C,
    entry: &Entry,
    locale: &str,
    prompt_guidance: &[String],
) -> Result<Translation> {
    if should_keep_unchanged(&entry.msgid, locale) {
        return Ok(Translation {
            msgctxt: entry.msgctxt.clone(),
            msgid: entry.msgid.clone(),
            msgstr: entry.msgid.clone(),
            reviewed_unchanged: true,
            notes: String::new(),
        });
    }
    if !is_supported_locale(locale) {
        bail!("Unsupported locale: {locale}");
    }
    let mut options = base_options();
    if entry.msgid.len() <= 24 {
        options.insert("temperature".to_string(), json!(0));
    }
    let mut previous_error: Option<String> = None;
    for attempt in 1..=FULL_TRANSLATION_ATTEMPTS {
        let result = (|| {
            let mut attempt_options = options.clone();
            if attempt > 1 {
                attempt_options.insert("temperature".to_string(), json!(0));
            }
            let prompt = build_translate_prompt(
                entry,
                locale,
                prompt_guidance,
                previous_error.as_deref(),
                true,
            );
            let result =
                client.localize(&attempt_options, &prompt.system_prompt, &prompt.user_prompt)?;
            let restored = restore_masked_tokens(&result.localized, &prompt.token_aliases);
            let localized = normalize_localized_capitalization(entry, locale, &restored);
            validate_localization(entry, &localized)?;
            validate_locale_specific_localization(entry, locale, &localized)?;
            Ok::<_, anyhow::Error>(Translation {
                msgctxt: entry.msgctxt.clone(),
                msgid: entry.msgid.clone(),
                msgstr: localized.clone(),
                reviewed_unchanged: localized == entry.msgid,
                notes: result.notes,
            })
        })();
        match result {
            Ok(translation) => return Ok(translation),
            Err(error) => {
                previous_error = Some(error.to_string());
                if attempt == FULL_TRANSLATION_ATTEMPTS {
                    if let Some(fallback) = try_literal_token_localization(
                        client,
                        entry,
                        locale,
                        prompt_guidance,
                        &options,
                        previous_error.as_deref().unwrap_or_default(),
                    )? {
                        return Ok(Translation {
                            msgctxt: entry.msgctxt.clone(),
                            msgid: entry.msgid.clone(),
                            msgstr: fallback.clone(),
                            reviewed_unchanged: fallback == entry.msgid,
                            notes: "literal token fallback".to_string(),
                        });
                    }
                    if let Some(fallback) = try_segmented_localization(
                        client,
                        entry,
                        locale,
                        prompt_guidance,
                        &options,
                        previous_error.as_deref().unwrap_or_default(),
                    )? {
                        return Ok(Translation {
                            msgctxt: entry.msgctxt.clone(),
                            msgid: entry.msgid.clone(),
                            msgstr: fallback.clone(),
                            reviewed_unchanged: fallback == entry.msgid,
                            notes: "segmented fallback".to_string(),
                        });
                    }
                    bail!(previous_error.unwrap_or_else(|| "translation failed".to_string()));
                }
                sleep_before_retry(attempt, 0.5);
            }
        }
    }
    bail!("unreachable")
}

#[derive(Debug)]
struct BatchLocalizationOutcome {
    translations: Vec<Translation>,
    failed_entries: Vec<(Entry, String)>,
}

fn localize_batch_partial<C: LocalizationClient>(
    client: &C,
    entries: &[Entry],
    locale: &str,
    prompt_guidance: &[String],
) -> Result<BatchLocalizationOutcome> {
    if entries.is_empty() {
        return Ok(BatchLocalizationOutcome {
            translations: Vec::new(),
            failed_entries: Vec::new(),
        });
    }
    if !is_supported_locale(locale) {
        bail!("Unsupported locale: {locale}");
    }
    let target_name = display_name(locale);
    let guidance = format_guidance(prompt_guidance);
    let mut batch_items = Vec::with_capacity(entries.len());
    let mut token_aliases_by_id = Vec::with_capacity(entries.len());
    for (index, entry) in entries.iter().enumerate() {
        let id = index.to_string();
        let (source, token_aliases) = build_masked_source(entry);
        token_aliases_by_id.push((id.clone(), token_aliases));
        let mut item = Map::new();
        item.insert("id".to_string(), json!(id));
        item.insert("source".to_string(), json!(source));
        let metadata = build_batch_translation_context(entry);
        if !metadata.is_empty() {
            item.insert("metadata".to_string(), json!(metadata));
        }
        let placeholder_hints = extract_placeholder_hints(entry);
        if !placeholder_hints.is_empty() {
            item.insert("placeholderHints".to_string(), json!(placeholder_hints));
        }
        let preservation = build_preservation_context(entry);
        if !preservation.is_empty() {
            item.insert("preservation".to_string(), json!(preservation));
        }
        batch_items.push(Value::Object(item));
    }
    let mut options = base_options();
    options.insert("temperature".to_string(), json!(0));
    let num_ctx = if entries.len() > 125 { 16384 } else { 8192 };
    let num_predict = if entries.len() > 125 {
        (entries.len() * 128).clamp(4096, 12000)
    } else {
        (entries.len() * 48).clamp(2048, 4096)
    };
    options.insert("num_ctx".to_string(), json!(num_ctx));
    options.insert("num_predict".to_string(), json!(num_predict));
    let system_prompt = [
        build_system_prompt(locale),
        "You are translating multiple independent UI strings in one request.".to_string(),
        "Return only valid compact JSON. Do not include markdown, comments, or explanatory text.".to_string(),
        "Each JSON property key must be the item id, and each value must be only the localized string.".to_string(),
        "Preserve every placeholder alias and literal token exactly as shown for its item.".to_string(),
    ]
    .join("\n\n");
    let user_prompt = [
        Some(format!(
            "Task: localize each source string from en-US to {target_name} ({locale})."
        )),
        Some(build_target_locale_prompt(locale)),
        optional_section("Persisted guidance excerpts", guidance.as_deref()),
        Some(
            "Return shape example: {\"0\":\"localized string\",\"1\":\"localized string\"}."
                .to_string(),
        ),
        Some("Translate only the human-readable UI text. Do not translate placeholder aliases, ICU argument names, rich-text tags, IDs, URLs, or code-like values.".to_string()),
        Some(format!(
            "Items JSON:\n{}",
            serde_json::to_string(&batch_items)?
        )),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n\n");
    let result = client.localize(&options, &system_prompt, &user_prompt)?;
    let object = parse_batch_response_object(&result.localized)?;
    let mut translations = Vec::with_capacity(entries.len());
    let mut failed_entries = Vec::new();
    for (index, entry) in entries.iter().enumerate() {
        let id = index.to_string();
        let Some(value) = object.get(&id) else {
            failed_entries.push((entry.clone(), format!("missing id {id}")));
            continue;
        };
        let Some(raw_localized) = batch_value_to_str(value) else {
            failed_entries.push((
                entry.clone(),
                format!("id {id} is not a string localization"),
            ));
            continue;
        };
        let token_aliases = token_aliases_by_id
            .iter()
            .find(|(candidate_id, _)| candidate_id == &id)
            .map(|(_, aliases)| aliases.as_slice())
            .unwrap_or(&[]);
        let localized = normalize_localized_capitalization(
            entry,
            locale,
            &restore_masked_tokens(raw_localized, token_aliases),
        );
        if let Err(error) = validate_localization(entry, &localized)
            .and_then(|_| validate_locale_specific_localization(entry, locale, &localized))
        {
            failed_entries.push((entry.clone(), error.to_string()));
            continue;
        }
        translations.push(Translation {
            msgctxt: entry.msgctxt.clone(),
            msgid: entry.msgid.clone(),
            msgstr: localized.clone(),
            reviewed_unchanged: localized == entry.msgid,
            notes: result.notes.clone(),
        });
    }
    Ok(BatchLocalizationOutcome {
        translations,
        failed_entries,
    })
}

fn parse_batch_response_object(content: &str) -> Result<Map<String, Value>> {
    let trimmed = content.trim();
    if let Ok(parsed) = serde_json::from_str::<Value>(trimmed)
        && let Some(object) = normalize_batch_response_value(parsed)
    {
        return Ok(object);
    }
    for (start, char) in trimmed.char_indices() {
        if char != '{' && char != '[' {
            continue;
        }
        let mut stream = serde_json::Deserializer::from_str(&trimmed[start..]).into_iter::<Value>();
        if let Some(Ok(parsed)) = stream.next()
            && let Some(object) = normalize_batch_response_value(parsed)
        {
            return Ok(object);
        }
    }
    bail!("Batch response did not contain a JSON object with numeric item keys")
}

fn normalize_batch_response_value(value: Value) -> Option<Map<String, Value>> {
    match value {
        Value::Object(mut object) => {
            if is_batch_response_object(&object) {
                return Some(object);
            }
            for key in [
                "translations",
                "localized",
                "localizations",
                "results",
                "items",
            ] {
                if let Some(value) = object.remove(key)
                    && let Some(normalized) = normalize_batch_response_value(value)
                {
                    return Some(normalized);
                }
            }
            None
        }
        Value::Array(items) => normalize_batch_response_array(items),
        _ => None,
    }
}

fn is_batch_response_object(object: &Map<String, Value>) -> bool {
    object.keys().any(|key| key.parse::<usize>().is_ok())
}

fn normalize_batch_response_array(items: Vec<Value>) -> Option<Map<String, Value>> {
    let mut object = Map::new();
    for item in items {
        let Value::Object(mut item) = item else {
            continue;
        };
        let Some(id) = item
            .remove("id")
            .and_then(|value| match value {
                Value::String(value) => Some(value),
                Value::Number(value) => Some(value.to_string()),
                _ => None,
            })
            .filter(|value| value.parse::<usize>().is_ok())
        else {
            continue;
        };
        let Some(localized) = item
            .remove("localized")
            .or_else(|| item.remove("translation"))
            .or_else(|| item.remove("value"))
            .or_else(|| item.remove("target"))
            .or_else(|| item.remove("text"))
            .or_else(|| item.remove("msgstr"))
        else {
            continue;
        };
        object.insert(id, localized);
    }
    if object.is_empty() {
        None
    } else {
        Some(object)
    }
}

fn batch_value_to_str(value: &Value) -> Option<&str> {
    if let Some(raw) = value.as_str() {
        return Some(raw);
    }
    value
        .as_object()
        .and_then(|object| object.get("localized"))
        .and_then(Value::as_str)
}

fn sleep_before_retry(attempt: usize, base_seconds: f64) {
    std::thread::sleep(Duration::from_secs_f64(base_seconds * attempt as f64));
}

fn try_literal_token_localization<C: LocalizationClient>(
    client: &C,
    entry: &Entry,
    locale: &str,
    prompt_guidance: &[String],
    options: &Map<String, Value>,
    previous_error: &str,
) -> Result<Option<String>> {
    if !previous_error.contains("did not preserve") || extract_preserved_tokens(entry).is_empty() {
        return Ok(None);
    }
    let mut literal_error = previous_error.to_string();
    for attempt in 1..=SEGMENT_TRANSLATION_ATTEMPTS {
        let result = (|| {
            let mut attempt_options = options.clone();
            attempt_options.insert("temperature".to_string(), json!(0));
            let prompt =
                build_translate_prompt(entry, locale, prompt_guidance, Some(&literal_error), false);
            let result =
                client.localize(&attempt_options, &prompt.system_prompt, &prompt.user_prompt)?;
            let localized = normalize_localized_capitalization(entry, locale, &result.localized);
            validate_localization(entry, &localized)?;
            validate_locale_specific_localization(entry, locale, &localized)?;
            Ok::<_, anyhow::Error>(localized)
        })();
        match result {
            Ok(localized) => return Ok(Some(localized)),
            Err(error) => {
                literal_error = error.to_string();
                if attempt == SEGMENT_TRANSLATION_ATTEMPTS {
                    return Ok(None);
                }
                sleep_before_retry(attempt, 0.3);
            }
        }
    }
    Ok(None)
}

fn try_segmented_localization<C: LocalizationClient>(
    client: &C,
    entry: &Entry,
    locale: &str,
    prompt_guidance: &[String],
    options: &Map<String, Value>,
    previous_error: &str,
) -> Result<Option<String>> {
    let segments = split_source_for_segment_fallback(&entry.msgid);
    if segments.len() <= 1 {
        return Ok(None);
    }
    let mut localized_segments = Vec::new();
    for (index, segment) in segments.iter().enumerate() {
        let mut segment_entry = Entry {
            comments: entry.comments.clone(),
            references: entry.references.clone(),
            msgctxt: entry.msgctxt.clone(),
            msgid: segment.clone(),
            msgstr: String::new(),
            line_number: entry.line_number,
        };
        segment_entry.comments.push(format!(
			"#. Segment fallback: segment {} of {} from a longer UI string. Translate only this source segment.",
			index + 1,
			segments.len()
		));
        let mut segment_error = previous_error.to_string();
        let mut localized_segment = None;
        for attempt in 1..=SEGMENT_TRANSLATION_ATTEMPTS {
            let result = (|| {
                let mut attempt_options = options.clone();
                attempt_options.insert("temperature".to_string(), json!(0));
                let prompt = build_translate_prompt(
                    &segment_entry,
                    locale,
                    prompt_guidance,
                    Some(&segment_error),
                    true,
                );
                let result = client.localize(
                    &attempt_options,
                    &prompt.system_prompt,
                    &prompt.user_prompt,
                )?;
                let localized = restore_masked_tokens(&result.localized, &prompt.token_aliases);
                validate_localization(&segment_entry, &localized)?;
                Ok::<_, anyhow::Error>(localized)
            })();
            match result {
                Ok(localized) => {
                    localized_segment = Some(localized);
                    break;
                }
                Err(error) => {
                    segment_error = error.to_string();
                    if attempt == SEGMENT_TRANSLATION_ATTEMPTS {
                        return Ok(None);
                    }
                    sleep_before_retry(attempt, 0.3);
                }
            }
        }
        let Some(localized_segment) = localized_segment else {
            return Ok(None);
        };
        localized_segments.push(localized_segment);
    }
    let localized = normalize_localized_capitalization(
        entry,
        locale,
        &join_localized_segments(&localized_segments),
    );
    validate_localization(entry, &localized)?;
    validate_locale_specific_localization(entry, locale, &localized)?;
    Ok(
        polish_segmented_localization(client, entry, locale, prompt_guidance, options, &localized)?
            .or(Some(localized)),
    )
}

fn polish_segmented_localization<C: LocalizationClient>(
    client: &C,
    entry: &Entry,
    locale: &str,
    prompt_guidance: &[String],
    options: &Map<String, Value>,
    draft: &str,
) -> Result<Option<String>> {
    let target_name = display_name(locale);
    let (masked_source, token_aliases) = build_masked_source(entry);
    let masked_draft = mask_text_with_aliases(draft, &token_aliases);
    let guidance = format_guidance(prompt_guidance);
    let system_prompt = [
		build_system_prompt(locale),
		"You are polishing a localized UI string that was translated in smaller segments.".to_string(),
		"Make only grammar, punctuation, spacing, and fluency fixes needed to make the localized string read as one coherent UI message.".to_string(),
		"Do not add, remove, reorder, translate, or alter placeholder aliases.".to_string(),
	]
	.join("\n\n");
    let mut user_prompt = [
		Some(format!(
			"Task: polish the {target_name} ({locale}) draft localization so it reads naturally as one UI string."
		)),
		Some("Return only the polished localized string.".to_string()),
		Some(build_target_locale_prompt(locale)),
		optional_section("Persisted guidance excerpts", guidance.as_deref()),
		(!token_aliases.is_empty()).then(|| build_token_alias_context(&token_aliases)),
		Some(format!("English source for meaning:\n{masked_source}")),
		Some(format!("Draft localization to polish:\n{masked_draft}")),
	]
	.into_iter()
	.flatten()
	.collect::<Vec<_>>()
	.join("\n\n");
    for attempt in 1..=POLISH_TRANSLATION_ATTEMPTS {
        let result = (|| {
            let mut attempt_options = options.clone();
            attempt_options.insert("temperature".to_string(), json!(0));
            let result = client.localize(&attempt_options, &system_prompt, &user_prompt)?;
            let localized = normalize_localized_capitalization(
                entry,
                locale,
                &restore_masked_tokens(&result.localized, &token_aliases),
            );
            validate_localization(entry, &localized)?;
            validate_locale_specific_localization(entry, locale, &localized)?;
            Ok::<_, anyhow::Error>(localized)
        })();
        match result {
            Ok(localized) => return Ok(Some(localized)),
            Err(error) => {
                if attempt == POLISH_TRANSLATION_ATTEMPTS {
                    return Ok(None);
                }
                user_prompt = [
					user_prompt,
					format!(
						"Previous polish attempt failed validation: {error}. Return a corrected polished string that preserves every placeholder alias exactly."
					),
				]
				.join("\n\n");
                sleep_before_retry(attempt, 0.3);
            }
        }
    }
    Ok(None)
}

pub fn build_translation_context(entry: &Entry) -> String {
    let comment_context = extract_translator_comments(entry).join("\n");
    let reference_context = entry
        .references
        .iter()
        .filter_map(|line| line.strip_prefix("#: "))
        .map(str::trim)
        .filter(|reference| !reference.is_empty())
        .collect::<Vec<_>>()
        .join(", ");
    let mut sections = Vec::new();
    if let Some(msgctxt) = &entry.msgctxt {
        sections.push(format!("Lingui msgctxt: {msgctxt}"));
    }
    if !comment_context.is_empty() {
        sections.push(format!("Translator comments:\n{comment_context}"));
    }
    if !reference_context.is_empty() {
        sections.push(format!(
			"Source references for locating the string, not translator instructions: {reference_context}"
		));
    }
    sections.join("\n\n")
}

fn build_batch_translation_context(entry: &Entry) -> String {
    let comment_context = extract_translator_comments(entry).join("\n");
    let mut sections = Vec::new();
    if let Some(msgctxt) = &entry.msgctxt {
        sections.push(format!("Lingui msgctxt: {msgctxt}"));
    }
    if !comment_context.is_empty() {
        sections.push(format!("Translator comments:\n{comment_context}"));
    }
    sections.join("\n\n")
}

pub fn build_preservation_context(entry: &Entry) -> String {
    let tokens = extract_preserved_tokens(entry);
    let controls = extract_icu_controls(&entry.msgid);
    let mut lines = Vec::new();
    if !tokens.is_empty() {
        lines.push(format!(
            "Required literal tokens that must appear exactly in localized: {}",
            tokens
                .iter()
                .map(|token| format!("{token:?}"))
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }
    if !controls.is_empty() {
        lines.push(format!(
            "Required ICU controls that must remain valid: {}",
            controls
                .iter()
                .map(|control| format!("{{{}, {}, ...}}", control.argument, control.kind))
                .collect::<Vec<_>>()
                .join(", ")
        ));
        lines.push("For ICU plural/select strings, keep the same argument names and clause keys such as one/other/few/many. Translate only the human-readable text inside each clause.".to_string());
    }
    if entry.msgid.contains('<') {
        lines.push("Numeric rich-text tags such as <0>, </0>, <1>, </1>, and <0/> are markup tokens. Keep every tag exactly, including slash placement and number. You may move paired tags only when grammar requires it.".to_string());
    }
    lines.join("\n")
}

pub fn build_literal_token_context(entry: &Entry) -> Option<String> {
    let tokens = extract_preserved_tokens(entry);
    if tokens.is_empty() {
        return None;
    }
    let mut lines = vec![
        "Preserved tokens are UI placeholders and markup.".to_string(),
        "Your answer must contain these exact preserved substrings:".to_string(),
    ];
    lines.extend(tokens.iter().map(|token| format!("- {token}")));
    lines.extend([
		"Keep each preserved token exactly where the corresponding source placeholder appears in the sentence.".to_string(),
		"Do not add a translated explanation, synonym, or parenthetical label next to a preserved token.".to_string(),
		"Do not turn paired tags into self-closing tags.".to_string(),
	]);
    Some(lines.join("\n"))
}

pub fn build_translate_prompt(
    entry: &Entry,
    locale: &str,
    prompt_guidance: &[String],
    previous_error: Option<&str>,
    use_token_aliases: bool,
) -> Prompt {
    let target_name = display_name(locale);
    let context = build_translation_context(entry);
    let placeholder_hints = extract_placeholder_hints(entry).join("\n");
    let preservation_context = build_preservation_context(entry);
    let (source, token_aliases, token_context) = if use_token_aliases {
        let (source, token_aliases) = build_masked_source(entry);
        let token_context =
            (!token_aliases.is_empty()).then(|| build_token_alias_context(&token_aliases));
        (source, token_aliases, token_context)
    } else {
        (
            entry.msgid.clone(),
            Vec::new(),
            build_literal_token_context(entry),
        )
    };
    let guidance = format_guidance(prompt_guidance);
    let parts = [
		Some(format!(
			"Task: localize the source string from en-US to {target_name} ({locale})."
		)),
		Some(format!("Return only the {target_name} localized string.")),
		Some(build_target_locale_prompt(locale)),
		optional_section("Persisted guidance excerpts", guidance.as_deref()),
		optional_section("Lingui metadata and source locations", not_empty(&context)),
		optional_section("Placeholder hints from Lingui comments", not_empty(&placeholder_hints)),
		token_context,
		optional_section("Preservation requirements", not_empty(&preservation_context)),
		previous_error.map(|error| {
			format!(
				"Previous attempt failed validation: {error}\nReturn a corrected localization that satisfies every preservation requirement exactly."
			)
		}),
		Some(format!("Source string:\n{source}")),
	]
	.into_iter()
	.flatten()
	.collect::<Vec<_>>();
    Prompt {
        system_prompt: build_system_prompt(locale),
        user_prompt: parts.join("\n\n"),
        token_aliases,
    }
}

fn optional_section(label: &str, content: Option<&str>) -> Option<String> {
    content.map(|content| format!("{label}:\n{content}"))
}

fn not_empty(value: &str) -> Option<&str> {
    (!value.is_empty()).then_some(value)
}

fn format_guidance(prompt_guidance: &[String]) -> Option<String> {
    (!prompt_guidance.is_empty()).then(|| prompt_guidance.join("\n\n"))
}

pub fn split_source_for_segment_fallback(source: &str) -> Vec<String> {
    if source.len() < 120 || source.contains('\n') || has_icu_control(source) {
        return vec![source.to_string()];
    }
    let sentence_segments = split_sentences(source)
        .into_iter()
        .map(|segment| segment.trim().to_string())
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<_>>();
    if sentence_segments.len() <= 1 {
        return vec![source.to_string()];
    }
    let segments = sentence_segments
        .iter()
        .flat_map(|segment| split_long_clause_segment(segment))
        .collect::<Vec<_>>();
    if segments.len() > 1 {
        segments
    } else {
        vec![source.to_string()]
    }
}

fn split_sentences(source: &str) -> Vec<&str> {
    let mut segments = Vec::new();
    let mut start = 0;
    let mut iter = source.char_indices().peekable();
    while let Some((index, character)) = iter.next() {
        if matches!(character, '.' | '!' | '?') {
            let next_is_boundary = iter
                .peek()
                .map(|(_next_index, next)| next.is_whitespace())
                .unwrap_or(true);
            if next_is_boundary {
                let end = index + character.len_utf8();
                segments.push(&source[start..end]);
                start = end;
            }
        }
    }
    if start < source.len() {
        segments.push(&source[start..]);
    }
    segments
}

pub fn split_long_clause_segment(segment: &str) -> Vec<String> {
    let token_count = extract_preserved_tokens(&Entry::with_msgid(segment)).len();
    if segment.len() < 140 || token_count < 3 {
        return vec![segment.to_string()];
    }
    let mut parts = Vec::new();
    let mut start = 0;
    for (comma_index, _) in segment.match_indices(',') {
        let after_comma = comma_index + 1;
        let whitespace_len = segment[after_comma..]
            .chars()
            .take_while(|character| character.is_whitespace())
            .map(char::len_utf8)
            .sum::<usize>();
        if whitespace_len == 0 {
            continue;
        }
        let next_start = after_comma + whitespace_len;
        let rest = &segment[next_start..];
        if starts_with_clause_keyword(rest) {
            parts.push(segment[start..after_comma].trim().to_string());
            start = next_start;
        }
    }
    if start == 0 {
        return vec![segment.to_string()];
    }
    parts.push(segment[start..].trim().to_string());
    parts.into_iter().filter(|part| !part.is_empty()).collect()
}

fn starts_with_clause_keyword(rest: &str) -> bool {
    ["allow", "then", "if", "fully"].iter().any(|keyword| {
        rest.len() >= keyword.len()
            && rest[..keyword.len()].eq_ignore_ascii_case(keyword)
            && rest[keyword.len()..]
                .chars()
                .next()
                .is_none_or(|character| !character.is_alphanumeric() && character != '_')
    })
}

pub fn join_localized_segments(segments: &[String]) -> String {
    Regex::new(r"\s+([,.;:!?])")
        .expect("valid segment join regex")
        .replace_all(&segments.join(" "), "$1")
        .into_owned()
}

fn format_duration(duration: Duration) -> String {
    let seconds = duration.as_secs_f64();
    if seconds < 60.0 {
        return format!("{seconds:.1}s");
    }
    let minutes = (seconds / 60.0).floor();
    let remaining_seconds = seconds % 60.0;
    if minutes < 60.0 {
        return format!("{}m {:.0}s", minutes as u64, remaining_seconds);
    }
    let hours = (minutes / 60.0).floor();
    let remaining_minutes = minutes % 60.0;
    format!("{}h {}m", hours as u64, remaining_minutes as u64)
}

fn format_float(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{}", value as u64)
    } else {
        format!("{value}")
    }
}

fn log(message: &str, error: bool) {
    if error {
        eprintln!("{message}");
    } else {
        println!("{message}");
    }
}
