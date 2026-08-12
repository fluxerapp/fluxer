// SPDX-License-Identifier: AGPL-3.0-or-later

use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use syn::{
    Expr, ExprLit, ExprMacro, Ident, Lit, Macro, Token, braced,
    parse::{Parse, ParseStream},
    visit::{self, Visit},
};

#[derive(Clone)]
struct LocaleInfo {
    code: &'static str,
    variant: &'static str,
    rtl: bool,
}

#[derive(Clone, Debug)]
struct Descriptor {
    const_name: String,
    key: String,
    message: String,
    comment: String,
    placeholders: BTreeSet<String>,
}

const LOCALES: &[LocaleInfo] = &[
    LocaleInfo {
        code: "ar",
        variant: "Ar",
        rtl: true,
    },
    LocaleInfo {
        code: "bg",
        variant: "Bg",
        rtl: false,
    },
    LocaleInfo {
        code: "cs",
        variant: "Cs",
        rtl: false,
    },
    LocaleInfo {
        code: "da",
        variant: "Da",
        rtl: false,
    },
    LocaleInfo {
        code: "de",
        variant: "De",
        rtl: false,
    },
    LocaleInfo {
        code: "el",
        variant: "El",
        rtl: false,
    },
    LocaleInfo {
        code: "en-GB",
        variant: "EnGb",
        rtl: false,
    },
    LocaleInfo {
        code: "en-US",
        variant: "EnUs",
        rtl: false,
    },
    LocaleInfo {
        code: "es-419",
        variant: "Es419",
        rtl: false,
    },
    LocaleInfo {
        code: "es-ES",
        variant: "EsEs",
        rtl: false,
    },
    LocaleInfo {
        code: "fi",
        variant: "Fi",
        rtl: false,
    },
    LocaleInfo {
        code: "fr",
        variant: "Fr",
        rtl: false,
    },
    LocaleInfo {
        code: "he",
        variant: "He",
        rtl: true,
    },
    LocaleInfo {
        code: "hi",
        variant: "Hi",
        rtl: false,
    },
    LocaleInfo {
        code: "hr",
        variant: "Hr",
        rtl: false,
    },
    LocaleInfo {
        code: "hu",
        variant: "Hu",
        rtl: false,
    },
    LocaleInfo {
        code: "id",
        variant: "Id",
        rtl: false,
    },
    LocaleInfo {
        code: "it",
        variant: "It",
        rtl: false,
    },
    LocaleInfo {
        code: "ja",
        variant: "Ja",
        rtl: false,
    },
    LocaleInfo {
        code: "ko",
        variant: "Ko",
        rtl: false,
    },
    LocaleInfo {
        code: "lt",
        variant: "Lt",
        rtl: false,
    },
    LocaleInfo {
        code: "nl",
        variant: "Nl",
        rtl: false,
    },
    LocaleInfo {
        code: "no",
        variant: "No",
        rtl: false,
    },
    LocaleInfo {
        code: "pl",
        variant: "Pl",
        rtl: false,
    },
    LocaleInfo {
        code: "pt-BR",
        variant: "PtBr",
        rtl: false,
    },
    LocaleInfo {
        code: "ro",
        variant: "Ro",
        rtl: false,
    },
    LocaleInfo {
        code: "ru",
        variant: "Ru",
        rtl: false,
    },
    LocaleInfo {
        code: "sv-SE",
        variant: "SvSe",
        rtl: false,
    },
    LocaleInfo {
        code: "th",
        variant: "Th",
        rtl: false,
    },
    LocaleInfo {
        code: "tr",
        variant: "Tr",
        rtl: false,
    },
    LocaleInfo {
        code: "uk",
        variant: "Uk",
        rtl: false,
    },
    LocaleInfo {
        code: "vi",
        variant: "Vi",
        rtl: false,
    },
    LocaleInfo {
        code: "zh-CN",
        variant: "ZhCn",
        rtl: false,
    },
    LocaleInfo {
        code: "zh-TW",
        variant: "ZhTw",
        rtl: false,
    },
];

fn main() {
    let manifest_dir =
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR missing"));
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR missing"));
    let locale_dir = manifest_dir.join("locales");
    let descriptors_path = manifest_dir.join("src/i18n/descriptors.rs");
    let generated_dir = out_dir.join("i18n");
    fs::create_dir_all(&generated_dir).expect("failed to create generated i18n dir");

    println!("cargo:rerun-if-changed=src/i18n/descriptors.rs");
    println!("cargo:rerun-if-changed=locales");
    println!("cargo:rerun-if-changed=package.json");
    println!("cargo:rerun-if-changed=src/styles/app.css");
    println!("cargo:rerun-if-changed=src");
    println!("cargo:rerun-if-changed=content");

    let descriptors = parse_descriptors(&descriptors_path);
    validate_gettext_catalogs(&locale_dir, &generated_dir, &descriptors);
    write_generated_i18n(&generated_dir.join("generated.rs"));
    build_fonts(&manifest_dir, &out_dir);
    build_tailwind(&manifest_dir, &out_dir);
}

fn parse_descriptors(path: &Path) -> Vec<Descriptor> {
    let mut descriptors = Vec::new();
    let mut seen_names = BTreeSet::new();
    let mut seen_keys = BTreeSet::new();
    let mut macro_count = 0;
    for descriptor_file in descriptor_source_files(path) {
        let source = fs::read_to_string(&descriptor_file)
            .unwrap_or_else(|err| panic!("failed to read {}: {}", descriptor_file.display(), err));
        let syntax = syn::parse_file(&source)
            .unwrap_or_else(|err| panic!("failed to parse {}: {}", descriptor_file.display(), err));
        let mut visitor = MarketingMessageVisitor::new(&descriptor_file);
        visitor.visit_file(&syntax);
        macro_count += visitor.macro_count;
        for descriptor in visitor.descriptors {
            let const_name = descriptor.const_name.clone();
            let key = descriptor.key.clone();
            let message = descriptor.message.clone();
            let comment = descriptor.comment.clone();
            if !seen_names.insert(const_name.clone()) {
                panic!("duplicate marketing descriptor const: {}", const_name);
            }
            if !seen_keys.insert(key.clone()) {
                panic!("duplicate marketing descriptor key: {}", key);
            }
            if message.trim().is_empty() {
                panic!(
                    "descriptor {} has an empty American English source string",
                    const_name
                );
            }
            if comment.trim().len() < 24 {
                panic!(
                    "descriptor {} needs a contextual translator comment",
                    const_name
                );
            }
            let placeholders = extract_placeholders(&message);
            validate_descriptor_comment(&const_name, &comment, &placeholders);
            descriptors.push(Descriptor {
                const_name: descriptor.const_name,
                key: descriptor.key,
                message: descriptor.message,
                comment: descriptor.comment,
                placeholders,
            });
        }
    }
    if macro_count != descriptors.len() {
        panic!(
            "parsed {} marketing_message! descriptors from {}, but found {} macro invocations",
            descriptors.len(),
            path.display(),
            macro_count,
        );
    }
    if descriptors.is_empty() {
        panic!(
            "no marketing_message! descriptors found in {}",
            path.display()
        );
    }
    descriptors
}

struct MarketingMessageVisitor<'a> {
    descriptor_file: &'a Path,
    descriptors: Vec<Descriptor>,
    macro_count: usize,
}

impl<'a> MarketingMessageVisitor<'a> {
    fn new(descriptor_file: &'a Path) -> Self {
        Self {
            descriptor_file,
            descriptors: Vec::new(),
            macro_count: 0,
        }
    }
}

impl<'ast> Visit<'ast> for MarketingMessageVisitor<'_> {
    fn visit_macro(&mut self, node: &'ast Macro) {
        if is_marketing_message_macro(node) {
            self.macro_count += 1;
            let input = node
                .parse_body::<MarketingMessageInput>()
                .unwrap_or_else(|err| {
                    panic!(
                        "failed to parse marketing_message! descriptor in {}: {}",
                        self.descriptor_file.display(),
                        err
                    )
                });
            self.descriptors
                .push(input.into_descriptor(self.descriptor_file));
        }
        visit::visit_macro(self, node);
    }
}

struct MarketingMessageInput {
    const_name: Ident,
    key: Expr,
    message: Expr,
    comment: Expr,
}

impl MarketingMessageInput {
    fn into_descriptor(self, descriptor_file: &Path) -> Descriptor {
        let const_name = self.const_name.to_string();
        if !const_name
            .chars()
            .all(|ch| ch == '_' || ch.is_ascii_uppercase() || ch.is_ascii_digit())
        {
            panic!(
                "marketing descriptor const must be uppercase snake case in {}: {}",
                descriptor_file.display(),
                const_name
            );
        }
        let key = expect_string_literal(&self.key, &const_name, "key", descriptor_file);
        let message = parse_descriptor_message(descriptor_file, &const_name, &self.message);
        let comment = expect_string_literal(&self.comment, &const_name, "comment", descriptor_file);
        Descriptor {
            const_name,
            key,
            message,
            comment,
            placeholders: BTreeSet::new(),
        }
    }
}

impl Parse for MarketingMessageInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![pub]>()?;
        input.parse::<Token![const]>()?;
        let const_name = input.parse()?;
        input.parse::<Token![=]>()?;

        let body;
        braced!(body in input);
        let key = parse_expected_field(&body, "key")?;
        body.parse::<Token![,]>()?;
        let message = parse_expected_field(&body, "message")?;
        body.parse::<Token![,]>()?;
        let comment = parse_expected_field(&body, "comment")?;
        if body.peek(Token![,]) {
            body.parse::<Token![,]>()?;
        }
        if !body.is_empty() {
            return Err(body.error("unexpected descriptor field"));
        }

        input.parse::<Token![;]>()?;
        if !input.is_empty() {
            return Err(input.error("unexpected tokens after descriptor"));
        }

        Ok(Self {
            const_name,
            key,
            message,
            comment,
        })
    }
}

fn parse_expected_field(input: ParseStream, expected_name: &str) -> syn::Result<Expr> {
    let name: Ident = input.parse()?;
    if name != expected_name {
        return Err(syn::Error::new(
            name.span(),
            format!("expected `{expected_name}` field"),
        ));
    }
    input.parse::<Token![:]>()?;
    input.parse()
}

fn is_marketing_message_macro(node: &Macro) -> bool {
    if node.path.leading_colon.is_some() {
        return false;
    }
    let segments = node
        .path
        .segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<_>>();
    matches!(
        segments.as_slice(),
        [name] if name == "marketing_message"
    ) || matches!(
        segments.as_slice(),
        [root, name] if root == "crate" && name == "marketing_message"
    )
}

fn expect_string_literal(
    expr: &Expr,
    const_name: &str,
    field_name: &str,
    descriptor_file: &Path,
) -> String {
    if let Expr::Lit(ExprLit {
        lit: Lit::Str(value),
        ..
    }) = expr
    {
        return value.value();
    }
    panic!(
        "descriptor {} field {} must be a string literal in {}",
        const_name,
        field_name,
        descriptor_file.display()
    );
}

fn parse_descriptor_message(descriptor_file: &Path, const_name: &str, message: &Expr) -> String {
    if let Expr::Lit(ExprLit {
        lit: Lit::Str(value),
        ..
    }) = message
    {
        return value.value();
    }
    let Expr::Macro(ExprMacro { mac, .. }) = message else {
        panic!(
            "descriptor {} message must be a string literal or include_str!(...) in {}",
            const_name,
            descriptor_file.display()
        );
    };
    if !is_include_str_macro(mac) {
        panic!(
            "descriptor {} message must be a string literal or include_str!(...) in {}",
            const_name,
            descriptor_file.display()
        );
    }
    let relative_path = mac.parse_body::<syn::LitStr>().unwrap_or_else(|err| {
        panic!(
            "invalid include_str! descriptor message in {} for {}: {}",
            descriptor_file.display(),
            const_name,
            err
        )
    });
    let source_path = descriptor_file
        .parent()
        .expect("descriptor file should have a parent directory")
        .join(relative_path.value());
    fs::read_to_string(&source_path).unwrap_or_else(|err| {
        panic!(
            "failed to read descriptor source {}: {}",
            source_path.display(),
            err
        )
    })
}

fn is_include_str_macro(node: &Macro) -> bool {
    node.path.leading_colon.is_none()
        && node.path.segments.len() == 1
        && node.path.segments[0].ident == "include_str"
}

fn validate_descriptor_comment(const_name: &str, comment: &str, placeholders: &BTreeSet<String>) {
    if placeholders.is_empty() {
        return;
    }
    let lowercase_comment = comment.to_ascii_lowercase();
    let mentions_placeholder_handling = lowercase_comment.contains("placeholder")
        || placeholders.iter().all(|placeholder| {
            comment.contains(&format!("{{{placeholder}}}")) || comment.contains(placeholder)
        });
    if !mentions_placeholder_handling {
        panic!(
            "descriptor {} uses placeholders {:?} but its translator comment does not explain placeholder handling",
            const_name, placeholders,
        );
    }
}

fn descriptor_source_files(path: &Path) -> Vec<PathBuf> {
    let mut files = vec![path.to_path_buf()];
    let Some(stem) = path.file_stem().and_then(|value| value.to_str()) else {
        return files;
    };
    let dir = path.with_file_name(stem);
    if dir.is_dir() {
        let mut children = fs::read_dir(&dir)
            .unwrap_or_else(|err| {
                panic!("failed to read descriptor dir {}: {}", dir.display(), err)
            })
            .map(|entry| entry.expect("failed to read descriptor dir entry").path())
            .filter(|child| child.extension().and_then(|ext| ext.to_str()) == Some("rs"))
            .collect::<Vec<_>>();
        children.sort();
        files.extend(children);
    }
    files
}

fn validate_gettext_catalogs(locale_dir: &Path, generated_dir: &Path, descriptors: &[Descriptor]) {
    let mut locale_codes = BTreeSet::new();
    for locale in LOCALES {
        locale_codes.insert(locale.code);
        let po_path = locale_dir.join(format!("{}.po", locale.code));
        if !po_path.exists() {
            panic!("missing gettext catalog: {}", po_path.display());
        }
    }
    for entry in fs::read_dir(locale_dir).expect("failed to read locale dir") {
        let entry = entry.expect("failed to read locale entry");
        let path = entry.path();
        if path.extension().and_then(|ext| ext.to_str()) == Some("po") {
            let code = path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .expect("invalid locale file name");
            if !locale_codes.contains(code) {
                panic!(
                    "unexpected gettext catalog without Locale enum entry: {}",
                    path.display()
                );
            }
        }
    }

    let source_messages = descriptors
        .iter()
        .map(|descriptor| (descriptor.key.clone(), descriptor))
        .collect::<BTreeMap<_, _>>();
    for locale in LOCALES {
        let po_path = locale_dir.join(format!("{}.po", locale.code));
        let catalog = polib::po_file::parse(&po_path)
            .unwrap_or_else(|err| panic!("failed to parse {}: {}", po_path.display(), err));
        let mut seen = BTreeSet::new();
        for message in catalog.messages() {
            if message.is_fuzzy() {
                panic!(
                    "{} has fuzzy translation for {}",
                    locale.code,
                    message.msgid()
                );
            }
            let key = message
                .msgctxt()
                .unwrap_or_else(|| panic!("message without msgctxt in {}", po_path.display()));
            if key.is_empty() {
                continue;
            }
            let expected = source_messages
                .get(key)
                .unwrap_or_else(|| panic!("{} has extra gettext key {}", locale.code, key));
            if message.msgid() != expected.message {
                panic!(
                    "{} has msgid drift for {} ({}): expected {:?}, got {:?}",
                    locale.code,
                    key,
                    expected.const_name,
                    expected.message,
                    message.msgid(),
                );
            }
            if message.extracted_comments().trim() != expected.comment {
                panic!(
                    "{} translator comment drift for {} ({}). Run cargo run --manifest-path tools/marketing/update-gettext-catalogs/Cargo.toml from the repository root.",
                    locale.code, key, expected.const_name,
                );
            }
            let msgstr = message
                .msgstr()
                .expect("marketing messages should be singular");
            if msgstr.trim().is_empty() {
                panic!("{} has empty translation for {}", locale.code, key);
            }
            if locale.code == "en-US" && msgstr != expected.message {
                panic!("en-US msgstr must match descriptor source for {}", key);
            }
            let actual_placeholders = extract_placeholders(msgstr);
            if actual_placeholders != expected.placeholders {
                panic!(
                    "{} placeholder mismatch for {}: expected {:?}, got {:?}",
                    locale.code, key, expected.placeholders, actual_placeholders,
                );
            }
            if !seen.insert(key.to_owned()) {
                panic!("{} has duplicate gettext key {}", locale.code, key);
            }
        }
        for key in source_messages.keys() {
            if !seen.contains(key) {
                panic!("{} is missing gettext key {}", locale.code, key);
            }
        }
        let mo_path = generated_dir.join(format!("{}.mo", locale_file_stem(locale.code)));
        polib::mo_file::compile_from_po(&po_path, &mo_path)
            .unwrap_or_else(|err| panic!("failed to compile {}: {}", po_path.display(), err));
    }
}

fn extract_placeholders(input: &str) -> BTreeSet<String> {
    let mut result = BTreeSet::new();
    let bytes = input.as_bytes();
    let mut index = 0;
    while index < bytes.len() {
        if bytes[index] != b'{' {
            index += 1;
            continue;
        }
        let start = index + 1;
        let Some(end_offset) = input[start..].find('}') else {
            index += 1;
            continue;
        };
        let end = start + end_offset;
        let candidate = &input[start..end];
        if is_placeholder_name(candidate) {
            result.insert(candidate.to_owned());
        }
        index = end + 1;
    }
    result
}

fn is_placeholder_name(value: &str) -> bool {
    let mut chars = value.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

fn write_generated_i18n(path: &Path) {
    let mut output = String::new();
    output.push_str("// SPDX-License-Identifier: AGPL-3.0-or-later\n");
    output.push_str("// @generated by fluxer_marketing/build.rs\n\n");
    output.push_str("#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]\n");
    output.push_str("pub enum Locale {\n");
    for locale in LOCALES {
        output.push_str(&format!("\t{},\n", locale.variant));
    }
    output.push_str("}\n\n");
    output.push_str("impl Locale {\n");
    output.push_str("\tpub const DEFAULT: Self = Self::EnUs;\n");
    output.push_str("\tpub const ALL: &'static [Self] = &[\n");
    for locale in LOCALES {
        output.push_str(&format!("\t\tSelf::{},\n", locale.variant));
    }
    output.push_str("\t];\n");
    output.push_str("\tpub const fn code(self) -> &'static str {\n\t\tmatch self {\n");
    for locale in LOCALES {
        output.push_str(&format!(
            "\t\t\tSelf::{} => {:?},\n",
            locale.variant, locale.code
        ));
    }
    output.push_str("\t\t}\n\t}\n");
    output.push_str("\tpub const fn is_rtl(self) -> bool {\n\t\tmatch self {\n");
    for locale in LOCALES {
        output.push_str(&format!(
            "\t\t\tSelf::{} => {},\n",
            locale.variant, locale.rtl
        ));
    }
    output.push_str("\t\t}\n\t}\n");
    output.push_str("\tpub const fn catalog_bytes(self) -> &'static [u8] {\n\t\tmatch self {\n");
    for locale in LOCALES {
        output.push_str(&format!(
            "\t\t\tSelf::{} => include_bytes!(concat!(env!(\"OUT_DIR\"), \"/i18n/{}.mo\")),\n",
            locale.variant,
            locale_file_stem(locale.code),
        ));
    }
    output.push_str("\t\t}\n\t}\n");
    output.push_str("}\n");
    fs::write(path, output).expect("failed to write generated i18n Rust");
}

const BUNDLED_FAMILIES: &[&str] = &["FluxerSans", "FluxerMono"];

const BUNDLED_WEIGHTS: &[u64] = &[400, 500, 600, 700];

const EXPECTED_FACE_COUNT: usize = 16;

struct Face {
    css_family: String,
    weight: u64,
    style: String,
    source: String,
}

struct Asset {
    name: String,
    content_type: &'static str,
}

fn build_fonts(manifest_dir: &Path, out_dir: &Path) {
    println!("cargo:rerun-if-changed=../packages/fonts/manifest.json");
    println!("cargo:rerun-if-changed=../packages/fonts/css/locale-fallbacks.css");
    println!("cargo:rerun-if-changed=../packages/fonts/files/FluxerSans");
    println!("cargo:rerun-if-changed=../packages/fonts/files/FluxerMono");
    println!("cargo:rerun-if-changed=../packages/fonts/NOTICE.md");
    println!("cargo:rerun-if-changed=../packages/fonts/LICENSE-IBM-PLEX.txt");

    let package_dir = manifest_dir.join("../packages/fonts");
    let fonts_dir = out_dir.join("static").join("fonts");
    let _ = fs::remove_dir_all(&fonts_dir);
    fs::create_dir_all(&fonts_dir).expect("failed to create generated font dir");

    let mut assets = Vec::new();
    let notice = emit_asset(
        &fonts_dir,
        &package_dir.join("NOTICE.md"),
        "text/plain; charset=utf-8",
        &mut assets,
    );
    let plex_license = emit_asset(
        &fonts_dir,
        &package_dir.join("LICENSE-IBM-PLEX.txt"),
        "text/plain; charset=utf-8",
        &mut assets,
    );

    let faces = select_faces(&package_dir);
    assert_eq!(
        faces.len(),
        EXPECTED_FACE_COUNT,
        "packages/fonts no longer offers the {} Latin-core faces fluxer_marketing renders; \
         reconcile BUNDLED_FAMILIES/BUNDLED_WEIGHTS with the manifest",
        EXPECTED_FACE_COUNT
    );

    let mut stylesheet = String::from("/* SPDX-License-Identifier: AGPL-3.0-or-later */\n");
    stylesheet.push_str(
        "/* @generated by fluxer_marketing/build.rs from packages/fonts. Do not edit by hand. */\n\n",
    );
    stylesheet.push_str(&format!(
        "/*\n\
         \x20* IBM Plex is licensed under the SIL Open Font License 1.1.\n\
         \x20* The IBM Plex faces below are Modified Versions renamed to \"Fluxer Sans\", so OFL\n\
         \x20* clause 3 requires the disclosure to travel with them. It is served beside them:\n\
         \x20*   ./{notice}\n\
         \x20*   ./{plex_license}\n\
         \x20*\n\
         \x20* Every url() below is relative, so it resolves against this stylesheet's own\n\
         \x20* directory. That keeps the sheet correct under any FLUXER_MARKETING_BASE_PATH\n\
         \x20* without the build having to know the runtime base path.\n\
         \x20*/\n"
    ));
    for face in &faces {
        let source = package_dir.join("files").join(&face.source);
        let file = emit_asset(&fonts_dir, &source, "font/woff2", &mut assets);
        stylesheet.push_str(&format!(
            "@font-face {{\n\
             \tfont-family: '{}';\n\
             \tsrc: url('{file}') format('woff2');\n\
             \tfont-weight: {};\n\
             \tfont-style: {};\n\
             \tfont-display: swap;\n\
             }}\n",
            face.css_family, face.weight, face.style
        ));
    }

    let fallbacks_path = package_dir.join("css").join("locale-fallbacks.css");
    let fallbacks = fs::read_to_string(&fallbacks_path)
        .unwrap_or_else(|err| panic!("failed to read {}: {err}", fallbacks_path.display()));
    stylesheet.push('\n');
    stylesheet.push_str(strip_generated_banner(&fallbacks));

    let stylesheet_name = write_hashed(
        &fonts_dir,
        "fonts.css",
        stylesheet.as_bytes(),
        "text/css; charset=utf-8",
        &mut assets,
    );

    write_font_asset_table(out_dir, &stylesheet_name, &assets);
}

fn strip_generated_banner(css: &str) -> &str {
    let mut rest = css;
    while let Some(line_end) = rest.find('\n') {
        let line = rest[..line_end].trim();
        if line.is_empty() || (line.starts_with("/*") && line.ends_with("*/")) {
            rest = &rest[line_end + 1..];
        } else {
            break;
        }
    }
    rest
}

fn select_faces(package_dir: &Path) -> Vec<Face> {
    let manifest_path = package_dir.join("manifest.json");
    let raw = fs::read_to_string(&manifest_path).unwrap_or_else(|err| {
        panic!(
            "failed to read {}: {err}. packages/fonts is generated by \
             `python3 tools/fonts/build_fonts.py`.",
            manifest_path.display()
        )
    });
    let manifest: serde_json::Value =
        serde_json::from_str(&raw).expect("failed to parse packages/fonts/manifest.json");
    let families = manifest["families"]
        .as_array()
        .expect("packages/fonts/manifest.json has no families array");

    let mut faces = Vec::new();
    for wanted in BUNDLED_FAMILIES {
        let family = families
            .iter()
            .find(|family| family["id"].as_str() == Some(wanted))
            .unwrap_or_else(|| panic!("packages/fonts/manifest.json has no family {wanted}"));
        assert_eq!(
            family["latinCore"].as_bool(),
            Some(true),
            "{wanted} is no longer a Latin-core family; it would need unicode-range gating"
        );
        let css_family = family["cssFamily"]
            .as_str()
            .unwrap_or_else(|| panic!("{wanted} has no cssFamily"))
            .to_owned();
        for face in family["faces"]
            .as_array()
            .unwrap_or_else(|| panic!("{wanted} has no faces array"))
        {
            let weight = face["weight"].as_u64().expect("face has no weight");
            if !BUNDLED_WEIGHTS.contains(&weight) {
                continue;
            }
            assert!(
                face["unicodeRange"].is_null(),
                "{wanted} face {} carries a unicode-range; Latin-core faces must not",
                face["file"]
            );
            faces.push(Face {
                css_family: css_family.clone(),
                weight,
                style: face["style"]
                    .as_str()
                    .expect("face has no style")
                    .to_owned(),
                source: face["file"].as_str().expect("face has no file").to_owned(),
            });
        }
    }
    faces
}

fn emit_asset(
    fonts_dir: &Path,
    source: &Path,
    content_type: &'static str,
    assets: &mut Vec<Asset>,
) -> String {
    let bytes =
        fs::read(source).unwrap_or_else(|err| panic!("failed to read {}: {err}", source.display()));
    let file_name = source
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or_else(|| panic!("{} has no file name", source.display()));
    write_hashed(fonts_dir, file_name, &bytes, content_type, assets)
}

fn write_hashed(
    fonts_dir: &Path,
    file_name: &str,
    bytes: &[u8],
    content_type: &'static str,
    assets: &mut Vec<Asset>,
) -> String {
    let (stem, extension) = file_name
        .rsplit_once('.')
        .unwrap_or_else(|| panic!("{file_name} has no extension to hash around"));
    let digest = Sha256::digest(bytes);
    let hash: String = digest
        .iter()
        .take(8)
        .map(|byte| format!("{byte:02x}"))
        .collect();
    let name = format!("{stem}.{hash}.{extension}");
    fs::write(fonts_dir.join(&name), bytes)
        .unwrap_or_else(|err| panic!("failed to write {name}: {err}"));
    assets.push(Asset {
        name: name.clone(),
        content_type,
    });
    name
}

fn write_font_asset_table(out_dir: &Path, stylesheet_name: &str, assets: &[Asset]) {
    let mut generated = String::from(
        "// @generated by fluxer_marketing/build.rs from packages/fonts. Do not edit by hand.\n\n",
    );
    generated.push_str(&format!(
        "/// File name of the content-hashed `@font-face` stylesheet, relative to `/static/fonts/`.\npub const STYLESHEET_FILE_NAME: &str = {stylesheet_name:?};\n\n"
    ));
    generated.push_str("/// `(file name, content type, bytes)` for everything served under `/static/fonts/`.\npub static ASSETS: &[(&str, &str, &[u8])] = &[\n");
    for asset in assets {
        generated.push_str(&format!(
            "    ({:?}, {:?}, include_bytes!(concat!(env!(\"OUT_DIR\"), \"/static/fonts/{}\"))),\n",
            asset.name, asset.content_type, asset.name
        ));
    }
    generated.push_str("];\n");
    fs::write(out_dir.join("static").join("fonts.rs"), generated)
        .expect("failed to write generated font asset table");
}

fn build_tailwind(manifest_dir: &Path, out_dir: &Path) {
    let output_dir = out_dir.join("static");
    fs::create_dir_all(&output_dir).expect("failed to create generated static dir");
    let input = manifest_dir.join("src/styles/app.css");
    let output = output_dir.join("app.css");
    let candidates = [
        manifest_dir.join("node_modules/.bin/tailwindcss"),
        manifest_dir.join("../node_modules/.bin/tailwindcss"),
    ];
    let cli = candidates
        .iter()
        .find(|candidate| candidate.exists())
        .unwrap_or_else(|| {
            panic!(
                "tailwindcss CLI not found. Expected one of: {}",
                candidates
                    .iter()
                    .map(|path| path.display().to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            )
        });
    let status = Command::new(cli)
        .arg("-i")
        .arg(&input)
        .arg("-o")
        .arg(&output)
        .arg("--minify")
        .arg("--cwd")
        .arg(manifest_dir)
        .status()
        .expect("failed to run tailwindcss");
    if !status.success() {
        panic!("tailwindcss failed with status {}", status);
    }
}

fn locale_file_stem(code: &str) -> String {
    code.replace('-', "_")
}
