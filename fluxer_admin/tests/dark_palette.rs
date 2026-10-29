// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

const HUES: &[&str] = &[
    "red", "green", "blue", "amber", "yellow", "orange", "purple", "indigo",
];

const UTILITIES: &[&str] = &[
    "bg", "text", "border", "ring", "divide", "outline", "from", "via", "to", "fill", "stroke",
];

const KEPT_LIGHT_VALUES: &[&str] = &["red-600", "green-600"];

fn manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn rust_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("read source dir") {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            rust_sources(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "rs") {
            out.push(path);
        }
    }
}

fn is_class_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '-'
}

fn used_shades() -> BTreeSet<String> {
    let mut files = Vec::new();
    rust_sources(&manifest_dir().join("src"), &mut files);
    let mut shades = BTreeSet::new();
    for file in files {
        let source = fs::read_to_string(&file).expect("read source");
        for token in source.split(|c: char| !is_class_char(c)) {
            let Some((utility, rest)) = token.split_once('-') else {
                continue;
            };
            if !UTILITIES.contains(&utility) {
                continue;
            }
            let Some((hue, step)) = rest.split_once('-') else {
                continue;
            };
            if HUES.contains(&hue) && !step.is_empty() && step.chars().all(|c| c.is_ascii_digit()) {
                shades.insert(format!("{hue}-{step}"));
            }
        }
    }
    shades
}

fn dark_block() -> String {
    let css = fs::read_to_string(manifest_dir().join("src/styles/app.css")).expect("read app.css");
    let start = css
        .find(":root[data-theme=\"dark\"] {")
        .expect("dark theme block");
    let body = &css[start..];
    let end = body.find('}').expect("dark theme block end");
    body[..end].to_string()
}

fn dark_value(block: &str, shade: &str) -> Option<(f64, f64, f64)> {
    let key = format!("--color-{shade}:");
    let after = &block[block.find(&key)? + key.len()..];
    let inner = after.split_once("oklch(")?.1.split_once(')')?.0;
    let parts: Vec<f64> = inner
        .split_whitespace()
        .map(|p| p.parse().expect("oklch component"))
        .collect();
    Some((parts[0], parts[1], parts[2]))
}

fn relative_luminance((l, c, h): (f64, f64, f64)) -> f64 {
    let a = c * h.to_radians().cos();
    let b = c * h.to_radians().sin();
    let l_ = (l + 0.396_337_777_4 * a + 0.215_803_757_3 * b).powi(3);
    let m_ = (l - 0.105_561_345_8 * a - 0.063_854_172_8 * b).powi(3);
    let s_ = (l - 0.089_484_177_5 * a - 1.291_485_548 * b).powi(3);
    let r = 4.076_741_662_1 * l_ - 3.307_711_591_3 * m_ + 0.230_969_929_2 * s_;
    let g = -1.268_438_004_6 * l_ + 2.609_757_401_1 * m_ - 0.341_319_396_5 * s_;
    let bl = -0.004_196_086_3 * l_ - 0.703_418_614_7 * m_ + 1.707_614_701 * s_;
    0.2126 * r.clamp(0.0, 1.0) + 0.7152 * g.clamp(0.0, 1.0) + 0.0722 * bl.clamp(0.0, 1.0)
}

fn contrast(fg: (f64, f64, f64), bg: (f64, f64, f64)) -> f64 {
    let (x, y) = (relative_luminance(fg), relative_luminance(bg));
    (x.max(y) + 0.05) / (x.min(y) + 0.05)
}

#[test]
fn every_used_chromatic_shade_has_a_dark_value() {
    let block = dark_block();
    let missing: Vec<String> = used_shades()
        .into_iter()
        .filter(|shade| !shade.ends_with("-500") && !KEPT_LIGHT_VALUES.contains(&shade.as_str()))
        .filter(|shade| dark_value(&block, shade).is_none())
        .collect();
    assert!(
        missing.is_empty(),
        "dark theme has no value for {missing:?}"
    );
}

#[test]
fn purple_badge_text_is_readable_in_dark_theme() {
    let block = dark_block();
    let text = dark_value(&block, "purple-700").expect("dark purple-700");
    let background = dark_value(&block, "purple-100").expect("dark purple-100");
    let ratio = contrast(text, background);
    assert!(ratio >= 4.5, "purple badge contrast is {ratio:.2}");
}
