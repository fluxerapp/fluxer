// SPDX-License-Identifier: AGPL-3.0-or-later

type Rgb = [f64; 3];

#[derive(Clone, Copy)]
pub enum Theme {
    Dark,
    DarkLegacy,
    Coal,
    Light,
}

impl std::fmt::Display for Theme {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::Dark => "dark",
            Self::DarkLegacy => "dark_legacy",
            Self::Coal => "coal",
            Self::Light => "light",
        })
    }
}

pub const THEMES: [Theme; 4] = [Theme::Dark, Theme::DarkLegacy, Theme::Coal, Theme::Light];

const TARGET_CONTRAST: f64 = 4.5 + 0.05;
const FOREGROUND_SELF_TINT_RATIO: f64 = 15.0;
const FOREGROUND_BRAND_TINT_RATIO: f64 = 22.0;
const LIGHTNESS_UNITS_PER_PERCENT: f64 = 100.0;
const MIN_LIGHTNESS_UNITS: f64 = 0.0;
const MAX_LIGHTNESS_UNITS: f64 = 10000.0;
const LIGHTNESS_SEARCH_TOLERANCE: f64 = 1.0;
const WHITE: Rgb = [255.0, 255.0, 255.0];
const BLACK: Rgb = [0.0, 0.0, 0.0];
const DARK_PRIMARY_STOP: f64 = 0.0;
const DARK_SECONDARY_STOP: f64 = 0.16;
const LIGHT_PRIMARY_STOP: f64 = 1.0;

#[derive(Clone, Copy)]
struct Hsl {
    hue: f64,
    saturation: f64,
    lightness: f64,
}

#[derive(Clone)]
struct Color {
    css: String,
    rgb: Rgb,
}

struct OnColor {
    text: Color,
    fill: Color,
    dark: bool,
}

#[derive(Clone, Copy)]
struct Family {
    hue: f64,
    saturation: f64,
}

const NEUTRAL_DARK: Family = Family {
    hue: 258.0,
    saturation: 10.0,
};
const NEUTRAL_LIGHT: Family = Family {
    hue: 220.0,
    saturation: 10.0,
};
const LEGACY_DARK: Family = Family {
    hue: 220.0,
    saturation: 13.0,
};

#[derive(Clone, Copy)]
enum Curve {
    EaseIn,
    EaseOut,
}

struct Overlay {
    family: Family,
    saturation: Option<f64>,
    lightness: f64,
    alpha: f64,
}

struct Tint {
    surface_stop: f64,
    rest: f64,
    hover: f64,
}

struct Palette {
    family: Family,
    range: (f64, f64),
    curve: Curve,
    stops: &'static [f64],
    primary_stop: f64,
    overlay: Overlay,
    tint: Tint,
}

const DARK_SURFACE_STOPS: &[f64] = &[
    DARK_PRIMARY_STOP,
    DARK_SECONDARY_STOP,
    0.22,
    0.28,
    0.4,
    0.34,
    0.38,
    0.5,
    0.5,
    0.3,
    0.85,
];

const COAL_SURFACE_STOPS: &[f64] = &[
    DARK_PRIMARY_STOP,
    DARK_SECONDARY_STOP,
    0.28,
    0.4,
    0.34,
    0.38,
    0.5,
    0.5,
    0.3,
    0.85,
];

const LIGHT_SURFACE_STOPS: &[f64] = &[
    0.0,
    0.12,
    0.2,
    0.35,
    0.42,
    0.5,
    0.63,
    0.74,
    0.83,
    0.88,
    LIGHT_PRIMARY_STOP,
];

const DARK_TINT: Tint = Tint {
    surface_stop: DARK_SECONDARY_STOP,
    rest: 36.0,
    hover: 48.0,
};

fn palette(theme: Theme) -> Palette {
    match theme {
        Theme::Coal => Palette {
            family: NEUTRAL_DARK,
            range: (1.0, 12.0),
            curve: Curve::EaseOut,
            stops: COAL_SURFACE_STOPS,
            primary_stop: DARK_PRIMARY_STOP,
            overlay: Overlay {
                family: NEUTRAL_DARK,
                saturation: None,
                lightness: 100.0,
                alpha: 0.08,
            },
            tint: Tint {
                surface_stop: DARK_PRIMARY_STOP,
                ..DARK_TINT
            },
        },
        Theme::DarkLegacy => Palette {
            family: LEGACY_DARK,
            range: (5.0, 26.0),
            curve: Curve::EaseOut,
            stops: DARK_SURFACE_STOPS,
            primary_stop: DARK_PRIMARY_STOP,
            overlay: Overlay {
                family: LEGACY_DARK,
                saturation: None,
                lightness: 100.0,
                alpha: 0.1,
            },
            tint: DARK_TINT,
        },
        Theme::Light => Palette {
            family: NEUTRAL_LIGHT,
            range: (86.0, 98.5),
            curve: Curve::EaseIn,
            stops: LIGHT_SURFACE_STOPS,
            primary_stop: LIGHT_PRIMARY_STOP,
            overlay: Overlay {
                family: NEUTRAL_LIGHT,
                saturation: Some(10.0),
                lightness: 10.0,
                alpha: 0.1,
            },
            tint: Tint {
                surface_stop: LIGHT_PRIMARY_STOP,
                rest: 12.0,
                hover: 18.0,
            },
        },
        Theme::Dark => Palette {
            family: NEUTRAL_DARK,
            range: (5.0, 24.0),
            curve: Curve::EaseOut,
            stops: DARK_SURFACE_STOPS,
            primary_stop: DARK_PRIMARY_STOP,
            overlay: Overlay {
                family: NEUTRAL_DARK,
                saturation: None,
                lightness: 100.0,
                alpha: 0.1,
            },
            tint: DARK_TINT,
        },
    }
}

fn js_round(value: f64) -> f64 {
    let floor = value.floor();
    if value - floor >= 0.5 {
        floor + 1.0
    } else {
        floor
    }
}

fn round2(value: f64) -> f64 {
    js_round(value * 100.0) / 100.0 + 0.0
}

fn parse_hex_color(value: &str) -> Option<Rgb> {
    let digits = value.strip_prefix('#')?;
    if digits.len() != 6
        || !digits
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
    {
        return None;
    }
    let mut channels = [0; 3];
    hex::decode_to_slice(digits, &mut channels).ok()?;
    Some(channels.map(f64::from))
}

fn rgb_to_hsl([red, green, blue]: Rgb) -> Hsl {
    let r = red / 255.0;
    let g = green / 255.0;
    let b = blue / 255.0;
    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let lightness = (max + min) / 2.0;
    let delta = max - min;
    if delta == 0.0 {
        return Hsl {
            hue: 0.0,
            saturation: 0.0,
            lightness: lightness * 100.0,
        };
    }
    let saturation = delta / (1.0 - (2.0 * lightness - 1.0).abs());
    let hue = if max == r {
        ((g - b) / delta) % 6.0
    } else if max == g {
        (b - r) / delta + 2.0
    } else {
        (r - g) / delta + 4.0
    };
    Hsl {
        hue: (hue * 60.0 + 360.0) % 360.0,
        saturation: saturation * 100.0,
        lightness: lightness * 100.0,
    }
}

fn hsl_to_rgb(hue: f64, saturation: f64, lightness: f64) -> Rgb {
    let s = saturation.clamp(0.0, 100.0) / 100.0;
    let l = lightness.clamp(0.0, 100.0) / 100.0;
    let chroma = (1.0 - (2.0 * l - 1.0).abs()) * s;
    let h = (((hue % 360.0) + 360.0) % 360.0) / 60.0;
    let x = chroma * (1.0 - ((h % 2.0) - 1.0).abs());
    let m = l - chroma / 2.0;
    let [r, g, b] = if h < 1.0 {
        [chroma, x, 0.0]
    } else if h < 2.0 {
        [x, chroma, 0.0]
    } else if h < 3.0 {
        [0.0, chroma, x]
    } else if h < 4.0 {
        [0.0, x, chroma]
    } else if h < 5.0 {
        [x, 0.0, chroma]
    } else {
        [chroma, 0.0, x]
    };
    [(r + m) * 255.0, (g + m) * 255.0, (b + m) * 255.0]
}

fn mix_rgb(first: Rgb, second: Rgb, first_percent: f64) -> Rgb {
    let ratio = first_percent / 100.0;
    [
        first[0] * ratio + second[0] * (1.0 - ratio),
        first[1] * ratio + second[1] * (1.0 - ratio),
        first[2] * ratio + second[2] * (1.0 - ratio),
    ]
}

fn relative_luminance(rgb: Rgb) -> f64 {
    let linear = |channel: f64| {
        let value = js_round(channel) / 255.0;
        if value <= 0.04045 {
            value / 12.92
        } else {
            ((value + 0.055) / 1.055).powf(2.4)
        }
    };
    0.2126 * linear(rgb[0]) + 0.7152 * linear(rgb[1]) + 0.0722 * linear(rgb[2])
}

fn contrast_ratio(first: Rgb, second: Rgb) -> f64 {
    let a = relative_luminance(first);
    let b = relative_luminance(second);
    (a.max(b) + 0.05) / (a.min(b) + 0.05)
}

fn hsl_color(hsl: Hsl) -> Color {
    let h = round2(hsl.hue);
    let s = round2(hsl.saturation);
    let l = round2(hsl.lightness);
    Color {
        css: format!("hsl({h}, {s}%, {l}%)"),
        rgb: hsl_to_rgb(h, s, l),
    }
}

fn rgb_color(rgb: Rgb) -> Color {
    let [r, g, b] = rgb.map(round2);
    Color {
        css: format!("rgb({r}, {g}, {b})"),
        rgb: [r, g, b],
    }
}

fn resolve_tone(family: Family, saturation: Option<f64>, lightness: f64) -> Rgb {
    hsl_to_rgb(
        round2(family.hue),
        round2(saturation.unwrap_or(family.saturation)),
        round2(lightness),
    )
}

fn stop_lightness(palette: &Palette, position: f64) -> f64 {
    let t = position.clamp(0.0, 1.0);
    let eased = match palette.curve {
        Curve::EaseIn => t * t,
        Curve::EaseOut => 1.0 - (1.0 - t) * (1.0 - t),
    };
    let lightness = palette.range.0 + (palette.range.1 - palette.range.0) * eased;
    js_round(lightness * 1000.0) / 1000.0
}

fn surface_stops(palette: &Palette) -> Vec<Rgb> {
    palette
        .stops
        .iter()
        .map(|position| resolve_surface(palette, *position))
        .collect()
}

fn resolve_surface(palette: &Palette, position: f64) -> Rgb {
    resolve_tone(palette.family, None, stop_lightness(palette, position))
}

fn resolve_surfaces(palette: &Palette) -> Vec<Rgb> {
    let overlay = &palette.overlay;
    let overlay_rgb = resolve_tone(overlay.family, overlay.saturation, overlay.lightness);
    let overlay_percent = overlay.alpha * 100.0;
    let stops = surface_stops(palette);
    let selected: Vec<Rgb> = stops
        .iter()
        .map(|stop| mix_rgb(overlay_rgb, *stop, overlay_percent))
        .collect();
    stops.into_iter().chain(selected).collect()
}

fn is_light_theme(palette: &Palette) -> bool {
    relative_luminance(resolve_surface(palette, palette.primary_stop)) > 0.5
}

fn passes(foreground: Rgb, backgrounds: &[Rgb]) -> bool {
    backgrounds
        .iter()
        .all(|background| contrast_ratio(foreground, *background) >= TARGET_CONTRAST)
}

fn with_lightness(start: Hsl, lightness: f64) -> Hsl {
    Hsl { lightness, ..start }
}

#[derive(Clone, Copy)]
enum LightnessDirection {
    Lighter,
    Darker,
}

fn shift_lightness(
    start: Hsl,
    direction: LightnessDirection,
    accept: impl Fn(&Color) -> bool,
) -> Color {
    let initial = hsl_color(start);
    if accept(&initial) {
        return initial;
    }
    let (step, limit) = match direction {
        LightnessDirection::Lighter => (1.0, MAX_LIGHTNESS_UNITS),
        LightnessDirection::Darker => (-1.0, MIN_LIGHTNESS_UNITS),
    };
    let mut failing = js_round(start.lightness * LIGHTNESS_UNITS_PER_PERCENT);
    let mut passing = limit;
    let mut value = failing + step * LIGHTNESS_UNITS_PER_PERCENT;
    while match direction {
        LightnessDirection::Lighter => value < limit,
        LightnessDirection::Darker => value > limit,
    } {
        if accept(&hsl_color(with_lightness(
            start,
            value / LIGHTNESS_UNITS_PER_PERCENT,
        ))) {
            passing = value;
            break;
        }
        failing = value;
        value += step * LIGHTNESS_UNITS_PER_PERCENT;
    }
    while (passing - failing).abs() > LIGHTNESS_SEARCH_TOLERANCE {
        let middle = js_round((passing + failing) / 2.0);
        if accept(&hsl_color(with_lightness(
            start,
            middle / LIGHTNESS_UNITS_PER_PERCENT,
        ))) {
            passing = middle;
        } else {
            failing = middle;
        }
    }
    hsl_color(with_lightness(start, passing / LIGHTNESS_UNITS_PER_PERCENT))
}

fn neutral(lightness: f64) -> Color {
    hsl_color(Hsl {
        hue: 0.0,
        saturation: 0.0,
        lightness,
    })
}

fn pick_on_color(background: Rgb) -> OnColor {
    let light = OnColor {
        text: neutral(98.0),
        fill: neutral(100.0),
        dark: false,
    };
    let dark = OnColor {
        text: neutral(8.0),
        fill: neutral(8.0),
        dark: true,
    };
    let best = if contrast_ratio(light.text.rgb, background)
        >= contrast_ratio(dark.text.rgb, background)
    {
        light
    } else {
        dark
    };
    if contrast_ratio(best.text.rgb, background) >= TARGET_CONTRAST {
        return best;
    }
    let pure = rgb_color(
        if contrast_ratio(WHITE, background) >= contrast_ratio(BLACK, background) {
            WHITE
        } else {
            BLACK
        },
    );
    OnColor {
        dark: pure.rgb[0] == 0.0,
        text: pure.clone(),
        fill: pure,
    }
}

fn readable_on_surfaces(start: Hsl, palette: &Palette, primary: Rgb) -> Color {
    let surfaces = resolve_surfaces(palette);
    let stops = &surfaces[..surfaces.len() / 2];
    let brand_tints: Vec<Rgb> = stops
        .iter()
        .map(|stop| mix_rgb(primary, *stop, FOREGROUND_BRAND_TINT_RATIO))
        .collect();
    let direction = if is_light_theme(palette) {
        LightnessDirection::Darker
    } else {
        LightnessDirection::Lighter
    };
    shift_lightness(start, direction, |color| {
        let self_tints: Vec<Rgb> = stops
            .iter()
            .map(|stop| mix_rgb(color.rgb, *stop, FOREGROUND_SELF_TINT_RATIO))
            .collect();
        passes(color.rgb, &surfaces)
            && passes(color.rgb, &self_tints)
            && passes(color.rgb, &brand_tints)
    })
}

fn largest_passing_ratio(text: Rgb, primary: Rgb, surface: Rgb, ratio: f64) -> Option<f64> {
    let mut candidate = ratio;
    while candidate >= 0.0 {
        if contrast_ratio(text, mix_rgb(primary, surface, candidate)) >= TARGET_CONTRAST {
            return Some(candidate);
        }
        candidate -= 1.0;
    }
    None
}

struct TintCandidate {
    text: Color,
    rest: f64,
    hover: f64,
}

struct BrandTint {
    rest: Color,
    hover: Color,
    text: Color,
}

fn brand_tint(brand: Hsl, primary: Rgb, palette: &Palette) -> Option<BrandTint> {
    let tint = &palette.tint;
    let surface = resolve_surface(palette, tint.surface_stop);
    if is_light_theme(palette) {
        let rest = rgb_color(mix_rgb(primary, surface, tint.rest));
        let hover = rgb_color(mix_rgb(primary, surface, tint.hover));
        let text = shift_lightness(brand, LightnessDirection::Darker, |color| {
            passes(color.rgb, &[rest.rgb, hover.rgb])
        });
        return Some(BrandTint { rest, hover, text });
    }
    let options: Vec<TintCandidate> = [
        neutral(98.0),
        neutral(8.0),
        rgb_color(WHITE),
        rgb_color(BLACK),
    ]
    .into_iter()
    .filter_map(|text| {
        let hover = largest_passing_ratio(text.rgb, primary, surface, tint.hover);
        let rest = largest_passing_ratio(text.rgb, primary, surface, tint.rest);
        let (hover, rest) = (hover?, rest?);
        Some(TintCandidate {
            text,
            rest: rest.min(hover),
            hover,
        })
    })
    .collect();
    let first = options.first()?;
    let best = options
        .iter()
        .find(|candidate| candidate.rest == tint.rest && candidate.hover == tint.hover)
        .unwrap_or_else(|| {
            options.iter().skip(1).fold(first, |current, option| {
                if option.rest + option.hover > current.rest + current.hover {
                    option
                } else {
                    current
                }
            })
        });
    Some(BrandTint {
        rest: rgb_color(mix_rgb(primary, surface, best.rest)),
        hover: rgb_color(mix_rgb(primary, surface, best.hover)),
        text: best.text.clone(),
    })
}

pub fn instance_brand_variables(
    theme_color: &str,
    theme: Theme,
) -> Option<[(&'static str, String); 13]> {
    let brand = rgb_to_hsl(parse_hex_color(theme_color)?);
    let palette = palette(theme);
    let primary = hsl_color(brand);
    let on_primary = pick_on_color(primary.rgb);
    let secondary = shift_lightness(
        Hsl {
            hue: brand.hue,
            saturation: brand.saturation * (6.0 / 7.0),
            lightness: brand.lightness * 0.89,
        },
        if on_primary.dark {
            LightnessDirection::Lighter
        } else {
            LightnessDirection::Darker
        },
        |color| {
            passes(on_primary.text.rgb, &[color.rgb]) && passes(on_primary.fill.rgb, &[color.rgb])
        },
    );
    let shade = rgb_color(if on_primary.dark { WHITE } else { BLACK });
    let foreground = readable_on_surfaces(brand, &palette, primary.rgb);
    let light = readable_on_surfaces(
        with_lightness(brand, brand.lightness + (100.0 - brand.lightness) * 0.65),
        &palette,
        primary.rgb,
    );
    let on_light = pick_on_color(light.rgb);
    let tint = brand_tint(brand, primary.rgb, &palette)?;
    Some([
        ("--instance-brand-primary", primary.css),
        ("--instance-brand-secondary", secondary.css),
        ("--instance-brand-primary-shade", shade.css),
        ("--instance-brand-primary-light", light.css),
        (
            "--instance-brand-primary-foreground",
            foreground.css.clone(),
        ),
        ("--instance-brand-primary-fill", on_primary.fill.css),
        ("--instance-brand-primary-light-fill", on_light.fill.css),
        ("--instance-text-on-brand-primary", on_primary.text.css),
        ("--instance-text-on-brand-primary-light", on_light.text.css),
        ("--instance-brand-tint", tint.rest.css),
        ("--instance-brand-tint-hover", tint.hover.css),
        ("--instance-brand-tint-border", foreground.css),
        ("--instance-text-on-brand-tint", tint.text.css),
    ])
}
