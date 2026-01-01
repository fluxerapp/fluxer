//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import fluxer_marketing/i18n
import fluxer_marketing/icons
import fluxer_marketing/web.{type Context}
import gleam/list
import gleam/option.{type Option, None, Some}
import kielet.{gettext as g_}
import kielet/context.{type Context as I18nContext}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub type Platform {
  Windows
  MacOS
  Linux
  IOS
  Android
}

pub type ButtonStyle {
  Light
  Dark
}

const light_bg = "bg-white"

const light_text = "text-[#4641D9]"

const light_hover = "hover:bg-gray-50"

const light_helper = "text-sm text-[#4641D9]/60"

const dark_bg = "bg-[#4641D9]"

const dark_text = "text-white"

const dark_hover = "hover:bg-[#3a36b0]"

const dark_helper = "text-sm text-white/70"

const btn_base = "download-link flex flex-col items-start justify-center gap-1 rounded-l-2xl px-6 py-5 md:px-8 md:py-6 transition-colors shadow-lg"

const chevron_base = "overlay-toggle flex items-center self-stretch rounded-r-2xl px-4 transition-colors shadow-lg"

const mobile_btn_base = "inline-flex flex-col items-center justify-center gap-1 rounded-2xl px-6 py-5 md:px-8 md:py-6 transition-colors shadow-lg"

fn channel_segment(ctx: Context) -> String {
  case web.is_canary(ctx) {
    True -> "canary"
    False -> "stable"
  }
}

fn desktop_redirect_url(
  ctx: Context,
  plat: String,
  arch: String,
  fmt: String,
) -> String {
  web.api_url(
    ctx,
    "/dl/desktop/"
      <> channel_segment(ctx)
      <> "/"
      <> plat
      <> "/"
      <> arch
      <> "/latest/"
      <> fmt,
  )
}

fn get_desktop_btn_classes(style: ButtonStyle) -> #(String, String, String) {
  case style {
    Light -> #(
      btn_base <> " " <> light_bg <> " " <> light_text <> " " <> light_hover,
      chevron_base
        <> " "
        <> light_bg
        <> " border-l border-gray-200 "
        <> light_text
        <> " "
        <> light_hover,
      light_helper,
    )
    Dark -> #(
      btn_base <> " " <> dark_bg <> " " <> dark_text <> " " <> dark_hover,
      chevron_base
        <> " "
        <> dark_bg
        <> " border-l border-white/20 "
        <> dark_text
        <> " "
        <> dark_hover,
      dark_helper,
    )
  }
}

fn get_mobile_btn_classes(style: ButtonStyle) -> #(String, String) {
  case style {
    Light -> #(
      mobile_btn_base
        <> " "
        <> light_bg
        <> " "
        <> light_text
        <> " hover:bg-white/90",
      "text-xs text-[#4641D9]/70",
    )
    Dark -> #(
      mobile_btn_base <> " " <> dark_bg <> " " <> dark_text <> " " <> dark_hover,
      "text-xs text-white/70",
    )
  }
}

fn format_arch_label(
  i18n_ctx: I18nContext,
  platform: Platform,
  arch: String,
  fmt: String,
) -> String {
  case platform {
    MacOS ->
      case arch {
        "arm64" -> g_(i18n_ctx, "Apple Silicon") <> " · " <> fmt
        _ -> g_(i18n_ctx, "Intel") <> " · " <> fmt
      }
    _ -> arch <> " · " <> fmt
  }
}

fn format_overlay_label(
  i18n_ctx: I18nContext,
  platform: Platform,
  arch: String,
  fmt: String,
) -> String {
  case platform {
    MacOS ->
      case arch {
        "arm64" -> g_(i18n_ctx, "Apple Silicon") <> " (" <> fmt <> ")"
        _ -> g_(i18n_ctx, "Intel") <> " (" <> fmt <> ")"
      }
    _ -> fmt <> " (" <> arch <> ")"
  }
}

fn default_architecture(ctx: Context, platform: Platform) -> String {
  case platform {
    MacOS ->
      case ctx.architecture {
        web.ARM64 -> "arm64"
        web.ArchUnknown -> "arm64"
        _ -> "x64"
      }
    _ ->
      case ctx.architecture {
        web.ARM64 -> "arm64"
        _ -> "x64"
      }
  }
}

pub fn get_platform_download_info(ctx: Context) -> #(String, String, Element(a)) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  case ctx.platform {
    web.Windows -> {
      let arch = default_architecture(ctx, Windows)
      #(
        desktop_redirect_url(ctx, "win32", arch, "setup"),
        g_(i18n_ctx, "Download for Windows"),
        icons.windows([attribute.class("h-5 w-5")]),
      )
    }
    web.MacOS -> {
      let arch = default_architecture(ctx, MacOS)
      #(
        desktop_redirect_url(ctx, "darwin", arch, "dmg"),
        g_(i18n_ctx, "Download for macOS"),
        icons.apple([attribute.class("h-5 w-5")]),
      )
    }
    web.Linux -> {
      let arch = default_architecture(ctx, Linux)
      #(
        desktop_redirect_url(ctx, "linux", arch, "deb"),
        g_(i18n_ctx, "Choose Linux distribution"),
        icons.linux([attribute.class("h-5 w-5")]),
      )
    }
    web.IOS -> #(
      web.prepend_base_path(ctx, "/download"),
      g_(i18n_ctx, "Mobile apps are underway"),
      icons.download([attribute.class("h-5 w-5")]),
    )
    web.Android -> #(
      web.prepend_base_path(ctx, "/download"),
      g_(i18n_ctx, "Mobile apps are underway"),
      icons.download([attribute.class("h-5 w-5")]),
    )
    web.Unknown -> #(
      web.prepend_base_path(ctx, "/download"),
      g_(i18n_ctx, "Download"),
      icons.download([attribute.class("h-5 w-5")]),
    )
  }
}

pub fn get_system_requirements(ctx: Context, platform: Platform) -> String {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  case platform {
    Windows -> g_(i18n_ctx, "Windows 10+")
    MacOS -> g_(i18n_ctx, "macOS 10.15+")
    Linux -> ""
    IOS -> g_(i18n_ctx, "iOS 15+")
    Android -> g_(i18n_ctx, "Android 8+")
  }
}

fn get_detected_platform_requirements(ctx: Context) -> String {
  case ctx.platform {
    web.Windows -> get_system_requirements(ctx, Windows)
    web.MacOS -> get_system_requirements(ctx, MacOS)
    web.Linux -> get_system_requirements(ctx, Linux)
    web.IOS -> get_system_requirements(ctx, IOS)
    web.Android -> get_system_requirements(ctx, Android)
    web.Unknown -> ""
  }
}

pub fn render_with_overlay(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let requirements = get_detected_platform_requirements(ctx)

  let button = case ctx.platform {
    web.Windows ->
      render_desktop_button(ctx, Windows, Light, None, False, False)
    web.MacOS -> render_desktop_button(ctx, MacOS, Light, None, False, False)
    web.Linux -> render_desktop_button(ctx, Linux, Light, None, False, False)
    web.IOS -> render_mobile_redirect_button(ctx, Light)
    web.Android -> render_mobile_redirect_button(ctx, Light)
    web.Unknown ->
      html.a(
        [
          attribute.href(web.prepend_base_path(ctx, "/download")),
          attribute.class(
            "inline-flex items-center justify-center gap-3 rounded-2xl "
            <> light_bg
            <> " px-8 py-5 md:px-10 md:py-6 text-lg md:text-xl font-semibold "
            <> light_text
            <> " transition-colors hover:bg-white/90 shadow-lg",
          ),
        ],
        [
          icons.download([attribute.class("h-6 w-6 shrink-0")]),
          html.span([], [html.text(g_(i18n_ctx, "Download Fluxer"))]),
        ],
      )
  }

  case requirements {
    "" -> button
    req ->
      html.div([attribute.class("relative")], [
        button,
        html.p(
          [
            attribute.class(
              "absolute left-1/2 -translate-x-1/2 top-full mt-2 text-xs text-white/50 text-center whitespace-nowrap",
            ),
          ],
          [html.text(req)],
        ),
      ])
  }
}

fn render_mobile_redirect_button(ctx: Context, style: ButtonStyle) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let #(btn_class, helper_class) = get_mobile_btn_classes(style)

  html.a(
    [
      attribute.class(btn_class),
      attribute.href(web.prepend_base_path(ctx, "/download")),
    ],
    [
      html.div([attribute.class("flex items-center gap-3")], [
        icons.download([attribute.class("h-6 w-6 shrink-0")]),
        html.span([attribute.class("text-base md:text-lg font-semibold")], [
          html.text(g_(i18n_ctx, "Mobile apps are underway")),
        ]),
      ]),
      html.span([attribute.class(helper_class)], [
        html.text(g_(i18n_ctx, "Use Fluxer in your mobile browser for now")),
      ]),
    ],
  )
}

fn linux_download_options(ctx: Context) -> List(#(String, String, String)) {
  [
    #("x64", "AppImage", desktop_redirect_url(ctx, "linux", "x64", "appimage")),
    #(
      "arm64",
      "AppImage",
      desktop_redirect_url(ctx, "linux", "arm64", "appimage"),
    ),
    #("x64", "DEB", desktop_redirect_url(ctx, "linux", "x64", "deb")),
    #("arm64", "DEB", desktop_redirect_url(ctx, "linux", "arm64", "deb")),
    #("x64", "RPM", desktop_redirect_url(ctx, "linux", "x64", "rpm")),
    #("arm64", "RPM", desktop_redirect_url(ctx, "linux", "arm64", "rpm")),
    #("x64", "tar.gz", desktop_redirect_url(ctx, "linux", "x64", "tar_gz")),
    #("arm64", "tar.gz", desktop_redirect_url(ctx, "linux", "arm64", "tar_gz")),
  ]
}

fn get_platform_config(
  ctx: Context,
  platform: Platform,
  i18n_ctx: I18nContext,
) -> #(String, String, Element(a), List(#(String, String, String))) {
  case platform {
    Windows -> #(
      "windows",
      g_(i18n_ctx, "Windows"),
      icons.windows([attribute.class("h-6 w-6 shrink-0")]),
      [
        #("x64", "EXE", desktop_redirect_url(ctx, "win32", "x64", "setup")),
        #("arm64", "EXE", desktop_redirect_url(ctx, "win32", "arm64", "setup")),
      ],
    )
    MacOS -> #(
      "macos",
      g_(i18n_ctx, "macOS"),
      icons.apple([attribute.class("h-6 w-6 shrink-0")]),
      [
        #("arm64", "DMG", desktop_redirect_url(ctx, "darwin", "arm64", "dmg")),
        #("x64", "DMG", desktop_redirect_url(ctx, "darwin", "x64", "dmg")),
      ],
    )
    Linux -> #(
      "linux",
      g_(i18n_ctx, "Linux"),
      icons.linux([attribute.class("h-6 w-6 shrink-0")]),
      linux_download_options(ctx),
    )
    _ -> #("", "", element.none(), [])
  }
}

pub fn render_desktop_button(
  ctx: Context,
  platform: Platform,
  style: ButtonStyle,
  id_prefix: Option(String),
  compact: Bool,
  full_width: Bool,
) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let #(base_platform_id, platform_name, icon, options) =
    get_platform_config(ctx, platform, i18n_ctx)

  let platform_id = case id_prefix {
    Some(prefix) -> prefix <> "-" <> base_platform_id
    None -> base_platform_id
  }

  let default_arch = default_architecture(ctx, platform)

  let #(default_arch_label, default_fmt, default_url) =
    list.find(options, fn(opt) {
      let #(arch, _, _) = opt
      arch == default_arch
    })
    |> fn(r) {
      case r {
        Ok(opt) -> opt
        Error(_) ->
          case options {
            [first, ..] -> first
            _ -> #("", "", "")
          }
      }
    }

  let helper_text = case platform {
    Linux -> g_(i18n_ctx, "Choose distribution")
    _ -> format_arch_label(i18n_ctx, platform, default_arch_label, default_fmt)
  }
  let #(btn_class, chevron_class, helper_class) = get_desktop_btn_classes(style)

  let container_class = case full_width {
    True -> "flex w-full"
    False -> "flex"
  }

  let width_modifier = case full_width {
    True -> " flex-1 w-full min-w-0"
    False -> ""
  }

  let button_class = btn_class <> width_modifier

  let button_label = case compact {
    True -> platform_name
    False -> g_(i18n_ctx, "Download for ") <> platform_name
  }

  let overlay_items =
    options
    |> list.map(fn(opt) {
      let #(arch, fmt, url) = opt
      html.a(
        [
          attribute.class(
            "download-overlay-link block px-4 py-3 text-sm text-gray-900 hover:bg-gray-100 transition-colors",
          ),
          attribute.attribute("data-base-url", url),
          attribute.attribute("data-arch", arch),
          attribute.attribute("data-format", fmt),
          attribute.attribute("data-platform", platform_id),
          attribute.href(url),
        ],
        [html.text(format_overlay_label(i18n_ctx, platform, arch, fmt))],
      )
    })

  html.div([attribute.class("relative")], [
    html.div(
      [
        attribute.class(container_class),
        attribute.id(platform_id <> "-download-buttons"),
      ],
      [
        html.a(
          [
            attribute.class(button_class),
            attribute.href(default_url),
            attribute.attribute("data-base-url", default_url),
            attribute.attribute("data-arch", default_arch_label),
            attribute.attribute("data-format", default_fmt),
            attribute.attribute("data-platform", platform_id),
          ],
          [
            html.div([attribute.class("flex items-center gap-3")], [
              icon,
              html.span(
                [attribute.class("text-base md:text-lg font-semibold")],
                [
                  html.text(button_label),
                ],
              ),
            ]),
            html.span([attribute.class(helper_class)], [
              html.text(helper_text),
            ]),
          ],
        ),
        html.button(
          [
            attribute.class(chevron_class),
            attribute.attribute(
              "data-overlay-target",
              platform_id <> "-overlay",
            ),
            attribute.attribute(
              "aria-label",
              g_(i18n_ctx, "Show download options"),
            ),
            attribute.attribute("type", "button"),
          ],
          [icons.caret_down([attribute.class("h-5 w-5")])],
        ),
      ],
    ),
    html.div(
      [
        attribute.id(platform_id <> "-overlay"),
        attribute.class(
          "download-overlay absolute left-0 z-20 mt-2 w-full min-w-[220px] rounded-xl bg-white shadow-xl border border-gray-200 hidden",
        ),
      ],
      [html.div([attribute.class("py-1")], overlay_items)],
    ),
  ])
}

pub fn render_mobile_button(
  ctx: Context,
  platform: Platform,
  style: ButtonStyle,
) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let #(platform_name, icon, url, helper_text) = case platform {
    IOS -> #(
      g_(i18n_ctx, "iOS"),
      icons.apple([attribute.class("h-6 w-6 shrink-0")]),
      web.api_url(ctx, "/dl/ios/testflight"),
      "TestFlight",
    )
    Android -> #(
      g_(i18n_ctx, "Android"),
      icons.android([attribute.class("h-6 w-6 shrink-0")]),
      web.api_url(ctx, "/dl/android/arm64/apk"),
      "APK",
    )
    _ -> #("", element.none(), "", "")
  }

  let #(btn_class, helper_class) = get_mobile_btn_classes(style)

  html.a([attribute.class(btn_class), attribute.href(url)], [
    html.div([attribute.class("flex items-center gap-3")], [
      icon,
      html.span([attribute.class("text-base md:text-lg font-semibold")], [
        html.text(g_(i18n_ctx, "Download for ") <> platform_name),
      ]),
    ]),
    html.span([attribute.class(helper_class)], [html.text(helper_text)]),
  ])
}
