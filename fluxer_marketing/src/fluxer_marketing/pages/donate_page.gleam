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

import fluxer_marketing/components/hero_base
import fluxer_marketing/i18n
import fluxer_marketing/icons
import fluxer_marketing/pages/layout
import fluxer_marketing/pages/layout/meta.{PageMeta}
import fluxer_marketing/web.{type Context}
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleam/uri
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import wisp

pub fn render(req: wisp.Request, ctx: Context) -> wisp.Response {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let donation_type = get_donation_type(req)

  let content = [
    hero_section(ctx),
    donation_section(ctx, donation_type),
    faq_section(ctx),
  ]

  layout.render(
    req,
    ctx,
    PageMeta(
      title: g_(i18n_ctx, "Support Fluxer"),
      description: g_(
        i18n_ctx,
        "Help support an independent communication platform. Your donation funds the platform's infrastructure and development.",
      ),
      og_type: "website",
    ),
    content,
  )
  |> element.to_document_string_tree
  |> wisp.html_response(200)
}

fn get_donation_type(req: wisp.Request) -> String {
  case uri.parse_query(req.query |> option.unwrap("")) {
    Ok(params) -> {
      params
      |> list.find_map(fn(pair) {
        case pair {
          #("type", value) -> Ok(value)
          _ -> Error(Nil)
        }
      })
      |> result.unwrap("individual")
    }
    Error(_) -> "individual"
  }
}

fn hero_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  hero_base.render(hero_base.HeroConfig(
    icon: icons.heart([
      attribute.class("h-14 w-14 md:h-18 md:w-18 text-white"),
    ]),
    title: g_(i18n_ctx, "Support Fluxer"),
    description: g_(
      i18n_ctx,
      "Help us build an independent communication platform that puts users first.",
    ),
    extra_content: element.none(),
    custom_padding: hero_base.default_padding(),
  ))
}

fn donation_section(ctx: Context, donation_type: String) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let is_business = donation_type == "business"
  let is_sweden = ctx.country_code == "SE"

  html.section(
    [
      attribute.class(
        "bg-gradient-to-b from-white to-gray-50 px-6 py-24 md:py-40",
      ),
    ],
    [
      html.div([attribute.class("mx-auto max-w-4xl")], [
        html.div(
          [
            attribute.class(
              "rounded-3xl bg-white border-2 border-gray-200 px-8 py-12 md:px-12 md:py-16 shadow-xl",
            ),
          ],
          [
            html.div([attribute.class("text-center")], [
              html.h2(
                [
                  attribute.class(
                    "display mb-6 text-black text-3xl md:text-4xl lg:text-5xl font-bold",
                  ),
                ],
                [html.text(g_(i18n_ctx, "Make a Donation"))],
              ),
              html.p(
                [
                  attribute.class(
                    "body-lg text-gray-700 mb-8 md:mb-10 max-w-3xl mx-auto leading-relaxed",
                  ),
                ],
                [
                  html.text(g_(
                    i18n_ctx,
                    "Help support an independent communication platform. Your donation funds the platform's infrastructure and development. It is a voluntary gift and does not constitute payment for any product or service or give you any ownership or special rights.",
                  )),
                ],
              ),
              html.div(
                [
                  attribute.class(
                    "mb-8 md:mb-10 inline-flex rounded-lg bg-gray-100 p-1",
                  ),
                ],
                [
                  html.a(
                    [
                      attribute.id("donate-tab-individual"),
                      web.href(ctx, "/donate?type=individual"),
                      attribute.attribute(
                        "onclick",
                        "return switchDonateTab('individual')",
                      ),
                      attribute.class(case is_business {
                        False ->
                          "donate-tab donate-tab-active rounded-md px-8 py-3 text-base font-semibold bg-[#4641D9] text-white shadow-md transition-all"
                        True ->
                          "donate-tab rounded-md px-8 py-3 text-base font-semibold text-gray-600 hover:text-gray-900 transition-all"
                      }),
                    ],
                    [html.text(g_(i18n_ctx, "Individual"))],
                  ),
                  html.a(
                    [
                      attribute.id("donate-tab-business"),
                      web.href(ctx, "/donate?type=business"),
                      attribute.attribute(
                        "onclick",
                        "return switchDonateTab('business')",
                      ),
                      attribute.class(case is_business {
                        True ->
                          "donate-tab donate-tab-active rounded-md px-8 py-3 text-base font-semibold bg-[#4641D9] text-white shadow-md transition-all"
                        False ->
                          "donate-tab rounded-md px-8 py-3 text-base font-semibold text-gray-600 hover:text-gray-900 transition-all"
                      }),
                    ],
                    [html.text(g_(i18n_ctx, "Business"))],
                  ),
                ],
              ),
              html.div(
                [
                  attribute.id("donate-content-individual"),
                  attribute.class(case is_business {
                    False ->
                      "donate-content flex flex-col sm:flex-row gap-4 justify-center items-center mb-6"
                    True ->
                      "donate-content flex flex-col sm:flex-row gap-4 justify-center items-center mb-6 hidden"
                  }),
                ],
                [
                  html.a(
                    [
                      attribute.href(
                        "https://donate.stripe.com/28EdR97ZU4Ou3IN9c11wY02",
                      ),
                      attribute.target("_blank"),
                      attribute.rel("noopener noreferrer"),
                      attribute.class(
                        "inline-flex items-center justify-center rounded-xl bg-[#4641D9] px-8 py-4 text-lg font-semibold text-white transition-colors hover:bg-[#3d38c7] shadow-lg min-w-[200px]",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Donate in USD"))],
                  ),
                  html.a(
                    [
                      attribute.href(
                        "https://donate.stripe.com/00w7sLfsm0yedjn4VL1wY03",
                      ),
                      attribute.target("_blank"),
                      attribute.rel("noopener noreferrer"),
                      attribute.class(
                        "inline-flex items-center justify-center rounded-xl bg-[#4641D9] px-8 py-4 text-lg font-semibold text-white transition-colors hover:bg-[#3d38c7] shadow-lg min-w-[200px]",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Donate in EUR"))],
                  ),
                ],
              ),
              html.div(
                [
                  attribute.id("donate-content-business"),
                  attribute.class(case is_business {
                    True ->
                      "donate-content flex flex-col sm:flex-row gap-4 justify-center items-center mb-6"
                    False ->
                      "donate-content flex flex-col sm:flex-row gap-4 justify-center items-center mb-6 hidden"
                  }),
                ],
                [
                  html.a(
                    [
                      attribute.href(
                        "https://donate.stripe.com/3cIcN57ZUa8Obbfag51wY00",
                      ),
                      attribute.target("_blank"),
                      attribute.rel("noopener noreferrer"),
                      attribute.class(
                        "inline-flex items-center justify-center rounded-xl bg-[#4641D9] px-8 py-4 text-lg font-semibold text-white transition-colors hover:bg-[#3d38c7] shadow-lg min-w-[200px]",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Donate in USD"))],
                  ),
                  html.a(
                    [
                      attribute.href(
                        "https://donate.stripe.com/5kQ00jdkegxc4MRbk91wY01",
                      ),
                      attribute.target("_blank"),
                      attribute.rel("noopener noreferrer"),
                      attribute.class(
                        "inline-flex items-center justify-center rounded-xl bg-[#4641D9] px-8 py-4 text-lg font-semibold text-white transition-colors hover:bg-[#3d38c7] shadow-lg min-w-[200px]",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Donate in EUR"))],
                  ),
                ],
              ),
              donate_tab_script(),
              case is_sweden {
                True -> swish_donation_button(ctx)
                False -> element.none()
              },
              html.p([attribute.class("body-sm text-gray-500")], [
                html.text(g_(i18n_ctx, "Minimum donation: $5 / €5")),
              ]),
            ]),
          ],
        ),
      ]),
    ],
  )
}

fn swish_donation_button(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.div([attribute.class("mt-6 mb-6")], [
    html.div([attribute.class("flex items-center justify-center gap-2 mb-4")], [
      html.div([attribute.class("flex-1 h-px bg-gray-300")], []),
      html.span([attribute.class("text-gray-500 text-sm px-4")], [
        html.text(g_(i18n_ctx, "or")),
      ]),
      html.div([attribute.class("flex-1 h-px bg-gray-300")], []),
    ]),
    html.a(
      [
        attribute.href(
          "swish://payment?data=%7B%22version%22%3A1%2C%22payee%22%3A%7B%22value%22%3A%221232376820%22%2C%22editable%22%3Afalse%7D%2C%22amount%22%3A%7B%22value%22%3A50%2C%22editable%22%3Atrue%7D%2C%22message%22%3A%7B%22value%22%3A%22Fluxer%20Donation%22%2C%22editable%22%3Afalse%7D%7D",
        ),
        attribute.class(
          "inline-flex items-center justify-center gap-3 rounded-xl bg-white border-2 border-[#00A2FF] px-8 py-4 text-lg font-semibold text-[#00A2FF] transition-colors hover:bg-[#00A2FF] hover:text-white shadow-lg min-w-[200px]",
        ),
      ],
      [
        icons.swish([attribute.class("h-6 w-6")]),
        html.text(g_(i18n_ctx, "Donate with Swish")),
      ],
    ),
    html.p([attribute.class("text-gray-500 text-xs mt-3")], [
      html.text(g_(i18n_ctx, "Available for Swedish bank accounts")),
    ]),
  ])
}

fn donate_tab_script() -> Element(a) {
  html.script(
    [],
    "function switchDonateTab(type) {
  var activeClass = 'donate-tab-active rounded-md px-8 py-3 text-base font-semibold bg-[#4641D9] text-white shadow-md transition-all';
  var inactiveClass = 'donate-tab rounded-md px-8 py-3 text-base font-semibold text-gray-600 hover:text-gray-900 transition-all';
  var individualTab = document.getElementById('donate-tab-individual');
  var businessTab = document.getElementById('donate-tab-business');
  var individualContent = document.getElementById('donate-content-individual');
  var businessContent = document.getElementById('donate-content-business');
  if (type === 'individual') {
    individualTab.className = activeClass;
    businessTab.className = inactiveClass;
    individualContent.classList.remove('hidden');
    businessContent.classList.add('hidden');
    history.replaceState(null, '', '/donate?type=individual');
  } else {
    businessTab.className = activeClass;
    individualTab.className = inactiveClass;
    businessContent.classList.remove('hidden');
    individualContent.classList.add('hidden');
    history.replaceState(null, '', '/donate?type=business');
  }
  return false;
}",
  )
}

fn faq_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section([attribute.class("bg-white px-6 py-24 md:py-40")], [
    html.div([attribute.class("mx-auto max-w-4xl")], [
      html.h2(
        [
          attribute.class(
            "display mb-12 md:mb-16 text-center text-black text-4xl md:text-5xl",
          ),
        ],
        [html.text(g_(i18n_ctx, "Frequently Asked Questions"))],
      ),
      html.div([attribute.class("space-y-8")], [
        faq_item(
          ctx,
          g_(i18n_ctx, "What does my donation support?"),
          g_(
            i18n_ctx,
            "Your donation helps fund Fluxer's infrastructure costs, including servers, bandwidth, storage, and ongoing development of new features. It helps us maintain and improve the platform for everyone.",
          ),
        ),
        faq_item(
          ctx,
          g_(i18n_ctx, "Is this a subscription?"),
          g_(
            i18n_ctx,
            "No, this is a one-time donation. You choose the amount you want to give, and there are no recurring charges.",
          ),
        ),
        faq_item_with_link(
          ctx,
          g_(i18n_ctx, "Do I get anything in return?"),
          g_(
            i18n_ctx,
            "Donations are voluntary gifts and do not provide you with any product, service, ownership, or special rights. If you're looking for premium features, check out {0}.",
          ),
          g_(i18n_ctx, "Fluxer Plutonium"),
          "/plutonium",
        ),
        faq_item(
          ctx,
          g_(i18n_ctx, "Is my donation tax-deductible?"),
          g_(
            i18n_ctx,
            "No, donations to Fluxer are not tax-deductible as we are not a registered charitable organization.",
          ),
        ),
        faq_item(
          ctx,
          g_(i18n_ctx, "What's the difference between Individual and Business?"),
          g_(
            i18n_ctx,
            "Both donation types support the same cause equally. The only difference is that Business donations allow you to specify a tax ID (if applicable) and receive a PDF invoice when finalized, which can be useful for company accounting purposes.",
          ),
        ),
      ]),
    ]),
  ])
}

fn faq_item(_ctx: Context, question: String, answer: String) -> Element(a) {
  html.div(
    [
      attribute.class(
        "rounded-2xl bg-gray-50 border border-gray-200 px-6 py-6 md:px-8 md:py-8",
      ),
    ],
    [
      html.h3([attribute.class("title mb-3 text-black text-xl md:text-2xl")], [
        html.text(question),
      ]),
      html.p([attribute.class("body-lg text-gray-700 leading-relaxed")], [
        html.text(answer),
      ]),
    ],
  )
}

fn faq_item_with_link(
  ctx: Context,
  question: String,
  answer_with_placeholder: String,
  link_text: String,
  link_url: String,
) -> Element(a) {
  let parts = string.split(answer_with_placeholder, "{0}")
  let before = list.first(parts) |> result.unwrap("")
  let after = list.last(parts) |> result.unwrap("")

  html.div(
    [
      attribute.class(
        "rounded-2xl bg-gray-50 border border-gray-200 px-6 py-6 md:px-8 md:py-8",
      ),
    ],
    [
      html.h3([attribute.class("title mb-3 text-black text-xl md:text-2xl")], [
        html.text(question),
      ]),
      html.p([attribute.class("body-lg text-gray-700 leading-relaxed")], [
        html.text(before),
        html.a(
          [
            web.href(ctx, link_url),
            attribute.class("text-[#4641D9] hover:underline font-semibold"),
          ],
          [html.text(link_text)],
        ),
        html.text(after),
      ]),
    ],
  )
}
