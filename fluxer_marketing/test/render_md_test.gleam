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

import fluxer_marketing/help_center
import fluxer_marketing/locale
import fluxer_marketing/markdown_utils
import fluxer_marketing/render_md
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

fn empty_help_data() -> help_center.HelpCenterData {
  help_center.HelpCenterData(categories: [], all_articles: [])
}

fn without_anchor_style(html: String) -> String {
  html
  |> string.replace(
    " style=\"scroll-margin-top: var(--anchor-offset, 96px)\"",
    "",
  )
  |> string.replace(
    " style=\"scroll-margin-top: var(--anchor-offset, 200px)\"",
    "",
  )
}

pub fn render_simple_paragraph_test() {
  render_md.render(
    "Hello world!",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal("<p>Hello world!</p>")
}

pub fn render_paragraph_with_line_breaks_test() {
  render_md.render(
    "Line one\nLine two",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal("<p>Line one<br>Line two</p>")
}

pub fn render_multiple_paragraphs_test() {
  render_md.render(
    "First paragraph\n\nSecond paragraph",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal("<p>First paragraph</p>\n\n<p>Second paragraph</p>")
}

pub fn render_header_followed_by_paragraph_without_blank_line_test() {
  render_md.render(
    "## Heading\nParagraph on next line",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(
    "<h2 id=\"heading\">Heading</h2>\n\n<p>Paragraph on next line</p>",
  )
}

pub fn render_header_followed_by_list_without_blank_line_test() {
  render_md.render(
    "## Table of contents\n- One\n- Two",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(
    "<h2 id=\"table-of-contents\">Table of contents</h2>\n\n<ul>\n<li>One</li>\n<li>Two</li>\n</ul>",
  )
}

pub fn render_setext_header_followed_by_list_without_blank_line_test() {
  render_md.render(
    "Heading\n-----\n- First\n- Second",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(
    "<h2 id=\"heading\">Heading</h2>\n\n<ul>\n<li>First</li>\n<li>Second</li>\n</ul>",
  )
}

pub fn render_h1_atx_test() {
  render_md.render(
    "# Heading 1",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<h1 id=\"heading-1\" style=\"scroll-margin-top: var(--anchor-offset, 200px)\">Heading 1</h1>",
  )
}

pub fn render_h2_atx_test() {
  render_md.render(
    "## Heading 2",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<h2 id=\"heading-2\" style=\"scroll-margin-top: var(--anchor-offset, 200px)\">Heading 2</h2>",
  )
}

pub fn render_h3_atx_test() {
  render_md.render(
    "### Heading 3",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<h3 id=\"heading-3\" style=\"scroll-margin-top: var(--anchor-offset, 200px)\">Heading 3</h3>",
  )
}

pub fn render_h6_atx_test() {
  render_md.render(
    "###### Heading 6",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<h6 id=\"heading-6\" style=\"scroll-margin-top: var(--anchor-offset, 200px)\">Heading 6</h6>",
  )
}

pub fn render_header_with_custom_id_test() {
  render_md.render(
    "## Heading with anchor {#custom-id}",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<h2 id=\"custom-id\" style=\"scroll-margin-top: var(--anchor-offset, 200px)\">Heading with anchor</h2>",
  )
}

pub fn render_arabic_header_with_custom_id_test() {
  render_md.render(
    "## مرحبًا بك في Fluxer {#welcome-to-fluxer}",
    "https://example.com",
    locale.Ar,
    empty_help_data(),
  )
  |> should.equal(
    "<h2 id=\"welcome-to-fluxer\" style=\"scroll-margin-top: var(--anchor-offset, 200px)\">مرحبًا بك في Fluxer</h2>",
  )
}

pub fn render_numbered_heading_slug_test() {
  render_md.render(
    "## 16. Contact Us",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal("<h2 id=\"16-contact-us\">16. Contact Us</h2>")
}

pub fn render_heading_with_punctuation_slug_test() {
  render_md.render(
    "### 9. Disclaimers, Limitation of Liability, and Indemnification",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(
    "<h3 id=\"9-disclaimers-limitation-of-liability-and-indemnification\">9. Disclaimers, Limitation of Liability, and Indemnification</h3>",
  )
}

pub fn render_h1_setext_test() {
  render_md.render(
    "Heading 1\n=========",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<h1 id=\"heading-1\" style=\"scroll-margin-top: var(--anchor-offset, 200px)\">Heading 1</h1>",
  )
}

pub fn render_h2_setext_test() {
  render_md.render(
    "Heading 2\n---------",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<h2 id=\"heading-2\" style=\"scroll-margin-top: var(--anchor-offset, 200px)\">Heading 2</h2>",
  )
}

pub fn render_horizontal_rule_test() {
  render_md.render("---", "https://example.com", locale.EnUS, empty_help_data())
  |> should.equal("<hr class=\"my-8\">")
}

pub fn render_unordered_list_test() {
  render_md.render(
    "- Item 1\n- Item 2\n- Item 3",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<ul>\n<li>Item 1</li>\n<li>Item 2</li>\n<li>Item 3</li>\n</ul>",
  )
}

pub fn render_ordered_list_test() {
  render_md.render(
    "1. Item 1\n2. Item 2\n3. Item 3",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<ol>\n<li>Item 1</li>\n<li>Item 2</li>\n<li>Item 3</li>\n</ol>",
  )
}

pub fn render_mixed_list_markers_test() {
  render_md.render(
    "* Item 1\n+ Item 2\n- Item 3",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<ul>\n<li>Item 1</li>\n<li>Item 2</li>\n<li>Item 3</li>\n</ul>",
  )
}

pub fn render_list_with_continuation_test() {
  render_md.render(
    "- Item 1\n  continuation\n- Item 2",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal("<ul>\n<li>Item 1 continuation</li>\n<li>Item 2</li>\n</ul>")
}

pub fn render_list_continuation_with_link_test() {
  let markdown =
    "- **Request a data export:** Request a full export of your account data\n  See [Exporting your account data](/help/en-us/articles/1445731738851475456-exporting-your-account-data) for current limits and details."

  let expected =
    "<ul>\n<li><strong>Request a data export:</strong> Request a full export of your account data See <a class=\"text-[#4641D9] hover:underline\" href=\"https://example.com/help/en-us/articles/1445731738851475456-exporting-your-account-data\">Exporting your account data</a> for current limits and details.</li>\n</ul>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_list_continuation_with_article_shortcut_test() {
  let article =
    help_center.HelpArticle(
      title: "Exporting your account data",
      description: "Export data",
      category: "account-settings",
      category_title: "Account Settings",
      category_icon: "gear",
      order: 1,
      slug: "exporting-your-account-data",
      snowflake_id: "111",
      content: "",
    )

  let help_data =
    help_center.HelpCenterData(categories: [], all_articles: [article])

  let markdown =
    "- **Request a data export:** Request a full export of your account data\n  See <% article 111 %> for current limits and details."

  let expected =
    "<ul>\n<li><strong>Request a data export:</strong> Request a full export of your account data See <a class=\"text-[#4641D9] hover:underline\" href=\"https://example.com/help/en-us/articles/111-exporting-your-account-data\">Exporting your account data</a> for current limits and details.</li>\n</ul>"

  render_md.render(markdown, "https://example.com", locale.EnUS, help_data)
  |> should.equal(expected)
}

pub fn render_bold_text_test() {
  render_md.render(
    "This is **bold** text",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal("<p>This is <strong>bold</strong> text</p>")
}

pub fn render_link_test() {
  render_md.render(
    "Visit [Google](https://google.com)",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>Visit <a class=\"text-[#4641D9] hover:underline\" href=\"https://google.com\">Google</a></p>",
  )
}

pub fn render_link_with_relative_url_test() {
  render_md.render(
    "Go to [home](/)",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>Go to <a class=\"text-[#4641D9] hover:underline\" href=\"https://example.com/\">home</a></p>",
  )
}

pub fn render_relative_link_with_trailing_slash_base_test() {
  render_md.render(
    "Read our [privacy policy](/privacy).",
    "https://example.com/",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>Read our <a class=\"text-[#4641D9] hover:underline\" href=\"https://example.com/privacy\">privacy policy</a>.</p>",
  )
}

pub fn render_relative_link_preserves_absolute_test() {
  render_md.render(
    "See [docs](https://docs.example.com/help).",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>See <a class=\"text-[#4641D9] hover:underline\" href=\"https://docs.example.com/help\">docs</a>.</p>",
  )
}

pub fn render_bold_and_link_test() {
  render_md.render(
    "This is **bold** and [linked](http://example.com)",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>This is <strong>bold</strong> and <a class=\"text-[#4641D9] hover:underline\" href=\"http://example.com\">linked</a></p>",
  )
}

pub fn render_html_escaping_test() {
  render_md.render(
    "This has <script> & \"quotes\" and 'apostrophes'",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>This has &lt;script&gt; &amp; &quot;quotes&quot; and &#39;apostrophes&#39;</p>",
  )
}

pub fn render_prefix_before_list_test() {
  let markdown =
    "**Chromium-based browsers** (Chrome, Edge, Brave, Opera):\n1. Navigate to example.com\n2. Click install\n3. Follow prompts"
  let expected =
    "<p><strong>Chromium-based browsers</strong> (Chrome, Edge, Brave, Opera):</p>\n\n<ol>\n<li>Navigate to example.com</li>\n<li>Click install</li>\n<li>Follow prompts</li>\n</ol>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_prefix_before_unordered_list_test() {
  let markdown = "Here are the steps:\n- First step\n- Second step"
  let expected =
    "<p>Here are the steps:</p>\n\n<ul>\n<li>First step</li>\n<li>Second step</li>\n</ul>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_multiple_prefix_lines_before_list_test() {
  let markdown =
    "Section title\nSome description text:\n1. First item\n2. Second item"
  let expected =
    "<p>Section title<br>Some description text:</p>\n\n<ol>\n<li>First item</li>\n<li>Second item</li>\n</ol>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_empty_lines_stripped_test() {
  render_md.render(
    "  \n\nHello\n\n  \n\nWorld\n\n  ",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal("<p>Hello</p>\n\n<p>World</p>")
}

pub fn render_complex_document_test() {
  let markdown =
    "# Main Title\n\nIntro paragraph with **bold** text.\n\n## Section\n\nParagraph before list:\n- Item one\n- Item two\n\n---\n\nFinal paragraph with [link](http://example.com)."
  let expected =
    "<h1 id=\"main-title\">Main Title</h1>\n\n<p>Intro paragraph with <strong>bold</strong> text.</p>\n\n<h2 id=\"section\">Section</h2>\n\n<p>Paragraph before list:</p>\n\n<ul>\n<li>Item one</li>\n<li>Item two</li>\n</ul>\n\n<hr class=\"my-8\">\n\n<p>Final paragraph with <a class=\"text-[#4641D9] hover:underline\" href=\"http://example.com\">link</a>.</p>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_actual_bug_case_test() {
  let markdown =
    "**Chromium-based browsers** (Chrome, Edge, Brave, Opera):\n1. Navigate to [fluxer.app/channels/@me](/channels/@me)\n2. Click the install icon in the address bar, or\n3. Go to browser menu > \"Install Fluxer\" or \"Add to Desktop\""
  let expected =
    "<p><strong>Chromium-based browsers</strong> (Chrome, Edge, Brave, Opera):</p>\n\n<ol>\n<li>Navigate to <a class=\"text-[#4641D9] hover:underline\" href=\"https://example.com/channels/@me\">fluxer.app/channels/@me</a></li>\n<li>Click the install icon in the address bar, or</li>\n<li>Go to browser menu &gt; &quot;Install Fluxer&quot; or &quot;Add to Desktop&quot;</li>\n</ol>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_nested_unordered_list_test() {
  let markdown =
    "- Parent item 1\n  - Nested item 1\n  - Nested item 2\n- Parent item 2"
  let expected =
    "<ul>\n<li>Parent item 1\n<ul>\n<li>Nested item 1</li>\n<li>Nested item 2</li>\n</ul>\n</li>\n<li>Parent item 2</li>\n</ul>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_nested_ordered_list_test() {
  let markdown =
    "1. Parent item 1\n  1. Nested item 1\n  2. Nested item 2\n2. Parent item 2"
  let expected =
    "<ol>\n<li>Parent item 1\n<ol>\n<li>Nested item 1</li>\n<li>Nested item 2</li>\n</ol>\n</li>\n<li>Parent item 2</li>\n</ol>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_mixed_nested_list_test() {
  let markdown =
    "- Parent item 1\n  1. Nested ordered 1\n  2. Nested ordered 2\n- Parent item 2"
  let expected =
    "<ul>\n<li>Parent item 1\n<ol>\n<li>Nested ordered 1</li>\n<li>Nested ordered 2</li>\n</ol>\n</li>\n<li>Parent item 2</li>\n</ul>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_terms_of_service_nested_list_test() {
  let markdown =
    "- **Hosting and infrastructural services:** OVHcloud (primary servers), AWS (backup storage)\n- **Payment processing:** Stripe (payment processing and creator payouts)\n- **Content Delivery Networks:**\n  - Cloudflare (proxy and CDN for user-generated content on **fluxerusercontent.com**; also provides CSAM scanning)\n- **Domain services:** Porkbun (domain registration)"
  let expected =
    "<ul>\n<li><strong>Hosting and infrastructural services:</strong> OVHcloud (primary servers), AWS (backup storage)</li>\n<li><strong>Payment processing:</strong> Stripe (payment processing and creator payouts)</li>\n<li><strong>Content Delivery Networks:</strong>\n<ul>\n<li>Cloudflare (proxy and CDN for user-generated content on <strong>fluxerusercontent.com</strong>; also provides CSAM scanning)</li>\n</ul>\n</li>\n<li><strong>Domain services:</strong> Porkbun (domain registration)</li>\n</ul>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_deeply_nested_list_test() {
  let markdown =
    "- Level 1\n  - Level 2\n    - Level 3\n  - Level 2 again\n- Level 1 again"
  let expected =
    "<ul>\n<li>Level 1\n<ul>\n<li>Level 2\n<ul>\n<li>Level 3</li>\n</ul>\n</li>\n<li>Level 2 again</li>\n</ul>\n</li>\n<li>Level 1 again</li>\n</ul>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_single_line_html_comment_test() {
  render_md.render(
    "<!-- This is a comment -->",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal("")
}

pub fn render_html_comment_with_paragraph_test() {
  render_md.render(
    "Hello world!\n\n<!-- This is a comment -->\n\nGoodbye!",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal("<p>Hello world!</p>\n\n<p>Goodbye!</p>")
}

pub fn render_inline_html_comment_test() {
  render_md.render(
    "This is text <!-- comment --> with more text",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal("<p>This is text  with more text</p>")
}

pub fn render_multiline_html_comment_test() {
  let markdown =
    "# Title\n\n<!-- This is a\nmultiline\ncomment -->\n\nParagraph after comment"
  let expected = "<h1 id=\"title\">Title</h1>\n\n<p>Paragraph after comment</p>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_multiple_html_comments_test() {
  let markdown =
    "<!-- Comment 1 -->\n\nVisible paragraph\n\n<!-- Comment 2 -->\n\nAnother visible paragraph\n\n<!-- Comment 3 -->"
  let expected = "<p>Visible paragraph</p>\n\n<p>Another visible paragraph</p>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_html_comment_in_list_test() {
  let markdown = "- Item 1\n<!-- comment -->\n- Item 2\n- Item 3"
  let expected =
    "<ul>\n<li>Item 1</li>\n<li>Item 2</li>\n<li>Item 3</li>\n</ul>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_html_comment_with_special_chars_test() {
  let markdown =
    "Text before\n\n<!-- Comment with <tags> and & symbols -->\n\nText after"
  let expected = "<p>Text before</p>\n\n<p>Text after</p>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_nested_html_comment_style_test() {
  let markdown =
    "# Heading\n\n<!-- \n  Multi-line comment\n  with indentation\n  and multiple lines\n-->\n\nContent"
  let expected = "<h1 id=\"heading\">Heading</h1>\n\n<p>Content</p>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> without_anchor_style
  |> should.equal(expected)
}

pub fn render_inline_code_test() {
  render_md.render(
    "This is `code` text",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal("<p>This is <strong>code</strong> text</p>")
}

pub fn render_multiple_inline_code_test() {
  render_md.render(
    "Use `foo` and `bar` together",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>Use <strong>foo</strong> and <strong>bar</strong> together</p>",
  )
}

pub fn render_inline_code_with_bold_test() {
  render_md.render(
    "This is **bold** and `code` text",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>This is <strong>bold</strong> and <strong>code</strong> text</p>",
  )
}

pub fn render_inline_code_in_heading_test() {
  render_md.render(
    "# Using `inline code` in headers",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<h1 id=\"using-inline-code-in-headers\" style=\"scroll-margin-top: var(--anchor-offset, 200px)\">Using <strong>inline code</strong> in headers</h1>",
  )
}

pub fn render_inline_code_in_list_test() {
  render_md.render(
    "- Use `command` to run\n- Try `option` for help",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<ul>\n<li>Use <strong>command</strong> to run</li>\n<li>Try <strong>option</strong> for help</li>\n</ul>",
  )
}

pub fn render_app_variable_replacement_test() {
  render_md.render(
    "Visit [%app.host%/channels/@me](%app.proto%://%app.host%/channels/@me)",
    "https://web.fluxer.app",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>Visit <a class=\"text-[#4641D9] hover:underline\" href=\"https://web.fluxer.app/channels/@me\">web.fluxer.app/channels/@me</a></p>",
  )
}

pub fn render_app_shortcut_test() {
  render_md.render(
    "Go to <%app channels/@me %>",
    "https://web.fluxer.app",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>Go to <a class=\"text-[#4641D9] hover:underline\" href=\"https://web.fluxer.app/channels/@me\">web.fluxer.app/channels/@me</a></p>",
  )
}

pub fn render_app_shortcut_in_list_test() {
  render_md.render(
    "- <%app channels/@me %>\n- <%app login %>",
    "https://web.fluxer.app",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<ul>\n<li><a class=\"text-[#4641D9] hover:underline\" href=\"https://web.fluxer.app/channels/@me\">web.fluxer.app/channels/@me</a></li>\n<li><a class=\"text-[#4641D9] hover:underline\" href=\"https://web.fluxer.app/login\">web.fluxer.app/login</a></li>\n</ul>",
  )
}

pub fn render_simple_table_test() {
  let markdown =
    "| Header 1 | Header 2 |\n|----------|----------|\n| Cell 1   | Cell 2   |\n| Cell 3   | Cell 4   |"
  let expected =
    "<table class=\"border-collapse border border-gray-300 w-full my-4\">\n<thead>\n<tr>\n<th class=\"border border-gray-300 px-4 py-2 bg-gray-50 font-semibold\">Header 1</th>\n<th class=\"border border-gray-300 px-4 py-2 bg-gray-50 font-semibold\">Header 2</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class=\"border border-gray-300 px-4 py-2\">Cell 1</td>\n<td class=\"border border-gray-300 px-4 py-2\">Cell 2</td>\n</tr>\n<tr>\n<td class=\"border border-gray-300 px-4 py-2\">Cell 3</td>\n<td class=\"border border-gray-300 px-4 py-2\">Cell 4</td>\n</tr>\n</tbody>\n</table>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(expected)
}

pub fn render_table_with_bold_test() {
  let markdown = "| Name | Status |\n|------|--------|\n| **Alice** | Active |"
  let expected =
    "<table class=\"border-collapse border border-gray-300 w-full my-4\">\n<thead>\n<tr>\n<th class=\"border border-gray-300 px-4 py-2 bg-gray-50 font-semibold\">Name</th>\n<th class=\"border border-gray-300 px-4 py-2 bg-gray-50 font-semibold\">Status</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class=\"border border-gray-300 px-4 py-2\"><strong>Alice</strong></td>\n<td class=\"border border-gray-300 px-4 py-2\">Active</td>\n</tr>\n</tbody>\n</table>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(expected)
}

pub fn render_table_with_links_test() {
  let markdown =
    "| Feature | Link |\n|---------|------|\n| Docs | [Read](https://example.com) |"
  let expected =
    "<table class=\"border-collapse border border-gray-300 w-full my-4\">\n<thead>\n<tr>\n<th class=\"border border-gray-300 px-4 py-2 bg-gray-50 font-semibold\">Feature</th>\n<th class=\"border border-gray-300 px-4 py-2 bg-gray-50 font-semibold\">Link</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class=\"border border-gray-300 px-4 py-2\">Docs</td>\n<td class=\"border border-gray-300 px-4 py-2\"><a class=\"text-[#4641D9] hover:underline\" href=\"https://example.com\">Read</a></td>\n</tr>\n</tbody>\n</table>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(expected)
}

pub fn render_table_without_outer_pipes_test() {
  let markdown = "Header 1 | Header 2\n---------|----------\nCell 1   | Cell 2"
  let expected =
    "<table class=\"border-collapse border border-gray-300 w-full my-4\">\n<thead>\n<tr>\n<th class=\"border border-gray-300 px-4 py-2 bg-gray-50 font-semibold\">Header 1</th>\n<th class=\"border border-gray-300 px-4 py-2 bg-gray-50 font-semibold\">Header 2</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class=\"border border-gray-300 px-4 py-2\">Cell 1</td>\n<td class=\"border border-gray-300 px-4 py-2\">Cell 2</td>\n</tr>\n</tbody>\n</table>"

  render_md.render(
    markdown,
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(expected)
}

pub fn render_email_link_test() {
  render_md.render(
    "Contact us at privacy@fluxer.app",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>Contact us at <a class=\"text-[#4641D9] hover:underline\" href=\"mailto:privacy@fluxer.app\">privacy@fluxer.app</a></p>",
  )
}

pub fn render_multiple_email_links_test() {
  render_md.render(
    "Email support@fluxer.app or privacy@fluxer.app",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>Email <a class=\"text-[#4641D9] hover:underline\" href=\"mailto:support@fluxer.app\">support@fluxer.app</a> or <a class=\"text-[#4641D9] hover:underline\" href=\"mailto:privacy@fluxer.app\">privacy@fluxer.app</a></p>",
  )
}

pub fn render_email_in_list_test() {
  render_md.render(
    "- Contact: privacy@fluxer.app\n- Support: support@fluxer.app",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<ul>\n<li>Contact: <a class=\"text-[#4641D9] hover:underline\" href=\"mailto:privacy@fluxer.app\">privacy@fluxer.app</a></li>\n<li>Support: <a class=\"text-[#4641D9] hover:underline\" href=\"mailto:support@fluxer.app\">support@fluxer.app</a></li>\n</ul>",
  )
}

pub fn render_email_with_other_formatting_test() {
  render_md.render(
    "Email **privacy@fluxer.app** for questions",
    "https://example.com",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>Email <strong><a class=\"text-[#4641D9] hover:underline\" href=\"mailto:privacy@fluxer.app\">privacy@fluxer.app</a></strong> for questions</p>",
  )
}

pub fn render_article_shortcut_test() {
  let test_article =
    help_center.HelpArticle(
      title: "How to delete your account",
      description: "Learn how to delete your account",
      category: "account-settings",
      category_title: "Account Settings",
      category_icon: "gear",
      order: 1,
      slug: "how-to-delete-your-account",
      snowflake_id: "1445724566704881664",
      content: "Article content",
    )

  let test_data =
    help_center.HelpCenterData(categories: [], all_articles: [test_article])

  render_md.render(
    "For more information, see <% article 1445724566704881664 %>.",
    "https://web.fluxer.app",
    locale.EnUS,
    test_data,
  )
  |> should.equal(
    "<p>For more information, see <a class=\"text-[#4641D9] hover:underline\" href=\"https://web.fluxer.app/help/en-us/articles/1445724566704881664-how-to-delete-your-account\">How to delete your account</a>.</p>",
  )
}

pub fn render_article_shortcut_french_test() {
  let test_article =
    help_center.HelpArticle(
      title: "Comment supprimer votre compte",
      description: "Apprenez comment supprimer votre compte",
      category: "account-settings",
      category_title: "Paramètres du compte",
      category_icon: "gear",
      order: 1,
      slug: "comment-supprimer-votre-compte",
      snowflake_id: "1445724566704881664",
      content: "Contenu de l'article",
    )

  let test_data =
    help_center.HelpCenterData(categories: [], all_articles: [test_article])

  render_md.render(
    "Pour plus d'informations, consultez <% article 1445724566704881664 %>.",
    "https://web.fluxer.app",
    locale.Fr,
    test_data,
  )
  |> should.equal(
    "<p>Pour plus d&#39;informations, consultez <a class=\"text-[#4641D9] hover:underline\" href=\"https://web.fluxer.app/help/fr/articles/1445724566704881664-comment-supprimer-votre-compte\">Comment supprimer votre compte</a>.</p>",
  )
}

pub fn render_article_shortcut_not_found_test() {
  render_md.render(
    "See <% article 9999999999999999999 %> for details.",
    "https://web.fluxer.app",
    locale.EnUS,
    empty_help_data(),
  )
  |> should.equal(
    "<p>See &lt;% article 9999999999999999999 %&gt; for details.</p>",
  )
}

pub fn render_multiple_article_shortcuts_test() {
  let article1 =
    help_center.HelpArticle(
      title: "Privacy Policy",
      description: "Our privacy policy",
      category: "legal",
      category_title: "Legal",
      category_icon: "document",
      order: 1,
      slug: "privacy-policy",
      snowflake_id: "1111111111111111111",
      content: "Content 1",
    )

  let article2 =
    help_center.HelpArticle(
      title: "Terms of Service",
      description: "Our terms",
      category: "legal",
      category_title: "Legal",
      category_icon: "document",
      order: 2,
      slug: "terms-of-service",
      snowflake_id: "2222222222222222222",
      content: "Content 2",
    )

  let test_data =
    help_center.HelpCenterData(categories: [], all_articles: [
      article1,
      article2,
    ])

  render_md.render(
    "Read our <% article 1111111111111111111 %> and <% article 2222222222222222222 %>.",
    "https://web.fluxer.app",
    locale.EnUS,
    test_data,
  )
  |> should.equal(
    "<p>Read our <a class=\"text-[#4641D9] hover:underline\" href=\"https://web.fluxer.app/help/en-us/articles/1111111111111111111-privacy-policy\">Privacy Policy</a> and <a class=\"text-[#4641D9] hover:underline\" href=\"https://web.fluxer.app/help/en-us/articles/2222222222222222222-terms-of-service\">Terms of Service</a>.</p>",
  )
}

pub fn render_real_article_shortcut_from_disk_test() {
  let help_data_en = help_center.load_help_articles(locale.EnUS)
  let help_data_fr = help_center.load_help_articles(locale.Fr)
  let help_data_ja = help_center.load_help_articles(locale.Ja)

  let article_id = "1445730947679911936"

  let article_en_title = case
    help_center.get_article_by_snowflake(help_data_en, article_id)
  {
    Ok(article) -> article.title
    Error(_) -> ""
  }

  let article_fr_title = case
    help_center.get_article_by_snowflake(help_data_fr, article_id)
  {
    Ok(article) -> article.title
    Error(_) -> ""
  }

  let article_ja_title = case
    help_center.get_article_by_snowflake(help_data_ja, article_id)
  {
    Ok(article) -> article.title
    Error(_) -> ""
  }

  let result_en =
    render_md.render(
      "See <% article 1445730947679911936 %> for details.",
      "https://web.fluxer.app",
      locale.EnUS,
      help_data_en,
    )

  result_en
  |> string.contains(article_en_title)
  |> should.be_true

  result_en
  |> string.contains("<% article")
  |> should.be_false

  let result_fr =
    render_md.render(
      "Voir <% article 1445730947679911936 %> pour plus de détails.",
      "https://web.fluxer.app",
      locale.Fr,
      help_data_fr,
    )

  result_fr
  |> string.contains(article_fr_title)
  |> should.be_true

  result_fr
  |> string.contains("<% article")
  |> should.be_false

  let result_ja =
    render_md.render(
      "詳細については <% article 1445730947679911936 %> をご覧ください。",
      "https://web.fluxer.app",
      locale.Ja,
      help_data_ja,
    )

  result_ja
  |> string.contains(article_ja_title)
  |> should.be_true

  result_ja
  |> string.contains("<% article")
  |> should.be_false
}

pub fn render_article_shortcut_with_extra_whitespace_test() {
  let test_article =
    help_center.HelpArticle(
      title: "Test Article",
      description: "Test",
      category: "test",
      category_title: "Test",
      category_icon: "test",
      order: 1,
      slug: "test-article",
      snowflake_id: "123456789",
      content: "Content",
    )

  let test_data =
    help_center.HelpCenterData(categories: [], all_articles: [test_article])

  render_md.render(
    "See <%  article   123456789  %> for info.",
    "https://web.fluxer.app",
    locale.EnUS,
    test_data,
  )
  |> string.contains("Test Article")
  |> should.be_true

  render_md.render(
    "See <%article 123456789%> for info.",
    "https://web.fluxer.app",
    locale.EnUS,
    test_data,
  )
  |> string.contains("Test Article")
  |> should.be_true
}

pub fn create_slug_arabic_with_latin_tail_test() {
  help_center.create_slug("كيفية عمل انتهاء صلاحية المرفقات على Fluxer")
  |> should.equal("كيفية-عمل-انتهاء-صلاحية-المرفقات-على-fluxer")
}

pub fn create_slug_all_non_latin_fallback_test() {
  help_center.create_slug("مرحبا بالعالم")
  |> should.equal("مرحبا-بالعالم")
}

pub fn render_article_shortcut_uses_marketing_base_url_test() {
  let article =
    help_center.HelpArticle(
      title: "Support guide",
      description: "Support",
      category: "support",
      category_title: "Support",
      category_icon: "question",
      order: 1,
      slug: "support-guide",
      snowflake_id: "444",
      content: "Content",
    )

  let help_data =
    help_center.HelpCenterData(categories: [], all_articles: [article])

  let result =
    render_md.render_with_base(
      "See <% article 444 %>",
      "https://fluxer.app",
      "https://web.fluxer.app",
      locale.EnUS,
      help_data,
    )

  result
  |> should.equal(
    "<p>See <a class=\"text-[#4641D9] hover:underline\" href=\"https://fluxer.app/help/en-us/articles/444-support-guide\">Support guide</a></p>",
  )
}

pub fn render_arabic_article_shortcut_with_base_url_test() {
  let article =
    help_center.HelpArticle(
      title: "طلب حذف البيانات",
      description: "حذف البيانات",
      category: "account-settings",
      category_title: "إعدادات الحساب",
      category_icon: "shield",
      order: 1,
      slug: "requesting-data-deletion-ar",
      snowflake_id: "1445730947679911936",
      content: "Content",
    )

  let help_data =
    help_center.HelpCenterData(categories: [], all_articles: [article])

  let result =
    render_md.render_with_base(
      "راجع <% article 1445730947679911936 %> للتفاصيل.",
      "https://marketing.example",
      "https://web.fluxer.app",
      locale.Ar,
      help_data,
    )

  result
  |> should.equal(
    "<p>راجع <a class=\"text-[#4641D9] hover:underline\" href=\"https://marketing.example/help/ar/articles/1445730947679911936-طلب-حذف-البيانات\">طلب حذف البيانات</a> للتفاصيل.</p>",
  )
}

pub fn render_article_shortcut_in_list_test() {
  let article1 =
    help_center.HelpArticle(
      title: "First Guide",
      description: "First",
      category: "guides",
      category_title: "Guides",
      category_icon: "book",
      order: 1,
      slug: "first-guide",
      snowflake_id: "111",
      content: "Content 1",
    )

  let article2 =
    help_center.HelpArticle(
      title: "Second Guide",
      description: "Second",
      category: "guides",
      category_title: "Guides",
      category_icon: "book",
      order: 2,
      slug: "second-guide",
      snowflake_id: "222",
      content: "Content 2",
    )

  let test_data =
    help_center.HelpCenterData(categories: [], all_articles: [
      article1,
      article2,
    ])

  let markdown = "Related articles:\n- <% article 111 %>\n- <% article 222 %>"

  let result =
    render_md.render(markdown, "https://web.fluxer.app", locale.EnUS, test_data)

  result
  |> string.contains("First Guide")
  |> should.be_true

  result
  |> string.contains("Second Guide")
  |> should.be_true

  result
  |> string.contains("<% article")
  |> should.be_false
}

pub fn render_article_shortcut_in_paragraph_test() {
  let test_article =
    help_center.HelpArticle(
      title: "Privacy Policy",
      description: "Privacy",
      category: "legal",
      category_title: "Legal",
      category_icon: "shield",
      order: 1,
      slug: "privacy-policy",
      snowflake_id: "999",
      content: "Content",
    )

  let test_data =
    help_center.HelpCenterData(categories: [], all_articles: [test_article])

  let markdown =
    "For more information about data privacy, please review our <% article 999 %> which explains how we handle your data.\n\nWe take privacy seriously."

  let result =
    render_md.render(markdown, "https://web.fluxer.app", locale.EnUS, test_data)

  result
  |> string.contains("Privacy Policy")
  |> should.be_true

  result
  |> string.contains("<% article")
  |> should.be_false

  result
  |> string.contains("which explains how we handle your data")
  |> should.be_true
}

pub fn render_terms_table_of_contents_and_lists_test() {
  let markdown =
    markdown_utils.load_markdown_with_fallback("priv/terms", locale.EnUS)
  let help_data = help_center.load_help_articles(locale.EnUS)

  let rendered =
    render_md.render(markdown, "https://web.fluxer.app", locale.EnUS, help_data)

  rendered
  |> string.contains(
    "<a class=\"text-[#4641D9] hover:underline\" href=\"#welcome-to-fluxer\">Welcome to Fluxer</a>",
  )
  |> should.be_true

  rendered
  |> string.contains(
    "we will automatically attempt to retry charging your payment method a reasonable number of times;",
  )
  |> should.be_true

  rendered
  |> string.contains(
    "/help/en-us/articles/1445724566704881664-how-to-delete-or-disable-your-account",
  )
  |> should.be_true

  rendered
  |> string.contains("<% article")
  |> should.be_false
}
