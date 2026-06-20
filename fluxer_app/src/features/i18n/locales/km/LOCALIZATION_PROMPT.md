# Khmer localization prompt

You are localizing the Fluxer client into Khmer for locale `km`.

## Core rules

- Preserve product names exactly: Fluxer, Fluxer Desktop, Fluxer API, Plutonium, Fluxer Plutonium, Fluxer HQ, FluxerTag.
- Use natural Khmer sentence style. Do not copy English Title Case; use natural Khmer capitalization conventions.
- Keep the tone fresh, clean, trustworthy, concise, and lightly humorous where the surface is low stakes.
- Keep auth, billing, privacy, safety, moderation, outages, and destructive actions calm and plain.
- Avoid legalistic, damning, corporate, or overly serious language.
- Avoid idioms, puns, and culture-specific jokes that are difficult to translate.
- Keep placeholders intact. Do not translate names, domains, URLs, emails, file names, keyboard shortcuts, permission constants, prices, counts, or protocol tokens inside placeholders.
- Reuse established translations for settings tabs, permission labels, shortcut names, key labels, status labels, and repeated command names.
- Keep punctuation consistent with Khmer conventions. Khmer does not use spaces between words but uses spaces between clauses.
- Follow familiar messaging-app terminology for Khmer speakers, while preserving Fluxer nouns such as community, Plutonium, and FluxerTag.
- Do not translate splash quotes one by one. Translate only the single fallback loading string for non-English locales if it appears.
- Khmer has no grammatical plurals (nplurals=1; plural=0). Use the same form for singular and plural.

## Locale guidance

Use natural Khmer (ភាសាខ្មែរ) as spoken in Cambodia. Prefer concise UI labels appropriate for mobile and desktop apps. Use polite but accessible register — not overly formal government language, but not overly casual either. When a precise Khmer technical term does not exist, it is acceptable to use the English loanword written in Khmer script, or keep the English term if it is universally recognized (e.g., WiFi, URL, Bot, GIF).

## Product terms

- community (សហគមន៍): use this for a group server space.
- channel (ឆានែល): use this for a named text or voice space within a community.
- DM (សារផ្ទាល់): use the Khmer term for direct message.
- group DM (សារជាក្រុម): use the Khmer term for a small private group chat.
- role (តួនាទី), permission (សិទ្ធិ), invite (អញ្ជើញ), webhook, passkey, OAuth, bot: use conventional tech terminology.
- favorites (ចំណូលចិត្ត): translate as saved or favorite items.
- Discovery (ការស្ទុះ): use as the app's explore or discover area.

## Quality check

Before returning translations, read them as an actual app screen. They should be short, familiar, grammatically correct in Khmer, and easy to scan. If a string sounds like a formal document, rewrite it more naturally.
