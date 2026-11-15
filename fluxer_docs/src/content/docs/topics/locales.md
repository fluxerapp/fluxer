---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Locales
description: The supported locales and how the API picks one for a request.
---

A locale is a BCP 47 tag that sets the language of human-readable text, such as the `message` in an [error response](/http-api/#error-response). Everything else in a response is the same in every locale.

## Supported locales

A `locale` field in a request, such as the one in [user settings](/http-api/users/settings/), has to be one of these values exactly, with the same case.

| Value | Description |
| --- | --- |
| ar | Arabic |
| be | Belarusian |
| bg | Bulgarian |
| bn | Bangla |
| ca | Catalan |
| cs | Czech |
| da | Danish |
| de | German |
| el | Greek |
| en-GB | English (United Kingdom) |
| en-US | English (United States) |
| es-ES | Spanish (Spain) |
| es-419 | Spanish (Latin America) |
| fi | Finnish |
| fil | Filipino |
| fr | French |
| he | Hebrew |
| hi | Hindi |
| hr | Croatian |
| hu | Hungarian |
| id | Indonesian |
| it | Italian |
| ja | Japanese |
| ko | Korean |
| lt | Lithuanian |
| lv | Latvian |
| nl | Dutch |
| no | Norwegian |
| pl | Polish |
| pt-BR | Portuguese (Brazil) |
| pt-PT | Portuguese (Portugal) |
| ro | Romanian |
| ru | Russian |
| sk | Slovak |
| sl | Slovenian |
| sq | Albanian |
| sv-SE | Swedish |
| th | Thai |
| tok | Toki Pona |
| tr | Turkish |
| uk | Ukrainian |
| vi | Vietnamese |
| zh-CN | Chinese (Simplified) |
| zh-TW | Chinese (Traditional) |

## Negotiation

When you're signed in and your account has a saved locale, the API uses it. Otherwise the API picks a locale from the `Accept-Language` header and uses `en-US` when the header is missing or nothing in it matches.

The API sorts the `Accept-Language` tags by `q` weight and makes two passes over them. Matching ignores case and accepts `_` in place of `-`.

1. The first tag that is a supported locale wins. `sv` counts as `sv-SE`, `tl` counts as `fil`, and `nb` and `nn` count as `no`.
2. If no tag matched, the first tag whose language subtag has a locale wins, and `fr-CA` picks `fr`. A Chinese tag with `Hant`, `TW`, `HK` or `MO` picks `zh-TW`. A Portuguese tag with a region other than `BR`, such as `pt-AO`, picks `pt-PT`. The language subtags below pick a set locale.

| Language subtag | Locale |
| --- | --- |
| `en` | `en-US` |
| `es` | `es-ES` |
| `pt` | `pt-BR` |
| `zh` | `zh-CN` |
