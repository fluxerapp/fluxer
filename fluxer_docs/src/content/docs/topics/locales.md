---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Locales
description: The supported locale registry and how Fluxer resolves a response locale.
---

A locale tells Fluxer which language to write human-readable text in. Each one is a BCP 47 tag from the registry below, either a bare language subtag such as `de`, or one joined to a region or numeric subtag by a hyphen, such as `en-GB`, `pt-BR`, and `es-419`.

## Supported locales

The registry below is the complete set for every Fluxer surface. Wherever Fluxer stores a locale, such as in [user settings](/http-api/users/settings/), it requires an exact value from this table.

| Value | Description |
| --- | --- |
| ar | Arabic |
| bg | Bulgarian |
| cs | Czech |
| da | Danish |
| de | German |
| el | Greek |
| en-GB | English (United Kingdom) |
| en-US<sup>1</sup> | English (United States) |
| es-ES | Spanish (Spain) |
| es-419 | Spanish (Latin America) |
| fi | Finnish |
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
| nl | Dutch |
| no | Norwegian |
| pl | Polish |
| pt-BR | Portuguese (Brazil) |
| ro | Romanian |
| ru | Russian |
| sv-SE<sup>2</sup> | Swedish |
| th | Thai |
| tr | Turkish |
| uk | Ukrainian |
| vi | Vietnamese |
| zh-CN | Chinese (Simplified) |
| zh-TW | Chinese (Traditional) |

<sup>1</sup> [Negotiation](#negotiation) resolves to this value whenever it selects no other registry value. The bare tag `en` is a registered alias for it during negotiation and is not itself a storable value

<sup>2</sup> The bare tag `sv` is a registered alias for it during negotiation and is not itself a storable value

## Negotiation

An authenticated user's saved locale takes precedence over `Accept-Language`. Otherwise, Fluxer negotiates that header against the [supported locales](#supported-locales), falling back to `en-US` when none matches.

Set the account locale through [user settings](/http-api/users/settings/) to make the choice persistent.

Matching ignores case and accepts underscores in place of hyphens. Fluxer first looks for an exact supported tag, or the alias `en` or `sv`, anywhere in the header. It uses a regional fallback only when the header has none. In each of those two passes, Fluxer takes the tag with the highest quality weight, and header order breaks ties.

An unsupported tag such as `fr-CA` or `de-AT` matches no locale. An unsupported tag whose language subtag is in this table selects the locale beside it:

| Language subtag | Selected locale |
| --- | --- |
| `en` | `en-US` |
| `es` | `es-ES` |
| `pt` | `pt-BR` |
| `zh` | `zh-CN` |
| `sv` | `sv-SE` |

For example, `en-AU` selects `en-US` and `pt-PT` selects `pt-BR`. A weight of 0 does not exclude a language, and `*` matches no locale. Send an explicit supported tag for a predictable result.

The resolved locale selects the localised `message` in an [error response](/http-api/#error-response) and in each element of a validation `errors` array.

:::note[Locale selection changes human-readable text only]
A field name, an enumeration value, an error `code`, a [snowflake](/snowflakes/), and a timestamp representation are identical under every locale.
:::
