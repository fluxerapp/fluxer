---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Locales
description: The supported locale registry and how Fluxer resolves a response locale.
---

A locale tells Fluxer which language to write human-readable text in. Each one is a BCP 47 tag from the registry below, either a bare language subtag such as `de`, or one joined to a region or numeric subtag by a hyphen, such as `en-GB`, `pt-BR`, and `es-419`.

## Supported locales

This registry is the complete set for every Fluxer surface. Wherever Fluxer stores a locale, such as in [user settings](/http-api/users/settings/), it requires an exact value from this table.

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

Fluxer resolves the response locale once for each request. When a request resolves an authenticated user with a stored locale, Fluxer takes that locale. Every other request negotiates the `Accept-Language` header against the [supported locale registry](#supported-locales), and `en-US` is the result whenever negotiation selects no registry value.

An account created by password registration stores the locale negotiated from its own registration request, so the `Accept-Language` header on that request sets the stored value. An account provisioned through single sign-on stores no account locale. Its [user settings](/http-api/users/settings/) locale reads `en-US`, and its requests negotiate `Accept-Language` until the locale setting is changed.

Fluxer splits the header on commas. It trims each member and then splits it on semicolons. The text before the first semicolon is the language range, and Fluxer reads only the first parameter after it, looking for a `q=` weight. A member with no readable `q=` value has weight 1. Fluxer orders the members by descending weight, and members of equal weight keep their header order.

Fluxer then runs two passes over that ordered list.

The first pass takes the earliest member whose range names a registry value exactly. Fluxer trims the range, replaces every underscore with a hyphen, and lowercases it before comparing, so `EN-GB` and `en_gb` both name `en-GB`. The bare tags `en` and `sv` are registered aliases for `en-US` and `sv-SE` and match in this pass.

The second pass runs only when the first selects nothing. It reduces each member in the same order to its language subtag. A language subtag with a declared preference selects that value. The declared preferences are `en` to `en-US`, `es` to `es-ES`, `pt` to `pt-BR`, `zh` to `zh-CN`, and `sv` to `sv-SE`, so `en-AU` selects `en-US` and `pt-PT` selects `pt-BR`. A language subtag without a declared preference selects the first registry value whose tag begins with that subtag and a hyphen.

No registry tag begins with a subtag outside those five followed by a hyphen, so `de-AT` and `xx-YY` both select nothing. A weight of 0 orders a member last, and that member can still be selected. A range of `*` matches no registry value.

The resolved locale selects the localised `message` in an [error response](/http-api/#error-response) and in each element of a validation `errors` array. The `code` field is never localised.

:::note[Locale selection changes human-readable text only]
A field name, an enumeration value, an error `code`, a [snowflake](/snowflakes/), and a timestamp representation are identical under every locale.
:::
