---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: API conventions
description: Normative keywords, protocol subjects, wire table notation, and endpoint entry structure.
---

This page defines the notation every reference page uses: the wire tables, the type names, the footnote markers, the endpoint entry layout, and what an omitted field means. An operation that states a different rule and names the difference overrides anything here. A code example illustrates the contract and never overrides prose, a wire table, a registry, or a state-transition table.

## Normative language

MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT, RECOMMENDED, NOT RECOMMENDED, MAY, and OPTIONAL have their [BCP 14](https://www.rfc-editor.org/info/bcp14) meanings only when they appear in all capitals. A keyword in any other form has its ordinary English meaning, including one that is part of an identifier such as `TWO_FACTOR_REQUIRED`. Literal wire text, including an error message template, retains its exact wording.

Normative force does not depend on a keyword appearing. A direct statement such as "Fluxer returns HTTP 204" defines observable behaviour, and the values in wire tables, registries, algorithms, and state-transition tables are part of the contract.

## Protocol subjects

These subjects have the meanings below throughout the reference.

| Subject | Description |
| --- | --- |
| Fluxer | The deployment whose behaviour this reference documents, also written as the instance |
| caller | The party whose credential, permissions, and address one operation evaluates |
| guild | A community with its own channels, roles, members, and configuration, defined by [Guilds](/http-api/guilds/) |
| session | One established Gateway connection, which is the subject of ordering, delivery, replay, and shard rules |
| user | An account represented by the user object, including a bot account |
| bot | A user account owned by an application and flagged as a bot |
| account | The subject used where a rule holds for a user and a bot alike |
| application | An OAuth2 application record that owns client credentials and at most one bot account |
| operator | The party configuring and running a deployment |
| administrator | An account acting through the Admin API under Admin ACLs |
| resource | The object family and operation set that one reference page defines |

A Dispatch is one Gateway server-to-client event, and a command is one client-to-server message. The reference writes ordinary user or non-bot user where a rule excludes bots.

Several of these words have an unrelated second sense. An authentication session is the stored login record defined by [Authentication](/authentication/), a voice server is the registered media machine defined by [Admin Voice](/admin-api/voice/), and a guild administrator is a member holding guild permissions.

## Wire table notation

A wire table describes one payload, parameter set, or object under `Field`, `Type`, and `Description` columns. A field name ending in `?`, before any footnote marker, is optional. A type beginning with `?` is nullable and permits JSON `null`. A field that is both has the marker in both positions, as in the field `communication_disabled_until?` with the type `?ISO8601 timestamp`. The name written in the table cell is authoritative.

The `Type` column uses this notation.

| Notation | Meaning |
| --- | --- |
| `snowflake` | An unsigned decimal string defined by [Snowflakes](/snowflakes/) |
| `decimal string` | Any other unsigned integer as a JSON string, because its range exceeds what a JSON number preserves exactly |
| `array[type]` | An array of the named type |
| `map[key, value]` | A JSON object keyed by the first type with values of the second |
| `ISO8601 timestamp` | An ISO 8601 timestamp string, which is also the representation of a timestamp field unless its description names another one |
| A link followed by `object` | The object defined at that link |
| `integer string`, `base64 string` | That representation in a JSON string |
| `binary`, `file` | A multipart file part |

The description of a duration field names its unit.

The type of a union lists its alternatives separated by a vertical bar, written as `type \| type` in a table cell. A field that accepts a small fixed set of literal values lists those exact wire values in the same form, as in `emoji \| sticker`.

A superscript marker such as <sup>1</sup> refers to the numbered footnote written beneath its table or paragraph. Numbering restarts in every table. A footnote records a presence condition, gate, bound, or computed value that does not fit in a description cell.

Bitfield tables and enumeration tables with symbolic names share the `Value`, `Name`, and `Description` columns. A table is a bitfield when every non-zero value cell holds a shift expression of the form `1 << n`. An enumeration whose values have no symbolic name uses `Value` and `Description` alone.

A registry is closed when its page states that it is complete, gives an exact count, or states that a value outside it is rejected. A value absent from a closed registry is unsupported even when its wire type could represent it.

A state-transition table uses `Event and condition`, `Action`, and `Next state` columns. Its first cell names an event one state accepts and then the condition that selects this outcome. A state accepts an event only when it appears in that state's table or in a table the section declares for every open state.

## Omission and null

On a request that modifies a stored entity and accepts a subset of its fields, omitting an optional field leaves the stored value unchanged. Sending `null` for a nullable field clears it. A field that is optional but not nullable can be set or left unchanged. A field that is nullable but not optional is always present, even when its value is `null`.

An operation that departs from either default states the departure in the field's description, in a footnote, or beside its body table. A departure can run in either direction, so an operation can accept `null` without clearing and can change a stored value that the request never named. An operation can also define an empty string or an empty array as the clearing value, and a supplied array replaces the stored collection completely.

[Modify meme](/http-api/memes/#modify-meme) accepts this body, which leaves the stored tags unchanged, clears the alt text, and sets the name:

```json
{
  "alt_text": null,
  "name": "party horn"
}
```

On a response, the page that owns a field states what an absent field means. That is commonly that the operation did not populate it, that the object variant does not own it, or that its value has been cleared. Each of those is distinct from a present field whose value is `null`.

## Endpoint entries

An operation entry opens with its method and path. The operation's path parameter table defines each braced segment in the path, such as `{guild_id}`, under the same name without the braces.

The method and path can be followed by capability labels. Only the labels shown apply, and a route whose only authorisation is a signed path or a capability URL has none.

| Label | Meaning |
| --- | --- |
| `Unauthenticated` | The operation accepts a request with no credential |
| `Bot` | The operation accepts a bot token |
| `Audit reason` | The operation reads the `X-Audit-Log-Reason` request header |
| `MFA` | Multi-factor authentication or an elevated [sudo session](/http-api/users/mfa/#sudo-mode) can be required, under the condition the operation states |

A label that is an [OAuth2 scope](/http-api/oauth2/#oauth2-scopes) name means the operation accepts an OAuth2 bearer credential and requires that scope of a bearer caller.

Prose then states the contract. The subsections that apply follow it in this order:

1. `Limitations`, which lists the preconditions an operation stacks, one trigger to a bullet.
2. `Path parameters`.
3. `Query parameters`.
4. `Request headers`.
5. The request body, under `JSON body`, `Form body`, `Multipart body`, or `Request body`.
6. Any subsection the surface defines for itself.
7. `Response body`.
8. `Response`.
9. `Response headers`, which states a header the operation sets for itself.
10. `Side effects`.
11. `Rate limit`, which states the bucket the operation draws on.

A subsection that does not apply is omitted. An object that a page defines has its own field table under `Structure`.

A response table uses `Status`, `Body`, and `Condition` columns. A `Body` cell uses the same type notation as a wire table, names `empty` where the response has no body, and names `response body` where the preceding subsection defines it. A response table has no header column. The shared contract is defined once under [standard response headers](/http-api/#standard-response-headers). A header an operation sets for itself is stated in prose under the operation.

## Describing behaviour

A present-tense statement about Fluxer states an observable contract. An internal storage, service, queue, or worker detail appears only where it determines an observable ordering rule, durability guarantee, limit, timeout, error, or security boundary.

Each failure names the value a client branches on. An HTTP API or Admin API failure names its status and its stable error `code`. An OAuth2 protocol failure answers with the RFC 6749 envelope, which has no Fluxer `code` and is matched on its `error` value. A Media Proxy failure has a plain-text reason phrase and no machine-readable code, so a client branches on its HTTP status. A WebSocket failure names its close code and, where the protocol defines one, the exact close reason.

A human-readable message can be localised, so a client matches only the machine-readable value its surface defines.

## Limits and bounds

Unless a page states otherwise, a limit is inclusive. A bound written as less than or before excludes the stated value.

An array bound counts elements. A string bound names its unit, written as characters, UTF-8 bytes, UTF-16 code units, or grapheme clusters, and a bound that names more than one unit applies all of them. A bound on an encoded value states whether it applies before or after decoding.

## Examples and notes

A `json` example shows one valid or representative wire value, and a `text` example shows an expression or a string form. Every identifier, token, hash, and host in an example is fabricated, and an example host uses the reserved `example.com` domain.

Inside inline code, angle brackets mark descriptive placeholder text standing for a real value, as in `Bot <token>` or `attachment://<filename>`. An ellipsis inside a JSON string value elides content the example does not need to show. Where inline code quotes an exact response body, the brackets are part of the literal value.

A note states a consequence or a relationship to another rule. A caution states a detail a client would otherwise get wrong, which covers a contract that breaks the opposite assumption, a value the client must keep confidential, and an effect that cannot be undone. A danger is reserved for an outcome that destroys stored data, files an external report, revokes every credential on an account, or discloses a credential no later operation can return. All three are binding.

## Independent protocol surfaces

The HTTP API and the Gateway each own an error registry, and the Media Proxy API owns none because its failures have no code. The Gateway alone defines opcodes and close codes.

The Admin API is a privileged namespace inside the HTTP API. It shares the `/v1` prefix, the request and response framing, the error envelope, and the standard response headers. It adds its own credential policy, ACL registry, audit contract, and rate limit registry.

An identifier other than a [snowflake](/snowflakes/) reaches a second surface only where that surface names it, as the Media Proxy API does for the upload capability and the asset hash the HTTP API issues.
