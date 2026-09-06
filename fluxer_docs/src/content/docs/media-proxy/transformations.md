---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Image and video transformations
description: Transformation parameters, geometry, formats, quality, animation, and limits.
---

The Media Proxy resizes and converts images, preserves or removes animation, and extracts a still image from a video. The [attachment](/media-proxy/routes/#get-attachment) and [signed external media](/media-proxy/routes/#get-signed-external-media) routes share one parameter set, and the [image asset contract](/media-proxy/routes/#image-asset-contract) has a smaller one of its own.

## When a transformation runs

The theme, entrance sound, and static object routes read no parameter and never transform.

An attachment or signed external request transforms when `width`, `height`, `format`, or `quality` is present, or when `animated` resolves to true. `download` and `effort` select no transformation.

A signed external request also transforms when the target filename ends in `.svg`, and when the origin body is SVG by media type or by its first bytes, whatever the filename. An SVG origin body is transformed unless the origin answered a forwarded range with a 206 under a non-SVG media type, which the proxy relays as raw bytes.

The Media Proxy still rasterises an SVG attachment that selects no transformation. That path always uses lossless WebP and runs only in [`mp` mode](/media-proxy/overview/#deployment-modes).

An image asset request always runs through the transformation engine. It returns the original bytes only when the resolved format already matches the source and the resolved size is at least the source dimensions.

The Media Proxy canonicalises nothing and issues no redirect, so two spellings of one selection are two separate HTTP cache entries.

## Representation parameters

| Field | Type | Description |
| --- | --- | --- |
| width?<sup>1</sup> | integer | The output width for an attachment or signed external request |
| height?<sup>1</sup> | integer | The output height for an attachment or signed external request |
| size?<sup>2</sup> | integer | The requested square edge for an image asset request |
| format?<sup>3</sup> | string | The requested output format under the rules for the route |
| quality? | string | The requested [quality profile](#quality) |
| animated?<sup>4</sup> | boolean | Whether animated output is requested |
| effort?<sup>5</sup> | integer | The WebP encoder effort for an attachment request |
| download? | boolean | Whether the response uses attachment disposition |

<sup>1</sup> An empty, unparsable, zero, or over-bound value returns 400. Any integer from 1 through 16,384 is accepted

<sup>2</sup> No value is rejected. An absent or unparsable value selects 128, a parsable value snaps to the ladder, and the result is then clamped by [asset size selection](#asset-size-selection)

<sup>3</sup> An image asset request accepts the same value under the name `fmt`, and reads `fmt` only when `format` is absent

<sup>4</sup> Overrides the route default in both directions

<sup>5</sup> An empty or unparsable value is ignored. A value from 10 through 255 is clamped to 9, and a value above 255 does not parse, so it too is ignored

A name that occurs more than once resolves to its final value, and an unknown name is ignored. `animated` and `download` are true only for case-insensitive `true` or the exact value `1`. Every other value, including `false` and `0`, is false.

## Allowed dimensions

`width` and `height` are free integers from 1 through 16,384. Any other value returns 400, and neither is clamped into range. The decoded source is bounded separately by [transformation limits](#transformation-limits).

`size` selects from a fixed ladder: 16, 20, 22, 24, 28, 32, 40, 44, 48, 56, 60, 64, 80, 96, 100, 128, 160, 240, 256, 300, 320, 480, 512, 600, 640, 1024, 1280, 1536, 2048, 3072, 4096, 8192, and 16,384. A requested value snaps up to the first rung that is at least as large, so 641 selects 1024. A request above 16,384 selects 16,384, and the class clamp in [asset size selection](#asset-size-selection) can reduce the result further.

## Geometry

Transformations never enlarge an image.

`width` alone scales proportionally to the requested width, and `height` alone scales proportionally to the requested height. A fit inside a rectangle uses the smaller of the two ratios, so a dimension requested larger than the source still shrinks when the other requested dimension is smaller than the source.

A cover crop scales a still image to cover the requested rectangle and crops it centrally. An attachment or signed external request asks for one by supplying both `width` and `height`. An emoji or sticker asset always uses one. Every other image asset fits inside the selected square and preserves its full aspect ratio.

:::note[An animated transformation fits the whole frame]
The Media Proxy downgrades a cover crop to a plain fit whenever it opens the decoder for every page. An animated emoji or sticker is fitted inside its square.
:::

## Asset size selection

An asset request snaps `size` to the ladder and then clamps the result into the range of its asset class. The resolved value is applied to both dimensions, and the [geometry rules](#geometry) then decide whether the image is cropped or fitted inside that square.

| Asset class | Routes | Range |
| --- | --- | --- |
| Icon | avatars, icons, branding, guild member avatars | 128 through 1024 |
| Banner<sup>1</sup> | banners, splashes, embed splashes, guild member banners | 480 through 2400 |
| Emoji | emojis | 32 through 512 |
| Sticker | stickers | 128 through 512 |

<sup>1</sup> The banner maximum of 2400 is not a ladder rung and is reachable only by clamping, so 3072 and every larger request collapse onto it

An absent `size` resolves to 128 before clamping, so an avatar defaults to 128 and a banner defaults to 480. An emoji and a sticker also default to 128. Every ladder value below a class minimum collapses onto that minimum, and every value above a class maximum collapses onto that maximum.

## Attachment and external formats

Format matching is case-insensitive. A value outside this table returns 400, and `auto` is not a value here.

| Value | Output format | Media type |
| --- | --- | --- |
| `png`<sup>1</sup> | PNG | `image/png` |
| `jpg`, `jpeg` | JPEG | `image/jpeg` |
| `webp`<sup>2</sup> | WebP | `image/webp` |
| `gif` | GIF | `image/gif` |
| `apng` | APNG | `image/apng` |
| `avif`, `heic`, `heif`, `jxl`, `svg` | WebP | `image/webp` |

<sup>1</sup> An APNG source requesting PNG output with `animated=true` keeps APNG and `image/apng`

<sup>2</sup> A GIF source requesting WebP output with `animated=true` keeps GIF and `image/gif`

AVIF, HEIC, HEIF, JXL, and SVG decode but have no output encoder, so they are returned as WebP. SVG is rasterised, and a transforming route never returns SVG bytes.

When `format` is omitted, an attachment uses the filename extension in its path. Signed external media uses the origin media type first and then the target filename. If neither identifies an image format, the output is WebP.

## Image asset formats

An image asset path can use PNG, JPEG, WebP, GIF, APNG, AVIF, HEIC, HEIF, JXL, or SVG. A path extension without an output encoder selects WebP.

The `format` query parameter, also accepted as `fmt`, overrides the path extension. It recognises case-insensitive `auto`, `png`, `jpg`, `jpeg`, `webp`, `gif`, `apng`, and `avif`. `auto` leaves the path extension to select the output, and `avif` selects WebP. Any other value, including `heic`, `heif`, `jxl`, `svg`, and an unknown name, falls back to path-based selection.

A sticker always selects WebP and ignores `format`. An animated GIF sticker stays GIF when `animated` resolves to true.

## Quality

Quality names are matched exactly and are case-sensitive. An unrecognised value selects `high`.

| Value | Description |
| --- | --- |
| `low`<sup>1</sup> | Encodes at quality 65 with WebP encoder effort 2 |
| `auto`<sup>2</sup> | Resolves to `lossless` or `high` |
| `high` | Encodes at quality 85 |
| `lossless`<sup>3</sup> | Selects lossless WebP encoding and JPEG quality 100 |

<sup>1</sup> PNG, APNG, and GIF have fixed encoder settings and ignore `quality`, so the quality number reaches WebP and JPEG output only

<sup>2</sup> Resolves to `lossless` only for animated WebP output whose source sniffs as GIF or APNG, is at most 4,194,304 bytes, and decodes to at most 16,777,216 pixels across all frames. It resolves to `high` otherwise, and an animated palette WebP that fails those tests uses WebP encoder effort 0 unless an attachment request supplies `effort`

<sup>3</sup> Lossless applies to WebP alone, and JPEG at quality 100 is still a lossy encode

An image asset defaults to `high`. An attachment or signed external image defaults to `lossless`, except that a JPEG, HEIC, or HEIF source defaults to `high`. Animated WebP output defaults to `auto` on every route that reads `quality`. A video thumbnail is extracted at `high`, and `quality` then applies only to the resize step that `width` or `height` requests. A non-transforming SVG rasterisation always uses `lossless`.

Encoder effort defaults to 2 for animated output or `low` quality and 4 otherwise, and it applies to WebP output only. JPEG and PNG have fixed encoder settings, and GIF always encodes at effort 7. The attachment-only `effort` parameter replaces the default and is clamped to 9. Still WebP output clamps it again to 6, and so does lossy animated WebP. Only lossless animated WebP uses 7 through 9.

## Animation

An owner-and-hash asset whose hash begins with `a_` requests animated output by default, and a bare hash requests static output by default. The prefix is stripped from the storage key, so both spellings read the same stored object.

The `animated` parameter overrides that default in both directions. It is read whenever the name is present and is true only for case-insensitive `true` or the exact value `1`, so `animated=false` forces static output even for an `a_` hash. Omitting the name keeps the route default.

Fluxer issues emoji and sticker paths without an `a_` prefix, so those requests default to static output and need `animated=true` for animation. An attachment or signed external request also defaults to static output.

When encoding occurs, animation survives only when `animated` resolves to true and the selected output is WebP, GIF, or APNG. The Media Proxy rejects the original bytes when the stored bytes sniff as animated and the request resolves to static, so that request is re-encoded to a single frame. Only a read that selects no transformation returns the source animation without `animated=true`.

:::caution[PNG and JPEG output stacks the frames]
PNG and JPEG have no animation, but `animated=true` still opens every page of the source. The encoder receives the frames stacked vertically and writes one tall image.
:::

An animated GIF request for GIF output is resized in its original container, and is returned untouched when the requested size would not change it. Requesting a cover crop routes it through the image pipeline, which still emits GIF.

## Video thumbnails

Fluxer extracts one thumbnail from a video only when the request supplies an explicit image `format`. The video itself is never transcoded. The extractor scans at most 512 packets to find a frame and encodes it as JPEG, PNG, WebP, GIF, or APNG. A `format` value with no output encoder has already been coerced to WebP, so no other target reaches the extractor. `width` and `height` fit the thumbnail inside the requested rectangle without cropping.

An attachment video request with another transformation parameter but no `format` returns 400. A signed external video request without `format` returns the original bytes instead.

An attachment source that is neither an image nor a video returns 400 when `format` is present and is otherwise returned unchanged. A signed external source that is neither an image nor a video is always returned unchanged.

## Original representations

The Media Proxy returns the original bytes when the source already has the selected format, no resize or crop is required, and no encoder option requires a new representation. A source whose bytes sniff as animated also requires a request that resolves to animated, and a static request against it is encoded.

An `effort` value forces encoding, and a `quality` value forces encoding for every source except GIF. An animated attachment or signed external request for the source's own GIF, WebP, or APNG format with neither `width` nor `height` bypasses both tests and can still reuse the original animation.

The Media Proxy derives the response `Content-Type` from the content when the stored media type is empty, is case-insensitively `application/octet-stream`, or is outside the set `image/jpeg`, `image/png`, `image/webp`, `image/gif`, `image/apng`, `image/avif`, `image/heic`, `image/heif`, `image/jxl`, and `image/svg+xml`. An original response can therefore use a different media type from the stored metadata. A stored media type from that set is trusted even when it disagrees with the bytes and is served unchanged.

## Transformation limits

Proxied or stored media is limited to 500 MiB. A stored object above that bound returns 413, and so does an external origin that declares or delivers more. The same bound applies to a transforming request and to every buffer it produces.

Decoded images are limited to 16,384 pixels on either edge and 268,435,456 pixels in total. Animated input is additionally limited to 20,000 decoded frames and 1,073,741,824 decoded pixels across all frames. These four bounds are fixed. Exceeding a decoded image or animation limit fails the transformation.

Animated WebP and animated APNG output is bounded again at encode time, and exceeding one of those bounds truncates the output. The encoder stops adding frames after 20,000 frames, or once the accumulated frame delays reach 30,000 ms of playback, and emits the frames it already has. An operator can configure the frame cap from 1 through 100,000 and the playback cap from 100 through 600,000 ms. Animated GIF output has neither cap on either of its paths, so the decode limits above are its only bound.

The overall transformation deadline defaults to 15,000 ms and can be configured from 1,000 through 120,000 ms. An animated WebP or APNG encode stops 3,000 ms before that deadline so it has time to flush what it has already encoded. Animated GIF output has no such headroom, and a GIF resize that reaches the deadline fails.

An attachment or signed external transformation failure returns 400 with the body `Bad Request`. An image asset transformation failure returns 500 with the body `Transcode Failed` when the source is not directly displayable, and otherwise returns 200 with the original stored bytes and no `Content-Disposition`.

A transformation refused admission returns 504, and so does one that exceeds its deadline. [Responses and limits](/media-proxy/responses-and-limits/) defines the admission capacity and the deadlines.
