---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Image and video transformations
description: Query parameters that resize, convert and animate images and extract video thumbnails, with their formats, quality profiles and limits.
---

The Media Proxy resizes and converts images, keeps or removes animation and extracts a still image from a video. The [attachment](/media-proxy/routes/#get-attachment) and [signed external media](/media-proxy/routes/#get-signed-external-media) routes share one parameter set, and the [image asset contract](/media-proxy/routes/#image-asset-contract) has a smaller one.

## When a transformation runs

An attachment or signed external request transforms when `width`, `height`, `format` or `quality` is present, or when `animated` is true. `download` and `effort` don't start a transformation.

A signed external request also transforms when the target filename ends in `.svg` or the origin body is SVG. An SVG attachment with no transformation parameter is still rasterized to lossless WebP, but only in [`mp` mode](/media-proxy/overview/#deployment-modes).

An image asset request always transforms, unless the requested format matches the source and the requested size is at least the source size.

## Representation parameters

| Field | Type | Description |
| --- | --- | --- |
| width? | integer | The output width for an attachment or signed external request (see [allowed dimensions](#allowed-dimensions)) |
| height? | integer | The output height for an attachment or signed external request (see [allowed dimensions](#allowed-dimensions)) |
| size? | integer | The requested square edge for an image asset request, snapped to the [size ladder](#allowed-dimensions) |
| format?<sup>1</sup> | string | The output format, see [attachment and external formats](#attachment-and-external-formats) and [image asset formats](#image-asset-formats) |
| quality? | string | The [quality profile](#quality) |
| animated? | boolean | Whether animated output is requested |
| effort?<sup>2</sup> | integer | The WebP encoder effort for an attachment request |
| download? | boolean | Whether the response uses attachment disposition |

<sup>1</sup> An image asset request also accepts `fmt`, which is used only when `format` is absent

<sup>2</sup> Values above 9 count as 9

A repeated parameter uses its last value, and an unknown parameter is ignored. `animated` and `download` are true only when set to `1` or `true` in any letter case.

## Allowed dimensions

`width` and `height` are free integers from 1 through 16,384, and any other value returns 400.

`size` selects from a fixed ladder: 16, 20, 22, 24, 28, 32, 40, 44, 48, 56, 60, 64, 80, 96, 100, 128, 160, 240, 256, 300, 320, 480, 512, 600, 640, 1024, 1280, 1536, 2048, 3072, 4096, 8192, and 16,384. A requested value snaps up to the next rung (641 selects 1024), and a value above 16,384 selects 16,384.

## Geometry

Transformations never enlarge an image. `width` or `height` alone scales proportionally.

A cover crop scales a still image to fill the rectangle and crops it from the center. It applies when an attachment or signed external request has both `width` and `height`, and to every emoji and sticker asset. Other image assets fit inside the square.

Animated output always fits the whole frame without cropping.

## Asset size selection

An asset request snaps `size` to the ladder and clamps it to the range of its asset class.

| Asset class | Routes | Range |
| --- | --- | --- |
| Icon | avatars, icons, guild member avatars | 128 through 1024 |
| Banner | banners, splashes, embed splashes, guild member banners | 480 through 2400 |
| Role icon | role icons | 16 through 512 |
| Emoji | emojis | 32 through 512 |
| Sticker | stickers | 128 through 512 |
| Branding | branding, sign-in backdrops | 16 through 4096 |

An absent `size` resolves to 128 before clamping, which gives a banner 480. A branding asset with no `size` is the exception and keeps its original size.

## Attachment and external formats

Matching is case-insensitive, and any value outside this table returns 400.

| Value | Output format | Media type |
| --- | --- | --- |
| `png`<sup>1</sup> | PNG | `image/png` |
| `jpg`, `jpeg` | JPEG | `image/jpeg` |
| `webp`<sup>2</sup> | WebP | `image/webp` |
| `gif` | GIF | `image/gif` |
| `apng` | APNG | `image/apng` |
| `avif`, `heic`, `heif`, `jxl`, `svg` | WebP | `image/webp` |

<sup>1</sup> An APNG source with `animated=true` keeps APNG and `image/apng`

<sup>2</sup> A GIF source with `animated=true` keeps GIF and `image/gif`

Without `format`, an attachment uses the extension in its path. Signed external media uses the origin media type, then the target filename. If neither is an image format, the output is WebP.

## Image asset formats

The path extension selects the output, and an extension with no encoder selects WebP. `format`, or `fmt`, overrides the extension when it is `png`, `jpg`, `jpeg`, `webp`, `gif` or `apng`. `avif` selects WebP, and `auto` or any other value keeps the path extension.

A sticker returns WebP whatever `format` says, unless it is an animated GIF requested with `animated=true`, which stays GIF.

## Quality

Values are case-sensitive, and an unrecognized value selects `high`.

| Value | Description |
| --- | --- |
| `low` | Encodes at quality 65 with WebP effort 2 |
| `auto`<sup>1</sup> | Resolves to `lossless` or `high` |
| `high` | Encodes at quality 85 |
| `lossless` | Lossless WebP, or JPEG at quality 100 |

PNG, APNG and GIF output ignores `quality`.

<sup>1</sup> Selects `lossless` for animated WebP from a GIF or APNG source of at most 4 MiB and 16,777,216 pixels across all frames, and `high` otherwise

An image asset defaults to `high`. An attachment or signed external image defaults to `lossless`, except that a JPEG, HEIC, HEIF or JXL source defaults to `high`. Animated WebP output defaults to `auto`.

`effort` applies only to WebP output. Values from 7 to 9 only take effect for lossless animated WebP.

## Animation

An avatar, icon, banner or other image asset whose hash starts with `a_` is animated by default, and one with a plain hash is static. Emojis, stickers, attachments and signed external media are static unless you pass `animated=true`, and `animated=false` turns animation off for an `a_` asset.

Animation needs WebP, GIF or APNG output. Static output has one frame. A request with no transformation parameters returns the original file, animation included.

PNG and JPEG can't animate, and requesting them with `animated=true` for an animated source returns the frames stacked into one tall image.

## Video thumbnails

An image `format` extracts a thumbnail from a video, and the video is never transcoded. `width` and `height` fit the thumbnail inside the rectangle without cropping.

On an attachment, a video request with a transformation parameter but no `format` returns 400. On signed external media it returns the original video.

## Original representations

The Media Proxy returns the original bytes when the source already has the requested format and size and the request has no `effort` or `quality` parameter, though a GIF source ignores `quality` here. An animated source also needs `animated` to be true, explicitly or by default.

Check the response `Content-Type`, which can differ from the filename extension.

## Transformation limits

A stored object or external origin above the [500 MiB media bound](/media-proxy/responses-and-limits/#request-and-media-limits) returns 413.

Decoded images are limited to 16,384 pixels on either edge and 268,435,456 pixels in total. Animated input is also limited to 20,000 frames and 1,073,741,824 pixels across all frames. Exceeding a limit fails the transformation.

Animated WebP and APNG output stops adding frames after 20,000 frames or 30,000 ms of playback and keeps the frames it has. Animated GIF output has no cap.

The transformation deadline is 15,000 ms by default. WebP and APNG animation can be shortened to meet it, and a GIF resize that runs past it fails.

| Status | Condition |
| --- | --- |
| 400 | An attachment or signed external transformation failed |
| 500 | An image asset's stored file isn't an image, or is AVIF, HEIC, HEIF or SVG, and the transformation failed |
| 504 | The transformation was refused [admission](/media-proxy/responses-and-limits/) or exceeded its deadline |

Any other failed image asset transformation returns 200 with the original bytes.
