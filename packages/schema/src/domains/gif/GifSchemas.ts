// SPDX-License-Identifier: AGPL-3.0-or-later

import {LocaleSchema} from '@fluxer/schema/src/primitives/LocaleSchema';
import {createStringType, Int32Type} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const GIF_PROVIDER_HEADER = 'X-Fluxer-GIF-Provider';
export const GIF_PROVIDER_DISPLAY_NAME_HEADER = 'X-Fluxer-GIF-Provider-Display-Name';
export const GIF_PROVIDER_ATTRIBUTION_HEADER = 'X-Fluxer-GIF-Provider-Attribution-Required';
const LocaleType = LocaleSchema.default('en-US').transform((v) => v.replace('-', '_'));
const GifProviderName = createStringType(1, 32).describe(
	'Identifier of the active GIF provider. KLIPY is currently the only supported provider.',
);

export const GifSearchQuery = z.object({
	q: createStringType(1, 256).describe('The search query'),
	locale: LocaleType,
});

export type GifSearchQuery = z.infer<typeof GifSearchQuery>;

export const GifLocaleQuery = z.object({
	locale: LocaleType,
});

export type GifLocaleQuery = z.infer<typeof GifLocaleQuery>;

export const GifRegisterShareRequest = z.object({
	id: createStringType(1, 300).describe('Provider-issued share identifier (slug or slug-id token).'),
	q: createStringType(0, 256).nullish().describe('Optional search query that produced the GIF.'),
	locale: LocaleType,
});

export type GifRegisterShareRequest = z.infer<typeof GifRegisterShareRequest>;

export const GifMediaFormat = z.object({
	src: z.string().describe('Direct URL to this format of the GIF media.'),
	proxy_src: z.string().describe('Proxied URL to this format of the GIF media.'),
	width: Int32Type.describe('Width of this format in pixels.'),
	height: Int32Type.describe('Height of this format in pixels.'),
});

export type GifMediaFormat = z.infer<typeof GifMediaFormat>;

export const GifResponse = z.object({
	id: z.string().describe('Provider-stable identifier for this GIF.'),
	slug: z.string().describe('Canonical slug (or slug-id token) used to share / re-resolve this GIF.'),
	provider: GifProviderName.describe('Name of the provider that produced this GIF.'),
	title: z.string().describe('Title or description of the GIF.'),
	url: z.string().describe('Provider page URL for the GIF.'),
	src: z.string().describe('Direct URL to the GIF media file (best format chosen by the server).'),
	proxy_src: z.string().describe('Proxied URL to the GIF media file (best format chosen by the server).'),
	width: Int32Type.describe('Width of the GIF in pixels (best format).'),
	height: Int32Type.describe('Height of the GIF in pixels (best format).'),
	media: z
		.record(z.string(), GifMediaFormat)
		.describe(
			'Map of format-name → media descriptor. Keys are provider-issued format names (e.g. "webm", "mp4", "webp", "gif", "tinygif", "nanogif"). Clients without webm support should pick "webp" / "gif" / "tinygif" / "nanogif" in that order.',
		),
	placeholder: z
		.string()
		.nullish()
		.describe(
			'Compact thumbhash placeholder produced by the media proxy. Clients render it as a low-res preview while the GIF loads, and persist it on favourites so the picker has a fallback if the source URL later disappears.',
		),
});

export type GifResponse = z.infer<typeof GifResponse>;

export const GifCategoryTagResponse = z.object({
	name: z.string().describe('Category search term (locale-translated label suitable for display).'),
	src: z.string().describe('Category preview image URL from the top GIF for this category search term.'),
	proxy_src: z.string().describe('Proxied category preview image URL from the top GIF for this category search term.'),
	gif: GifResponse.nullable().describe(
		'Enriched category preview GIF from the top search result for this category. Null only when no preview GIF was available.',
	),
});

export type GifCategoryTagResponse = z.infer<typeof GifCategoryTagResponse>;

export const GifFeaturedResponse = z.object({
	gifs: z.array(GifResponse).describe('Array of featured GIFs.'),
	categories: z.array(GifCategoryTagResponse).describe('Array of GIF categories.'),
});

export type GifFeaturedResponse = z.infer<typeof GifFeaturedResponse>;
