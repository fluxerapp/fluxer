/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {TenorResolver} from '@fluxer/api/src/unfurler/resolvers/TenorResolver';
import {createMockContent, MockMediaService} from '@fluxer/api/src/unfurler/tests/ResolverTestUtils';
import {EmbedMediaFlags} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

function createTenorHtml(options: {thumbnailUrl?: string; videoUrl?: string; ogImage?: string}): string {
	const jsonLd: Record<string, unknown> = {};

	if (options.thumbnailUrl) {
		jsonLd.image = {thumbnailUrl: options.thumbnailUrl};
	}
	if (options.videoUrl) {
		jsonLd.video = {contentUrl: options.videoUrl};
	}

	const metaTags: Array<string> = [];
	if (options.ogImage) {
		metaTags.push(`<meta property="og:image" content="${options.ogImage}" />`);
	}

	return `<!DOCTYPE html>
<html>
<head>
${metaTags.join('\n')}
<script class="dynamic" type="application/ld+json">
${JSON.stringify(jsonLd)}
</script>
</head>
<body></body>
</html>`;
}

describe('TenorResolver', () => {
	let mediaService: MockMediaService;
	let resolver: TenorResolver;

	beforeEach(() => {
		mediaService = new MockMediaService();
		resolver = new TenorResolver(mediaService);
	});

	afterEach(() => {
		mediaService.reset();
	});

	describe('match', () => {
		it('matches tenor.com URLs with text/html', () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const result = resolver.match(url, 'text/html', createMockContent(''));
			expect(result).toBe(true);
		});

		it('matches tenor.com view URLs', () => {
			const url = new URL('https://tenor.com/view/funny-reaction-gif-67890');
			const result = resolver.match(url, 'text/html', createMockContent(''));
			expect(result).toBe(true);
		});

		it('matches tenor.com URLs with various paths', () => {
			const url = new URL('https://tenor.com/ko/view/excited-gif-123');
			const result = resolver.match(url, 'text/html', createMockContent(''));
			expect(result).toBe(true);
		});

		it('does not match non-tenor domains', () => {
			const url = new URL('https://giphy.com/gifs/cat-12345');
			const result = resolver.match(url, 'text/html', createMockContent(''));
			expect(result).toBe(false);
		});

		it('does not match tenor.com with non-HTML content type', () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const result = resolver.match(url, 'application/json', createMockContent(''));
			expect(result).toBe(false);
		});

		it('does not match tenor subdomains', () => {
			const url = new URL('https://media.tenor.com/something.gif');
			const result = resolver.match(url, 'text/html', createMockContent(''));
			expect(result).toBe(false);
		});

		it('does not match image mime types even on tenor domain', () => {
			const url = new URL('https://tenor.com/image.gif');
			const result = resolver.match(url, 'image/gif', createMockContent(''));
			expect(result).toBe(false);
		});
	});

	describe('resolve', () => {
		it('prefers og:image GIF URL over JSON-LD video', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = createTenorHtml({
				ogImage: 'https://media.tenor.com/cat-gif-AAAAC/cat.gif',
				thumbnailUrl: 'https://media.tenor.com/thumbnail.png',
				videoUrl: 'https://media.tenor.com/video.mp4',
			});

			mediaService.setMetadata('https://media.tenor.com/cat-gif-AAAAC/cat.gif', {
				content_type: 'image/gif',
				animated: true,
				width: 320,
				height: 240,
			});

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(1);
			expect(embeds[0]!.type).toBe('gifv');
			expect(embeds[0]!.url).toBe('https://tenor.com/view/cat-gif-12345');
			expect(embeds[0]!.provider).toEqual({name: 'Tenor', url: 'https://tenor.com'});
			expect(embeds[0]!.thumbnail).toBeDefined();
			expect(embeds[0]!.thumbnail!.url).toBe('https://media.tenor.com/cat-gif-AAAAC/cat.gif');
			expect(embeds[0]!.thumbnail!.content_type).toBe('image/gif');
			expect(embeds[0]!.thumbnail!.flags).toBe(EmbedMediaFlags.IS_ANIMATED);
			expect(embeds[0]!.video).toBeUndefined();
		});

		it('falls back to JSON-LD when og:image is not a GIF', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = createTenorHtml({
				ogImage: 'https://media.tenor.com/thumbnail.png',
				thumbnailUrl: 'https://media.tenor.com/thumbnail.png',
				videoUrl: 'https://media.tenor.com/video.mp4',
			});

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(1);
			expect(embeds[0]!.type).toBe('gifv');
			expect(embeds[0]!.thumbnail).toBeDefined();
			expect(embeds[0]!.video).toBeDefined();
		});

		it('falls back to JSON-LD when og:image is absent', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = createTenorHtml({
				thumbnailUrl: 'https://media.tenor.com/thumbnail.png',
				videoUrl: 'https://media.tenor.com/video.mp4',
			});

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(1);
			expect(embeds[0]!.type).toBe('gifv');
			expect(embeds[0]!.thumbnail).toBeDefined();
			expect(embeds[0]!.video).toBeDefined();
		});

		it('falls back to JSON-LD when og:image GIF fails to resolve', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = createTenorHtml({
				ogImage: 'https://media.tenor.com/broken.gif',
				thumbnailUrl: 'https://media.tenor.com/thumbnail.png',
				videoUrl: 'https://media.tenor.com/video.mp4',
			});

			mediaService.markAsFailing('https://media.tenor.com/broken.gif');

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(1);
			expect(embeds[0]!.type).toBe('gifv');
			expect(embeds[0]!.thumbnail).toBeDefined();
			expect(embeds[0]!.video).toBeDefined();
		});

		it('handles tenor page with only thumbnail in JSON-LD', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = createTenorHtml({
				thumbnailUrl: 'https://media.tenor.com/thumbnail.png',
			});

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(1);
			expect(embeds[0]!.type).toBe('gifv');
			expect(embeds[0]!.thumbnail).toBeDefined();
		});

		it('handles tenor page with only video in JSON-LD', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = createTenorHtml({
				videoUrl: 'https://media.tenor.com/video.mp4',
			});

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(1);
			expect(embeds[0]!.type).toBe('gifv');
			expect(embeds[0]!.video).toBeDefined();
		});

		it('returns empty array when no JSON-LD found and no og:image', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = '<!DOCTYPE html><html><head></head><body></body></html>';

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(0);
		});

		it('returns empty array when JSON-LD is empty and no og:image', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = `<!DOCTYPE html>
<html>
<head>
<script class="dynamic" type="application/ld+json">
{}
</script>
</head>
<body></body>
</html>`;

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(1);
		});

		it('returns empty array for invalid JSON-LD and no og:image', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = `<!DOCTYPE html>
<html>
<head>
<script class="dynamic" type="application/ld+json">
{invalid json}
</script>
</head>
<body></body>
</html>`;

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(0);
		});

		it('handles NSFW content with og:image GIF', async () => {
			const url = new URL('https://tenor.com/view/adult-gif-12345');
			const html = createTenorHtml({
				ogImage: 'https://media.tenor.com/nsfw.gif',
			});

			mediaService.markAsNsfw('https://media.tenor.com/nsfw.gif');
			mediaService.setMetadata('https://media.tenor.com/nsfw.gif', {
				content_type: 'image/gif',
				animated: true,
			});

			const embeds = await resolver.resolve(url, createMockContent(html), true);

			expect(embeds).toHaveLength(1);
			expect(embeds[0]!.thumbnail!.flags).toBe(EmbedMediaFlags.IS_ANIMATED | EmbedMediaFlags.CONTAINS_EXPLICIT_MEDIA);
		});

		it('preserves URL in embed output', async () => {
			const url = new URL('https://tenor.com/view/special-chars-gif%20test-12345');
			const html = createTenorHtml({
				ogImage: 'https://media.tenor.com/test.gif',
			});

			mediaService.setMetadata('https://media.tenor.com/test.gif', {
				content_type: 'image/gif',
				animated: true,
			});

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds[0]!.url).toBe('https://tenor.com/view/special-chars-gif%20test-12345');
		});

		it('handles missing dynamic class on script tag with og:image', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = `<!DOCTYPE html>
<html>
<head>
<meta property="og:image" content="https://media.tenor.com/cat.gif" />
<script type="application/ld+json">
{"image": {"thumbnailUrl": "https://media.tenor.com/thumbnail.png"}}
</script>
</head>
<body></body>
</html>`;

			mediaService.setMetadata('https://media.tenor.com/cat.gif', {
				content_type: 'image/gif',
				animated: true,
			});

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(1);
			expect(embeds[0]!.thumbnail!.url).toBe('https://media.tenor.com/cat.gif');
			expect(embeds[0]!.video).toBeUndefined();
		});

		it('handles og:image with uppercase .GIF extension', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = createTenorHtml({
				ogImage: 'https://media.tenor.com/cat.GIF',
			});

			mediaService.setMetadata('https://media.tenor.com/cat.GIF', {
				content_type: 'image/gif',
				animated: true,
			});

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(1);
			expect(embeds[0]!.thumbnail!.url).toBe('https://media.tenor.com/cat.GIF');
			expect(embeds[0]!.video).toBeUndefined();
		});

		it('resolves og:image GIF with only og:image present (no JSON-LD)', async () => {
			const url = new URL('https://tenor.com/view/cat-gif-12345');
			const html = `<!DOCTYPE html>
<html>
<head>
<meta property="og:image" content="https://media.tenor.com/cat.gif" />
</head>
<body></body>
</html>`;

			mediaService.setMetadata('https://media.tenor.com/cat.gif', {
				content_type: 'image/gif',
				animated: true,
				width: 500,
				height: 400,
			});

			const embeds = await resolver.resolve(url, createMockContent(html));

			expect(embeds).toHaveLength(1);
			expect(embeds[0]!.type).toBe('gifv');
			expect(embeds[0]!.thumbnail).toBeDefined();
			expect(embeds[0]!.thumbnail!.url).toBe('https://media.tenor.com/cat.gif');
			expect(embeds[0]!.thumbnail!.width).toBe(500);
			expect(embeds[0]!.thumbnail!.height).toBe(400);
			expect(embeds[0]!.video).toBeUndefined();
		});
	});
});
