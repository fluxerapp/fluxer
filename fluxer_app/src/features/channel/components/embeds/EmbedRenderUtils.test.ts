// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BLUESKY_EMBED_MEDIA_FALLBACK_OUTER_WIDTH,
	BLUESKY_EMBED_MIN_CONTENT_WIDTH,
	calculateBlueskyMediaContainerWidth,
	calculateBlueskyOuterMaxWidth,
	EMBED_TEXT_OUTER_WIDTH,
	formatResponsiveEmbedWidth,
} from '@app/features/channel/components/embeds/EmbedRenderUtils';
import {describe, expect, it} from 'vitest';

describe('formatResponsiveEmbedWidth', () => {
	it('caps in rem so the width tracks the root font size at any zoom', () => {
		expect(formatResponsiveEmbedWidth(EMBED_TEXT_OUTER_WIDTH)).toBe('min(100%, 32.25rem)');
		expect(formatResponsiveEmbedWidth(BLUESKY_EMBED_MEDIA_FALLBACK_OUTER_WIDTH)).toBe('min(100%, 27rem)');
	});

	it('rounds to whole pixels before converting', () => {
		expect(formatResponsiveEmbedWidth(400.4)).toBe('min(100%, 25rem)');
	});

	it('clamps non-positive and non-finite widths', () => {
		expect(formatResponsiveEmbedWidth(0)).toBe('min(100%, 0rem)');
		expect(formatResponsiveEmbedWidth(Number.NaN)).toBe('min(100%, 0rem)');
	});
});

describe('calculateBlueskyOuterMaxWidth', () => {
	it('falls back to the shared text width when there is no media', () => {
		expect(calculateBlueskyOuterMaxWidth({hasMedia: false, chromeWidth: 29})).toBe(EMBED_TEXT_OUTER_WIDTH);
	});

	it('adds chrome around the media container width', () => {
		expect(calculateBlueskyOuterMaxWidth({mediaWidth: 480, hasMedia: true, chromeWidth: 29})).toBe(509);
	});

	it('never goes below the minimum content width', () => {
		expect(calculateBlueskyMediaContainerWidth(120)).toBe(BLUESKY_EMBED_MIN_CONTENT_WIDTH);
	});
});
