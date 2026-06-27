// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageEmbedTypes} from '@fluxer/constants/src/ChannelConstants';
import {MAX_EMBEDS_PER_MESSAGE} from '@fluxer/constants/src/LimitConstants';
import {describe, expect, it} from 'vitest';
import type {MessageEmbed} from '../../../database/types/MessageTypes';
import {mergeRichFirst} from './MessageEmbedMerge';

function richEmbed(title: string): MessageEmbed {
	return {
		type: MessageEmbedTypes.RICH,
		title,
		description: null,
		url: null,
		timestamp: null,
		color: null,
		author: null,
		provider: null,
		thumbnail: null,
		image: null,
		video: null,
		audio: null,
		footer: null,
		fields: null,
		html: null,
		html_width: null,
		html_height: null,
		nsfw: null,
		children: null,
	};
}

function urlEmbed(url: string): MessageEmbed {
	return {
		type: MessageEmbedTypes.LINK,
		title: null,
		description: null,
		url,
		timestamp: null,
		color: null,
		author: null,
		provider: null,
		thumbnail: null,
		image: null,
		video: null,
		audio: null,
		footer: null,
		fields: null,
		html: null,
		html_width: null,
		html_height: null,
		nsfw: null,
		children: null,
	};
}

describe('mergeRichFirst', () => {
	it('places preserved rich embeds first, then the derived set', () => {
		const merged = mergeRichFirst([richEmbed('kept')], [urlEmbed('https://example.com/')]);
		expect(merged.map((e) => e.type)).toEqual([MessageEmbedTypes.RICH, MessageEmbedTypes.LINK]);
		expect(merged[0]?.title).toBe('kept');
	});

	it('drops non-rich embeds from the existing set (only rich embeds are preserved)', () => {
		const staleUrl = urlEmbed('https://old.example.com/');
		const merged = mergeRichFirst([richEmbed('kept'), staleUrl], [urlEmbed('https://new.example.com/')]);
		expect(merged.map((e) => e.url)).toEqual([null, 'https://new.example.com/']);
	});

	it('does not exceed MAX_EMBEDS_PER_MESSAGE when rich + derived overflow the total cap', () => {
		// One author rich embed + a derived set already at the cap. Without a
		// re-cap the merge would persist/broadcast MAX_EMBEDS_PER_MESSAGE + 1.
		const rich = [richEmbed('author')];
		const derived = Array.from({length: MAX_EMBEDS_PER_MESSAGE}, (_, i) => urlEmbed(`https://example.com/${i}`));
		const merged = mergeRichFirst(rich, derived);
		expect(merged.length).toBe(MAX_EMBEDS_PER_MESSAGE);
	});

	it('never drops the author rich embed when truncating to the cap', () => {
		const rich = [richEmbed('author')];
		const derived = Array.from({length: MAX_EMBEDS_PER_MESSAGE}, (_, i) => urlEmbed(`https://example.com/${i}`));
		const merged = mergeRichFirst(rich, derived);
		// The author rich embed survives rich-first; only the derived tail is cut.
		expect(merged[0]?.type).toBe(MessageEmbedTypes.RICH);
		expect(merged[0]?.title).toBe('author');
		expect(merged.filter((e) => e.type === MessageEmbedTypes.RICH)).toHaveLength(1);
	});

	it('keeps every author rich embed even when rich alone exceeds the cap, never truncating author content', () => {
		// Pathological: more rich embeds than the cap. We refuse to drop author
		// content, so all rich are kept and no derived is appended.
		const rich = Array.from({length: MAX_EMBEDS_PER_MESSAGE + 2}, (_, i) => richEmbed(`author-${i}`));
		const derived = [urlEmbed('https://example.com/')];
		const merged = mergeRichFirst(rich, derived);
		expect(merged.length).toBe(MAX_EMBEDS_PER_MESSAGE + 2);
		expect(merged.every((e) => e.type === MessageEmbedTypes.RICH)).toBe(true);
	});
});
