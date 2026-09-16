// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildForwardMessagePreviewThumbnailURL,
	type ForwardMessagePreviewMessage,
	type ForwardMessagePreviewThumbnail,
	resolveForwardMessagePreviewContent,
} from '@app/features/messaging/components/modals/ForwardMessagePreviewContent';
import {EmbedMediaFlags, MessageAttachmentFlags, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageEmbed} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {MessageAttachment, MessageSnapshot} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {describe, expect, it} from 'vitest';

function attachment(id: string, overrides: Partial<MessageAttachment> = {}): MessageAttachment {
	return {
		id,
		filename: `${id}.bin`,
		size: 1024,
		url: `https://cdn.example.com/attachments/${id}`,
		proxy_url: `https://media.example.com/attachments/${id}`,
		flags: 0,
		...overrides,
	};
}

function image(id: string, overrides: Partial<MessageAttachment> = {}): MessageAttachment {
	return attachment(id, {content_type: 'image/png', width: 800, height: 600, ...overrides});
}

function video(id: string, overrides: Partial<MessageAttachment> = {}): MessageAttachment {
	return attachment(id, {content_type: 'video/mp4', width: 1920, height: 1080, ...overrides});
}

function file(id: string): MessageAttachment {
	return attachment(id, {content_type: 'application/pdf'});
}

function embed(overrides: Partial<MessageEmbed> = {}): MessageEmbed {
	return {type: 'link', ...overrides};
}

function snapshot(overrides: Partial<MessageSnapshot> = {}): MessageSnapshot {
	return {type: MessageTypes.DEFAULT, content: '', timestamp: '2026-09-01T12:00:00.000Z', ...overrides};
}

function message(overrides: Partial<ForwardMessagePreviewMessage> = {}): ForwardMessagePreviewMessage {
	return {
		attachments: [],
		content: '',
		editedTimestamp: null,
		embeds: [],
		mentionChannels: [],
		messageSnapshots: undefined,
		...overrides,
	};
}

function thumbnail(overrides: Partial<ForwardMessagePreviewThumbnail> = {}): ForwardMessagePreviewThumbnail {
	return {
		proxyUrl: 'https://media.example.com/attachments/a',
		isVideo: false,
		showsPlayIcon: false,
		showsFilePlaceholder: false,
		isSensitive: false,
		width: null,
		height: null,
		...overrides,
	};
}

function searchParams(url: string): Record<string, string> {
	return Object.fromEntries(new URL(url).searchParams.entries());
}

describe('forward message preview content', () => {
	it('previews plain message text with no attachment row or thumbnail', () => {
		const preview = resolveForwardMessagePreviewContent({
			message: message({content: 'Lunch at noon?'}),
			mediaSelection: undefined,
			canEmbedLinks: true,
		});
		expect(preview.text).toBe('Lunch at noon?');
		expect(preview.isEdited).toBe(false);
		expect(preview.attachmentSummary).toBeNull();
		expect(preview.thumbnail).toBeNull();
		expect(preview.overflowCount).toBe(0);
	});

	it('previews the first snapshot of a forwarded message', () => {
		const preview = resolveForwardMessagePreviewContent({
			message: message({
				messageSnapshots: [
					snapshot({content: 'Original words', attachments: [image('a')]}),
					snapshot({content: 'Second snapshot'}),
				],
			}),
			mediaSelection: undefined,
			canEmbedLinks: true,
		});
		expect(preview.text).toBe('Original words');
		expect(preview.attachmentSummary).toEqual({kind: 'images', count: 1});
		expect(preview.thumbnail?.proxyUrl).toBe('https://media.example.com/attachments/a');
	});

	it('marks the preview edited only when the forwarded message itself has text it edited', () => {
		const editedMessage = resolveForwardMessagePreviewContent({
			message: message({content: 'Fixed typo', editedTimestamp: new Date('2026-09-02T08:00:00.000Z')}),
			mediaSelection: undefined,
			canEmbedLinks: true,
		});
		expect(editedMessage.isEdited).toBe(true);
		const editedWithoutContent = resolveForwardMessagePreviewContent({
			message: message({
				editedTimestamp: new Date('2026-09-02T08:00:00.000Z'),
				embeds: [embed({url: 'https://example.com/a'})],
			}),
			mediaSelection: undefined,
			canEmbedLinks: true,
		});
		expect(editedWithoutContent.isEdited).toBe(false);
		const forwardOfEditedMessage = resolveForwardMessagePreviewContent({
			message: message({
				messageSnapshots: [snapshot({content: 'Edited', edited_timestamp: '2026-09-02T08:00:00.000Z'})],
			}),
			mediaSelection: undefined,
			canEmbedLinks: true,
		});
		expect(forwardOfEditedMessage.isEdited).toBe(false);
	});

	it('keeps only the selected attachments and hides the text for an attachment selection', () => {
		const preview = resolveForwardMessagePreviewContent({
			message: message({
				content: 'Holiday photos',
				attachments: [image('a'), image('b'), image('c')],
				embeds: [
					embed({
						url: 'https://example.com/trip',
						thumbnail: {url: 'x', proxy_url: 'https://media.example.com/x', flags: 0},
					}),
				],
			}),
			mediaSelection: {attachmentIds: ['b']},
			canEmbedLinks: true,
		});
		expect(preview.text).toBeNull();
		expect(preview.attachmentSummary).toEqual({kind: 'images', count: 1});
		expect(preview.thumbnail?.proxyUrl).toBe('https://media.example.com/attachments/b');
		expect(preview.overflowCount).toBe(0);
	});

	it('previews the selected embed urls for an embed selection', () => {
		const preview = resolveForwardMessagePreviewContent({
			message: message({
				content: 'Two links',
				attachments: [image('a')],
				embeds: [
					embed({url: 'https://example.com/first'}),
					embed({
						url: 'https://example.com/second',
						thumbnail: {url: 'x', proxy_url: 'https://media.example.com/second-thumb', flags: 0},
					}),
				],
			}),
			mediaSelection: {embedIndices: [1]},
			canEmbedLinks: true,
		});
		expect(preview.text).toBe('https://example.com/second');
		expect(preview.attachmentSummary).toBeNull();
		expect(preview.thumbnail?.proxyUrl).toBe('https://media.example.com/second-thumb');
		expect(preview.overflowCount).toBe(0);
	});

	it('falls back to the embed urls when the source has no text', () => {
		const preview = resolveForwardMessagePreviewContent({
			message: message({embeds: [embed({url: 'https://example.com/a'}), embed({url: 'https://example.com/b'})]}),
			mediaSelection: undefined,
			canEmbedLinks: true,
		});
		expect(preview.text).toBe('https://example.com/a\nhttps://example.com/b');
	});

	it('falls back to the first embed description when no embed has a url', () => {
		const preview = resolveForwardMessagePreviewContent({
			message: message({embeds: [embed({type: 'rich', description: 'Weekly notes'})]}),
			mediaSelection: undefined,
			canEmbedLinks: true,
		});
		expect(preview.text).toBe('Weekly notes');
	});

	it('drops embeds when the destination cannot embed links and the message has content', () => {
		const linkEmbed = embed({
			url: 'https://example.com/a',
			thumbnail: {url: 'x', proxy_url: 'https://media.example.com/a-thumb', flags: 0},
		});
		const withContent = resolveForwardMessagePreviewContent({
			message: message({content: 'See https://example.com/a', embeds: [linkEmbed]}),
			mediaSelection: undefined,
			canEmbedLinks: false,
		});
		expect(withContent.text).toBe('See https://example.com/a');
		expect(withContent.thumbnail).toBeNull();
		const withoutContent = resolveForwardMessagePreviewContent({
			message: message({embeds: [linkEmbed]}),
			mediaSelection: undefined,
			canEmbedLinks: false,
		});
		expect(withoutContent.text).toBe('https://example.com/a');
		expect(withoutContent.thumbnail?.proxyUrl).toBe('https://media.example.com/a-thumb');
	});
});

describe('forward message preview attachment summary', () => {
	function summarize(attachments: ReadonlyArray<MessageAttachment>) {
		return resolveForwardMessagePreviewContent({
			message: message({attachments}),
			mediaSelection: undefined,
			canEmbedLinks: true,
		}).attachmentSummary;
	}

	it('counts images and videos together when both are present', () => {
		expect(summarize([image('a'), video('b'), image('c'), file('d')])).toEqual({
			kind: 'images_and_videos',
			imageCount: 2,
			videoCount: 1,
		});
	});

	it('counts only the videos when there are no images', () => {
		expect(summarize([video('a'), file('b')])).toEqual({kind: 'videos', count: 1});
	});

	it('counts only the images when there are no videos', () => {
		expect(summarize([image('a'), image('b'), file('c')])).toEqual({kind: 'images', count: 2});
	});

	it('counts every attachment as a file when none is a displayable image or video', () => {
		expect(summarize([file('a'), file('b'), image('c', {width: undefined, height: undefined})])).toEqual({
			kind: 'files',
			count: 3,
		});
	});
});

describe('forward message preview thumbnail', () => {
	function preview(attachments: ReadonlyArray<MessageAttachment>, embeds: ReadonlyArray<MessageEmbed> = []) {
		return resolveForwardMessagePreviewContent({
			message: message({attachments, embeds}),
			mediaSelection: undefined,
			canEmbedLinks: true,
		});
	}

	it('shows a playable still frame when every attachment is a video', () => {
		const result = preview([video('a'), video('b')]);
		expect(result.thumbnail).toMatchObject({
			proxyUrl: 'https://media.example.com/attachments/a',
			isVideo: true,
			showsPlayIcon: true,
			width: 1920,
			height: 1080,
		});
		expect(result.overflowCount).toBe(1);
	});

	it('shows the first attachment without a play icon when the attachments are mixed', () => {
		const result = preview([video('a'), image('b'), file('c')]);
		expect(result.thumbnail).toMatchObject({isVideo: true, showsPlayIcon: false});
		expect(result.overflowCount).toBe(2);
	});

	it('keeps the box and the overflow badge when the first attachment is not displayable media', () => {
		const result = preview([file('a'), image('b')]);
		expect(result.thumbnail).toMatchObject({
			proxyUrl: 'https://media.example.com/attachments/a',
			showsFilePlaceholder: true,
			isVideo: false,
			showsPlayIcon: false,
		});
		expect(result.overflowCount).toBe(1);
		const unsized = preview([image('a', {width: undefined, height: undefined}), image('b')]);
		expect(unsized.thumbnail).toMatchObject({showsFilePlaceholder: true});
		expect(unsized.overflowCount).toBe(1);
	});

	it('uses only the first embed thumbnail when there are no attachments', () => {
		const result = preview(
			[],
			[
				embed({url: 'https://example.com/a'}),
				embed({
					url: 'https://example.com/b',
					thumbnail: {url: 'x', proxy_url: 'https://media.example.com/b', flags: 0},
				}),
			],
		);
		expect(result.thumbnail).toBeNull();
		const withThumbnail = preview(
			[],
			[
				embed({
					url: 'https://example.com/a',
					thumbnail: {url: 'x', proxy_url: 'https://media.example.com/a', flags: 0},
				}),
			],
		);
		expect(withThumbnail.thumbnail).toMatchObject({proxyUrl: 'https://media.example.com/a', isVideo: false});
	});

	it('marks spoiler and explicit media as sensitive', () => {
		expect(preview([image('a', {flags: MessageAttachmentFlags.IS_SPOILER})]).thumbnail?.isSensitive).toBe(true);
		expect(preview([image('a', {flags: MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA})]).thumbnail?.isSensitive).toBe(
			true,
		);
		expect(preview([image('a', {nsfw: true})]).thumbnail?.isSensitive).toBe(true);
		expect(preview([image('a')]).thumbnail?.isSensitive).toBe(false);
		const explicitEmbed = embed({
			thumbnail: {url: 'x', proxy_url: 'https://media.example.com/x', flags: EmbedMediaFlags.CONTAINS_EXPLICIT_MEDIA},
		});
		expect(preview([], [explicitEmbed]).thumbnail?.isSensitive).toBe(true);
	});
});

describe('forward message preview thumbnail url', () => {
	it('requests a square webp crop for images', () => {
		const url = buildForwardMessagePreviewThumbnailURL(thumbnail());
		expect(searchParams(url)).toEqual({format: 'webp', width: '56', height: '56'});
	});

	it('requests a still frame wide enough to cover the square for landscape video', () => {
		const url = buildForwardMessagePreviewThumbnailURL(thumbnail({isVideo: true, width: 1920, height: 1080}));
		expect(searchParams(url)).toEqual({format: 'webp', width: '100'});
	});

	it('requests a still frame at the square width for portrait or unsized video', () => {
		const portrait = buildForwardMessagePreviewThumbnailURL(thumbnail({isVideo: true, width: 1080, height: 1920}));
		expect(searchParams(portrait)).toEqual({format: 'webp', width: '56'});
		const unsized = buildForwardMessagePreviewThumbnailURL(thumbnail({isVideo: true}));
		expect(searchParams(unsized)).toEqual({format: 'webp', width: '56'});
	});

	it('replaces media params already on the proxy url and keeps the rest', () => {
		const url = buildForwardMessagePreviewThumbnailURL(
			thumbnail({proxyUrl: 'https://media.example.com/attachments/a?ex=abc&width=10&format=png&animated=true'}),
		);
		expect(searchParams(url)).toEqual({ex: 'abc', format: 'webp', width: '56', height: '56'});
	});
});
