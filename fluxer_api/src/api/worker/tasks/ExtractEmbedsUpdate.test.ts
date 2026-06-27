// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageEmbedTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';
import {createChannelID, createMessageID, createUserID} from '../../BrandedTypes';
import type {ChannelRepository} from '../../channel/ChannelRepository';
import type {MessageEmbed} from '../../database/types/MessageTypes';
import {Message} from '../../models/Message';
import {updateMessageEmbeds} from './ExtractEmbeds';

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

function makeMessage(embeds: Array<MessageEmbed> | null): Message {
	return new Message({
		channel_id: createChannelID(1n),
		bucket: 0,
		message_id: createMessageID(2n),
		author_id: createUserID(3n),
		type: MessageTypes.DEFAULT,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content: 'see https://example.com',
		edited_timestamp: null,
		pinned_timestamp: null,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: null,
		embeds,
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		nsfw_emojis: null,
		has_reaction: null,
		version: 1,
	});
}

// Captures the Message handed to channelRepository.updateEmbeds so the test can
// assert against the row the real worker would persist.
function capturingChannelRepository(): {repo: ChannelRepository; written: () => Message | null} {
	let writtenMessage: Message | null = null;
	const repo = {
		async updateEmbeds(message: Message) {
			writtenMessage = message;
		},
	} as never as ChannelRepository;
	return {repo, written: () => writtenMessage};
}

describe('ExtractEmbeds.updateMessageEmbeds', () => {
	it('does not drop an existing rich embed when the deferred unfurl yields only URL embeds', async () => {
		const freshMessage = makeMessage([richEmbed('kept')]);
		const orderedEmbeds = [urlEmbed('https://example.com/')];
		const {repo, written} = capturingChannelRepository();

		const result = await updateMessageEmbeds(repo, freshMessage, orderedEmbeds);

		expect(result).not.toBeNull();
		const writtenMessage = written();
		expect(writtenMessage).not.toBeNull();
		const writtenTypes = (writtenMessage?.embeds ?? []).map((e) => e.type);
		// The rich embed the author set must survive the async URL unfurl, and
		// must come before the freshly-unfurled URL embeds (rich-first).
		expect(writtenTypes).toContain(MessageEmbedTypes.RICH);
		expect(writtenTypes).toContain(MessageEmbedTypes.LINK);
		expect(writtenTypes.indexOf(MessageEmbedTypes.RICH)).toBeLessThan(writtenTypes.indexOf(MessageEmbedTypes.LINK));
		const richTitle = (writtenMessage?.embeds ?? []).find((e) => e.type === MessageEmbedTypes.RICH)?.title;
		expect(richTitle).toBe('kept');
	});

	it('skips the write when the merged embeds equal the existing embeds', async () => {
		const freshMessage = makeMessage([richEmbed('kept')]);
		const {repo, written} = capturingChannelRepository();

		const result = await updateMessageEmbeds(repo, freshMessage, []);

		// merged = [rich] === existing [rich] -> equivalence short-circuit, no write.
		expect(result).toBe(freshMessage);
		expect(written()).toBeNull();
	});
});
