// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {createAttachmentID, createChannelID, createMessageID, createUserID} from '../../../BrandedTypes';
import type {MessageRow} from '../../../database/types/MessageTypes';
import {Message} from '../../../models/Message';
import {convertToSearchableMessage} from '../MessageSearchSerializer';

describe('MessageSearchSerializer', () => {
	it('extracts unique hosts and attachment metadata when indexing a message', () => {
		const row: MessageRow = {
			channel_id: createChannelID(111n),
			bucket: 0,
			message_id: createMessageID(222n),
			author_id: createUserID(333n),
			type: 0,
			webhook_id: null,
			webhook_name: null,
			webhook_avatar_hash: null,
			content: 'Check https://example.com/one and <https://example.com>',
			edited_timestamp: null,
			pinned_timestamp: null,
			flags: 0,
			mention_everyone: false,
			mention_users: new Set([createUserID(444n)]),
			mention_roles: new Set(),
			mention_channels: new Set(),
			attachments: [
				{
					attachment_id: createAttachmentID(555n),
					filename: 'screenshot.png',
					size: 1024n,
					title: null,
					description: null,
					width: null,
					height: null,
					content_type: 'image/png',
					content_hash: null,
					placeholder: null,
					flags: 0,
					duration: null,
					nsfw: null,
					waveform: null,
				},
				{
					attachment_id: createAttachmentID(556n),
					filename: 'screenshot.png',
					size: 2048n,
					title: null,
					description: null,
					width: null,
					height: null,
					content_type: 'image/png',
					content_hash: null,
					placeholder: null,
					flags: 0,
					duration: null,
					nsfw: null,
					waveform: null,
				},
			],
			embeds: [
				{
					type: 'image',
					title: null,
					description: null,
					url: 'https://embed.example.net/image',
					timestamp: null,
					color: null,
					author: null,
					provider: {
						name: 'EmbedCo',
						url: null,
					},
					thumbnail: null,
					image: null,
					video: null,
					footer: null,
					fields: null,
					nsfw: null,
				},
			],
			sticker_items: [],
			message_reference: null,
			message_snapshots: [],
			call: null,
			nsfw_emojis: null,
			has_reaction: null,
			version: 1,
		};
		const message = new Message(row);
		const result = convertToSearchableMessage(message, true);
		expect(result.linkHostnames).toEqual(['example.com', 'embed.example.net']);
		expect(result.attachmentFilenames).toEqual(['screenshot.png']);
		expect(result.attachmentExtensions).toEqual(['png']);
		expect(result.embedProviders).toEqual(['EmbedCo']);
		expect(result.hasLink).toBe(true);
		expect(result.hasEmbed).toBe(true);
		expect(result.authorType).toBe('bot');
		expect(result.mentionedUserIds).toEqual([createUserID(444n).toString()]);
	});
	it('merges forwarded snapshot content into the host document so combined has: filters match', () => {
		const row: MessageRow = {
			channel_id: createChannelID(111n),
			bucket: 0,
			message_id: createMessageID(223n),
			author_id: createUserID(333n),
			type: 0,
			webhook_id: null,
			webhook_name: null,
			webhook_avatar_hash: null,
			content: null,
			edited_timestamp: null,
			pinned_timestamp: null,
			flags: 0,
			mention_everyone: false,
			mention_users: new Set(),
			mention_roles: new Set(),
			mention_channels: new Set(),
			attachments: [],
			embeds: [],
			sticker_items: [],
			message_reference: {
				channel_id: createChannelID(999n),
				message_id: createMessageID(998n),
				guild_id: null,
				type: 1,
			},
			message_snapshots: [
				{
					content: 'forwarded note linking https://snap.example.com/path',
					timestamp: new Date(0),
					edited_timestamp: null,
					mention_users: new Set([createUserID(777n)]),
					mention_roles: null,
					mention_channels: null,
					attachments: [
						{
							attachment_id: createAttachmentID(901n),
							filename: 'forwarded.png',
							size: 512n,
							title: null,
							description: null,
							width: null,
							height: null,
							content_type: 'image/png',
							content_hash: null,
							placeholder: null,
							flags: 0,
							duration: null,
							nsfw: null,
							waveform: null,
						},
					],
					embeds: [
						{
							type: 'video',
							title: null,
							description: null,
							url: 'https://embed.snap.example/video',
							timestamp: null,
							color: null,
							author: null,
							provider: {name: 'SnapCo', url: null},
							thumbnail: null,
							image: null,
							video: null,
							footer: null,
							fields: null,
							nsfw: null,
						},
					],
					sticker_items: [],
					type: 0,
					flags: 0,
				},
			],
			call: null,
			nsfw_emojis: null,
			has_reaction: null,
			version: 1,
		};
		const message = new Message(row);
		const result = convertToSearchableMessage(message);
		expect(result.hasForward).toBe(true);
		expect(result.hasImage).toBe(true);
		expect(result.hasFile).toBe(true);
		expect(result.hasEmbed).toBe(true);
		expect(result.hasLink).toBe(true);
		expect(result.content).toBe('forwarded note linking https://snap.example.com/path');
		expect(result.attachmentFilenames).toEqual(['forwarded.png']);
		expect(result.attachmentExtensions).toEqual(['png']);
		expect(result.embedTypes).toEqual(['video']);
		expect(result.embedProviders).toEqual(['SnapCo']);
		expect(result.linkHostnames).toEqual(['snap.example.com', 'embed.snap.example']);
		expect(result.mentionedUserIds).toEqual([createUserID(777n).toString()]);
	});
	it('does not merge snapshots when the message is not a forward', () => {
		const row: MessageRow = {
			channel_id: createChannelID(111n),
			bucket: 0,
			message_id: createMessageID(224n),
			author_id: createUserID(333n),
			type: 0,
			webhook_id: null,
			webhook_name: null,
			webhook_avatar_hash: null,
			content: null,
			edited_timestamp: null,
			pinned_timestamp: null,
			flags: 0,
			mention_everyone: false,
			mention_users: new Set(),
			mention_roles: new Set(),
			mention_channels: new Set(),
			attachments: [],
			embeds: [],
			sticker_items: [],
			message_reference: {
				channel_id: createChannelID(999n),
				message_id: createMessageID(998n),
				guild_id: null,
				type: 0,
			},
			message_snapshots: [
				{
					content: 'should be ignored',
					timestamp: new Date(0),
					edited_timestamp: null,
					mention_users: null,
					mention_roles: null,
					mention_channels: null,
					attachments: [
						{
							attachment_id: createAttachmentID(902n),
							filename: 'ignored.png',
							size: 512n,
							title: null,
							description: null,
							width: null,
							height: null,
							content_type: 'image/png',
							content_hash: null,
							placeholder: null,
							flags: 0,
							duration: null,
							nsfw: null,
							waveform: null,
						},
					],
					embeds: null,
					sticker_items: null,
					type: 0,
					flags: 0,
				},
			],
			call: null,
			nsfw_emojis: null,
			has_reaction: null,
			version: 1,
		};
		const message = new Message(row);
		const result = convertToSearchableMessage(message);
		expect(result.hasForward).toBe(false);
		expect(result.hasImage).toBe(false);
		expect(result.hasFile).toBe(false);
		expect(result.content).toBeNull();
		expect(result.attachmentFilenames).toEqual([]);
	});
	it('indexes poll presence and poll text', () => {
		const row: MessageRow = {
			channel_id: createChannelID(111n),
			bucket: 0,
			message_id: createMessageID(225n),
			author_id: createUserID(333n),
			type: 0,
			webhook_id: null,
			webhook_name: null,
			webhook_avatar_hash: null,
			content: null,
			edited_timestamp: null,
			pinned_timestamp: null,
			flags: 0,
			mention_everyone: false,
			mention_users: new Set(),
			mention_roles: new Set(),
			mention_channels: new Set(),
			attachments: [],
			embeds: [],
			sticker_items: [],
			message_reference: null,
			message_snapshots: [],
			call: null,
			poll: {
				poll_id: createMessageID(900n),
				title: 'Where should we ship first?',
				options: [
					{
						option_id: createMessageID(901n),
						text: 'Desktop',
						attachment_id: null,
						vote_count: 0,
					},
					{
						option_id: createMessageID(902n),
						text: 'Mobile',
						attachment_id: null,
						vote_count: 0,
					},
				],
				expires_at: new Date(Date.now() + 60_000),
				closed_at: null,
				anonymous: false,
				allow_ranked_choice: false,
				allow_custom_answers: false,
			},
			nsfw_emojis: null,
			has_reaction: null,
			version: 1,
		};
		const message = new Message(row);
		const result = convertToSearchableMessage(message);
		expect(result.hasPoll).toBe(true);
		expect(result.content).toBe('Where should we ship first?\nDesktop\nMobile');
	});
});
