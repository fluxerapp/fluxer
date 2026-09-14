// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAttachmentID, createChannelID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {purgeMessageAttachments} from '@app/api/channel/services/message/MessageHelpers';
import type {MessageEmbed} from '@app/api/database/types/MessageTypes';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Message} from '@app/api/models/Message';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

const CHANNEL_ID = createChannelID(10n);
const ATTACHMENT_KEY = 'attachments/10/200/ação.png';
const OTHER_MESSAGE_ATTACHMENT_KEY = 'attachments/11/300/photo.jpg';

function imageEmbed(key: string): MessageEmbed {
	return {
		type: 'image',
		title: null,
		description: null,
		url: null,
		timestamp: null,
		color: null,
		author: null,
		provider: null,
		thumbnail: null,
		image: {
			url: `${Config.endpoints.media}/${key}`,
			width: 16,
			height: 16,
			description: null,
			content_type: 'image/png',
			content_hash: null,
			placeholder: null,
			flags: 0,
			duration: null,
		},
		video: null,
		footer: null,
		fields: null,
		nsfw: null,
	};
}

function makeMessageWithMedia(): Message {
	return new Message({
		channel_id: CHANNEL_ID,
		bucket: 0,
		message_id: createMessageID(100n),
		author_id: createUserID(3n),
		type: MessageTypes.DEFAULT,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content: '',
		edited_timestamp: null,
		pinned_timestamp: null,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: [
			{
				attachment_id: createAttachmentID(200n),
				filename: 'ação.png',
				size: 1024n,
				title: null,
				description: null,
				width: 16,
				height: 16,
				content_type: 'image/png',
				content_hash: null,
				placeholder: null,
				flags: 0,
				duration: null,
				nsfw: null,
				waveform: null,
			},
		],
		embeds: [imageEmbed(ATTACHMENT_KEY), imageEmbed(OTHER_MESSAGE_ATTACHMENT_KEY)],
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		has_reaction: null,
		version: 1,
	});
}

describe('purgeMessageAttachments', () => {
	it('queues each stored media URL of a deleted message once and leaves media of other messages alone', async () => {
		const deletedObjects: Array<string> = [];
		const queuedUrls: Array<string> = [];
		const storageService = {
			deleteObject: async (bucket: string, key: string) => {
				deletedObjects.push(`${bucket}/${key}`);
			},
		} as unknown as IStorageService;
		const purgeQueue: IPurgeQueue = {
			addUrls: async (urls) => {
				queuedUrls.push(...urls);
			},
		};

		await purgeMessageAttachments(makeMessageWithMedia(), storageService, purgeQueue);

		expect(deletedObjects).toEqual([`${Config.s3.buckets.cdn}/${ATTACHMENT_KEY}`]);
		expect(queuedUrls).toEqual([`${Config.endpoints.media}/${ATTACHMENT_KEY}`]);
	});
});
