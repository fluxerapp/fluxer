// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {InvalidStreamThumbnailPayloadError} from '@fluxer/errors/src/domains/channel/InvalidStreamThumbnailPayloadError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {beforeEach, describe, expect, it} from 'vitest';
import {createChannelID, createUserID} from '../../BrandedTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ChannelService} from './ChannelService';
import type {StreamPreviewService} from './StreamPreviewService';
import {StreamService} from './StreamService';

const USER_ID = createUserID(7n);
const CHANNEL_ID = createChannelID(12n);
const CONNECTION_ID = 'conn-1';
const STREAM_KEY = `dm:${CHANNEL_ID}:${CONNECTION_ID}`;
const JPEG_BASE64 = Buffer.from(new Uint8Array([0xff, 0xd8, 0x00, 0xff, 0xd9])).toString('base64');

describe('StreamService.uploadPreview', () => {
	let uploaded: Array<{body: Uint8Array}>;
	let streamService: StreamService;

	beforeEach(() => {
		uploaded = [];
		const channelService = {
			channelData: {
				operations: {
					getChannel: async () => ({guildId: null, type: ChannelTypes.DM}),
				},
			},
		} as unknown as ChannelService;
		const gatewayService = {
			getVoiceStatesForChannel: async () => ({
				voiceStates: [
					{
						connectionId: CONNECTION_ID,
						userId: USER_ID.toString(),
						channelId: CHANNEL_ID.toString(),
					},
				],
			}),
		} as unknown as IGatewayService;
		const streamPreviewService = {
			uploadPreview: async (params: {body: Uint8Array}) => {
				uploaded.push(params);
			},
		} as unknown as StreamPreviewService;
		streamService = new StreamService(
			{} as unknown as ICacheService,
			channelService,
			gatewayService,
			streamPreviewService,
		);
	});

	const upload = (thumbnail: string) =>
		streamService.uploadPreview({
			userId: USER_ID,
			streamKey: STREAM_KEY,
			channelId: CHANNEL_ID,
			thumbnail,
			contentType: 'image/jpeg',
		});

	it('rejects a thumbnail that is not base64 at all', async () => {
		await expect(upload('!!!!')).rejects.toBeInstanceOf(InvalidStreamThumbnailPayloadError);
		expect(uploaded).toHaveLength(0);
	});

	it('rejects an unpadded thumbnail instead of decoding it leniently', async () => {
		await expect(upload('YQ')).rejects.toBeInstanceOf(InvalidStreamThumbnailPayloadError);
		expect(uploaded).toHaveLength(0);
	});

	it('rejects a thumbnail carrying no base64 digits', async () => {
		await expect(upload('====')).rejects.toBeInstanceOf(InvalidStreamThumbnailPayloadError);
		expect(uploaded).toHaveLength(0);
	});

	it('accepts a canonical base64 thumbnail and forwards the decoded bytes', async () => {
		await upload(JPEG_BASE64);
		expect(uploaded).toHaveLength(1);
		expect(Array.from(uploaded[0].body)).toEqual([0xff, 0xd8, 0x00, 0xff, 0xd9]);
	});
});
