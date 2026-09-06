// SPDX-License-Identifier: AGPL-3.0-or-later

import {STREAM_PREVIEW_CONTENT_TYPE_JPEG, STREAM_PREVIEW_MAX_BYTES} from '@fluxer/constants/src/StreamConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {Config} from '../../Config';
import {getCacheService} from '../../middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createDmChannel, createFriendship} from './ChannelTestUtils';

const CONNECTION_ID = 'conn-oversized';

describe('stream preview object size ceiling', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		harness.storageService.reset();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createStoredPreview(fileData: Uint8Array): Promise<{token: string; streamKey: string}> {
		const owner = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		await createFriendship(harness, owner, viewer);
		const dm = await createDmChannel(harness, owner.token, viewer.userId);
		const streamKey = `dm:${dm.id}:${CONNECTION_ID}`;
		await getCacheService().set(
			`stream_preview:${streamKey}`,
			{
				bucket: Config.s3.buckets.uploads,
				key: `stream_previews/${dm.id}-${CONNECTION_ID}.jpg`,
				updatedAt: Date.now(),
				ownerId: owner.userId,
				channelId: dm.id,
				contentType: STREAM_PREVIEW_CONTENT_TYPE_JPEG,
			},
			60,
		);
		harness.storageService.configure({fileData});
		return {token: owner.token, streamKey};
	}

	it('answers an empty 404 when the stored object is larger than the preview ceiling', async () => {
		const {token, streamKey} = await createStoredPreview(new Uint8Array(STREAM_PREVIEW_MAX_BYTES + 1));
		const response = await harness.requestJson({
			path: `/v1/streams/${streamKey}/preview`,
			headers: {authorization: token},
		});
		expect(response.status).toBe(404);
		expect(await response.text()).toBe('');
		expect(harness.storageService.readObjectSpy).toHaveBeenCalledWith(
			Config.s3.buckets.uploads,
			expect.any(String),
			STREAM_PREVIEW_MAX_BYTES,
		);
	});

	it('serves a stored object that fits inside the preview ceiling', async () => {
		const {token, streamKey} = await createStoredPreview(new Uint8Array([0xff, 0xd8, 0x00, 0xff, 0xd9]));
		const response = await harness.requestJson({
			path: `/v1/streams/${streamKey}/preview`,
			headers: {authorization: token},
		});
		expect(response.status).toBe(200);
		expect(response.headers.get('content-type')).toBe(STREAM_PREVIEW_CONTENT_TYPE_JPEG);
		expect(new Uint8Array(await response.arrayBuffer())).toEqual(new Uint8Array([0xff, 0xd8, 0x00, 0xff, 0xd9]));
	});
});
