// SPDX-License-Identifier: AGPL-3.0-or-later

import {loadFixture, sendMessageWithAttachments} from '@app/api/channel/tests/AttachmentTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {FavoriteMemeResponse} from '@fluxer/schema/src/domains/meme/MemeSchemas';

interface MessageWithAttachment {
	id: string;
	channel_id: string;
	attachments: Array<{
		id: string;
		filename: string;
		size: number;
		url?: string | null;
	}>;
}

export async function listFavoriteMemes(harness: ApiTestHarness, token: string): Promise<Array<FavoriteMemeResponse>> {
	return createBuilder<Array<FavoriteMemeResponse>>(harness, token).get('/users/@me/memes').execute();
}

export async function getFavoriteMeme(
	harness: ApiTestHarness,
	token: string,
	memeId: string,
): Promise<FavoriteMemeResponse> {
	return createBuilder<FavoriteMemeResponse>(harness, token).get(`/users/@me/memes/${memeId}`).execute();
}

export async function createFavoriteMemeFromMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
	data: {
		attachment_id?: string;
		embed_index?: number;
		name: string;
		alt_text?: string;
		tags?: Array<string>;
	},
): Promise<FavoriteMemeResponse> {
	return createBuilder<FavoriteMemeResponse>(harness, token)
		.post(`/channels/${channelId}/messages/${messageId}/memes`)
		.body(data)
		.expect(HTTP_STATUS.CREATED)
		.execute();
}

export async function createFavoriteMemeFromUrl(
	harness: ApiTestHarness,
	token: string,
	data: {
		url: string;
		name?: string;
		alt_text?: string;
		tags?: Array<string>;
		gif_slug?: string;
		gif_provider?: string;
	},
): Promise<FavoriteMemeResponse> {
	return createBuilder<FavoriteMemeResponse>(harness, token)
		.post('/users/@me/memes')
		.body(data)
		.expect(HTTP_STATUS.CREATED)
		.execute();
}

export async function updateFavoriteMeme(
	harness: ApiTestHarness,
	token: string,
	memeId: string,
	data: {
		name?: string;
		alt_text?: string | null;
		tags?: Array<string>;
	},
): Promise<FavoriteMemeResponse> {
	return createBuilder<FavoriteMemeResponse>(harness, token).patch(`/users/@me/memes/${memeId}`).body(data).execute();
}

export async function deleteFavoriteMeme(harness: ApiTestHarness, token: string, memeId: string): Promise<void> {
	await createBuilder<void>(harness, token)
		.delete(`/users/@me/memes/${memeId}`)
		.expect(HTTP_STATUS.NO_CONTENT)
		.execute();
}

export async function createMessageWithImageAttachment(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	filename = 'yeah.png',
): Promise<MessageWithAttachment> {
	const fileData = loadFixture(filename);
	const {response, json} = await sendMessageWithAttachments(
		harness,
		token,
		channelId,
		{
			content: 'Test message with attachment',
			attachments: [{id: 0, filename}],
		},
		[{index: 0, filename, data: fileData}],
	);
	if (response.status !== 200) {
		throw new Error(`Failed to create message with attachment: ${response.status}`);
	}
	return {
		id: json.id,
		channel_id: channelId,
		attachments: json.attachments ?? [],
	};
}
