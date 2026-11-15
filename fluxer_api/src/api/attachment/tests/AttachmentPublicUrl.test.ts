// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	apiFormOfMediaUrl,
	isAttachmentMediaRequest,
	isGuardedAttachmentUrl,
	makeApiAttachmentUrl,
	makePublicAttachmentUrl,
	parseApiAttachmentUrl,
	parseMediaAttachmentUrl,
	publicAttachmentUrl,
} from '@app/api/attachment/AttachmentPublicUrl';
import {makeSignedAttachmentCdnUrl, signAttachmentUrl} from '@app/api/attachment/AttachmentUrls';
import {createChannelID, createMessageID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {setCachedProxyAttachmentsThroughApi} from '@app/api/instance/AttachmentAccessMode';
import {extractURLs} from '@app/api/utils/UnfurlerUtils';
import {createSnowflakeGenerator} from '@fluxer/snowflake/src/Snowflake';
import {afterEach, describe, expect, test} from 'vitest';

const snowflakes = createSnowflakeGenerator();
const CHANNEL_ID = snowflakes.generate();
const ATTACHMENT_ID = snowflakes.generate();
const MESSAGE_ID = snowflakes.generate();

function media(path: string): string {
	return `${Config.endpoints.media}${path}`;
}

function api(path: string): string {
	return `${Config.endpoints.apiClient}${path}`;
}

const INPUTS = [
	media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/cat.png`),
	media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/cat.png?ex=1&is=2&hm=3`),
	media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/cat.png?width=64&format=webp`),
	media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/a%20b.png`),
	media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/../cat.png`),
	media(`/avatars/${CHANNEL_ID}/abc.png`),
	api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png?mid=${MESSAGE_ID}`),
	'https://example.test/attachments/1/2/cat.png',
	'not a url',
];

describe('attachment links', () => {
	afterEach(() => {
		setCachedProxyAttachmentsThroughApi(null);
	});

	test('with the setting off every link matches the media proxy form', () => {
		for (const input of INPUTS) {
			expect(publicAttachmentUrl(input)).toBe(signAttachmentUrl(input));
		}
		expect(makePublicAttachmentUrl(createChannelID(CHANNEL_ID), ATTACHMENT_ID, 'cat.png')).toBe(
			makeSignedAttachmentCdnUrl(createChannelID(CHANNEL_ID), ATTACHMENT_ID, 'cat.png'),
		);
		setCachedProxyAttachmentsThroughApi(false);
		for (const input of INPUTS) {
			expect(publicAttachmentUrl(input)).toBe(signAttachmentUrl(input));
			expect(isGuardedAttachmentUrl(input)).toBe(false);
		}
		const routePath = `/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png`;
		for (const path of [routePath, `/v1${routePath}`, `/api${routePath}`]) {
			expect(isAttachmentMediaRequest('GET', path)).toBe(false);
			expect(isAttachmentMediaRequest('HEAD', path)).toBe(false);
		}
	});

	test('with the setting on no media proxy link is handed out', () => {
		setCachedProxyAttachmentsThroughApi(true);
		expect(publicAttachmentUrl(media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/cat.png?ex=1&is=2&hm=3`))).toBe(
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png`),
		);
		expect(publicAttachmentUrl(media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/cat.png?width=64&mt=x&mid=1`))).toBe(
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png?width=64`),
		);
		expect(makePublicAttachmentUrl(createChannelID(CHANNEL_ID), ATTACHMENT_ID, 'cat.png')).toBe(
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png`),
		);
		for (const input of INPUTS) {
			expect(publicAttachmentUrl(input).includes('hm=')).toBe(false);
		}
		const routePath = `/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png`;
		expect(isAttachmentMediaRequest('GET', `/api/v1${routePath}`)).toBe(true);
		expect(isAttachmentMediaRequest('POST', routePath)).toBe(false);
		expect(isAttachmentMediaRequest('GET', `/channels/${CHANNEL_ID}/messages`)).toBe(false);
	});

	test('with the setting on links to stored attachments in either form need the requester checked', () => {
		setCachedProxyAttachmentsThroughApi(true);
		const guarded = [
			media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/cat.png`),
			media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/cat.png?ex=1&is=2&hm=3`),
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png?mid=${MESSAGE_ID}`),
		];
		for (const url of guarded) expect(isGuardedAttachmentUrl(url)).toBe(true);
		for (const url of [
			media(`/avatars/${CHANNEL_ID}/abc.png`),
			media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/../cat.png`),
			'https://example.test/attachments/1/2/cat.png',
			'not a url',
		]) {
			expect(isGuardedAttachmentUrl(url)).toBe(false);
		}
		expect(parseMediaAttachmentUrl(media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/a%20b.png?ex=1`))).toEqual({
			channelId: CHANNEL_ID,
			attachmentId: ATTACHMENT_ID,
			filename: 'a b.png',
			messageId: null,
		});
	});

	test('links to stored attachments are only sent to the unfurler while the setting is off', () => {
		const stored = media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/cat.png`);
		const proxied = api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png?mid=${MESSAGE_ID}`);
		const content = `${stored}?ex=1&is=2&hm=3 ${proxied} https://example.test/page`;
		const off = extractURLs(content);
		expect(off).toContain(stored);
		expect(off).toContain(proxied);
		setCachedProxyAttachmentsThroughApi(true);
		expect(extractURLs(content)).toEqual(['https://example.test/page']);
	});

	test('a media link names its message only when the message owns the stored file', () => {
		const storageChannelId = createChannelID(CHANNEL_ID);
		const copyChannelId = createChannelID(snowflakes.generate());
		const owner = {channelId: copyChannelId, messageId: createMessageID(MESSAGE_ID), storageChannelId};
		const url = media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/cat.png?ex=1&is=2&hm=3&mt=4&width=64`);
		expect(apiFormOfMediaUrl(url, owner)).toBe(
			api(`/channels/${copyChannelId}/attachments/${ATTACHMENT_ID}/cat.png?mid=${MESSAGE_ID}&width=64`),
		);
		expect(apiFormOfMediaUrl(url, null)).toBe(
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png?width=64`),
		);
		expect(apiFormOfMediaUrl(url, {...owner, storageChannelId: copyChannelId})).toBe(
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png?width=64`),
		);
	});

	test('an api link parses back to its parts and rejects tricks', () => {
		const url = makeApiAttachmentUrl(
			createChannelID(CHANNEL_ID),
			ATTACHMENT_ID,
			'a%20b.png',
			createMessageID(MESSAGE_ID),
		);
		expect(parseApiAttachmentUrl(url)).toEqual({
			channelId: CHANNEL_ID,
			attachmentId: ATTACHMENT_ID,
			filename: 'a b.png',
			messageId: MESSAGE_ID,
		});
		expect(
			parseApiAttachmentUrl(api(`/v1/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png`)),
		).not.toBeNull();
		for (const rejected of [
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/%2E%2E/cat.png`),
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/a/../cat.png`),
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat%00.png`),
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/..%2f..%2Fusers%2F%40me`),
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/..%5c..%5Cusers`),
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat%2epng`),
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png?mid=abc`),
			api(`/channels/${CHANNEL_ID}/attachments/complete`),
			`https://example.test/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png`,
			media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/cat.png`),
		]) {
			expect(parseApiAttachmentUrl(rejected)).toBeNull();
		}
	});
});
