// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	makeApiAttachmentUrl,
	makePublicAttachmentUrl,
	parseApiAttachmentUrl,
	publicAttachmentUrl,
} from '@app/api/attachment/AttachmentPublicUrl';
import {makeSignedAttachmentCdnUrl, signAttachmentUrl} from '@app/api/attachment/AttachmentUrls';
import {createChannelID, createMessageID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {setCachedProxyAttachmentsThroughApi} from '@app/api/instance/AttachmentAccessMode';
import {afterEach, describe, expect, test} from 'vitest';

const CHANNEL_ID = 1_544_725_486_800_732_163n;
const ATTACHMENT_ID = 1_544_971_349_200_470_016n;
const MESSAGE_ID = 1_544_971_349_200_470_123n;

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
			api(`/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png?mid=abc`),
			api(`/channels/${CHANNEL_ID}/attachments/complete`),
			`https://example.test/channels/${CHANNEL_ID}/attachments/${ATTACHMENT_ID}/cat.png`,
			media(`/attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/cat.png`),
		]) {
			expect(parseApiAttachmentUrl(rejected)).toBeNull();
		}
	});
});
