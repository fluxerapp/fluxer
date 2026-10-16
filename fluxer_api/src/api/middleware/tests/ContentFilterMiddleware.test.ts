// SPDX-License-Identifier: AGPL-3.0-or-later

import {extractStringValues, shouldSkipContentFilterPath} from '@app/api/middleware/ContentFilterMiddleware';
import {describe, expect, test} from 'vitest';

describe('extractStringValues', () => {
	test('returns text from user-generated content fields', () => {
		const body = {
			content: 'visible message content',
			code: 'public-vanity-code',
			embeds: [{title: 'embed title', description: 'embed description'}],
			attachments: [{filename: 'visible-filename.png', title: 'attachment title'}],
		};
		const result = extractStringValues(body);
		expect(result).toEqual([
			'visible message content',
			'public-vanity-code',
			'embed title',
			'embed description',
			'visible-filename.png',
			'attachment title',
		]);
	});
	test('omits machine metadata fields from nested payloads', () => {
		const body = {
			content: 'visible message content',
			attachments: [
				{
					id: '1',
					upload_filename: '2f31b930-d1ea-453f-aa6a-4f7ddc7d04d5',
					content_type: 'image/png',
					waveform: 'AA==',
					title: 'visible attachment title',
				},
			],
			allowed_mentions: {
				users: ['111111111111111111'],
				roles: ['222222222222222222'],
			},
			message_reference: {
				channel_id: '333333333333333333',
				message_id: '444444444444444444',
			},
			response: {
				clientDataJSON: 'eyJjaGFsbGVuZ2UiOiJub25jZSJ9',
			},
			synced_preferences: 'CgIIAQ==',
			thumbnail: 'data:image/png;base64,AAAA',
		};
		const result = extractStringValues(body);
		expect(result).toEqual(['visible message content', 'visible attachment title']);
	});
	test('omits fields with identifier and secret suffixes', () => {
		const body = {
			label: 'visible label',
			actor_user_id: '111111111111111111',
			channel_ids: ['222222222222222222'],
			content_hash: 'f'.repeat(64),
			encryption_key: 'p256dh-value',
			original_proof: 'email-proof-token',
			state_token: 'state-token',
		};
		const result = extractStringValues(body);
		expect(result).toEqual(['visible label']);
	});
	test('omits the structural fields of report flow submissions', () => {
		const body = {
			channel_id: '111111111111111111',
			message_id: '222222222222222222',
			user_id: '333333333333333333',
			guild_id: '444444444444444444',
			revision_hash: '6b1f0c9e2a7d4413',
			locale: 'de',
			steps: [
				{screen_id: 'root_message', option_id: 'private_info'},
				{screen_id: 'private_info', item_ids: ['email', 'phone']},
			],
		};
		const result = extractStringValues(body);
		expect(result).toEqual([]);
	});
});

describe('shouldSkipContentFilterPath', () => {
	test('skips credential-only request bodies', () => {
		const paths = [
			'/users/@me/email-change/verify-original',
			'/users/@me/mfa/totp/enable',
			'/users/@me/password-change/complete',
			'/reports/dsa/email/verify',
		];
		const result = paths.map((path) => shouldSkipContentFilterPath(path));
		expect(result).toEqual([true, true, true, true]);
	});
	test('skips every DSA report body, which has to quote the reported content', () => {
		const paths = ['/reports/dsa', '/reports/dsa/email/send', '/reports/dsa/email/verify'];
		const result = paths.map((path) => shouldSkipContentFilterPath(path));
		expect(result).toEqual([true, true, true]);
	});
	test('does not skip in-app report submissions', () => {
		const paths = ['/reports/message', '/reports/user', '/reports/flows/message/submissions'];
		const result = paths.map((path) => shouldSkipContentFilterPath(path));
		expect(result).toEqual([false, false, false]);
	});
	test('does not skip public content update request bodies', () => {
		const paths = ['/guilds/123/vanity-url', '/channels/123/messages', '/users/@me'];
		const result = paths.map((path) => shouldSkipContentFilterPath(path));
		expect(result).toEqual([false, false, false]);
	});
	test('skips blocklist writes whose values are the blocked content', () => {
		const paths = [
			'/admin/blocklists/phrase/entries',
			'/admin/blocklists/url/entries',
			'/admin/blocklists/url-domain/entries',
			'/admin/blocklists/profile-substring/entries',
		];
		const result = paths.map((path) => shouldSkipContentFilterPath(path));
		expect(result).toEqual([true, true, true, false]);
	});
});
