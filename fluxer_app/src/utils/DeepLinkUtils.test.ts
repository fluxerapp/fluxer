/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {afterAll, beforeAll, describe, expect, test} from 'vitest';
import {parseChannelJumpLink, parseChannelUrl, parseMessageJumpLink} from './DeepLinkUtils';

const CANARY_BASE = 'https://canary.fluxer.app';
const STABLE_BASE = 'https://fluxer.app';
const CHANNEL_LINKS = [
	`${CANARY_BASE}/channels/@me/1130650140672000000/1447659936007000077`,
	`${CANARY_BASE}/channels/@me/1130650140672000000`,
	`${CANARY_BASE}/channels/12345678901234567/23456789012345678`,
	`${CANARY_BASE}/channels/12345678901234567/23456789012345678/34567890123456789`,
	`${STABLE_BASE}/channels/@me/1130650140672000000/1447659936007000077`,
	`${STABLE_BASE}/channels/@me/1130650140672000000`,
	`${STABLE_BASE}/channels/12345678901234567/23456789012345678`,
	`${STABLE_BASE}/channels/12345678901234567/23456789012345678/34567890123456789`,
];
const GUILD_CHANNEL = `${STABLE_BASE}/channels/12345678901234567/23456789012345678`;
const GUILD_MESSAGE = `${STABLE_BASE}/channels/12345678901234567/23456789012345678/34567890123456789`;

describe('parseChannelUrl', () => {
	const originalLocationHref = globalThis.location.href;

	beforeAll(() => {
		globalThis.location.href = CANARY_BASE;
	});

	afterAll(() => {
		globalThis.location.href = originalLocationHref;
	});

	for (const url of CHANNEL_LINKS) {
		test(`returns pathname for ${url}`, () => {
			const expectedPath = new URL(url).pathname;
			expect(parseChannelUrl(url)).toBe(expectedPath);
		});
	}
});

describe('parseMessageJumpLink', () => {
	const originalLocationHref = globalThis.location.href;

	beforeAll(() => {
		globalThis.location.href = CANARY_BASE;
	});

	afterAll(() => {
		globalThis.location.href = originalLocationHref;
	});

	test('returns channel, guild, and message info when the path points to a message', () => {
		expect(parseMessageJumpLink(GUILD_MESSAGE)).toEqual({
			scope: '12345678901234567',
			channelId: '23456789012345678',
			messageId: '34567890123456789',
		});
	});
});

describe('parseChannelJumpLink', () => {
	const originalLocationHref = globalThis.location.href;

	beforeAll(() => {
		globalThis.location.href = STABLE_BASE;
	});

	afterAll(() => {
		globalThis.location.href = originalLocationHref;
	});

	test('returns scope and channel for a DM path', () => {
		const url = `${STABLE_BASE}/channels/@me/1130650140672000000`;
		expect(parseChannelJumpLink(url)).toEqual({
			scope: '@me',
			channelId: '1130650140672000000',
		});
	});

	test('returns scope and channel for a guild path', () => {
		expect(parseChannelJumpLink(GUILD_CHANNEL)).toEqual({
			scope: '12345678901234567',
			channelId: '23456789012345678',
		});
	});

	test('still returns scope and channel when a message ID is appended', () => {
		expect(parseChannelJumpLink(GUILD_MESSAGE)).toEqual({
			scope: '12345678901234567',
			channelId: '23456789012345678',
		});
	});
});
