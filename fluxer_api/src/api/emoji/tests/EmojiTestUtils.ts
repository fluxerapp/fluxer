// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {GuildEmojiWithUserResponse} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
export const VALID_PNG_BASE64 =
	'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==';
export const VALID_GIF_BASE64 = 'R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7';
export function getPngDataUrl(base64: string = VALID_PNG_BASE64): string {
	return `data:image/png;base64,${base64}`;
}
export function getGifDataUrl(base64: string = VALID_GIF_BASE64): string {
	return `data:image/gif;base64,${base64}`;
}
export async function createTestGuild(harness: ApiTestHarness, token: string, name?: string): Promise<GuildResponse> {
	const guildName = name ?? `Test Guild ${randomUUID()}`;
	return createBuilder<GuildResponse>(harness, token).post('/guilds').body({name: guildName}).execute();
}
export async function createEmoji(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	params: {
		name: string;
		image: string;
	},
): Promise<GuildEmojiWithUserResponse> {
	return createBuilder<GuildEmojiWithUserResponse>(harness, token)
		.post(`/guilds/${guildId}/emojis`)
		.body(params)
		.execute();
}
