// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestGuild, VALID_PNG_BASE64} from '@app/api/emoji/tests/EmojiTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {EMOJI_MAX_SIZE, STICKER_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

function paddedImage(byteLength: number): string {
	const image = Buffer.from(VALID_PNG_BASE64, 'base64');
	return `data:image/png;base64,${Buffer.concat([image, Buffer.alloc(byteLength - image.length)]).toString('base64')}`;
}

describe('emoji and sticker size ceilings', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	it.each([
		{path: 'emojis', maxSize: EMOJI_MAX_SIZE, body: {name: 'too_big'}},
		{path: 'stickers', maxSize: STICKER_MAX_SIZE, body: {name: 'too_big', description: 'too big', tags: []}},
	])('rejects $path one byte over the built-in ceiling', async ({path, maxSize, body}) => {
		const owner = await createTestAccount(harness);
		const guild = await createTestGuild(harness, owner.token);
		const json = await createBuilder<{errors?: Array<{path?: string; code?: string}>}>(harness, owner.token)
			.post(`/guilds/${guild.id}/${path}`)
			.body({...body, image: paddedImage(maxSize + 1)})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(json.errors?.[0]).toMatchObject({path: 'image', code: ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT});
	});
});
