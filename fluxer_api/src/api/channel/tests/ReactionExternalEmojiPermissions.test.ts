// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createPermissionOverwrite,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {createEmoji, createTestGuild, getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {setLimitOverride} from '@app/api/test/LimitTestUtils';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Reactions without USE_EXTERNAL_EMOJIS', () => {
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

	it('allows emojis from the same community', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const [reactor] = members;
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, guild.id, {
			type: 0,
			allow: '0',
			deny: Permissions.USE_EXTERNAL_EMOJIS.toString(),
		});
		const message = await sendChannelMessage(harness, owner.token, systemChannel.id, 'react to me');
		const react = (emoji: {id: string; name: string}) =>
			createBuilder(harness, reactor.token)
				.put(`/channels/${systemChannel.id}/messages/${message.id}/reactions/${emoji.name}:${emoji.id}/@me`)
				.body(null);

		const localEmoji = await createEmoji(harness, owner.token, guild.id, {name: 'local', image: getPngDataUrl()});
		await react(localEmoji).expect(HTTP_STATUS.NO_CONTENT).execute();

		const restore = await setLimitOverride(harness, {feature_global_expressions: 1});
		try {
			const otherGuild = await createTestGuild(harness, reactor.token);
			const externalEmoji = await createEmoji(harness, reactor.token, otherGuild.id, {
				name: 'external',
				image: getPngDataUrl(),
			});
			await react(externalEmoji).expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS).execute();
		} finally {
			await restore();
		}
	});
});
