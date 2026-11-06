// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createGuild,
	createPermissionOverwrite,
	getChannel,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {createEmoji, createTestGuild, getGifDataUrl, getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {setLimitOverride} from '@app/api/test/LimitTestUtils';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Message custom emoji sanitization', () => {
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
	it('preserves guild-local static and animated custom emojis for free users', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Local emoji guild');
		if (!guild.system_channel_id) {
			throw new Error('Guild system channel id is missing');
		}
		const channel = await getChannel(harness, account.token, guild.system_channel_id);
		const staticEmoji = await createEmoji(harness, account.token, guild.id, {
			name: 'local_static',
			image: getPngDataUrl(),
		});
		const animatedEmoji = await createEmoji(harness, account.token, guild.id, {
			name: 'local_animated',
			image: getGifDataUrl(),
		});
		const content = `Static <:${staticEmoji.name}:${staticEmoji.id}> animated <a:${animatedEmoji.name}:${animatedEmoji.id}>`;
		const message = await sendChannelMessage(harness, account.token, channel.id, content);
		expect(message.content).toBe(content);
	});
	it('strips external custom emojis for free users', async () => {
		const account = await createTestAccount(harness);
		const destinationGuild = await createGuild(harness, account.token, 'Destination guild');
		if (!destinationGuild.system_channel_id) {
			throw new Error('Destination guild system channel id is missing');
		}
		const destinationChannel = await getChannel(harness, account.token, destinationGuild.system_channel_id);
		const sourceGuild = await createGuild(harness, account.token, 'External emoji guild');
		const externalEmoji = await createEmoji(harness, account.token, sourceGuild.id, {
			name: 'external_emoji',
			image: getPngDataUrl(),
		});
		const content = `External <:${externalEmoji.name}:${externalEmoji.id}>`;
		const message = await sendChannelMessage(harness, account.token, destinationChannel.id, content);
		expect(message.content).toBe('External :external_emoji:');
	});
	it('treats only real code spans as code when sanitizing external custom emojis', async () => {
		const account = await createTestAccount(harness);
		const destinationGuild = await createGuild(harness, account.token, 'Destination guild');
		if (!destinationGuild.system_channel_id) {
			throw new Error('Destination guild system channel id is missing');
		}
		const destinationChannel = await getChannel(harness, account.token, destinationGuild.system_channel_id);
		const sourceGuild = await createGuild(harness, account.token, 'External emoji guild');
		const externalEmoji = await createEmoji(harness, account.token, sourceGuild.id, {
			name: 'external_emoji',
			image: getPngDataUrl(),
		});
		const markup = `<:${externalEmoji.name}:${externalEmoji.id}>`;
		const send = async (content: string) =>
			(await sendChannelMessage(harness, account.token, destinationChannel.id, content)).content;
		expect(await send(`a \\\`${markup}\\\` b`)).toBe('a \\`:external_emoji:\\` b');
		expect(await send(`a \`x\n${markup}\` b`)).toBe('a `x\n:external_emoji:` b');
		expect(await send(`a \`${markup}\` b`)).toBe(`a \`${markup}\` b`);
		expect(await send(`a \`\`x ${markup}\`\` b`)).toBe(`a \`\`x ${markup}\`\` b`);
		expect(await send(`\`\`\`\n${markup}\n\`\`\``)).toBe(`\`\`\`\n${markup}\n\`\`\``);
		expect(await send(`> \`\`\`\n${markup}\n\`\`\``)).toBe('> ```\n:external_emoji:\n```');
		expect(await send(`- \`\`\`\n${markup}\n\`\`\``)).toBe('- ```\n:external_emoji:\n```');
		expect(await send(`\`\`\`bash ${markup}\n\`\`\``)).toBe('```bash :external_emoji:\n```');
	});
	it('strips external custom emojis on edit when USE_EXTERNAL_EMOJIS is denied', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, guild.id, {
			type: 0,
			allow: '0',
			deny: Permissions.USE_EXTERNAL_EMOJIS.toString(),
		});
		const restore = await setLimitOverride(harness, {feature_global_expressions: 1});
		try {
			const sourceGuild = await createTestGuild(harness, member.token);
			const externalEmoji = await createEmoji(harness, member.token, sourceGuild.id, {
				name: 'external_emoji',
				image: getPngDataUrl(),
			});
			const message = await sendChannelMessage(harness, member.token, systemChannel.id, 'hello');
			const edited = await createBuilder<MessageResponse>(harness, member.token)
				.patch(`/channels/${systemChannel.id}/messages/${message.id}`)
				.body({content: `edit <:${externalEmoji.name}:${externalEmoji.id}>`})
				.execute();
			expect(edited.content).toBe('edit :external_emoji:');
		} finally {
			await restore();
		}
	});
});
