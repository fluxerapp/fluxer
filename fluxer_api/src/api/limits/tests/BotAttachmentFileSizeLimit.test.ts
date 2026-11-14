// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {getConfig} from '@app/api/Config';
import {createMultipartFormData} from '@app/api/channel/tests/AttachmentTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {setInjectedMediaService} from '@app/api/middleware/ServiceRegistry';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {TestMediaService} from '@app/api/test/TestMediaService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {LimitConfigGetResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {z} from 'zod';

type LimitConfigResponse = z.infer<typeof LimitConfigGetResponse>;

const MIB = 1024 * 1024;

describe('bot attachment file size limit', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let owner: TestAccount;
	let channelId: string;
	let botToken: string;
	let originalSelfHosted: boolean;
	let originalPresigned: boolean;
	let originalPremiumMode: ReturnType<typeof getCachedInstancePremiumMode>;

	beforeAll(async () => {
		originalSelfHosted = getConfig().instance.selfHosted;
		originalPresigned = getConfig().presignedAttachmentUploadsEnabled;
		originalPremiumMode = getCachedInstancePremiumMode();
		getConfig().instance.selfHosted = true;
		getConfig().presignedAttachmentUploadsEnabled = true;
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		setInjectedMediaService(new TestMediaService(harness.storageService));
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
			AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
			AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
		]);
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({policy: {premium_mode: 'everyone'}})
			.execute();
		owner = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		const guild = await createGuild(harness, owner.token, 'Bot Upload Guild');
		channelId = guild.system_channel_id!;
		const bot = await createTestBotAccount(harness);
		await authorizeBot(harness, owner.token, bot.appId, ['bot'], guild.id, Permissions.ADMINISTRATOR.toString());
		botToken = `Bot ${bot.botToken}`;
	});

	afterAll(async () => {
		await harness.shutdown();
		getConfig().instance.selfHosted = originalSelfHosted;
		getConfig().presignedAttachmentUploadsEnabled = originalPresigned;
		setCachedInstancePremiumMode(originalPremiumMode);
	});

	async function setDefaultRuleLimit(key: string, value: number): Promise<number> {
		const {limit_config: config} = await createBuilder<LimitConfigResponse>(harness, admin.token)
			.get('/admin/limit-config')
			.execute();
		const {response} = await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({
				limit_config: {
					traitDefinitions: config.traitDefinitions,
					rules: config.rules.map(({modifiedFields: _modifiedFields, ...rule}) =>
						rule.id === 'default' ? {...rule, limits: {...rule.limits, [key]: value}} : rule,
					),
				},
			})
			.executeRaw();
		return response.status;
	}

	async function sendFormData(token: string, size: number): Promise<number> {
		const {body, contentType} = createMultipartFormData(
			{content: 'upload', attachments: [{id: 0, filename: 'file.bin'}]},
			[{index: 0, filename: 'file.bin', data: Buffer.alloc(size)}],
		);
		const response = await harness.app.request(`/channels/${channelId}/messages`, {
			method: 'POST',
			headers: {Authorization: token, 'Content-Type': contentType, 'x-forwarded-for': '127.0.0.1'},
			body,
		});
		return response.status;
	}

	async function requestPresigned(token: string, size: number): Promise<{status: number; code?: string}> {
		const {response, json} = await createBuilder<{code?: string}>(harness, token)
			.post(`/channels/${channelId}/attachments`)
			.body({attachments: [{id: 0, filename: 'file.bin', file_size: size, content_type: 'application/octet-stream'}]})
			.executeRaw();
		return {status: response.status, code: json?.code};
	}

	const tooLarge = {status: 400, code: 'FILE_SIZE_TOO_LARGE'};
	const accepted = {status: 200, code: undefined};

	it('keeps bots at 50 MiB by default', async () => {
		expect(await sendFormData(botToken, 51 * MIB)).toBe(400);
		expect(await requestPresigned(botToken, 51 * MIB)).toEqual(tooLarge);
	});

	it('lets an admin raise the bot limit in the default rule', async () => {
		expect(await setDefaultRuleLimit('max_bot_attachment_file_size', 200 * MIB)).toBe(200);

		expect(await sendFormData(botToken, 60 * MIB)).toBe(200);
		expect(await requestPresigned(botToken, 60 * MIB)).toEqual(accepted);
		expect(await requestPresigned(botToken, 201 * MIB)).toEqual(tooLarge);
	});

	it('does not apply the bot limit to people', async () => {
		expect(await requestPresigned(owner.token, 400 * MIB)).toEqual(accepted);
	});
});
