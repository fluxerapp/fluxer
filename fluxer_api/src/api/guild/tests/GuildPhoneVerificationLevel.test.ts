// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {createGuild, updateGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {GuildVerificationLevel} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Guild verification level without phone verification', () => {
	let harness: ApiTestHarness;
	let savedPhoneVerificationEnabled: boolean;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
		savedPhoneVerificationEnabled = Config.instance.phoneVerificationEnabled;
	});
	afterEach(() => {
		Config.instance.phoneVerificationEnabled = savedPhoneVerificationEnabled;
	});

	test('rejects very high when phone verification is unavailable', async () => {
		Config.instance.phoneVerificationEnabled = false;
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'No Phone Guild');
		await createBuilder(harness, account.token)
			.patch(`/guilds/${guild.id}`)
			.body({verification_level: GuildVerificationLevel.VERY_HIGH})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		const updated = await updateGuild(harness, account.token, guild.id, {
			verification_level: GuildVerificationLevel.HIGH,
		});
		expect(updated.verification_level).toBe(GuildVerificationLevel.HIGH);
	});

	test('accepts very high when phone verification is available', async () => {
		Config.instance.phoneVerificationEnabled = true;
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Phone Guild');
		const updated = await updateGuild(harness, account.token, guild.id, {
			verification_level: GuildVerificationLevel.VERY_HIGH,
		});
		expect(updated.verification_level).toBe(GuildVerificationLevel.VERY_HIGH);
	});

	test('accepts an unchanged stored very high when phone verification is unavailable', async () => {
		Config.instance.phoneVerificationEnabled = true;
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Stored Phone Guild');
		await updateGuild(harness, account.token, guild.id, {verification_level: GuildVerificationLevel.VERY_HIGH});
		Config.instance.phoneVerificationEnabled = false;
		const updated = await updateGuild(harness, account.token, guild.id, {
			name: 'Renamed Phone Guild',
			verification_level: GuildVerificationLevel.VERY_HIGH,
		});
		expect(updated.name).toBe('Renamed Phone Guild');
		expect(updated.verification_level).toBe(GuildVerificationLevel.VERY_HIGH);
	});

	test('clamps a template level to high when phone verification is unavailable', async () => {
		Config.instance.phoneVerificationEnabled = false;
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Template Guild',
				template: {
					name: 'Template Source',
					description: null,
					verification_level: GuildVerificationLevel.VERY_HIGH,
					default_message_notifications: 0,
					explicit_content_filter: 0,
					system_channel_id: 1001,
					afk_timeout: 300,
					system_channel_flags: 0,
					roles: [{id: 0, name: '@everyone', permissions: '0'}],
					channels: [{id: 1001, type: ChannelTypes.GUILD_TEXT, name: 'general', position: 0}],
				},
			})
			.execute();
		expect(guild.verification_level).toBe(GuildVerificationLevel.HIGH);
	});
});
