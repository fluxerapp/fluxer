// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, expect, test} from 'vitest';

describe('UserSettings guild_folders validation', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('preserves guild folders in database after unrelated settings update', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Persist Guild');
		type SettingsResponse = {
			guild_folders: Array<{
				id: number;
				name: string | null;
				color: number | null;
				guild_ids: Array<string>;
			}>;
		};
		await createBuilder(harness, account.token)
			.patch('/users/@me/settings')
			.body({
				guild_folders: [
					{
						id: 42,
						name: 'Persist Folder',
						color: 0x00ff00,
						guild_ids: [guild.id],
					},
				],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, account.token)
			.patch('/users/@me/settings')
			.body({developer_mode: true})
			.expect(HTTP_STATUS.OK)
			.execute();
		const {json} = await createBuilder<SettingsResponse>(harness, account.token)
			.get('/users/@me/settings')
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		const folder = json.guild_folders.find((entry) => entry.id === 42);
		expect(folder).toBeDefined();
		expect(folder?.name).toBe('Persist Folder');
		expect(folder?.color).toBe(0x00ff00);
		expect(folder?.guild_ids).toContain(guild.id);
	});
});
