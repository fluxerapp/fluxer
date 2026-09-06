// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount, createUniqueEmail} from '../../auth/tests/AuthTestUtils';
import {sendMessageWithAttachments, setupTestGuildAndChannel} from '../../channel/tests/AttachmentTestUtils';
import {ensureSessionStarted} from '../../message/tests/MessageTestUtils';
import {phraseBlocklistCache} from '../../middleware/PhraseBlocklistCache';
import {profileSubstringBlocklistCache} from '../../middleware/ProfileSubstringBlocklistCache';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS, TEST_CREDENTIALS, TEST_USER_DATA} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface ValidationErrorResponse {
	code: string;
	message?: string;
	errors?: Array<{
		path?: string;
		code?: string;
		message?: string;
	}>;
}

describe('User profile text validation', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterEach(() => {
		for (const scope of ['username', 'global_name', 'bio', 'pronouns'] as const) {
			profileSubstringBlocklistCache.remove(scope, 'blockedslug');
		}
		phraseBlocklistCache.remove('blockedphrase');
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('includes min/max in bio length validation message', async () => {
		const account = await createTestAccount(harness);
		await ensureSessionStarted(harness, account.token);
		const json = await createBuilder<ValidationErrorResponse>(harness, account.token)
			.patch('/users/@me')
			.body({bio: 'a'.repeat(321)})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();
		const error = json.errors?.find((e) => e.path === 'bio');
		expect(error?.code).toBe(ValidationErrorCodes.STRING_LENGTH_INVALID);
		expect(error?.message).toBe('String length must be between 1 and 320 characters.');
		expect(error?.message).not.toContain('undefined');
	});
	it('blocks banned substrings in account profile text fields', async () => {
		const account = await createTestAccount(harness);
		await ensureSessionStarted(harness, account.token);
		for (const scope of ['username', 'global_name', 'bio', 'pronouns'] as const) {
			profileSubstringBlocklistCache.add(scope, 'blockedslug');
		}
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({username: 'myblockedslugname', password: TEST_CREDENTIALS.STRONG_PASSWORD})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({global_name: 'BlockedSlug Display'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({bio: 'bio with blocked slug'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({pronouns: 'blockedslug'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
	});
	it('screens a JSON body declared under a non-JSON content type', async () => {
		const account = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		await ensureSessionStarted(harness, account.token);
		phraseBlocklistCache.add('blockedphrase');
		await createBuilder(harness, account.token)
			.put(`/users/@me/notes/${target.userId}`)
			.header('content-type', 'text/plain')
			.body({note: 'note carrying a blockedphrase value'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/themes')
			.header('content-type', 'text/plain')
			.body({css: 'body { content: "blockedphrase"; }'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
	});
	it('leaves a multipart body for the route to parse', async () => {
		const account = await createTestAccount(harness);
		const {guild, channel} = await setupTestGuildAndChannel(harness, account);
		const channelId = guild.system_channel_id ?? channel.id;
		const {response} = await sendMessageWithAttachments(
			harness,
			account.token,
			channelId,
			{content: 'multipart message body', attachments: [{id: 0, filename: 'upload.txt'}]},
			[{index: 0, filename: 'upload.txt', data: Buffer.from('uploaded through multipart')}],
		);
		expect(response.status).toBe(HTTP_STATUS.OK);
	});
	it('blocks banned substrings during account registration', async () => {
		profileSubstringBlocklistCache.add('username', 'blockedslug');
		profileSubstringBlocklistCache.add('global_name', 'blockedslug');
		await createBuilder(harness, '')
			.post('/auth/register')
			.body({
				email: createUniqueEmail('blocked-username'),
				username: 'blockedsluguser',
				global_name: TEST_USER_DATA.DEFAULT_GLOBAL_NAME,
				password: TEST_CREDENTIALS.STRONG_PASSWORD,
				date_of_birth: TEST_USER_DATA.DEFAULT_DATE_OF_BIRTH,
				consent: true,
			})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
		await createBuilder(harness, '')
			.post('/auth/register')
			.body({
				email: createUniqueEmail('blocked-display'),
				username: 'allowedregistrationuser',
				global_name: 'BlockedSlug Display',
				password: TEST_CREDENTIALS.STRONG_PASSWORD,
				date_of_birth: TEST_USER_DATA.DEFAULT_DATE_OF_BIRTH,
				consent: true,
			})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
	});
});
