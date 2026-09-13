// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {fetchUserNote, setUserNote} from '@app/api/user/tests/UserTestUtils';
import {beforeEach, describe, expect, test} from 'vitest';

describe('User Note Lifecycle', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('user can set and retrieve note for another user', async () => {
		const user = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const noteContent = 'This is a test note';
		await setUserNote(harness, user.token, target.userId, noteContent);
		const {json} = await fetchUserNote(harness, user.token, target.userId);
		const note = json as {
			note: string | null;
		};
		expect(note.note).toBe(noteContent);
	});
	test('user can update note for another user', async () => {
		const user = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		await setUserNote(harness, user.token, target.userId, 'Initial note');
		const updatedNote = 'Updated note content';
		await setUserNote(harness, user.token, target.userId, updatedNote);
		const {json} = await fetchUserNote(harness, user.token, target.userId);
		const note = json as {
			note: string | null;
		};
		expect(note.note).toBe(updatedNote);
	});
	test('user can clear note for another user', async () => {
		const user = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		await setUserNote(harness, user.token, target.userId, 'Note to clear');
		await setUserNote(harness, user.token, target.userId, null);
		await createBuilder(harness, user.token)
			.get(`/users/@me/notes/${target.userId}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
});
