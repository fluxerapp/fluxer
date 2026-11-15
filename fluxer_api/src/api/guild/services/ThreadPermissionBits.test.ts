// SPDX-License-Identifier: AGPL-3.0-or-later

import {seedThreadOverwriteBits, seedThreadOverwriteValue} from '@app/api/worker/tasks/SeedThreadPermissions';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {DEFAULT_THREAD_PERMISSIONS, ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import {describe, expect, it} from 'vitest';

const MANAGE_THREADS = ThreadPermissionFlags.MANAGE_THREADS;

describe('thread permission seeding rules', () => {
	it('clears raw thread bits on overwrites without mirroring SEND_MESSAGES', () => {
		expect(seedThreadOverwriteBits(MANAGE_THREADS | Permissions.VIEW_CHANNEL)).toBe(Permissions.VIEW_CHANNEL);
		expect(seedThreadOverwriteBits(Permissions.SEND_MESSAGES | MANAGE_THREADS)).toBe(Permissions.SEND_MESSAGES);
		expect(seedThreadOverwriteBits(Permissions.SEND_MESSAGES) & DEFAULT_THREAD_PERMISSIONS).toBe(0n);
		expect(seedThreadOverwriteBits(seedThreadOverwriteBits(Permissions.SEND_MESSAGES))).toBe(Permissions.SEND_MESSAGES);
	});

	it('mirrors SEND_MESSAGES into the thread bits on either overwrite side', () => {
		expect(seedThreadOverwriteValue(Permissions.SEND_MESSAGES | MANAGE_THREADS)).toBe(
			Permissions.SEND_MESSAGES | DEFAULT_THREAD_PERMISSIONS,
		);
		expect(seedThreadOverwriteValue(MANAGE_THREADS | Permissions.VIEW_CHANNEL)).toBe(Permissions.VIEW_CHANNEL);
		expect(seedThreadOverwriteValue(seedThreadOverwriteValue(Permissions.SEND_MESSAGES))).toBe(
			Permissions.SEND_MESSAGES | DEFAULT_THREAD_PERMISSIONS,
		);
	});
});
