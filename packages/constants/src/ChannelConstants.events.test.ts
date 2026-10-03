// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ALL_PERMISSIONS,
	DEFAULT_PERMISSIONS,
	ElevatedPermissions,
	Permissions,
} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

describe('community event permissions', () => {
	it('keeps Discord-compatible create and manage event bits', () => {
		expect(Permissions.MANAGE_EVENTS).toBe(1n << 33n);
		expect(Permissions.CREATE_EVENTS).toBe(1n << 44n);
	});

	it('does not collide with other permission bits', () => {
		const otherBits = Object.entries(Permissions)
			.filter(([name]) => name !== 'CREATE_EVENTS' && name !== 'MANAGE_EVENTS')
			.map(([, bit]) => bit);
		for (const bit of [Permissions.CREATE_EVENTS, Permissions.MANAGE_EVENTS]) {
			expect(otherBits.some((other) => (other & bit) === bit)).toBe(false);
		}
	});

	it('includes create events in defaults and manage events in elevated permissions', () => {
		expect((DEFAULT_PERMISSIONS & Permissions.CREATE_EVENTS) === Permissions.CREATE_EVENTS).toBe(true);
		expect((ElevatedPermissions & Permissions.MANAGE_EVENTS) === Permissions.MANAGE_EVENTS).toBe(true);
		expect((ALL_PERMISSIONS & Permissions.CREATE_EVENTS) === Permissions.CREATE_EVENTS).toBe(true);
		expect((ALL_PERMISSIONS & Permissions.MANAGE_EVENTS) === Permissions.MANAGE_EVENTS).toBe(true);
	});
});
