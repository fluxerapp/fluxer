// SPDX-License-Identifier: AGPL-3.0-or-later

import {canAuthorizeBotInvite, normalizeBotInvitePermissions} from '@fluxer/constants/src/BotPermissionUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

describe('canAuthorizeBotInvite', () => {
	it('requires permission to invite bots even when no permissions are requested', () => {
		expect(canAuthorizeBotInvite({userPermissions: 0n, requestedPermissions: 0n})).toBe(false);
		expect(canAuthorizeBotInvite({userPermissions: Permissions.MANAGE_GUILD, requestedPermissions: 0n})).toBe(true);
	});
	it('rejects administrator requests from users who only have Manage Guild', () => {
		expect(
			canAuthorizeBotInvite({
				userPermissions: Permissions.MANAGE_GUILD,
				requestedPermissions: Permissions.ADMINISTRATOR,
			}),
		).toBe(false);
	});
	it('allows non-admin users to grant only permissions they already have', () => {
		const userPermissions = Permissions.MANAGE_GUILD | Permissions.SEND_MESSAGES;
		expect(canAuthorizeBotInvite({userPermissions, requestedPermissions: Permissions.SEND_MESSAGES})).toBe(true);
		expect(canAuthorizeBotInvite({userPermissions, requestedPermissions: Permissions.BAN_MEMBERS})).toBe(false);
	});
	it('allows administrators to grant any known bot permissions', () => {
		expect(
			canAuthorizeBotInvite({
				userPermissions: Permissions.ADMINISTRATOR,
				requestedPermissions: Permissions.BAN_MEMBERS | Permissions.MANAGE_MESSAGES,
			}),
		).toBe(true);
	});
	it('ignores unknown permission bits consistently with bot role creation', () => {
		const unknownBit = 1n << 60n;
		expect(normalizeBotInvitePermissions(unknownBit)).toBe(0n);
		expect(
			canAuthorizeBotInvite({
				userPermissions: Permissions.MANAGE_GUILD,
				requestedPermissions: unknownBit,
			}),
		).toBe(true);
	});
});
