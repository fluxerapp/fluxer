// SPDX-License-Identifier: AGPL-3.0-or-later

import {overwriteGrantedBits} from '@app/api/utils/PermissionUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

const BOT_OVERWRITE = {
	allow: Permissions.ADD_REACTIONS | Permissions.SEND_MESSAGES | Permissions.MANAGE_MESSAGES | Permissions.PIN_MESSAGES,
	deny: 0n,
};

describe('overwriteGrantedBits', () => {
	it('grants nothing when an overwrite is resubmitted unmodified', () => {
		expect(overwriteGrantedBits(BOT_OVERWRITE, {...BOT_OVERWRITE})).toBe(0n);
	});

	it('ignores allow bits that were already set when another bit is flipped', () => {
		const before = {allow: Permissions.PIN_MESSAGES, deny: 0n};
		const after = {allow: Permissions.PIN_MESSAGES | Permissions.SEND_MESSAGES, deny: 0n};
		expect(overwriteGrantedBits(before, after)).toBe(Permissions.SEND_MESSAGES);
	});

	it('grants nothing when an allow bit is withdrawn', () => {
		const before = {allow: Permissions.SEND_MESSAGES | Permissions.MANAGE_MESSAGES, deny: 0n};
		const after = {allow: Permissions.SEND_MESSAGES, deny: 0n};
		expect(overwriteGrantedBits(before, after)).toBe(0n);
	});

	it('grants nothing when a deny is added for a permission the editor lacks', () => {
		const after = {allow: Permissions.VIEW_CHANNEL, deny: Permissions.MANAGE_MESSAGES};
		expect(overwriteGrantedBits(null, after)).toBe(Permissions.VIEW_CHANNEL);
	});

	it('grants the bits lifted out of deny', () => {
		const before = {allow: 0n, deny: Permissions.ADD_REACTIONS | Permissions.SEND_MESSAGES};
		const after = {allow: 0n, deny: Permissions.ADD_REACTIONS};
		expect(overwriteGrantedBits(before, after)).toBe(Permissions.SEND_MESSAGES);
	});

	it('treats a removed overwrite as granting everything it denied', () => {
		const before = {allow: Permissions.SEND_MESSAGES, deny: Permissions.ADD_REACTIONS};
		expect(overwriteGrantedBits(before, undefined)).toBe(Permissions.ADD_REACTIONS);
	});
});
