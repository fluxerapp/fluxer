// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	computeGuildVisitorChannelPermissions,
	type GuildVisitorChannelInput,
} from '@fluxer/constants/src/GuildVisitConstants';
import cases from '@fluxer/constants/src/GuildVisitorPermissionCases.json' with {type: 'json'};
import {describe, expect, it} from 'vitest';

interface PermissionCase {
	name: string;
	kind: 'permissions';
	input: {
		guildId: string;
		everyonePermissions: string;
		guildNsfw: boolean;
		channel: GuildVisitorChannelInput;
		parent?: {nsfw_override: boolean | null};
		thread?: {private: boolean};
		adultContentAllowed: boolean;
	};
	expected: string;
}

const rows = (cases as Array<{kind: string}>).filter((row): row is PermissionCase => row.kind === 'permissions');

describe('guild visitor permissions', () => {
	for (const row of rows) {
		it(row.name, () => {
			const input = {...row.input, everyonePermissions: BigInt(row.input.everyonePermissions)};
			expect(computeGuildVisitorChannelPermissions(input).toString()).toBe(row.expected);
		});
	}

	it('has permission cases', () => {
		expect(rows.length).toBeGreaterThan(0);
	});
});
