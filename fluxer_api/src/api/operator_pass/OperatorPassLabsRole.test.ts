// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {labsRolesFor} from '@app/api/operator_pass/OperatorPassLabsRole';
import type {ProfileBadgeKey} from '@fluxer/constants/src/BadgeConstants';
import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, expect, test} from 'vitest';

const original = {...Config.operatorPass};

beforeEach(() => {
	Config.operatorPass.labsGuildId = 7001n;
	Config.operatorPass.labsOperatorRoleId = 7002n;
	Config.operatorPass.labsVisionaryRoleId = 7003n;
	Config.operatorPass.labsAccessRoleId = 7004n;
});

afterEach(() => {
	Object.assign(Config.operatorPass, original);
});

function account(premiumType: number | null, hidden: Array<ProfileBadgeKey> = [], premiumFlags = 0) {
	return {premiumType, premiumFlags, hiddenProfileBadges: new Set(hidden)} as Parameters<typeof labsRolesFor>[0];
}

test('Labs shows the Operator and Visionary roles only to people who show the badge, and everyone with access gets the access role', () => {
	expect(labsRolesFor(account(null), true)).toEqual({operator: true, visionary: false, access: true});
	expect(labsRolesFor(account(null, ['operator_pass']), true)).toEqual({
		operator: false,
		visionary: false,
		access: true,
	});
	expect(labsRolesFor(account(UserPremiumTypes.LIFETIME), false)).toEqual({
		operator: false,
		visionary: true,
		access: true,
	});
	expect(labsRolesFor(account(UserPremiumTypes.LIFETIME, ['visionary']), false)).toEqual({
		operator: false,
		visionary: false,
		access: true,
	});
	expect(labsRolesFor(account(UserPremiumTypes.LIFETIME, [], PremiumFlags.BADGE_MASKED), false)).toEqual({
		operator: false,
		visionary: false,
		access: true,
	});
	expect(labsRolesFor(account(UserPremiumTypes.SUBSCRIPTION), false)).toEqual({
		operator: false,
		visionary: false,
		access: false,
	});
});

test('without an access role, Operators and Visionaries keep their roles whether or not they show the badge', () => {
	Config.operatorPass.labsAccessRoleId = null;
	expect(labsRolesFor(account(null, ['operator_pass']), true)).toEqual({
		operator: true,
		visionary: false,
		access: false,
	});
	expect(labsRolesFor(account(UserPremiumTypes.LIFETIME, ['visionary']), false)).toEqual({
		operator: false,
		visionary: true,
		access: false,
	});
});
