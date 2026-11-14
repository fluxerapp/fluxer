// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {type LabsRoleState, labsRolesFor} from '@app/api/operator_pass/OperatorPassLabsRole';
import type {ProfileBadgeKey} from '@fluxer/constants/src/BadgeConstants';
import {
	PremiumFlags,
	type ProfilePrivacyLevel,
	ProfilePrivacyLevels,
	UserFlags,
	type UserPremiumType,
	UserPremiumTypes,
} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, expect, test} from 'vitest';

const original = {...Config.operatorPass};

beforeEach(() => {
	Config.operatorPass.labsGuildId = 7001n;
	Config.operatorPass.labsOperatorRoleId = 7002n;
	Config.operatorPass.labsVisionaryRoleId = 7003n;
});

afterEach(() => {
	Object.assign(Config.operatorPass, original);
});

interface Account {
	premiumType?: UserPremiumType | null;
	hidden?: Array<ProfileBadgeKey>;
	premiumFlags?: number;
	flags?: bigint;
	profilePrivacy?: ProfilePrivacyLevel;
	entitled?: boolean;
}

function state({
	premiumType = null,
	hidden = [],
	premiumFlags = 0,
	flags = 0n,
	profilePrivacy = ProfilePrivacyLevels.ALL_GUILDS,
	entitled = false,
}: Account): LabsRoleState {
	return {
		user: {
			premiumType,
			premiumFlags,
			premiumSince: null,
			premiumLifetimeSequence: null,
			hiddenProfileBadges: new Set(hidden),
			flags,
			isSystem: false,
			tempBannedUntil: null,
			pendingDeletionAt: null,
			deletionReasonCode: null,
		},
		profilePrivacy,
		entitled,
	};
}

const lifetime = UserPremiumTypes.LIFETIME;

const none = {operator: false, visionary: false, memberAccess: false};

const accessOnly = {operator: false, visionary: false, memberAccess: true};

test('Operators and Visionaries who show the badge to every community get the matching role and no member access', () => {
	expect(labsRolesFor(state({entitled: true}))).toEqual({operator: true, visionary: false, memberAccess: false});
	expect(labsRolesFor(state({premiumType: lifetime}))).toEqual({operator: false, visionary: true, memberAccess: false});
	expect(labsRolesFor(state({premiumType: lifetime, entitled: true}))).toEqual({
		operator: true,
		visionary: true,
		memberAccess: false,
	});
	expect(labsRolesFor(state({premiumType: UserPremiumTypes.SUBSCRIPTION}))).toEqual(none);
});

test('a hidden or masked badge drops the matching role and gives member access to #operators', () => {
	expect(labsRolesFor(state({entitled: true, hidden: ['operator_pass']}))).toEqual(accessOnly);
	expect(labsRolesFor(state({premiumType: lifetime, hidden: ['visionary']}))).toEqual(accessOnly);
	expect(labsRolesFor(state({premiumType: lifetime, premiumFlags: PremiumFlags.BADGE_MASKED}))).toEqual(accessOnly);
});

test('a role that still shows is enough for #operators, so a person with one hidden badge gets no member access', () => {
	expect(labsRolesFor(state({premiumType: lifetime, entitled: true, hidden: ['operator_pass']}))).toEqual({
		operator: false,
		visionary: true,
		memberAccess: false,
	});
});

test.each([
	['friends and small communities only', {profilePrivacy: ProfilePrivacyLevels.SMALL_GUILDS_ONLY}],
	['friends only', {profilePrivacy: ProfilePrivacyLevels.FRIENDS_ONLY}],
	['a hidden profile', {flags: UserFlags.PROFILE_HIDDEN}],
	['enforcement', {flags: UserFlags.SPAMMER}],
] as const)('%s drops the Operator and Visionary roles and gives member access', (_name, limited) => {
	expect(labsRolesFor(state({premiumType: lifetime, entitled: true, ...limited}))).toEqual(accessOnly);
});

test('hiding only the date or count on the badge keeps every role', () => {
	const premiumFlags =
		PremiumFlags.OPERATOR_PASS_BADGE_TIMESTAMP_HIDDEN |
		PremiumFlags.OPERATOR_PASS_BADGE_COUNT_HIDDEN |
		PremiumFlags.BADGE_TIMESTAMP_HIDDEN;
	expect(
		labsRolesFor(state({premiumType: lifetime, entitled: true, premiumFlags, hidden: ['visionary_number']})),
	).toEqual({operator: true, visionary: true, memberAccess: false});
});

test('turning off #operators takes away every kind of Labs access, for Operators and Visionaries alike', () => {
	const premiumFlags = PremiumFlags.OPERATORS_CHANNEL_HIDDEN;
	expect(labsRolesFor(state({entitled: true, premiumFlags}))).toEqual(none);
	expect(labsRolesFor(state({premiumType: lifetime, premiumFlags}))).toEqual(none);
	expect(labsRolesFor(state({premiumType: lifetime, entitled: true, premiumFlags, hidden: ['operator_pass']}))).toEqual(
		none,
	);
});
