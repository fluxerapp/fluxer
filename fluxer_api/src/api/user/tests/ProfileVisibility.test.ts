// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {EMPTY_USER_ROW, type UserRow} from '@app/api/database/types/UserTypes';
import {mapGuildMemberToResponse} from '@app/api/guild/GuildModel';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {LimitConfigService, resetGlobalLimitConfigServiceForTesting} from '@app/api/limits/LimitConfigService';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {GuildMember} from '@app/api/models/GuildMember';
import {User} from '@app/api/models/User';
import {isHiddenPartial, isProfileHidden, isUnderEnforcement, profilePseudonym} from '@app/api/user/ProfileVisibility';
import {
	hasPartialUserFieldsChanged,
	mapGuildMemberToProfileResponse,
	mapUserToPartialResponse,
	mapUserToPrivateResponse,
	mapUserToProfileResponse,
} from '@app/api/user/UserMappers';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {NON_SELF_HOSTED_RESERVED_DISCRIMINATORS} from '@fluxer/constants/src/DiscriminatorConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {InMemoryProvider} from '@pkgs/cache/src/providers/InMemoryProvider';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';

const NOW = Date.now();
const HOUR = 3_600_000;
const USER_ID = 1174109840998400001n;
const PROFILE_HIDDEN_BIT = Number(UserFlags.PROFILE_HIDDEN);
const GENERATED_USERNAME = /^[A-Z][a-z0-9_]*[A-Z][a-z0-9_]*$/;

function user(overrides: Partial<UserRow> = {}): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: createUserID(USER_ID),
		username: 'ada',
		discriminator: 7,
		global_name: 'Ada Lovelace',
		avatar_hash: 'a1b2c3',
		avatar_color: 42,
		banner_hash: 'b4n',
		banner_color: 7,
		bio: 'analytical engine enjoyer',
		pronouns: 'she/her',
		accent_color: 99,
		flags: 0n,
		...overrides,
	});
}

const STATES: Array<[string, Partial<UserRow>, boolean]> = [
	['a normal account', {}, false],
	['a staff ban', {flags: UserFlags.DISABLED, temp_banned_until: new Date(NOW + HOUR)}, true],
	['a lifted ban', {flags: 0n, temp_banned_until: null}, false],
	['a ban that ran out', {flags: UserFlags.DISABLED, temp_banned_until: new Date(NOW - HOUR)}, false],
	['a self-disabled account', {flags: UserFlags.DISABLED}, false],
	['the spammer flag', {flags: UserFlags.SPAMMER}, true],
	[
		'a pending deletion for abuse',
		{
			flags: UserFlags.DELETED,
			pending_deletion_at: new Date(NOW + 30 * 24 * HOUR),
			deletion_reason_code: DeletionReasons.HATE_SPEECH_OR_EXTREMIST_CONTENT,
		},
		true,
	],
	[
		'a self-requested deletion',
		{
			flags: UserFlags.SELF_DELETED,
			pending_deletion_at: new Date(NOW + 14 * 24 * HOUR),
			deletion_reason_code: DeletionReasons.USER_REQUESTED,
		},
		false,
	],
	[
		'an inactivity deletion',
		{
			flags: UserFlags.DELETED,
			pending_deletion_at: new Date(NOW + 30 * 24 * HOUR),
			deletion_reason_code: DeletionReasons.INACTIVITY,
		},
		false,
	],
	['a cancelled deletion', {flags: 0n, pending_deletion_at: null, deletion_reason_code: null}, false],
	['a hidden profile', {flags: UserFlags.PROFILE_HIDDEN}, true],
	['a hidden system account', {flags: UserFlags.PROFILE_HIDDEN, system: true}, false],
];

describe('profile visibility', () => {
	beforeAll(() => {
		new LimitConfigService(new InstanceConfigRepository(), new InMemoryProvider()).setAsGlobalInstance();
	});

	afterAll(() => {
		resetGlobalLimitConfigServiceForTesting();
	});

	it.each(STATES)('%s', (_name, overrides, hidden) => {
		const subject = user(overrides);
		expect(isProfileHidden(subject, NOW)).toBe(hidden);
		const partial = mapUserToPartialResponse(subject);
		expect(partial.flags & PROFILE_HIDDEN_BIT).toBe(0);
		if (hidden) {
			expect(partial).toMatchObject({
				id: subject.id.toString(),
				...profilePseudonym(USER_ID),
				global_name: null,
				avatar: null,
				avatar_color: null,
			});
			expect(isHiddenPartial(partial)).toBe(true);
			expect(mapUserToProfileResponse(subject)).toEqual({
				bio: null,
				pronouns: null,
				banner: null,
				banner_color: null,
				accent_color: null,
			});
		} else {
			expect(isHiddenPartial(partial)).toBe(false);
			expect(partial.username).toBe('ada');
			expect(partial.global_name).toBe('Ada Lovelace');
			expect(partial.avatar).toBe('a1b2c3');
			expect(mapUserToProfileResponse(subject)).toMatchObject({bio: 'analytical engine enjoyer', pronouns: 'she/her'});
		}
	});

	it('keeps the stored profile for the account owner', () => {
		const own = mapUserToPrivateResponse(user({flags: UserFlags.PROFILE_HIDDEN}));
		expect(own).toMatchObject({
			username: 'ada',
			discriminator: '0007',
			global_name: 'Ada Lovelace',
			avatar: 'a1b2c3',
			bio: 'analytical engine enjoyer',
			pronouns: 'she/her',
			accent_color: 99,
		});
		expect(own.flags & PROFILE_HIDDEN_BIT).toBe(0);
	});

	it('gives each account one stable pseudonym in the generated username format', () => {
		const first = profilePseudonym(USER_ID);
		expect(profilePseudonym(USER_ID)).toEqual(first);
		expect(mapUserToPartialResponse(user({flags: UserFlags.SPAMMER}))).toMatchObject(first);
		expect(mapUserToPartialResponse(user({flags: UserFlags.PROFILE_HIDDEN}))).toMatchObject(first);
		const seen = new Set<string>();
		for (let offset = 0n; offset < 200n; offset++) {
			const pseudonym = profilePseudonym(USER_ID + offset);
			seen.add(`${pseudonym.username}#${pseudonym.discriminator}`);
			expect(pseudonym.username).toMatch(GENERATED_USERNAME);
			expect(pseudonym.username.length).toBeLessThanOrEqual(32);
			expect(pseudonym.discriminator).toMatch(/^\d{4}$/);
			const discriminator = Number(pseudonym.discriminator);
			expect(discriminator).toBeGreaterThanOrEqual(1);
			expect(discriminator).toBeLessThanOrEqual(9999);
			expect(NON_SELF_HOSTED_RESERVED_DISCRIMINATORS.has(discriminator)).toBe(false);
		}
		expect(seen.size).toBe(200);
		expect(profilePseudonym(USER_ID, 'another-secret')).not.toEqual(first);
	});

	it('restores the stored profile as soon as the mask lifts', () => {
		const masked = mapUserToPartialResponse(user({flags: UserFlags.PROFILE_HIDDEN}));
		const lifted = mapUserToPartialResponse(user({flags: 0n}));
		expect(masked.username).not.toBe('ada');
		expect(lifted).toMatchObject({
			username: 'ada',
			discriminator: '0007',
			global_name: 'Ada Lovelace',
			avatar: 'a1b2c3',
		});
		expect(isHiddenPartial(lifted)).toBe(false);
	});

	it('does not mistake a deleted account for a masked one', () => {
		const deleted = mapUserToPartialResponse(user({flags: UserFlags.DELETED | UserFlags.SPAMMER}));
		expect(deleted).toMatchObject({username: 'DeletedUser', discriminator: '0000', global_name: 'Deleted User'});
		expect(isHiddenPartial(deleted)).toBe(false);
	});

	it('treats hiding and restoring as a partial change so clients are told both ways', () => {
		const open = user();
		const banned = user({flags: UserFlags.DISABLED, temp_banned_until: new Date(NOW + HOUR)});
		expect(hasPartialUserFieldsChanged(open, banned)).toBe(true);
		expect(hasPartialUserFieldsChanged(banned, open)).toBe(true);
		expect(hasPartialUserFieldsChanged(open, user({flags: UserFlags.HAS_SESSION_STARTED}))).toBe(false);
	});

	it('only counts staff enforcement as enforcement', () => {
		expect(isUnderEnforcement(user({flags: UserFlags.PROFILE_HIDDEN}), NOW)).toBe(false);
		expect(isUnderEnforcement(user({flags: UserFlags.SPAMMER}), NOW)).toBe(true);
	});

	it('hides guild-specific profile details for a hidden member', async () => {
		const member = new GuildMember({
			guild_id: createGuildID(5n),
			user_id: createUserID(USER_ID),
			joined_at: new Date(NOW - HOUR),
			nick: 'Countess',
			avatar_hash: 'm4v',
			banner_hash: 'm8n',
			bio: 'member bio',
			pronouns: 'she/her',
			accent_color: 12,
			join_source_type: null,
			source_invite_code: null,
			inviter_id: null,
			deaf: false,
			mute: false,
			communication_disabled_until: null,
			role_ids: null,
			is_premium_sanitized: false,
			temporary: false,
			profile_flags: null,
			version: 1,
		});
		const cache = (partial: ReturnType<typeof mapUserToPartialResponse>) =>
			({getUserPartialResponse: async () => partial}) as unknown as Pick<UserCacheService, 'getUserPartialResponse'>;
		const hidden = await mapGuildMemberToResponse(
			member,
			cache(mapUserToPartialResponse(user({flags: UserFlags.SPAMMER}))),
			createRequestCache(),
		);
		expect(hidden).toMatchObject({nick: null, avatar: null, banner: null, accent_color: null});
		expect(hidden.user).toMatchObject(profilePseudonym(USER_ID));
		const shown = await mapGuildMemberToResponse(member, cache(mapUserToPartialResponse(user())), createRequestCache());
		expect(shown).toMatchObject({nick: 'Countess', avatar: 'm4v', banner: 'm8n', accent_color: 12});
		expect(mapGuildMemberToProfileResponse(member, {hidden: true})).toEqual({
			bio: null,
			pronouns: null,
			banner: null,
			accent_color: null,
		});
		expect(mapGuildMemberToProfileResponse(member)).toMatchObject({bio: 'member bio', pronouns: 'she/her'});
	});
});
