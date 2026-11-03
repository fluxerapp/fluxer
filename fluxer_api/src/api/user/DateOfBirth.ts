// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {AgeBand} from '@app/api/infrastructure/activity/Contract.generated';
import {usesUniqueUsernames} from '@app/api/instance/AccountIdentityModeCache';
import {getDateOfBirthPolicy} from '@app/api/instance/AccountsConfigCache';
import {instanceCollectsDateOfBirth} from '@app/api/instance/DateOfBirthCollectionCache';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getDiscriminatorService, getKVAccountDeletionQueue} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {UserSettings} from '@app/api/models/UserSettings';
import {scheduleSystemDeletion} from '@app/api/user/AccountCleanup';
import type {TierContext} from '@app/api/user/AccountTier';
import {executeEntries, type GuildEntryDeps, takePendingEntries} from '@app/api/user/GuildEntry';
import {deriveAvailableUsername} from '@app/api/user/UniqueUsernames';
import {mapUserSettingsToResponse, mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {USERNAME_MODE_DISCRIMINATOR} from '@app/api/user/UserTag';
import {BELOW_MINIMUM_AGE_TRAIT, DOB_DEFERRED_TRAIT} from '@app/api/user/UserTraits';
import {calculateAge, parseDateOfBirth} from '@app/api/utils/AgeUtils';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {generateRandomUsername} from '@app/api/utils/UsernameGenerator';
import {DateOfBirthPolicies} from '@fluxer/constants/src/AccountConfigConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {getRegionalMinimumAge} from '@fluxer/constants/src/RegionalMinimumAge';
import {SensitiveMediaFilterLevel} from '@fluxer/constants/src/UserConstants';
import {AccountBelowMinimumAgeError} from '@fluxer/errors/src/domains/auth/AccountBelowMinimumAgeError';
import {DateOfBirthAlreadySetError} from '@fluxer/errors/src/domains/auth/DateOfBirthAlreadySetError';
import {NotFoundError} from '@fluxer/errors/src/domains/core/NotFoundError';
import type {DateOfBirthUpdateResponse} from '@fluxer/schema/src/domains/user/AccountSchemas';

const UNDERAGE_DELETION_DELAY_MS = 72 * 3600 * 1000;
const ADULT_AGE = 18;

type DateOfBirthUser = Pick<User, 'isBot' | 'isSystem' | 'dateOfBirth'> & {traits: ReadonlySet<string>};

export function isDateOfBirthMissing(user: DateOfBirthUser): boolean {
	return (
		instanceCollectsDateOfBirth() &&
		user.dateOfBirth === null &&
		!user.isBot &&
		!user.isSystem &&
		!user.traits.has(BELOW_MINIMUM_AGE_TRAIT)
	);
}

export function isDateOfBirthEnforced(user: DateOfBirthUser): boolean {
	if (!isDateOfBirthMissing(user)) return false;
	switch (getDateOfBirthPolicy()) {
		case DateOfBirthPolicies.OFF:
		case DateOfBirthPolicies.ASK:
			return false;
		case DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS:
			return user.traits.has(DOB_DEFERRED_TRAIT);
		case DateOfBirthPolicies.REQUIRE_ALL:
			return true;
	}
}

function settingsForAge(settings: UserSettings, adult: boolean): UserSettings | null {
	const teen = UserSettings.getDefaultUserSettings({userId: settings.userId, locale: settings.locale, isAdult: false});
	const adultDefaults = UserSettings.getDefaultUserSettings({
		userId: settings.userId,
		locale: settings.locale,
		isAdult: true,
	});
	const [from, to] = adult ? [teen, adultDefaults] : [adultDefaults, teen];
	const row = settings.toRow();
	let changed = false;
	for (const key of ['explicit_content_filter', 'friend_source_flags', 'sensitive_content_friend_dm_filter'] as const) {
		if (row[key] === from[key] && row[key] !== to[key]) {
			Object.assign(row, {[key]: to[key]});
			changed = true;
		}
	}
	if (!adult) {
		for (const key of [
			'sensitive_content_friend_dm_filter',
			'sensitive_content_non_friend_dm_filter',
			'sensitive_content_guild_filter',
		] as const) {
			if (row[key] === SensitiveMediaFilterLevel.SHOW) {
				Object.assign(row, {[key]: teen[key]});
				changed = true;
			}
		}
	}
	return changed ? new UserSettings(row) : null;
}

async function releaseUsername(ctx: ApiContext, user: User): Promise<{username: string; discriminator: number}> {
	const base = generateRandomUsername();
	if (usesUniqueUsernames()) {
		return {
			username: await deriveAvailableUsername(ctx.services.users, base),
			discriminator: USERNAME_MODE_DISCRIMINATOR,
		};
	}
	const result = await getDiscriminatorService().generateDiscriminator({username: base});
	return result.available && result.discriminator !== -1
		? {username: base, discriminator: result.discriminator}
		: {username: user.username, discriminator: user.discriminator};
}

function emitDateOfBirthSet(userId: User['id'], result: AgeBand): void {
	void emitActivity('date_of_birth_set', userId.toString(), {user_id: userId.toString(), result});
}

async function handleUnderage(ctx: ApiContext, user: User): Promise<never> {
	const traits = user.traits;
	traits.delete(DOB_DEFERRED_TRAIT);
	traits.add(BELOW_MINIMUM_AGE_TRAIT);
	const released = await releaseUsername(ctx, user);
	const updated = await ctx.services.users.patchUpsert(
		user.id,
		{traits, date_of_birth: null, username: released.username, discriminator: released.discriminator},
		user.toRow(),
	);
	await scheduleSystemDeletion(
		{users: ctx.services.users, gateway: ctx.services.gateway, deletionQueue: getKVAccountDeletionQueue()},
		updated,
		{
			reasonCode: DeletionReasons.BELOW_MINIMUM_AGE,
			delayMs: UNDERAGE_DELETION_DELAY_MS,
			cancelledBySignIn: false,
			endSessions: true,
		},
	);
	emitDateOfBirthSet(user.id, 'underage');
	throw new AccountBelowMinimumAgeError();
}

export async function setDateOfBirth(
	ctx: ApiContext,
	deps: GuildEntryDeps,
	params: {
		user: User;
		dateOfBirth: string;
		clientIp: string;
		tierContext: TierContext;
		requestCache: RequestCache;
	},
): Promise<DateOfBirthUpdateResponse> {
	const {user} = params;
	if (!instanceCollectsDateOfBirth()) {
		throw new NotFoundError({code: APIErrorCodes.NOT_FOUND});
	}
	if (user.dateOfBirth !== null) throw new DateOfBirthAlreadySetError();
	const dateOfBirth = parseDateOfBirth(params.dateOfBirth);
	const geoip = await lookupGeoip(params.clientIp);
	const age = calculateAge(params.dateOfBirth);
	if (age < getRegionalMinimumAge(geoip.countryCode)) {
		return await handleUnderage(ctx, user);
	}
	const traits = user.traits;
	traits.delete(DOB_DEFERRED_TRAIT);
	const updated = await ctx.services.users.patchUpsert(
		user.id,
		{date_of_birth: dateOfBirth, traits: traits.size > 0 ? traits : null},
		user.toRow(),
	);
	const adult = age >= ADULT_AGE;
	const settings = await ctx.services.users.findSettings(user.id);
	const adjusted = settings ? settingsForAge(settings, adult) : null;
	if (adjusted) {
		const saved = await ctx.services.users.upsertSettings(adjusted.toRow());
		await ctx.services.gateway.dispatchPresence({
			userId: user.id,
			event: 'USER_SETTINGS_UPDATE',
			data: mapUserSettingsToResponse({settings: saved}),
		});
	}
	await ctx.services.gateway.dispatchPresence({
		userId: user.id,
		event: 'USER_UPDATE',
		data: mapUserToPrivateResponse(updated),
	});
	for (const guildId of await ctx.services.users.getUserGuildIds(user.id)) {
		await ctx.services.gateway.joinGuild({userId: user.id, guildId});
	}
	emitDateOfBirthSet(user.id, adult ? 'adult' : 'teen');
	const entry = await executeEntries(deps, {
		user: updated,
		entries: await takePendingEntries(ctx.services.kv, user.id),
		tierContext: params.tierContext,
		requestCache: params.requestCache,
		ip: params.clientIp,
	});
	return {user: mapUserToPrivateResponse(updated), entry};
}
