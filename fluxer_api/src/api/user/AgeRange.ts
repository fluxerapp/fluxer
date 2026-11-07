// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {executeConditional, fetchMany} from '@app/api/database/CassandraQueryExecution';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {
	type AppAttestAgeAccepted,
	type AppAttestOutcome,
	type AppAttestRejectionCode,
	createUsersServiceClient,
	type PlayIntegrityOutcome,
	type PlayIntegrityRejectionCode,
} from '@app/api/infrastructure/UsersServiceClient';
import type {User} from '@app/api/models/User';
import {AppAttestKeyLookup, AppAttestKeys} from '@app/api/Tables';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {type AppAttestChallengePurpose, AttestedAgeFloors} from '@fluxer/constants/src/AgeRangeConstants';
import {FeatureNotAvailableSelfHostedError} from '@fluxer/errors/src/domains/core/FeatureNotAvailableSelfHostedError';
import {AgeRangeNotAdultError} from '@fluxer/errors/src/domains/user/AgeRangeNotAdultError';
import {AgeRangeNotNeededError} from '@fluxer/errors/src/domains/user/AgeRangeNotNeededError';
import {AppAttestAssertionInvalidError} from '@fluxer/errors/src/domains/user/AppAttestAssertionInvalidError';
import {AppAttestAttestationInvalidError} from '@fluxer/errors/src/domains/user/AppAttestAttestationInvalidError';
import {AppAttestChallengeInvalidError} from '@fluxer/errors/src/domains/user/AppAttestChallengeInvalidError';
import {AppAttestKeyInUseError} from '@fluxer/errors/src/domains/user/AppAttestKeyInUseError';
import {AppAttestKeyNotRegisteredError} from '@fluxer/errors/src/domains/user/AppAttestKeyNotRegisteredError';
import {AppAttestNotAvailableError} from '@fluxer/errors/src/domains/user/AppAttestNotAvailableError';
import {AppAttestRequestInvalidError} from '@fluxer/errors/src/domains/user/AppAttestRequestInvalidError';
import {PlayIntegrityAppNotRecognizedError} from '@fluxer/errors/src/domains/user/PlayIntegrityAppNotRecognizedError';
import {PlayIntegrityChallengeInvalidError} from '@fluxer/errors/src/domains/user/PlayIntegrityChallengeInvalidError';
import {PlayIntegrityDeviceNotTrustedError} from '@fluxer/errors/src/domains/user/PlayIntegrityDeviceNotTrustedError';
import {PlayIntegrityNotAvailableError} from '@fluxer/errors/src/domains/user/PlayIntegrityNotAvailableError';
import {PlayIntegrityRequestInvalidError} from '@fluxer/errors/src/domains/user/PlayIntegrityRequestInvalidError';
import {PlayIntegrityTokenInvalidError} from '@fluxer/errors/src/domains/user/PlayIntegrityTokenInvalidError';
import {PlayIntegrityUnavailableError} from '@fluxer/errors/src/domains/user/PlayIntegrityUnavailableError';
import type {
	AgeRangeSubmitRequest,
	AgeRangeSubmitResponse,
	AppAttestChallengeResponse,
	AppAttestKeyRegisterRequest,
	PlayIntegrityAgeSubmitRequest,
} from '@fluxer/schema/src/domains/user/AgeRangeSchemas';
import type {GeoipResult} from '@pkgs/geoip/src/GeoipLookup';

const FETCH_APP_ATTEST_KEYS_FOR_USER_CQL = AppAttestKeys.selectCql({
	columns: ['key_id', 'app_id'],
	where: AppAttestKeys.where.eq('user_id'),
});

const LIST_APP_ATTEST_KEYS_FOR_EXPORT_CQL = AppAttestKeys.selectCql({
	columns: ['key_id', 'environment', 'counter', 'created_at', 'last_used_at'],
	where: AppAttestKeys.where.eq('user_id'),
});

export interface ExportedAppAttestKey {
	key_id: string;
	environment: string;
	counter: bigint;
	created_at: Date;
	last_used_at: Date | null;
}

function rejectionError(code: AppAttestRejectionCode): Error {
	switch (code) {
		case 'not_configured':
			return new AppAttestNotAvailableError();
		case 'invalid_request':
			return new AppAttestRequestInvalidError();
		case 'invalid_challenge':
			return new AppAttestChallengeInvalidError();
		case 'invalid_attestation':
			return new AppAttestAttestationInvalidError();
		case 'key_in_use':
			return new AppAttestKeyInUseError();
		case 'unknown_key':
			return new AppAttestKeyNotRegisteredError();
		case 'invalid_assertion':
			return new AppAttestAssertionInvalidError();
	}
}

function unwrap<T>(outcome: AppAttestOutcome<T>): T {
	if (!outcome.ok) throw rejectionError(outcome.code);
	return outcome.value;
}

function playIntegrityRejectionError(code: PlayIntegrityRejectionCode): Error {
	switch (code) {
		case 'not_configured':
			return new PlayIntegrityNotAvailableError();
		case 'invalid_request':
			return new PlayIntegrityRequestInvalidError();
		case 'invalid_challenge':
			return new PlayIntegrityChallengeInvalidError();
		case 'invalid_token':
			return new PlayIntegrityTokenInvalidError();
		case 'app_not_recognized':
			return new PlayIntegrityAppNotRecognizedError();
		case 'device_not_trusted':
			return new PlayIntegrityDeviceNotTrustedError();
		case 'unavailable':
			return new PlayIntegrityUnavailableError();
	}
}

function unwrapPlayIntegrity<T>(outcome: PlayIntegrityOutcome<T>): T {
	if (!outcome.ok) throw playIntegrityRejectionError(outcome.code);
	return outcome.value;
}

type GeoLookup = (clientIp: string) => Promise<Pick<GeoipResult, 'countryCode' | 'regionCode'>>;

let lookupClientGeo: GeoLookup = lookupGeoip;

export function setAgeRangeGeoLookupForTesting(lookup: GeoLookup | null): void {
	lookupClientGeo = lookup ?? lookupGeoip;
}

async function assertAgeRangeWanted(clientIp: string | null): Promise<void> {
	if (Config.instance.selfHosted) throw new FeatureNotAvailableSelfHostedError();
	if (clientIp === null) throw new AgeRangeNotNeededError();
	const geoip = await lookupClientGeo(clientIp);
	const regionCode = geoip.regionCode ?? null;
	const restricted = Config.instance.agePolicy.restricted.some(
		(entry) =>
			entry.countryCode === geoip.countryCode && (entry.regionCode === null || entry.regionCode === regionCode),
	);
	if (!restricted) throw new AgeRangeNotNeededError();
}

export async function createAppAttestChallenge(
	clientIp: string | null,
	userId: UserID,
	purpose: AppAttestChallengePurpose,
): Promise<AppAttestChallengeResponse> {
	await assertAgeRangeWanted(clientIp);
	return unwrap(await createUsersServiceClient().appAttestCreateChallenge(userId, purpose));
}

export async function registerAppAttestKey(
	clientIp: string | null,
	userId: UserID,
	body: AppAttestKeyRegisterRequest,
): Promise<void> {
	await assertAgeRangeWanted(clientIp);
	unwrap(
		await createUsersServiceClient().appAttestRegisterKey({
			userId,
			keyId: body.key_id,
			attestationBase64: body.attestation,
			challenge: body.challenge,
		}),
	);
}

export async function submitAttestedAge(
	ctx: ApiContext,
	clientIp: string | null,
	user: User,
	body: AgeRangeSubmitRequest,
): Promise<AgeRangeSubmitResponse> {
	try {
		await assertAgeRangeWanted(clientIp);
		const accepted = unwrap(
			await createUsersServiceClient().appAttestSubmitAge({
				userId: user.id,
				keyId: body.key_id,
				assertionBase64: body.assertion,
				clientDataBase64: body.client_data,
			}),
		);
		return await applyAttestedAge(ctx, user, accepted);
	} finally {
		await deleteAllAppAttestKeys(user.id);
	}
}

export async function createPlayIntegrityChallenge(
	clientIp: string | null,
	userId: UserID,
): Promise<AppAttestChallengeResponse> {
	await assertAgeRangeWanted(clientIp);
	return unwrapPlayIntegrity(await createUsersServiceClient().playIntegrityCreateChallenge(userId));
}

export async function submitPlayIntegrityAge(
	ctx: ApiContext,
	clientIp: string | null,
	user: User,
	body: PlayIntegrityAgeSubmitRequest,
): Promise<AgeRangeSubmitResponse> {
	await assertAgeRangeWanted(clientIp);
	const accepted = unwrapPlayIntegrity(
		await createUsersServiceClient().playIntegritySubmitAge({
			userId: user.id,
			packageName: body.package_name,
			integrityToken: body.integrity_token,
			clientDataBase64: body.client_data,
		}),
	);
	return applyAttestedAge(ctx, user, accepted);
}

async function applyAttestedAge(
	ctx: ApiContext,
	user: User,
	accepted: AppAttestAgeAccepted,
): Promise<AgeRangeSubmitResponse> {
	if (accepted.age_floor !== AttestedAgeFloors.ADULT) throw new AgeRangeNotAdultError();
	const attestedAt = new Date(accepted.attested_at);
	const storedAdultIsNewer =
		user.attestedAgeFloor === AttestedAgeFloors.ADULT &&
		user.ageAttestedAt !== null &&
		user.ageAttestedAt.getTime() >= attestedAt.getTime();
	if (!storedAdultIsNewer) {
		await saveAttestedAge(ctx, user, {attested_age_floor: AttestedAgeFloors.ADULT, age_attested_at: attestedAt});
	}
	return {age_floor: AttestedAgeFloors.ADULT};
}

export async function removeAttestedAge(ctx: ApiContext, user: User): Promise<User> {
	await deleteAllAppAttestKeys(user.id);
	if (user.attestedAgeFloor === null && user.ageAttestedAt === null) return user;
	return saveAttestedAge(ctx, user, {attested_age_floor: null, age_attested_at: null});
}

async function saveAttestedAge(
	ctx: ApiContext,
	user: User,
	patch: Pick<UserRow, 'attested_age_floor' | 'age_attested_at'>,
): Promise<User> {
	const updated = await ctx.services.users.patchUpsert(user.id, patch, user.toRow());
	if (user.attestedAgeFloor !== updated.attestedAgeFloor) {
		await ctx.services.gateway.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(updated),
		});
	}
	return updated;
}

export async function deleteAllAppAttestKeys(userId: UserID): Promise<void> {
	const keys = await fetchMany<{key_id: string; app_id: string}>(FETCH_APP_ATTEST_KEYS_FOR_USER_CQL, {
		user_id: userId,
	});
	for (const key of keys) {
		await executeConditional(
			AppAttestKeys.conditionalDeleteByPk({user_id: userId, key_id: key.key_id}, {app_id: key.app_id}),
		);
		await executeConditional(AppAttestKeyLookup.conditionalDeleteByPk({key_id: key.key_id}, {user_id: userId}));
	}
}

export async function listAppAttestKeysForExport(userId: UserID): Promise<Array<ExportedAppAttestKey>> {
	return fetchMany<ExportedAppAttestKey>(LIST_APP_ATTEST_KEYS_FOR_EXPORT_CQL, {user_id: userId});
}
