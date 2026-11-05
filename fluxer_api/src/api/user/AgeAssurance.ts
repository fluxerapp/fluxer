// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {UserID} from '@app/api/BrandedTypes';
import {executeConditional, fetchMany} from '@app/api/database/CassandraQueryExecution';
import {
	type AppAttestOutcome,
	type AppAttestRejectionCode,
	createUsersServiceClient,
} from '@app/api/infrastructure/UsersServiceClient';
import type {User} from '@app/api/models/User';
import {AppAttestKeyLookup, AppAttestKeys} from '@app/api/Tables';
import {settingsForAge} from '@app/api/user/DateOfBirth';
import {mapUserSettingsToResponse, mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {canUserAccessNsfwContent} from '@app/api/utils/AgeUtils';
import type {AppAttestChallengePurpose} from '@fluxer/constants/src/AgeAssuranceConstants';
import {AppAttestAssertionInvalidError} from '@fluxer/errors/src/domains/user/AppAttestAssertionInvalidError';
import {AppAttestAttestationInvalidError} from '@fluxer/errors/src/domains/user/AppAttestAttestationInvalidError';
import {AppAttestChallengeInvalidError} from '@fluxer/errors/src/domains/user/AppAttestChallengeInvalidError';
import {AppAttestKeyInUseError} from '@fluxer/errors/src/domains/user/AppAttestKeyInUseError';
import {AppAttestKeyNotRegisteredError} from '@fluxer/errors/src/domains/user/AppAttestKeyNotRegisteredError';
import {AppAttestNotAvailableError} from '@fluxer/errors/src/domains/user/AppAttestNotAvailableError';
import {AppAttestRequestInvalidError} from '@fluxer/errors/src/domains/user/AppAttestRequestInvalidError';
import type {
	AgeAssuranceSubmitRequest,
	AgeAssuranceSubmitResponse,
	AppAttestChallengeResponse,
	AppAttestKeyRegisterRequest,
} from '@fluxer/schema/src/domains/user/AgeAssuranceSchemas';

const FETCH_APP_ATTEST_KEYS_FOR_USER_CQL = AppAttestKeys.selectCql({
	columns: ['key_id', 'app_id'],
	where: AppAttestKeys.where.eq('user_id'),
});

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

export async function createAppAttestChallenge(
	userId: UserID,
	purpose: AppAttestChallengePurpose,
): Promise<AppAttestChallengeResponse> {
	return unwrap(await createUsersServiceClient().appAttestCreateChallenge(userId, purpose));
}

export async function registerAppAttestKey(userId: UserID, request: AppAttestKeyRegisterRequest): Promise<void> {
	unwrap(
		await createUsersServiceClient().appAttestRegisterKey({
			userId,
			keyId: request.key_id,
			attestationBase64: request.attestation,
			challenge: request.challenge,
		}),
	);
}

export async function submitAttestedAge(
	ctx: ApiContext,
	user: User,
	request: AgeAssuranceSubmitRequest,
): Promise<AgeAssuranceSubmitResponse> {
	const accepted = unwrap(
		await createUsersServiceClient().appAttestSubmitAge({
			userId: user.id,
			keyId: request.key_id,
			assertionBase64: request.assertion,
			clientDataBase64: request.client_data,
		}),
	);
	const attestedAt = new Date(accepted.attested_at);
	if (user.ageAttestedAt !== null && user.ageAttestedAt.getTime() > attestedAt.getTime()) {
		return {age_floor: accepted.age_floor};
	}
	const updated = await ctx.services.users.patchUpsert(
		user.id,
		{attested_age_floor: accepted.age_floor, age_attested_at: attestedAt},
		user.toRow(),
	);
	if (canUserAccessNsfwContent(user) !== canUserAccessNsfwContent(updated)) {
		const settings = await ctx.services.users.findSettings(user.id);
		const adjusted = settings ? settingsForAge(settings, canUserAccessNsfwContent(updated)) : null;
		if (adjusted) {
			const saved = await ctx.services.users.upsertSettings(adjusted.toRow());
			await ctx.services.gateway.dispatchPresence({
				userId: user.id,
				event: 'USER_SETTINGS_UPDATE',
				data: mapUserSettingsToResponse({settings: saved}),
			});
		}
	}
	if (user.attestedAgeFloor !== updated.attestedAgeFloor) {
		await ctx.services.gateway.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(updated),
		});
	}
	return {age_floor: accepted.age_floor};
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
