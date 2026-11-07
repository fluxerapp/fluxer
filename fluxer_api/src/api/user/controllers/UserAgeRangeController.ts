// SPDX-License-Identifier: AGPL-3.0-or-later

import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {
	createAppAttestChallenge,
	createPlayIntegrityChallenge,
	registerAppAttestKey,
	removeAttestedAge,
	submitAttestedAge,
	submitPlayIntegrityAge,
} from '@app/api/user/AgeRange';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {getRequestClientIp} from '@app/api/utils/RequestClientIp';
import {Validator} from '@app/api/Validator';
import {
	AgeRangeSubmitRequest,
	AgeRangeSubmitResponse,
	AppAttestChallengeRequest,
	AppAttestChallengeResponse,
	AppAttestKeyRegisterRequest,
	PlayIntegrityAgeSubmitRequest,
} from '@fluxer/schema/src/domains/user/AgeRangeSchemas';
import {UserPrivateResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export function UserAgeRangeController(app: HonoApp) {
	app.post(
		'/users/@me/app-attest/challenge',
		RateLimitMiddleware(RateLimitConfigs.USER_APP_ATTEST_CHALLENGE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', AppAttestChallengeRequest),
		OpenAPI({
			operationId: 'create_app_attest_challenge',
			summary: 'Create App Attest challenge',
			responseSchema: AppAttestChallengeResponse,
			statusCode: 200,
			errorStatusCodes: [403, 404],
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Creates a single use challenge for the iOS app, valid for 5 minutes. Use purpose attest before registering a new App Attest key, and purpose assert before submitting an age range. Returns APP_ATTEST_NOT_AVAILABLE with status 404 when this instance does not accept App Attest. Returns FEATURE_NOT_AVAILABLE_SELF_HOSTED with status 403 on a self-hosted instance, and AGE_RANGE_NOT_NEEDED with status 403 when the request does not come from a region where a date of birth typed in is not enough for mature content.',
		}),
		async (ctx) => {
			return ctx.json(
				await createAppAttestChallenge(getRequestClientIp(ctx), ctx.get('user').id, ctx.req.valid('json').purpose),
			);
		},
	);
	app.post(
		'/users/@me/app-attest/keys',
		RateLimitMiddleware(RateLimitConfigs.USER_APP_ATTEST_KEY_REGISTER),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', AppAttestKeyRegisterRequest),
		OpenAPI({
			operationId: 'register_app_attest_key',
			summary: 'Register App Attest key',
			responseSchema: null,
			statusCode: 204,
			errorStatusCodes: [403, 404, 409],
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Registers an App Attest key for the current user. The attestation must be made with clientDataHash set to the SHA-256 of the UTF-8 challenge string from a challenge with purpose attest. The server keeps the key id, public key, environment, App ID and counter for at most one hour, or until the next age range submit, and does not keep the attestation itself. Registering the same key again for the same user succeeds and changes nothing. Returns APP_ATTEST_NOT_AVAILABLE with status 404 when this instance does not accept App Attest. Returns FEATURE_NOT_AVAILABLE_SELF_HOSTED with status 403 on a self-hosted instance, and AGE_RANGE_NOT_NEEDED with status 403 when the request does not come from a region where a date of birth typed in is not enough for mature content.',
		}),
		async (ctx) => {
			await registerAppAttestKey(getRequestClientIp(ctx), ctx.get('user').id, ctx.req.valid('json'));
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/users/@me/age-range',
		RateLimitMiddleware(RateLimitConfigs.USER_AGE_RANGE_SUBMIT),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', AgeRangeSubmitRequest),
		OpenAPI({
			operationId: 'submit_age_range',
			summary: 'Submit attested age floor',
			responseSchema: AgeRangeSubmitResponse,
			statusCode: 200,
			errorStatusCodes: [403, 404],
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				"Submits the age floor the device reported, signed by a registered App Attest key. client_data holds the exact JSON bytes the app signed, with the challenge from a challenge with purpose assert and the age_floor. Only the adult floor is accepted. It stores that the account's age range is 18 or over, with the time it was received, which unlocks mature content in regions where a date of birth typed in is not enough for mature content. It does not change the date of birth or any setting. Any other floor returns AGE_RANGE_NOT_ADULT with status 403 and nothing is stored. When the request ends, whatever the outcome, every App Attest key registered to the current user is deleted, so the app registers a new key before each submit. Returns APP_ATTEST_KEY_NOT_REGISTERED when the key is unknown, in which case the app should register a new key. Returns APP_ATTEST_NOT_AVAILABLE with status 404 when this instance does not accept App Attest. Returns FEATURE_NOT_AVAILABLE_SELF_HOSTED with status 403 on a self-hosted instance, and AGE_RANGE_NOT_NEEDED with status 403 when the request does not come from a region where a date of birth typed in is not enough for mature content.",
		}),
		async (ctx) => {
			return ctx.json(
				await submitAttestedAge(ctx.get('apiContext'), getRequestClientIp(ctx), ctx.get('user'), ctx.req.valid('json')),
			);
		},
	);
	app.delete(
		'/users/@me/age-range',
		RateLimitMiddleware(RateLimitConfigs.USER_AGE_RANGE_REMOVE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'remove_age_range',
			summary: 'Remove the stored age range',
			responseSchema: UserPrivateResponse,
			statusCode: 200,
			security: ['sessionToken'],
			tags: ['Users'],
			description:
				'Removes the stored age range with the time it was received, and deletes every App Attest key registered to the current user. After that, mature content is locked again in regions where a date of birth typed in is not enough for mature content. The date of birth and settings do not change. When no age range is stored, the request still succeeds. Returns the updated user.',
		}),
		async (ctx) => {
			return ctx.json(mapUserToPrivateResponse(await removeAttestedAge(ctx.get('apiContext'), ctx.get('user'))));
		},
	);
	app.post(
		'/users/@me/play-integrity/challenge',
		RateLimitMiddleware(RateLimitConfigs.USER_PLAY_INTEGRITY_CHALLENGE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'create_play_integrity_challenge',
			summary: 'Create Play Integrity challenge',
			responseSchema: AppAttestChallengeResponse,
			statusCode: 200,
			errorStatusCodes: [403, 404],
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Creates a single use challenge for the Android app, valid for 5 minutes. Put it in client_data before submitting an age range with a Play Integrity token. Returns PLAY_INTEGRITY_NOT_AVAILABLE with status 404 when this instance does not accept Play Integrity. Returns FEATURE_NOT_AVAILABLE_SELF_HOSTED with status 403 on a self-hosted instance, and AGE_RANGE_NOT_NEEDED with status 403 when the request does not come from a region where a date of birth typed in is not enough for mature content.',
		}),
		async (ctx) => {
			return ctx.json(await createPlayIntegrityChallenge(getRequestClientIp(ctx), ctx.get('user').id));
		},
	);
	app.post(
		'/users/@me/age-range/play-integrity',
		RateLimitMiddleware(RateLimitConfigs.USER_PLAY_INTEGRITY_AGE_SUBMIT),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', PlayIntegrityAgeSubmitRequest),
		OpenAPI({
			operationId: 'submit_play_integrity_age_range',
			summary: 'Submit Play age range',
			responseSchema: AgeRangeSubmitResponse,
			statusCode: 200,
			errorStatusCodes: [403, 404, 503],
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Submits the age range that the Google Play Age Signals API reported, bound to a standard Play Integrity token. client_data holds the exact JSON bytes the app hashed, with the challenge from the Play Integrity challenge endpoint, age_lower, age_upper and age_range_source. The token must be requested with requestHash set to the SHA-256 of those bytes as unpadded base64url. Fluxer checks the token with Google Play and accepts the range only when age_lower is 18. An accepted range is stored as the adult floor with the time it was received, as with the iOS app. The token, the verdict, the range and its source are not stored. Any other range returns AGE_RANGE_NOT_ADULT with status 403 and nothing is stored. Returns PLAY_INTEGRITY_APP_NOT_RECOGNIZED when the app was not installed from Google Play, and PLAY_INTEGRITY_DEVICE_NOT_TRUSTED when the device fails the Play device check. Returns PLAY_INTEGRITY_UNAVAILABLE with status 503 when Google Play could not be asked, in which case the app should try again later with a new challenge. Returns PLAY_INTEGRITY_NOT_AVAILABLE with status 404 when this instance does not accept Play Integrity. Returns FEATURE_NOT_AVAILABLE_SELF_HOSTED with status 403 on a self-hosted instance, and AGE_RANGE_NOT_NEEDED with status 403 when the request does not come from a region where a date of birth typed in is not enough for mature content.',
		}),
		async (ctx) => {
			return ctx.json(
				await submitPlayIntegrityAge(
					ctx.get('apiContext'),
					getRequestClientIp(ctx),
					ctx.get('user'),
					ctx.req.valid('json'),
				),
			);
		},
	);
}
