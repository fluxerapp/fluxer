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
	submitAttestedAge,
	submitPlayIntegrityAge,
} from '@app/api/user/AgeAssurance';
import {Validator} from '@app/api/Validator';
import {
	AgeAssuranceSubmitRequest,
	AgeAssuranceSubmitResponse,
	AppAttestChallengeRequest,
	AppAttestChallengeResponse,
	AppAttestKeyRegisterRequest,
	PlayIntegrityAgeSubmitRequest,
} from '@fluxer/schema/src/domains/user/AgeAssuranceSchemas';

export function UserAgeAssuranceController(app: HonoApp) {
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
			errorStatusCodes: [404],
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Creates a single use challenge for the iOS app, valid for 5 minutes. Use purpose attest before registering a new App Attest key, and purpose assert before submitting an age floor. Returns APP_ATTEST_NOT_AVAILABLE with status 404 when this instance does not accept App Attest.',
		}),
		async (ctx) => {
			return ctx.json(await createAppAttestChallenge(ctx.get('user').id, ctx.req.valid('json').purpose));
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
			errorStatusCodes: [404, 409],
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Registers an App Attest key for the current user. The attestation must be made with clientDataHash set to the SHA-256 of the UTF-8 challenge string from a challenge with purpose attest. The server keeps the key id, public key, environment, App ID and counter, and does not keep the attestation itself. Registering the same key again for the same user succeeds and changes nothing. Returns APP_ATTEST_NOT_AVAILABLE with status 404 when this instance does not accept App Attest.',
		}),
		async (ctx) => {
			await registerAppAttestKey(ctx.get('user').id, ctx.req.valid('json'));
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/users/@me/age-assurance',
		RateLimitMiddleware(RateLimitConfigs.USER_AGE_ASSURANCE_SUBMIT),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', AgeAssuranceSubmitRequest),
		OpenAPI({
			operationId: 'submit_age_assurance',
			summary: 'Submit attested age floor',
			responseSchema: AgeAssuranceSubmitResponse,
			statusCode: 200,
			errorStatusCodes: [404],
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Submits the age floor the device reported, signed by a registered App Attest key. client_data holds the exact JSON bytes the app signed, with the challenge from a challenge with purpose assert and the age_floor. The newest accepted floor replaces the stored one. The stored floor decides the age of the account in place of its date of birth: adult makes age-restricted content available, and minor or under_minimum makes it unavailable. An adult or minor floor also counts as a date of birth for routes that need one. Returns APP_ATTEST_KEY_NOT_REGISTERED when the key is unknown, in which case the app should register a new key. Returns APP_ATTEST_NOT_AVAILABLE with status 404 when this instance does not accept App Attest.',
		}),
		async (ctx) => {
			return ctx.json(await submitAttestedAge(ctx.get('apiContext'), ctx.get('user'), ctx.req.valid('json')));
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
			errorStatusCodes: [404],
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Creates a single use challenge for the Android app, valid for 5 minutes. Put it in client_data before submitting an age range with a Play Integrity token. Returns PLAY_INTEGRITY_NOT_AVAILABLE with status 404 when this instance does not accept Play Integrity.',
		}),
		async (ctx) => {
			return ctx.json(await createPlayIntegrityChallenge(ctx.get('user').id));
		},
	);
	app.post(
		'/users/@me/age-assurance/play-integrity',
		RateLimitMiddleware(RateLimitConfigs.USER_PLAY_INTEGRITY_AGE_SUBMIT),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', PlayIntegrityAgeSubmitRequest),
		OpenAPI({
			operationId: 'submit_play_integrity_age_assurance',
			summary: 'Submit Play age range',
			responseSchema: AgeAssuranceSubmitResponse,
			statusCode: 200,
			errorStatusCodes: [404, 503],
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Submits the age range that the Google Play Age Signals API reported, bound to a standard Play Integrity token. client_data holds the exact JSON bytes the app hashed, with the challenge from the Play Integrity challenge endpoint, age_lower, age_upper and age_range_source. The token must be requested with requestHash set to the SHA-256 of those bytes as unpadded base64url. Fluxer checks the token with Google Play and works out the age floor from the range: adult when age_lower is 18, minor when age_lower is 13 or more, and under_minimum otherwise. The newest accepted floor replaces the stored one, as with the iOS age check. Returns PLAY_INTEGRITY_APP_NOT_RECOGNIZED when the app was not installed from Google Play, and PLAY_INTEGRITY_DEVICE_NOT_TRUSTED when the device fails the Play device check. Returns PLAY_INTEGRITY_UNAVAILABLE with status 503 when Google Play could not be asked, in which case the app should try again later with a new challenge. Returns PLAY_INTEGRITY_NOT_AVAILABLE with status 404 when this instance does not accept Play Integrity.',
		}),
		async (ctx) => {
			return ctx.json(await submitPlayIntegrityAge(ctx.get('apiContext'), ctx.get('user'), ctx.req.valid('json')));
		},
	);
}
