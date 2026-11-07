// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	APP_ATTEST_ASSERTION_MAX_BYTES,
	APP_ATTEST_ATTESTATION_MAX_BYTES,
	APP_ATTEST_CHALLENGE_BYTES,
	APP_ATTEST_CLIENT_DATA_MAX_BYTES,
	APP_ATTEST_KEY_ID_BYTES,
	AppAttestChallengePurposes,
	AttestedAgeFloors,
	PLAY_INTEGRITY_CLIENT_DATA_MAX_BYTES,
	PLAY_INTEGRITY_PACKAGE_NAME_MAX_LENGTH,
	PLAY_INTEGRITY_TOKEN_MAX_LENGTH,
} from '@fluxer/constants/src/AgeRangeConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {withBase64Binary} from '@fluxer/schema/src/primitives/Base64BinaryFields';
import {base64LengthForBytes, isValidBase64} from '@fluxer/schema/src/primitives/FileValidators';
import {createNamedStringLiteralUnion} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const BASE64URL_REGEX = /^[A-Za-z0-9_-]+$/;
const ANDROID_PACKAGE_NAME_REGEX = /^[A-Za-z][A-Za-z0-9_]*(\.[A-Za-z][A-Za-z0-9_]*)+$/;
const PLAY_INTEGRITY_TOKEN_REGEX = /^[\x21\x23-\x5b\x5d-\x7e]+$/;

function decodedBase64Length(value: string): number {
	const padding = value.length - value.replace(/=+$/, '').length;
	return (value.length / 4) * 3 - padding;
}

function base64BytesType(minBytes: number, maxBytes: number) {
	const minLength = base64LengthForBytes(minBytes);
	const maxLength = base64LengthForBytes(maxBytes);
	return withBase64Binary(
		z
			.string()
			.superRefine((value, ctx) => {
				if (value.length < minLength || value.length > maxLength) {
					ctx.addIssue({
						code: 'custom',
						message: ValidationErrorCodes.BASE64_LENGTH_INVALID,
						params: {min: minLength, maxLength},
					});
					return;
				}
				if (!isValidBase64(value)) {
					ctx.addIssue({code: 'custom', message: ValidationErrorCodes.INVALID_BASE64_FORMAT});
					return;
				}
				const bytes = decodedBase64Length(value);
				if (bytes < minBytes || bytes > maxBytes) {
					ctx.addIssue({
						code: 'custom',
						message: ValidationErrorCodes.BASE64_LENGTH_INVALID,
						params: {min: minLength, maxLength},
					});
				}
			})
			.meta({minLength, maxLength}),
	);
}

const CHALLENGE_LENGTH = Math.ceil((APP_ATTEST_CHALLENGE_BYTES * 4) / 3);

const AppAttestChallengeType = z
	.string()
	.superRefine((value, ctx) => {
		if (value.length !== CHALLENGE_LENGTH || !BASE64URL_REGEX.test(value)) {
			ctx.addIssue({code: 'custom', message: ValidationErrorCodes.INVALID_BASE64_FORMAT});
		}
	})
	.meta({minLength: CHALLENGE_LENGTH, maxLength: CHALLENGE_LENGTH})
	.describe('Challenge from the challenge endpoint, 32 bytes as unpadded base64url');

const AppAttestKeyIdType = base64BytesType(APP_ATTEST_KEY_ID_BYTES, APP_ATTEST_KEY_ID_BYTES).describe(
	'App Attest key identifier from DCAppAttestService, 32 bytes as standard base64',
);

const AppAttestChallengePurposeSchema = createNamedStringLiteralUnion(
	[
		[AppAttestChallengePurposes.ATTEST, 'ATTEST', 'Challenge for registering a new App Attest key'],
		[AppAttestChallengePurposes.ASSERT, 'ASSERT', 'Challenge for an assertion signed by a registered key'],
	],
	'What the challenge will be used for',
);

export const AttestedAgeFloorSchema = z
	.literal(AttestedAgeFloors.ADULT)
	.describe('Age floor that was accepted, which is always adult');

export const AppAttestChallengeRequest = z.object({
	purpose: AppAttestChallengePurposeSchema,
});

export type AppAttestChallengeRequest = z.infer<typeof AppAttestChallengeRequest>;

export const AppAttestChallengeResponse = z.object({
	challenge: z.string().describe('Single use challenge, 32 bytes as unpadded base64url'),
	expires_at: z.iso.datetime().describe('When the challenge stops working'),
});

export type AppAttestChallengeResponse = z.infer<typeof AppAttestChallengeResponse>;

export const AppAttestKeyRegisterRequest = z.object({
	key_id: AppAttestKeyIdType,
	attestation: base64BytesType(1, APP_ATTEST_ATTESTATION_MAX_BYTES).describe(
		'Attestation object from DCAppAttestService.attestKey as standard base64',
	),
	challenge: AppAttestChallengeType,
});

export type AppAttestKeyRegisterRequest = z.infer<typeof AppAttestKeyRegisterRequest>;

export const AgeRangeSubmitRequest = z.object({
	key_id: AppAttestKeyIdType,
	assertion: base64BytesType(1, APP_ATTEST_ASSERTION_MAX_BYTES).describe(
		'Assertion from DCAppAttestService.generateAssertion as standard base64',
	),
	client_data: base64BytesType(1, APP_ATTEST_CLIENT_DATA_MAX_BYTES).describe(
		'The exact UTF-8 JSON bytes the app signed, as standard base64. The JSON holds challenge and age_floor',
	),
});

export type AgeRangeSubmitRequest = z.infer<typeof AgeRangeSubmitRequest>;

export const AgeRangeSubmitResponse = z.object({
	age_floor: AttestedAgeFloorSchema,
});

export type AgeRangeSubmitResponse = z.infer<typeof AgeRangeSubmitResponse>;

export const PlayIntegrityAgeSubmitRequest = z.object({
	package_name: z
		.string()
		.max(PLAY_INTEGRITY_PACKAGE_NAME_MAX_LENGTH)
		.regex(ANDROID_PACKAGE_NAME_REGEX)
		.describe('Package name of the Android app that asked for the integrity token, such as com.fluxer'),
	integrity_token: z
		.string()
		.min(1)
		.max(PLAY_INTEGRITY_TOKEN_MAX_LENGTH)
		.regex(PLAY_INTEGRITY_TOKEN_REGEX)
		.describe(
			'Token from a standard Play Integrity request, made with requestHash set to the SHA-256 of client_data as unpadded base64url',
		),
	client_data: base64BytesType(1, PLAY_INTEGRITY_CLIENT_DATA_MAX_BYTES).describe(
		'The exact UTF-8 JSON bytes the app hashed, as standard base64 on one line with no line breaks (Base64.NO_WRAP on Android). The JSON holds challenge, age_lower, age_upper and age_range_source',
	),
});

export type PlayIntegrityAgeSubmitRequest = z.infer<typeof PlayIntegrityAgeSubmitRequest>;
