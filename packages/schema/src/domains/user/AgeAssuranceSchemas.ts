// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	APP_ATTEST_ASSERTION_MAX_BYTES,
	APP_ATTEST_ATTESTATION_MAX_BYTES,
	APP_ATTEST_CHALLENGE_BYTES,
	APP_ATTEST_CLIENT_DATA_MAX_BYTES,
	APP_ATTEST_KEY_ID_BYTES,
	AppAttestChallengePurposes,
	AttestedAgeFloors,
} from '@fluxer/constants/src/AgeAssuranceConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {withBase64Binary} from '@fluxer/schema/src/primitives/Base64BinaryFields';
import {base64LengthForBytes, isValidBase64} from '@fluxer/schema/src/primitives/FileValidators';
import {createNamedStringLiteralUnion} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const BASE64URL_REGEX = /^[A-Za-z0-9_-]+$/;

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

export const AttestedAgeFloorSchema = createNamedStringLiteralUnion(
	[
		[AttestedAgeFloors.ADULT, 'ADULT', 'The device reports an adult'],
		[AttestedAgeFloors.MINOR, 'MINOR', 'The device reports someone under 18'],
		[AttestedAgeFloors.UNDER_MINIMUM, 'UNDER_MINIMUM', 'The device reports someone under 13'],
	],
	'Age floor reported by the device',
);

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

export const AgeAssuranceSubmitRequest = z.object({
	key_id: AppAttestKeyIdType,
	assertion: base64BytesType(1, APP_ATTEST_ASSERTION_MAX_BYTES).describe(
		'Assertion from DCAppAttestService.generateAssertion as standard base64',
	),
	client_data: base64BytesType(1, APP_ATTEST_CLIENT_DATA_MAX_BYTES).describe(
		'The exact UTF-8 JSON bytes the app signed, as standard base64. The JSON holds challenge and age_floor',
	),
});

export type AgeAssuranceSubmitRequest = z.infer<typeof AgeAssuranceSubmitRequest>;

export const AgeAssuranceSubmitResponse = z.object({
	age_floor: AttestedAgeFloorSchema,
});

export type AgeAssuranceSubmitResponse = z.infer<typeof AgeAssuranceSubmitResponse>;
