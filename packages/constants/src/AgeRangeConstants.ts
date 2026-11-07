// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const AttestedAgeFloors = {
	ADULT: 'adult',
	MINOR: 'minor',
	UNDER_MINIMUM: 'under_minimum',
} as const;

export type AttestedAgeFloor = ValueOf<typeof AttestedAgeFloors>;

const ATTESTED_AGE_FLOOR_VALUES: ReadonlyArray<AttestedAgeFloor> = Object.values(AttestedAgeFloors);

export function isAttestedAgeFloor(value: unknown): value is AttestedAgeFloor {
	return typeof value === 'string' && (ATTESTED_AGE_FLOOR_VALUES as ReadonlyArray<string>).includes(value);
}

export const AppAttestChallengePurposes = {
	ATTEST: 'attest',
	ASSERT: 'assert',
} as const;

export type AppAttestChallengePurpose = ValueOf<typeof AppAttestChallengePurposes>;

export const APP_ATTEST_KEY_ID_BYTES = 32;
export const APP_ATTEST_CHALLENGE_BYTES = 32;
export const APP_ATTEST_ATTESTATION_MAX_BYTES = 16 * 1024;
export const APP_ATTEST_ASSERTION_MAX_BYTES = 4 * 1024;
export const APP_ATTEST_CLIENT_DATA_MAX_BYTES = 1024;
export const PLAY_INTEGRITY_PACKAGE_NAME_MAX_LENGTH = 255;
export const PLAY_INTEGRITY_TOKEN_MAX_LENGTH = 16 * 1024;
export const PLAY_INTEGRITY_CLIENT_DATA_MAX_BYTES = 1024;
