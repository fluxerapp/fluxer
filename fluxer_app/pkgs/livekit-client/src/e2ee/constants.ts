// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {KeyProviderOptions} from './types.ts';

export const ENCRYPTION_ALGORITHM = 'AES-GCM';

export const DECRYPTION_FAILURE_TOLERANCE = 10;

export const UNENCRYPTED_BYTES = {
	key: 10,
	delta: 3,
	audio: 1,
	empty: 0,
} as const;

export const IV_LENGTH = 12;

export const E2EE_FLAG = 'lk_e2ee';

export const E2EE_TRACK_ID = 'lk_e2ee_track_id';

export const SALT = 'LKFrameEncryptionKey';

export const KEY_PROVIDER_DEFAULTS: KeyProviderOptions = {
	sharedKey: false,
	ratchetSalt: SALT,
	ratchetWindowSize: 8,
	failureTolerance: DECRYPTION_FAILURE_TOLERANCE,
	keyringSize: 16,
	keySize: 128,
} as const;

export const MAX_SIF_COUNT = 100;
export const MAX_SIF_DURATION = 2000;
