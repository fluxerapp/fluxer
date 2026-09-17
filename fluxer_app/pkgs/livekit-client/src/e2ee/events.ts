// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type Participant from '../room/participant/Participant.ts';
import type {CryptorError} from './errors.ts';
import type {KeyInfo, RatchetResult} from './types.ts';

export enum KeyProviderEvent {
	SetKey = 'setKey',
	RatchetRequest = 'ratchetRequest',
	KeyRatcheted = 'keyRatcheted',
}

export type KeyProviderCallbacks = {
	[KeyProviderEvent.SetKey]: (keyInfo: KeyInfo, updateCurrentKeyIndex: boolean) => void;
	[KeyProviderEvent.RatchetRequest]: (participantIdentity?: string, keyIndex?: number) => void;
	[KeyProviderEvent.KeyRatcheted]: (
		ratchetedResult: RatchetResult,
		participantIdentity?: string,
		keyIndex?: number,
	) => void;
};

export enum KeyHandlerEvent {
	KeyRatcheted = 'keyRatcheted',
}

export type ParticipantKeyHandlerCallbacks = {
	[KeyHandlerEvent.KeyRatcheted]: (
		ratchetResult: RatchetResult,
		participantIdentity: string,
		keyIndex?: number,
	) => void;
};

export enum EncryptionEvent {
	ParticipantEncryptionStatusChanged = 'participantEncryptionStatusChanged',
	EncryptionError = 'encryptionError',
}

export type E2EEManagerCallbacks = {
	[EncryptionEvent.ParticipantEncryptionStatusChanged]: (enabled: boolean, participant: Participant) => void;
	[EncryptionEvent.EncryptionError]: (error: Error, participantIdentity?: string) => void;
};

export type CryptorCallbacks = {
	[CryptorEvent.Error]: (error: CryptorError) => void;
};

export enum CryptorEvent {
	Error = 'cryptorError',
}
