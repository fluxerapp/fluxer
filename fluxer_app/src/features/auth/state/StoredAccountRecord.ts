// SPDX-License-Identifier: AGPL-3.0-or-later

import {type RuntimeConfigSnapshot, storedInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import {requireRuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';
import type {
	QualifiedStoredAccount,
	StoredAccountData,
	UserData,
} from '@app/features/auth/state/AccountStorageContract';
import {
	accountStorageKey,
	accountStorageKeyFromInstanceKey,
	parseAccountStorageKey,
} from '@app/features/auth/state/AccountStorageKey';
import {StoredAccountRuntimeRecoveryError} from '@app/features/auth/state/StoredAccountInventoryContract';
import {
	cloneStoredAccountStructuredRecord,
	type StoredAccountStructuredRecord,
} from '@app/features/auth/state/StoredAccountRevision';
import type {CustomStatus} from '@app/features/user/state/CustomStatus';
import {isStatusType, type StatusType} from '@fluxer/constants/src/StatusConstants';

export interface DecodedStoredAccountRecord {
	readonly data: StoredAccountData;
	readonly persistedStorageKey: string | null;
	readonly instance: unknown;
}

export interface StoredAccountRecordIdentity {
	readonly storageKey: string | null;
	readonly userId: string | null;
}

function requireRecord(value: unknown, label: string): Record<string, unknown> {
	if (typeof value !== 'object' || value === null || Array.isArray(value)) {
		throw new StoredAccountRuntimeRecoveryError(`${label} must be an object`);
	}
	return value as Record<string, unknown>;
}

function readOptionalNullableString(
	source: Record<string, unknown>,
	key: string,
	label: string,
): string | null | undefined {
	const value = source[key];
	if (value === undefined || value === null || typeof value === 'string') {
		return value;
	}
	throw new StoredAccountRuntimeRecoveryError(`${label} must be a string, null, or absent`);
}

function readRequiredNullableString(source: Record<string, unknown>, key: string, label: string): string | null {
	const value = source[key];
	if (value === null || typeof value === 'string') {
		return value;
	}
	throw new StoredAccountRuntimeRecoveryError(`${label} must be a string or null`);
}

function readOptionalNullableBoolean(
	source: Record<string, unknown>,
	key: string,
	label: string,
): boolean | null | undefined {
	const value = source[key];
	if (value === undefined || value === null || typeof value === 'boolean') {
		return value;
	}
	throw new StoredAccountRuntimeRecoveryError(`${label} must be a boolean, null, or absent`);
}

function readStorageSnapshot(value: unknown, label: string): Record<string, string> {
	const source = requireRecord(value, label);
	const snapshot: Record<string, string> = Object.create(null);
	for (const [key, entry] of Object.entries(source)) {
		if (typeof entry !== 'string') {
			throw new StoredAccountRuntimeRecoveryError(`${label}.${key} must be a string`);
		}
		snapshot[key] = entry;
	}
	return snapshot;
}

function readUserData(value: unknown): UserData | undefined {
	if (value === undefined) {
		return undefined;
	}
	const source = requireRecord(value, 'Stored account user data');
	if (typeof source.username !== 'string' || typeof source.discriminator !== 'string') {
		throw new StoredAccountRuntimeRecoveryError('Stored account user data must contain a username and discriminator');
	}
	const localAvatarSize = source.localAvatarSize;
	if (
		localAvatarSize !== undefined &&
		localAvatarSize !== null &&
		(typeof localAvatarSize !== 'number' || !Number.isFinite(localAvatarSize))
	) {
		throw new StoredAccountRuntimeRecoveryError('Stored account local avatar size must be finite, null, or absent');
	}
	const userData: UserData = {
		username: source.username,
		discriminator: source.discriminator,
	};
	if (Object.hasOwn(source, 'globalName')) {
		userData.globalName = readOptionalNullableString(source, 'globalName', 'Stored account global name');
	}
	if (Object.hasOwn(source, 'email')) {
		userData.email = readOptionalNullableString(source, 'email', 'Stored account email');
	}
	if (Object.hasOwn(source, 'avatar')) {
		userData.avatar = readOptionalNullableString(source, 'avatar', 'Stored account avatar');
	}
	if (Object.hasOwn(source, 'localAvatarHash')) {
		userData.localAvatarHash = readOptionalNullableString(
			source,
			'localAvatarHash',
			'Stored account local avatar hash',
		);
	}
	if (Object.hasOwn(source, 'localAvatarURL')) {
		userData.localAvatarURL = readOptionalNullableString(source, 'localAvatarURL', 'Stored account local avatar URL');
	}
	if (Object.hasOwn(source, 'localAvatarSize')) {
		userData.localAvatarSize = localAvatarSize;
	}
	return userData;
}

function readCustomStatus(value: unknown): CustomStatus | null {
	if (value === null) {
		return null;
	}
	const source = requireRecord(value, 'Stored account custom status');
	const customStatus: CustomStatus = {
		text: readRequiredNullableString(source, 'text', 'Stored account custom status text'),
		expiresAt: readRequiredNullableString(source, 'expiresAt', 'Stored account custom status expiry'),
		emojiId: readRequiredNullableString(source, 'emojiId', 'Stored account custom status emoji ID'),
		emojiName: readRequiredNullableString(source, 'emojiName', 'Stored account custom status emoji name'),
	};
	if (Object.hasOwn(source, 'emojiAnimated')) {
		customStatus.emojiAnimated = readOptionalNullableBoolean(
			source,
			'emojiAnimated',
			'Stored account custom status animated flag',
		);
	}
	return customStatus;
}

function readNullableStatus(value: unknown, label: string): StatusType | null {
	if (value === null) {
		return null;
	}
	if (!isStatusType(value)) {
		throw new StoredAccountRuntimeRecoveryError(`${label} must be a status or null`);
	}
	return value;
}

function readPresenceIntent(value: unknown): StoredAccountData['presenceIntent'] {
	if (value === undefined || value === null) {
		return value;
	}
	const source = requireRecord(value, 'Stored account presence intent');
	if (!isStatusType(source.status)) {
		throw new StoredAccountRuntimeRecoveryError('Stored account presence status is invalid');
	}
	const capturedAt = source.capturedAt;
	if (typeof capturedAt !== 'number' || !Number.isFinite(capturedAt)) {
		throw new StoredAccountRuntimeRecoveryError('Stored account presence capture time must be finite');
	}
	return {
		status: source.status,
		statusResetsAt: readRequiredNullableString(source, 'statusResetsAt', 'Stored account presence reset time'),
		statusResetsTo: readNullableStatus(source.statusResetsTo, 'Stored account presence reset status'),
		customStatus: readCustomStatus(source.customStatus),
		capturedAt,
	};
}

function readStoredAccountData(source: Record<string, unknown>): StoredAccountData {
	const userId = source.userId;
	if (typeof userId !== 'string' || userId.trim().length === 0) {
		throw new StoredAccountRuntimeRecoveryError('Stored account must contain a user identity');
	}
	const token = source.token;
	if (token !== null && typeof token !== 'string') {
		throw new StoredAccountRuntimeRecoveryError('Stored account token must be a string or null');
	}
	const lastActive = source.lastActive;
	if (typeof lastActive !== 'number' || !Number.isFinite(lastActive)) {
		throw new StoredAccountRuntimeRecoveryError('Stored account last-active time must be finite');
	}
	const isValid = source.isValid;
	if (isValid !== undefined && typeof isValid !== 'boolean') {
		throw new StoredAccountRuntimeRecoveryError('Stored account validity must be a boolean or absent');
	}
	const data: StoredAccountData = {
		userId,
		token,
		localStorageData: readStorageSnapshot(source.localStorageData, 'Stored account local storage'),
		lastActive,
	};
	if (Object.hasOwn(source, 'userData')) {
		data.userData = readUserData(source.userData);
	}
	if (Object.hasOwn(source, 'presenceIntent')) {
		data.presenceIntent = readPresenceIntent(source.presenceIntent);
	}
	if (Object.hasOwn(source, 'managedStorageData')) {
		data.managedStorageData =
			source.managedStorageData === undefined
				? undefined
				: readStorageSnapshot(source.managedStorageData, 'Stored account managed storage');
	}
	if (Object.hasOwn(source, 'isValid')) {
		data.isValid = isValid;
	}
	return data;
}

function readPersistedStorageKey(
	source: Record<string, unknown>,
	authoritativeStorageKey: string | null,
): string | null {
	const persisted = source.storageKey;
	if (persisted !== undefined && typeof persisted !== 'string') {
		throw new StoredAccountRuntimeRecoveryError('Stored account key must be a string or absent');
	}
	if (authoritativeStorageKey !== null && persisted !== undefined && persisted !== authoritativeStorageKey) {
		throw new StoredAccountRuntimeRecoveryError('Stored account key disagrees with repository authority');
	}
	return authoritativeStorageKey ?? persisted ?? null;
}

function runtimeIdentityFromStorageKey(storageKey: string, userId: string): string | null {
	const parsed = parseAccountStorageKey(storageKey);
	if (parsed === null || parsed.userId !== userId) {
		return null;
	}
	return accountStorageKeyFromInstanceKey(userId, parsed.instanceKey) === storageKey
		? storedInstanceKey(parsed.instanceKey)
		: null;
}

function runtimeIdentityFromLegacyInstance(value: unknown): string | null {
	if (typeof value !== 'object' || value === null || Array.isArray(value)) {
		return null;
	}
	const descriptor = Object.getOwnPropertyDescriptor(value, 'apiEndpoint');
	return descriptor !== undefined && 'value' in descriptor && typeof descriptor.value === 'string'
		? storedInstanceKey(descriptor.value)
		: null;
}

export function decodeStoredAccountRecord(
	source: StoredAccountStructuredRecord,
	authoritativeStorageKey: string | null,
): DecodedStoredAccountRecord {
	const record = requireRecord(source, 'Stored account revision');
	return {
		data: readStoredAccountData(record),
		persistedStorageKey: readPersistedStorageKey(record, authoritativeStorageKey),
		instance: record.instance,
	};
}

export function storedAccountLegacyInstanceKey(record: DecodedStoredAccountRecord): string | null {
	return (
		runtimeIdentityFromLegacyInstance(record.instance) ??
		(record.persistedStorageKey === null
			? null
			: runtimeIdentityFromStorageKey(record.persistedStorageKey, record.data.userId))
	);
}

export function storedAccountKeyNamesInstance(storageKey: string, userId: string, instanceKey: string): boolean {
	return runtimeIdentityFromStorageKey(storageKey, userId) === instanceKey;
}

export function createQualifiedStoredAccount(
	data: StoredAccountData,
	instance: RuntimeConfigSnapshot,
	storageKey: string,
): QualifiedStoredAccount {
	if (accountStorageKey(data.userId, instance) !== storageKey) {
		throw new StoredAccountRuntimeRecoveryError('Stored account runtime does not match its qualified identity');
	}
	return {...data, instance, storageKey};
}

export function decodeQualifiedStoredAccount(value: unknown, authoritativeStorageKey: string): QualifiedStoredAccount {
	const source = cloneStoredAccountStructuredRecord(value, 'Stored account replacement');
	const decoded = decodeStoredAccountRecord(source, authoritativeStorageKey);
	if (decoded.persistedStorageKey === null) {
		throw new StoredAccountRuntimeRecoveryError('Stored account replacement has no qualified identity');
	}
	return createQualifiedStoredAccount(
		decoded.data,
		requireRuntimeConfigSnapshot(decoded.instance),
		decoded.persistedStorageKey,
	);
}

function bestEffortOwnString(value: unknown, key: string): string | null {
	if (typeof value !== 'object' || value === null || Array.isArray(value)) {
		return null;
	}
	const descriptor = Object.getOwnPropertyDescriptor(value, key);
	return descriptor !== undefined && 'value' in descriptor && typeof descriptor.value === 'string'
		? descriptor.value
		: null;
}

export function bestEffortStoredAccountIdentity(
	value: unknown,
	authoritativeStorageKey: string | null,
): StoredAccountRecordIdentity {
	return {
		storageKey: authoritativeStorageKey ?? bestEffortOwnString(value, 'storageKey'),
		userId: bestEffortOwnString(value, 'userId'),
	};
}
