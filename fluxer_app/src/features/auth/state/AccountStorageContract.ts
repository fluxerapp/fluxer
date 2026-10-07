// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {requireRuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';
import {
	accountStorageKey,
	deriveAccountStorageKey,
	parseAccountStorageKey,
} from '@app/features/auth/state/AccountStorageKey';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {countTelemetryEvent, TelemetryEvent} from '@app/features/platform/utils/AppTelemetry';
import type {CustomStatus} from '@app/features/user/state/CustomStatus';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';

const logger = new Logger('AccountStorage');

export interface UserData {
	username: string;
	discriminator: string;
	globalName?: string | null;
	email?: string | null;
	avatar?: string | null;
}

export interface AccountPresenceIntent {
	status: StatusType;
	statusResetsAt: string | null;
	statusResetsTo: StatusType | null;
	customStatus: CustomStatus | null;
	capturedAt: number;
}

export interface StoredAccountData {
	userId: string;
	token: string | null;
	userData?: UserData;
	presenceIntent?: AccountPresenceIntent | null;
	localStorageData: Record<string, string>;
	managedStorageData?: Record<string, string>;
	lastActive: number;
	isValid?: boolean;
}

export interface StoredAccount extends StoredAccountData {
	instance?: RuntimeConfigSnapshot;
	storageKey?: string;
}

export type KeyedStoredAccount = StoredAccount & {storageKey: string};

export interface QualifiedStoredAccount extends StoredAccountData {
	instance: RuntimeConfigSnapshot;
	storageKey: string;
}

export type StoredAccountSource = 'desktop' | 'idb';

export type BrowserAccountStorageOperation = 'open' | 'list' | 'read' | 'write' | 'delete' | 'migrate';

export interface StoredAccountList {
	records: Array<StoredAccount>;
	source: StoredAccountSource;
}

export interface AccountRekeyTarget {
	readonly userId: string;
	readonly token: string;
	readonly instance: RuntimeConfigSnapshot;
}

export interface AccountRekeyContext {
	readonly target: AccountRekeyTarget;
	readonly now: number;
}

export interface AccountRekeyPlan {
	readonly qualifiedRecords: ReadonlyArray<KeyedStoredAccount>;
	readonly deferredRecords: ReadonlyArray<StoredAccount>;
}

export interface AccountRekeyOutcome {
	status: 'complete' | 'deferred' | 'skipped' | 'failed';
	source: StoredAccountSource;
	qualifiedRecords: ReadonlyArray<KeyedStoredAccount>;
	deferredRecords: ReadonlyArray<StoredAccount>;
	written: number;
}

export class CrossInstanceAccountCollisionError extends Error {
	readonly userId: string;
	readonly existingStorageKey: string;
	readonly incomingStorageKey: string | null;

	constructor(userId: string, existingStorageKey: string, incomingStorageKey: string | null) {
		super(`Account ${userId} is already stored under ${existingStorageKey} on another instance`);
		this.name = 'CrossInstanceAccountCollisionError';
		this.userId = userId;
		this.existingStorageKey = existingStorageKey;
		this.incomingStorageKey = incomingStorageKey;
	}
}

export class DesktopAccountStorageAuthorityError extends Error {
	constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'DesktopAccountStorageAuthorityError';
	}
}

export class BrowserAccountStorageUnavailableError extends Error {
	readonly operation: BrowserAccountStorageOperation;

	constructor(operation: BrowserAccountStorageOperation, message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'BrowserAccountStorageUnavailableError';
		this.operation = operation;
	}
}

export class BrowserAccountStorageTimeoutError extends BrowserAccountStorageUnavailableError {
	readonly timeoutMs: number;

	constructor(operation: BrowserAccountStorageOperation, label: string, timeoutMs: number) {
		super(operation, `Browser account storage timed out while ${label} after ${timeoutMs}ms`);
		this.name = 'BrowserAccountStorageTimeoutError';
		this.timeoutMs = timeoutMs;
	}
}

export class InvalidAccountStorageSnapshotError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'InvalidAccountStorageSnapshotError';
	}
}

export function recordMatchesAccountKey(record: StoredAccount, accountKey: string): boolean {
	const parsed = parseAccountStorageKey(accountKey);
	if (parsed === null) {
		return record.userId === accountKey;
	}
	return deriveAccountStorageKey(record) === accountKey;
}

export function normalizeStoredAccount(record: StoredAccount): StoredAccount {
	const managedStorageData = cloneStorageSnapshot(record.managedStorageData ?? record.localStorageData ?? {});
	return {
		...record,
		userData: record.userData ? {...record.userData} : undefined,
		presenceIntent: clonePresenceIntent(record.presenceIntent),
		localStorageData: managedStorageData,
		managedStorageData,
		instance: record.instance ? requireRuntimeConfigSnapshot(record.instance) : undefined,
		storageKey: deriveAccountStorageKey(record) ?? undefined,
	};
}

export function assertAccountWritePreservesInstance(existing: StoredAccount | null, incoming: StoredAccount): void {
	if (existing === null) {
		return;
	}
	const existingKey = deriveAccountStorageKey(existing);
	const incomingKey = deriveAccountStorageKey(incoming);
	if (existingKey === null || existingKey === incomingKey) {
		return;
	}
	countTelemetryEvent(TelemetryEvent.ACCOUNT_CROSS_INSTANCE_USERID_COLLISION, 1, existingKey, incomingKey);
	logger.warn(`Refusing a cross-instance write for account ${incoming.userId} stored under ${existingKey}`);
	throw new CrossInstanceAccountCollisionError(incoming.userId, existingKey, incomingKey);
}

export function planAccountRekey(
	records: ReadonlyArray<StoredAccount>,
	context: AccountRekeyContext,
): AccountRekeyPlan {
	const planned = new Map<string, KeyedStoredAccount>();
	const deferredRecords: Array<StoredAccount> = [];
	for (const record of records) {
		if (typeof record?.userId !== 'string' || record.userId.trim().length === 0) {
			logger.error('Deferring a stored account record with no usable userId');
			deferredRecords.push(record);
			continue;
		}
		const keyed = keyAccountRecord(record, context.target);
		if (keyed === null) {
			logger.warn(`Leaving stored account ${record.userId} unkeyed until a usable instance key is available`);
			deferredRecords.push(record);
			continue;
		}
		const next: KeyedStoredAccount = {
			...record,
			...keyed,
			lastActive: Number.isFinite(record.lastActive) ? record.lastActive : context.now,
		};
		const existing = planned.get(keyed.storageKey);
		if (existing == null) {
			planned.set(keyed.storageKey, next);
			continue;
		}
		countTelemetryEvent(TelemetryEvent.ACCOUNT_STORAGE_KEY_COLLISION, 1, keyed.storageKey);
		logger.warn(`Deferring a colliding stored account under ${keyed.storageKey}`);
		deferredRecords.push(record);
	}
	return {qualifiedRecords: [...planned.values()], deferredRecords};
}

function cloneStorageSnapshot(snapshot: unknown): Record<string, string> {
	if (typeof snapshot !== 'object' || snapshot === null || Array.isArray(snapshot)) {
		throw new InvalidAccountStorageSnapshotError('Account storage snapshot must be an object');
	}
	const cloned: Record<string, string> = {};
	for (const [key, value] of Object.entries(snapshot)) {
		if (typeof value !== 'string') {
			throw new InvalidAccountStorageSnapshotError(`Account storage snapshot value for ${key} must be a string`);
		}
		cloned[key] = value;
	}
	return cloned;
}

function clonePresenceIntent(intent?: AccountPresenceIntent | null): AccountPresenceIntent | null | undefined {
	if (intent === undefined) {
		return undefined;
	}
	if (intent === null) {
		return null;
	}
	return {
		...intent,
		statusResetsAt: intent.statusResetsAt ?? null,
		statusResetsTo: intent.statusResetsTo ?? null,
		customStatus: intent.customStatus ? {...intent.customStatus} : null,
	};
}

function keyAccountRecord(
	record: StoredAccount,
	target: AccountRekeyTarget,
): {storageKey: string; instance?: RuntimeConfigSnapshot} | null {
	const ownKey = deriveAccountStorageKey(record);
	if (
		ownKey !== null &&
		record.instance !== undefined &&
		accountStorageKey(record.userId, record.instance) === ownKey
	) {
		return {storageKey: ownKey, instance: record.instance};
	}
	const healedKey = accountStorageKey(record.userId, target.instance);
	if (healedKey === null || (ownKey !== null && ownKey !== healedKey)) {
		return null;
	}
	const keyedOnTargetInstance = ownKey !== null && record.instance === undefined;
	const isTarget = record.userId === target.userId && record.token === target.token;
	if (!keyedOnTargetInstance && !isTarget) {
		return null;
	}
	return {storageKey: healedKey, instance: target.instance};
}
