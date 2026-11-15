// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StoredAccountSource} from '@app/features/auth/state/AccountStorageContract';
import {
	type StoredAccountRevision,
	StoredAccountRuntimeRecoveryError,
} from '@app/features/auth/state/StoredAccountInventoryContract';

type StoredAccountStructuredValue =
	| string
	| number
	| boolean
	| null
	| undefined
	| ReadonlyArray<StoredAccountStructuredValue>
	| StoredAccountStructuredRecord;

export interface StoredAccountStructuredRecord {
	readonly [key: string]: StoredAccountStructuredValue;
}

interface StoredAccountRevisionSnapshot {
	readonly source: StoredAccountSource;
	readonly record: StoredAccountStructuredRecord;
}

const REVISION_SNAPSHOTS = new WeakMap<StoredAccountRevision, StoredAccountRevisionSnapshot>();

function cloneStructuredArray(
	value: ReadonlyArray<unknown>,
	path: string,
	ancestors: Set<object>,
): ReadonlyArray<StoredAccountStructuredValue> {
	if (Object.getPrototypeOf(value) !== Array.prototype) {
		throw new StoredAccountRuntimeRecoveryError(`${path} must use a plain array prototype`);
	}
	if (ancestors.has(value)) {
		throw new StoredAccountRuntimeRecoveryError(`${path} must not contain a cycle`);
	}
	ancestors.add(value);
	try {
		const clone: Array<StoredAccountStructuredValue> = new Array(value.length);
		for (const key of Reflect.ownKeys(value)) {
			if (key === 'length') {
				continue;
			}
			if (typeof key !== 'string') {
				throw new StoredAccountRuntimeRecoveryError(`${path} must not contain symbol properties`);
			}
			const index = Number(key);
			if (!Number.isSafeInteger(index) || index < 0 || index >= value.length || index.toString() !== key) {
				throw new StoredAccountRuntimeRecoveryError(`${path}.${key} is not a plain array index`);
			}
			const descriptor = Object.getOwnPropertyDescriptor(value, key);
			if (descriptor === undefined || !descriptor.enumerable || !('value' in descriptor)) {
				throw new StoredAccountRuntimeRecoveryError(`${path}.${key} must be an enumerable data property`);
			}
			clone[index] = cloneStructuredValue(descriptor.value, `${path}[${key}]`, ancestors);
		}
		return Object.freeze(clone);
	} finally {
		ancestors.delete(value);
	}
}

function cloneStructuredObject(value: object, path: string, ancestors: Set<object>): StoredAccountStructuredRecord {
	const prototype = Object.getPrototypeOf(value);
	if (prototype !== Object.prototype && prototype !== null) {
		throw new StoredAccountRuntimeRecoveryError(`${path} must use a plain object prototype`);
	}
	if (ancestors.has(value)) {
		throw new StoredAccountRuntimeRecoveryError(`${path} must not contain a cycle`);
	}
	ancestors.add(value);
	try {
		const clone: Record<string, StoredAccountStructuredValue> = Object.create(null);
		for (const key of Reflect.ownKeys(value)) {
			if (typeof key !== 'string') {
				throw new StoredAccountRuntimeRecoveryError(`${path} must not contain symbol properties`);
			}
			const descriptor = Object.getOwnPropertyDescriptor(value, key);
			if (descriptor === undefined || !descriptor.enumerable || !('value' in descriptor)) {
				throw new StoredAccountRuntimeRecoveryError(`${path}.${key} must be an enumerable data property`);
			}
			clone[key] = cloneStructuredValue(descriptor.value, `${path}.${key}`, ancestors);
		}
		return Object.freeze(clone);
	} finally {
		ancestors.delete(value);
	}
}

function cloneStructuredValue(value: unknown, path: string, ancestors: Set<object>): StoredAccountStructuredValue {
	if (
		value === null ||
		value === undefined ||
		typeof value === 'string' ||
		typeof value === 'number' ||
		typeof value === 'boolean'
	) {
		return value;
	}
	if (Array.isArray(value)) {
		return cloneStructuredArray(value, path, ancestors);
	}
	if (typeof value === 'object') {
		return cloneStructuredObject(value, path, ancestors);
	}
	throw new StoredAccountRuntimeRecoveryError(`${path} contains an unsupported ${typeof value} value`);
}

function isStructuredArray(value: StoredAccountStructuredValue): value is ReadonlyArray<StoredAccountStructuredValue> {
	return Array.isArray(value);
}

export function cloneStoredAccountStructuredRecord(value: unknown, path: string): StoredAccountStructuredRecord {
	const cloned = cloneStructuredValue(value, path, new Set<object>());
	if (cloned === null || typeof cloned !== 'object' || isStructuredArray(cloned)) {
		throw new StoredAccountRuntimeRecoveryError(`${path} must be a plain structured record`);
	}
	return cloned;
}

function structuredValuesAreSame(left: StoredAccountStructuredValue, right: StoredAccountStructuredValue): boolean {
	if (Object.is(left, right)) {
		return true;
	}
	if (left === null || right === null || typeof left !== 'object' || typeof right !== 'object') {
		return false;
	}
	if (isStructuredArray(left) || isStructuredArray(right)) {
		if (!isStructuredArray(left) || !isStructuredArray(right) || left.length !== right.length) {
			return false;
		}
		const leftKeys = Object.keys(left);
		const rightKeys = Object.keys(right);
		if (leftKeys.length !== rightKeys.length) {
			return false;
		}
		for (const key of leftKeys) {
			if (!Object.hasOwn(right, key) || !structuredValuesAreSame(left[Number(key)], right[Number(key)])) {
				return false;
			}
		}
		return true;
	}
	const leftKeys = Object.keys(left);
	const rightKeys = Object.keys(right);
	if (leftKeys.length !== rightKeys.length) {
		return false;
	}
	for (const key of leftKeys) {
		if (!Object.hasOwn(right, key) || !structuredValuesAreSame(left[key], right[key])) {
			return false;
		}
	}
	return true;
}

function requireRevisionSnapshot(revision: StoredAccountRevision): StoredAccountRevisionSnapshot {
	const snapshot = REVISION_SNAPSHOTS.get(revision);
	if (snapshot === undefined) {
		throw new StoredAccountRuntimeRecoveryError('Stored account revision is not owned by this storage inventory');
	}
	return snapshot;
}

export function createStoredAccountRevision(value: unknown, source: StoredAccountSource): StoredAccountRevision {
	const record = cloneStoredAccountStructuredRecord(value, 'Stored account revision');
	const revision: StoredAccountRevision = Object.freeze({kind: 'stored-account-revision'});
	REVISION_SNAPSHOTS.set(revision, Object.freeze({source, record}));
	return revision;
}

export function storedAccountRevisionValue(
	revision: StoredAccountRevision,
	expectedSource: StoredAccountSource,
): StoredAccountStructuredRecord {
	const snapshot = requireRevisionSnapshot(revision);
	if (snapshot.source !== expectedSource) {
		throw new StoredAccountRuntimeRecoveryError('Stored account revision belongs to another storage authority');
	}
	return snapshot.record;
}

export function storedAccountRevisionsAreSame(left: StoredAccountRevision, right: StoredAccountRevision): boolean {
	const leftSnapshot = requireRevisionSnapshot(left);
	const rightSnapshot = requireRevisionSnapshot(right);
	return (
		leftSnapshot.source === rightSnapshot.source && structuredValuesAreSame(leftSnapshot.record, rightSnapshot.record)
	);
}
