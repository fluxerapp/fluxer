// SPDX-License-Identifier: AGPL-3.0-or-later

import {readdirSync, readFileSync, statSync} from 'node:fs';
import {join, relative} from 'node:path';
import {fileURLToPath} from 'node:url';
import {
	EVICTION_SYMBOLS,
	EVICTION_TARGET_PREFIX,
	FROZEN_STORAGE_REGISTRY,
	FrozenStorageDeletableAt,
	type FrozenStorageEntry,
	FrozenStorageKind,
} from '@app/features/platform/state/FrozenStorageRegistry';
import {isNotMigratedLegacyKey, NOT_MIGRATED_LEGACY_KEYS} from '@app/features/platform/state/LegacyAppStorageKeyMap';
import {describe, expect, test} from 'vitest';

const REPO_ROOT = fileURLToPath(new URL('../../../../../', import.meta.url));

const SCANNED_ROOTS: ReadonlyArray<string> = ['fluxer_app/src', 'fluxer_desktop/src', 'packages'];
const SCANNED_EXTENSIONS: ReadonlyArray<string> = ['.ts', '.tsx', '.mjs', '.cjs', '.js'];
const EXCLUDED_PATH_PATTERN = /node_modules|\.test\.|\.spec\.|__tests__|__fixtures__|\/dist\//;

const DESTRUCTIVE_CALL_PATTERNS: ReadonlyArray<(target: string) => RegExp> = [
	(target) => new RegExp(`removeItem\\(\\s*${target}`),
	(target) => new RegExp(`deleteDatabase\\(\\s*${target}`),
	(target) => new RegExp(`deleteObjectStore\\(\\s*${target}`),
	(target) => new RegExp(`writeRawStorageItem\\(\\s*${target}\\s*,\\s*null`),
];

function escapeForRegExp(value: string): string {
	return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function collectSourceFiles(root: string): Array<string> {
	const absoluteRoot = join(REPO_ROOT, root);
	const files: Array<string> = [];
	const walk = (directory: string): void => {
		for (const name of readdirSync(directory)) {
			const path = join(directory, name);
			if (EXCLUDED_PATH_PATTERN.test(path)) {
				continue;
			}
			if (statSync(path).isDirectory()) {
				walk(path);
				continue;
			}
			if (SCANNED_EXTENSIONS.some((extension) => name.endsWith(extension))) {
				files.push(path);
			}
		}
	};
	walk(absoluteRoot);
	return files;
}

const SOURCE_FILES: ReadonlyArray<string> = SCANNED_ROOTS.flatMap(collectSourceFiles);

const SOURCE_BY_PATH: ReadonlyMap<string, string> = new Map(
	SOURCE_FILES.map((path) => [relative(REPO_ROOT, path), readFileSync(path, 'utf8')]),
);

function destructiveReferences(entry: FrozenStorageEntry): Array<string> {
	const targets = [
		...entry.names.map((name) => `['"\`]${escapeForRegExp(name)}['"\`]`),
		...entry.aliases.map((alias) => escapeForRegExp(alias)),
	];
	const offenders: Array<string> = [];
	for (const [path, source] of SOURCE_BY_PATH) {
		if (entry.clearableBy.includes(path)) {
			continue;
		}
		for (const target of targets) {
			if (DESTRUCTIVE_CALL_PATTERNS.some((build) => build(target).test(source))) {
				offenders.push(path);
				break;
			}
		}
	}
	return offenders;
}

const NEVER_DELETABLE_ENTRIES = FROZEN_STORAGE_REGISTRY.filter(
	(entry) => entry.deletableAt === FrozenStorageDeletableAt.NEVER,
);

describe('the frozen storage registry describes real stores', () => {
	test('every entry has a unique id and at least one name', () => {
		const ids = FROZEN_STORAGE_REGISTRY.map((entry) => entry.id);
		expect(new Set(ids).size).toBe(ids.length);
		for (const entry of FROZEN_STORAGE_REGISTRY) {
			expect(entry.names.length).toBeGreaterThan(0);
		}
	});

	test('an IndexedDB entry pins its version and no other kind claims one', () => {
		for (const entry of FROZEN_STORAGE_REGISTRY) {
			if (entry.kind === FrozenStorageKind.INDEXED_DB) {
				expect(entry.version, entry.id).toBeTypeOf('number');
				continue;
			}
			expect(entry.version, entry.id).toBeNull();
		}
	});

	test('every declared writer module exists and still performs its write', () => {
		for (const entry of FROZEN_STORAGE_REGISTRY) {
			for (const writer of entry.writers) {
				const source = SOURCE_BY_PATH.get(writer.module);
				expect(source, `${entry.id}: ${writer.module} is gone`).toBeTypeOf('string');
				expect(source?.includes(writer.marker), `${entry.id}: ${writer.module} no longer writes ${writer.marker}`).toBe(
					true,
				);
			}
		}
	});

	test('a never-deletable store is written by live code, or is a legacy key nothing migrates', () => {
		for (const entry of NEVER_DELETABLE_ENTRIES) {
			if (entry.writers.length > 0) {
				continue;
			}
			for (const name of entry.names) {
				expect(isNotMigratedLegacyKey(name), `${entry.id}: ${name} is written by nobody and migrated anyway`).toBe(
					true,
				);
			}
		}
	});

	test('the registry classifies every frozen legacy key', () => {
		const registered = new Set(
			FROZEN_STORAGE_REGISTRY.filter(
				(entry) => entry.kind === FrozenStorageKind.LOCAL_STORAGE || entry.kind === FrozenStorageKind.SESSION_STORAGE,
			).flatMap((entry) => entry.names),
		);
		for (const key of NOT_MIGRATED_LEGACY_KEYS) {
			expect(registered.has(key), `${key} is frozen out of the migration but absent from the registry`).toBe(true);
		}
	});
});

describe('nothing deletes a never-deletable store', () => {
	test('no source file deletes an IndexedDB database or object store', () => {
		const offenders = [...SOURCE_BY_PATH]
			.filter(([, source]) => /deleteDatabase\(|deleteObjectStore\(/.test(source))
			.map(([path]) => path);
		expect(offenders).toEqual([]);
	});

	test.each(NEVER_DELETABLE_ENTRIES.map((entry) => [entry.id, entry] as const))(
		'%s is cleared only by the modules that own it',
		(_id, entry) => {
			expect(destructiveReferences(entry)).toEqual([]);
		},
	);
});

describe('eviction and prune stay bounded', () => {
	test('no eviction path exists outside the state cache', () => {
		for (const [path, source] of SOURCE_BY_PATH) {
			for (const symbol of EVICTION_SYMBOLS) {
				if (!source.includes(symbol)) {
					continue;
				}
				expect(
					source.includes(EVICTION_TARGET_PREFIX),
					`${path} evicts something that is not ${EVICTION_TARGET_PREFIX}`,
				).toBe(true);
			}
		}
	});

	test('the account prune contract cannot be called without an authoritative key list', () => {
		const contract = SOURCE_BY_PATH.get('packages/desktop_ipc/src/AccountContract.ts');
		expect(contract).toContain('readonly listIsAuthoritative: boolean;');
	});
});
