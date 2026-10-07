// SPDX-License-Identifier: AGPL-3.0-or-later

const SEMVER_PATTERN =
	/^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[A-Za-z-][0-9A-Za-z-]*)(?:\.(?:0|[1-9]\d*|\d*[A-Za-z-][0-9A-Za-z-]*))*))?(?:\+([0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*))?$/u;

const ModuleVersionIdentifierKind = Object.freeze({
	NUMERIC: 'numeric',
	TEXT: 'text',
} as const);

type ModuleVersionIdentifier =
	| {readonly kind: typeof ModuleVersionIdentifierKind.NUMERIC; readonly value: bigint}
	| {readonly kind: typeof ModuleVersionIdentifierKind.TEXT; readonly value: string};

export interface ModuleVersion {
	readonly source: string;
	readonly major: bigint;
	readonly minor: bigint;
	readonly patch: bigint;
	readonly prerelease: ReadonlyArray<ModuleVersionIdentifier>;
}

export class ModuleVersionMalformedError extends Error {
	public constructor(description: string, value: string) {
		super(`${description} is not a valid semantic version: ${value}`);
		this.name = 'ModuleVersionMalformedError';
	}
}

function parsePrereleaseIdentifier(identifier: string): ModuleVersionIdentifier {
	if (/^\d+$/u.test(identifier)) {
		return {kind: ModuleVersionIdentifierKind.NUMERIC, value: BigInt(identifier)};
	}
	return {kind: ModuleVersionIdentifierKind.TEXT, value: identifier};
}

export function parseModuleVersion(value: string, description: string): ModuleVersion {
	const match = SEMVER_PATTERN.exec(value);
	if (match == null) {
		throw new ModuleVersionMalformedError(description, value);
	}
	const prerelease = match[4] == null ? [] : match[4].split('.').map(parsePrereleaseIdentifier);
	return Object.freeze({
		source: value,
		major: BigInt(match[1]),
		minor: BigInt(match[2]),
		patch: BigInt(match[3]),
		prerelease: Object.freeze(prerelease),
	});
}

function compareBigInt(left: bigint, right: bigint): number {
	if (left === right) {
		return 0;
	}
	return left < right ? -1 : 1;
}

function comparePrereleaseIdentifier(left: ModuleVersionIdentifier, right: ModuleVersionIdentifier): number {
	if (left.kind === ModuleVersionIdentifierKind.NUMERIC) {
		if (right.kind === ModuleVersionIdentifierKind.TEXT) {
			return -1;
		}
		return compareBigInt(left.value, right.value);
	}
	if (right.kind === ModuleVersionIdentifierKind.NUMERIC) {
		return 1;
	}
	if (left.value === right.value) {
		return 0;
	}
	return left.value < right.value ? -1 : 1;
}

export function compareModuleVersions(left: ModuleVersion, right: ModuleVersion): number {
	for (const [leftPart, rightPart] of [
		[left.major, right.major],
		[left.minor, right.minor],
		[left.patch, right.patch],
	] as const) {
		const comparison = compareBigInt(leftPart, rightPart);
		if (comparison !== 0) {
			return comparison;
		}
	}
	if (left.prerelease.length === 0 || right.prerelease.length === 0) {
		if (left.prerelease.length === right.prerelease.length) {
			return 0;
		}
		return left.prerelease.length === 0 ? 1 : -1;
	}
	const sharedLength = Math.min(left.prerelease.length, right.prerelease.length);
	for (let index = 0; index < sharedLength; index += 1) {
		const comparison = comparePrereleaseIdentifier(left.prerelease[index], right.prerelease[index]);
		if (comparison !== 0) {
			return comparison;
		}
	}
	if (left.prerelease.length === right.prerelease.length) {
		return 0;
	}
	return left.prerelease.length < right.prerelease.length ? -1 : 1;
}
