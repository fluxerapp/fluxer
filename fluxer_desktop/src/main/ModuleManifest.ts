// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Buffer} from 'node:buffer';
import {
	compareModuleVersions,
	type ModuleVersion,
	ModuleVersionMalformedError,
	parseModuleVersion,
} from '@electron/main/ModuleVersion';
import {isDesktopModuleName} from '@fluxer/desktop_ipc/src/ModuleContract';

const MODULE_MANIFEST_VERSION = 1;
const MODULE_PACKAGE_MAX_BYTES = 2 * 1024 * 1024 * 1024;
const SHA256_PATTERN = /^[0-9a-f]{64}$/u;
const MODULE_BUILD_VERSION_PATTERN = /^\d+\.\d+\.\d+$/u;

export type DesktopModuleArchitecture = 'arm64' | 'x64';

export interface DesktopModuleManifestTarget {
	readonly releaseChannel: string;
	readonly platform: string;
	readonly arch: DesktopModuleArchitecture;
}

export interface DesktopModuleManifestEntry {
	readonly sha256: string;
	readonly bytes: number;
	readonly url: string;
	readonly minimumShellVersion: ModuleVersion | null;
	readonly maximumShellVersion: ModuleVersion | null;
}

export interface DesktopModuleShellRequirement {
	readonly latestVersion: ModuleVersion;
	readonly minimumVersion: ModuleVersion;
}

export interface DesktopLinuxSecurityMinimum {
	readonly version: ModuleVersion;
	readonly requiredModules: ReadonlyArray<string>;
}

export interface DesktopModuleUpdateManifest {
	readonly manifestVersion: number;
	readonly releaseChannel: string;
	readonly platform: string;
	readonly arch: DesktopModuleArchitecture;
	readonly buildVersion: ModuleVersion;
	readonly pubDate: string;
	readonly metadataVersion: number;
	readonly shell: DesktopModuleShellRequirement;
	readonly modules: Readonly<Record<string, DesktopModuleManifestEntry>>;
	readonly requiredModules: ReadonlyArray<string>;
	readonly linuxSecurityMinimum: DesktopLinuxSecurityMinimum | null;
}

class ModuleManifestMalformedError extends Error {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModuleManifestMalformedError';
	}
}

class UnsupportedModuleArchitectureError extends Error {
	public readonly architecture: string;

	public constructor(architecture: string) {
		super(`Unsupported desktop module architecture: ${architecture}`);
		this.name = 'UnsupportedModuleArchitectureError';
		this.architecture = architecture;
	}
}

export function resolveDesktopModuleArchitecture(architecture: NodeJS.Architecture): DesktopModuleArchitecture {
	switch (architecture) {
		case 'arm64':
		case 'x64':
			return architecture;
		default:
			throw new UnsupportedModuleArchitectureError(architecture);
	}
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function requireString(record: Record<string, unknown>, key: string): string {
	const value = record[key];
	if (typeof value !== 'string' || value.length === 0) {
		throw new ModuleManifestMalformedError(`manifest field ${key} must be a non-empty string`);
	}
	return value;
}

function requireInteger(record: Record<string, unknown>, key: string, minimum: number, maximum: number): number {
	const value = record[key];
	if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < minimum || value > maximum) {
		throw new ModuleManifestMalformedError(`manifest field ${key} must be an integer in [${minimum}, ${maximum}]`);
	}
	return value;
}

function parseVersion(value: string, description: string): ModuleVersion {
	try {
		return parseModuleVersion(value, description);
	} catch (error) {
		if (error instanceof ModuleVersionMalformedError) {
			throw new ModuleManifestMalformedError(error.message, {cause: error});
		}
		throw error;
	}
}

function requireVersion(record: Record<string, unknown>, key: string, description: string): ModuleVersion {
	return parseVersion(requireString(record, key), description);
}

function optionalVersion(record: Record<string, unknown>, key: string, description: string): ModuleVersion | null {
	const value = record[key];
	if (value === null || value === undefined) {
		return null;
	}
	if (typeof value !== 'string' || value.length === 0) {
		throw new ModuleManifestMalformedError(`manifest field ${key} must be a version string or null`);
	}
	return parseVersion(value, description);
}

function parseShellRequirement(value: unknown): DesktopModuleShellRequirement {
	if (!isRecord(value)) {
		throw new ModuleManifestMalformedError('manifest field shell must be an object');
	}
	return Object.freeze({
		latestVersion: requireVersion(value, 'latest_version', 'manifest shell.latest_version'),
		minimumVersion: requireVersion(value, 'minimum_version', 'manifest shell.minimum_version'),
	});
}

function parseManifestEntry(moduleName: string, value: unknown): DesktopModuleManifestEntry {
	if (!isRecord(value)) {
		throw new ModuleManifestMalformedError(`manifest entry ${moduleName} must be an object`);
	}
	const sha256 = requireString(value, 'sha256');
	if (!SHA256_PATTERN.test(sha256)) {
		throw new ModuleManifestMalformedError(`manifest entry ${moduleName} sha256 is not a digest`);
	}
	const url = requireString(value, 'url');
	if (!URL.canParse(url)) {
		throw new ModuleManifestMalformedError(`manifest entry ${moduleName} url is not absolute`);
	}
	return Object.freeze({
		sha256,
		bytes: requireInteger(value, 'bytes', 0, MODULE_PACKAGE_MAX_BYTES),
		url,
		minimumShellVersion: optionalVersion(
			value,
			'minimum_shell_version',
			`manifest module ${moduleName}.minimum_shell_version`,
		),
		maximumShellVersion: optionalVersion(
			value,
			'maximum_shell_version',
			`manifest module ${moduleName}.maximum_shell_version`,
		),
	});
}

function requireBuildVersion(record: Record<string, unknown>): ModuleVersion {
	const source = requireString(record, 'build_version');
	const version = parseVersion(source, 'manifest build_version');
	if (!MODULE_BUILD_VERSION_PATTERN.test(source)) {
		throw new ModuleManifestMalformedError(
			`manifest build_version must contain exactly three numeric components: ${source}`,
		);
	}
	return version;
}

export function parseModuleUpdateManifest(
	bytes: Buffer,
	expected: DesktopModuleManifestTarget,
): DesktopModuleUpdateManifest {
	let parsed: unknown;
	try {
		parsed = JSON.parse(bytes.toString('utf8'));
	} catch (error) {
		throw new ModuleManifestMalformedError('manifest is not valid json', {cause: error});
	}
	if (!isRecord(parsed)) {
		throw new ModuleManifestMalformedError('manifest must be an object');
	}
	const manifestVersion = requireInteger(parsed, 'manifest_version', 1, Number.MAX_SAFE_INTEGER);
	if (manifestVersion !== MODULE_MANIFEST_VERSION) {
		throw new ModuleManifestMalformedError(`unsupported manifest_version ${manifestVersion}`);
	}
	const releaseChannel = requireString(parsed, 'release_channel');
	const platform = requireString(parsed, 'platform');
	const arch = requireString(parsed, 'arch');
	if (releaseChannel !== expected.releaseChannel || platform !== expected.platform || arch !== expected.arch) {
		throw new ModuleManifestMalformedError(
			`manifest targets ${releaseChannel}/${platform}/${arch}, expected ${expected.releaseChannel}/${expected.platform}/${expected.arch}`,
		);
	}
	const rawModules = parsed['modules'];
	if (!isRecord(rawModules)) {
		throw new ModuleManifestMalformedError('manifest field modules must be an object');
	}
	const modules: Record<string, DesktopModuleManifestEntry> = {};
	for (const [moduleName, entry] of Object.entries(rawModules)) {
		if (!isDesktopModuleName(moduleName)) {
			throw new ModuleManifestMalformedError(`unsupported module name: ${moduleName}`);
		}
		modules[moduleName] = parseManifestEntry(moduleName, entry);
	}
	const rawRequired = parsed['required_modules'];
	if (!Array.isArray(rawRequired)) {
		throw new ModuleManifestMalformedError('manifest field required_modules must be an array');
	}
	const requiredModules: Array<string> = [];
	const seenRequiredModules = new Set<string>();
	for (const moduleName of rawRequired) {
		if (typeof moduleName !== 'string' || !Object.hasOwn(modules, moduleName)) {
			throw new ModuleManifestMalformedError(`required_modules names an undeclared module: ${String(moduleName)}`);
		}
		if (seenRequiredModules.has(moduleName)) {
			throw new ModuleManifestMalformedError(`required_modules contains a duplicate module: ${moduleName}`);
		}
		seenRequiredModules.add(moduleName);
		requiredModules.push(moduleName);
	}
	const buildVersion = requireBuildVersion(parsed);
	const shell = parseShellRequirement(parsed['shell']);
	const rawLinuxSecurityMinimum = parsed['linux_security_minimum'];
	let linuxSecurityMinimum: DesktopLinuxSecurityMinimum | null = null;
	if (rawLinuxSecurityMinimum != null) {
		if (!isRecord(rawLinuxSecurityMinimum)) {
			throw new ModuleManifestMalformedError('manifest linux_security_minimum must be an object or null');
		}
		const rawSecurityModules = rawLinuxSecurityMinimum['required_modules'];
		if (!Array.isArray(rawSecurityModules)) {
			throw new ModuleManifestMalformedError('manifest linux_security_minimum.required_modules must be an array');
		}
		const securityModules: Array<string> = [];
		const seenSecurityModules = new Set<string>();
		for (const moduleName of rawSecurityModules) {
			if (typeof moduleName !== 'string' || !Object.hasOwn(modules, moduleName)) {
				throw new ModuleManifestMalformedError(
					`linux_security_minimum.required_modules names an undeclared module: ${String(moduleName)}`,
				);
			}
			if (seenSecurityModules.has(moduleName)) {
				throw new ModuleManifestMalformedError(
					`linux_security_minimum.required_modules contains a duplicate module: ${moduleName}`,
				);
			}
			seenSecurityModules.add(moduleName);
			securityModules.push(moduleName);
		}
		securityModules.sort();
		linuxSecurityMinimum = Object.freeze({
			version: requireVersion(rawLinuxSecurityMinimum, 'version', 'manifest linux_security_minimum.version'),
			requiredModules: Object.freeze(securityModules),
		});
	}
	if (platform !== 'linux' && linuxSecurityMinimum != null) {
		throw new ModuleManifestMalformedError('manifest linux security minimum is only valid for Linux');
	}
	if (linuxSecurityMinimum != null) {
		if (compareModuleVersions(buildVersion, linuxSecurityMinimum.version) < 0) {
			throw new ModuleManifestMalformedError('manifest build_version cannot be below linux_security_minimum.version');
		}
		if (compareModuleVersions(shell.minimumVersion, linuxSecurityMinimum.version) < 0) {
			throw new ModuleManifestMalformedError(
				'manifest shell.minimum_version cannot be below linux_security_minimum.version',
			);
		}
	}
	return Object.freeze({
		manifestVersion,
		releaseChannel,
		platform,
		arch: expected.arch,
		buildVersion,
		pubDate: requireString(parsed, 'pub_date'),
		metadataVersion: requireInteger(parsed, 'metadata_version', 0, Number.MAX_SAFE_INTEGER),
		shell,
		modules: Object.freeze(modules),
		requiredModules: Object.freeze(requiredModules),
		linuxSecurityMinimum,
	});
}

export function blockingModuleNames(manifest: DesktopModuleUpdateManifest): ReadonlySet<string> {
	const names = new Set(manifest.requiredModules);
	for (const moduleName of manifest.linuxSecurityMinimum?.requiredModules ?? []) {
		names.add(moduleName);
	}
	return names;
}
