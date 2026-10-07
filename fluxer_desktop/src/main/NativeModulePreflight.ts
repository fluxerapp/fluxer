// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {existsSync, mkdirSync, readFileSync, writeFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import path from 'node:path';
import {APP_STORE_ADDON_PACKAGE} from '@electron/main/AppStoreNativeBoundary';
import {GATEWAY_SOCKET_ADDON_PACKAGE} from '@electron/main/GatewaySocketNativeBoundary';
import {app} from 'electron';
import log from 'electron-log';

const requireModule = createRequire(import.meta.url);
const PREFLIGHT_TIMEOUT_MS = 60_000;
const SKIP_PREFLIGHT_ENV = 'FLUXER_SKIP_NATIVE_PREFLIGHT';
const PREFLIGHT_MARKER_FILENAME = 'native-module-preflight-v2.json';
const PREFLIGHT_MARKER_VERSION = 2;

interface PreflightMarker {
	readonly fingerprint: string;
	readonly completedAt: string;
	readonly degraded: ReadonlyArray<string>;
}

type NativeModuleSeverity = 'fatal' | 'degraded';

interface NativeModulePreflightSpec {
	readonly name: string;
	readonly platforms: ReadonlySet<NodeJS.Platform>;
	readonly severity: NativeModuleSeverity;
}

interface NativeModulePreflightFailure {
	readonly name: string;
	readonly severity: NativeModuleSeverity;
	readonly modulePath?: string;
	readonly reason: string;
	readonly stdout?: string;
	readonly stderr?: string;
}

export interface NativeModulePreflightResult {
	readonly degraded: ReadonlyArray<string>;
}

const NATIVE_MODULE_PREFLIGHT_SPECS: ReadonlyArray<NativeModulePreflightSpec> = [
	{
		name: '@fluxer/webauthn',
		platforms: new Set(['darwin', 'linux', 'win32']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/platform-info',
		platforms: new Set(['darwin', 'linux', 'win32']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/mac-app-audio',
		platforms: new Set(['darwin']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/mac-clipboard',
		platforms: new Set(['darwin']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/mac-sysctl',
		platforms: new Set(['darwin']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/mac-tcc',
		platforms: new Set(['darwin']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/macos-input-hook',
		platforms: new Set(['darwin']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/linux-audio-capture',
		platforms: new Set(['linux']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/linux-evdev',
		platforms: new Set(['linux']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/linux-input-hook',
		platforms: new Set(['linux']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/linux-notifications',
		platforms: new Set(['linux']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/linux-portals',
		platforms: new Set(['linux']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/linux-screen-capture',
		platforms: new Set(['linux']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/system-hunspell',
		platforms: new Set(['linux']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/win-clipboard',
		platforms: new Set(['win32']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/win-process-loopback',
		platforms: new Set(['win32']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/win-shell',
		platforms: new Set(['win32']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/win-toast',
		platforms: new Set(['win32']),
		severity: 'fatal',
	},
	{
		name: '@fluxer/windows-input-hook',
		platforms: new Set(['win32']),
		severity: 'fatal',
	},
	{
		name: APP_STORE_ADDON_PACKAGE,
		platforms: new Set(['darwin', 'linux', 'win32']),
		severity: 'degraded',
	},
	{
		name: GATEWAY_SOCKET_ADDON_PACKAGE,
		platforms: new Set(['darwin', 'linux', 'win32']),
		severity: 'degraded',
	},
];
const PROBE_SCRIPT = `
const modulePaths = process.argv.slice(1);
function write(obj) {
	process.stdout.write(JSON.stringify(obj) + '\\n');
}
for (let i = 0; i < modulePaths.length; i++) {
	const modulePath = modulePaths[i];
	write({i, modulePath, phase: 'start'});
	try {
		const mod = require(modulePath);
		if (mod && mod.loadError) {
			const error = mod.loadError;
			write({
				i,
				modulePath,
				phase: 'load-error',
				message: error && error.message ? String(error.message) : String(error),
				diagnostics: error && error.nativeDiagnostics ? error.nativeDiagnostics : null,
				stack: error && error.stack ? String(error.stack) : null,
			});
			continue;
		}
		write({i, modulePath, phase: 'done'});
	} catch (error) {
		write({
			i,
			modulePath,
			phase: 'throw',
			message: error && error.message ? String(error.message) : String(error),
			diagnostics: error && error.nativeDiagnostics ? error.nativeDiagnostics : null,
			stack: error && error.stack ? String(error.stack) : null,
		});
	}
}
`;

interface ProbeRecord {
	i: number;
	modulePath: string;
	phase: 'start' | 'done' | 'load-error' | 'throw';
	message?: string;
	stack?: string;
	diagnostics?: unknown;
}

function parseProbeRecords(stdout: string): Array<ProbeRecord> {
	const records: Array<ProbeRecord> = [];
	for (const line of stdout.split('\n')) {
		const trimmed = line.trim();
		if (!trimmed) continue;
		try {
			records.push(JSON.parse(trimmed) as ProbeRecord);
		} catch {}
	}
	return records;
}

function trimOutput(value: string | null | undefined): string | undefined {
	const trimmed = value?.trim();
	if (!trimmed) return undefined;
	return trimmed.length > 20000 ? `${trimmed.slice(0, 20000)}\n...<truncated>` : trimmed;
}

function currentPlatformSpecs(): ReadonlyArray<NativeModulePreflightSpec> {
	return NATIVE_MODULE_PREFLIGHT_SPECS.filter((spec) => spec.platforms.has(process.platform));
}

function shouldRunNativeModulePreflight(): boolean {
	if (process.env[SKIP_PREFLIGHT_ENV] === '1') return false;
	return app.isPackaged;
}

function preflightMarkerPath(): string {
	return path.join(app.getPath('userData'), PREFLIGHT_MARKER_FILENAME);
}

function preflightFingerprint(resolved: ReadonlyArray<ResolvedSpec>): string {
	const hash = createHash('sha256');
	hash.update(process.execPath);
	hash.update('\0');
	hash.update(app.getVersion());
	hash.update('\0');
	hash.update(process.platform);
	hash.update('/');
	hash.update(process.arch);
	hash.update('\0');
	for (const entry of resolved) {
		hash.update(entry.spec.name);
		hash.update('=');
		hash.update(entry.modulePath);
		hash.update('\0');
	}
	return hash.digest('hex');
}

function parsePreflightMarker(raw: string): PreflightMarker | null {
	let parsed: {version?: unknown; fingerprint?: unknown; completedAt?: unknown; degraded?: unknown};
	try {
		parsed = JSON.parse(raw) as typeof parsed;
	} catch {
		return null;
	}
	if (typeof parsed.fingerprint !== 'string') return null;
	const completedAt = typeof parsed.completedAt === 'string' ? parsed.completedAt : '';
	if (parsed.version === 1) return {fingerprint: parsed.fingerprint, completedAt, degraded: []};
	if (parsed.version !== PREFLIGHT_MARKER_VERSION) return null;
	if (!Array.isArray(parsed.degraded) || parsed.degraded.some((name) => typeof name !== 'string')) return null;
	return {fingerprint: parsed.fingerprint, completedAt, degraded: parsed.degraded as Array<string>};
}

function readPreflightMarker(): PreflightMarker | null {
	try {
		return parsePreflightMarker(readFileSync(preflightMarkerPath(), 'utf-8'));
	} catch {
		return null;
	}
}

function writePreflightMarker(fingerprint: string, degraded: ReadonlyArray<string>): void {
	try {
		const markerPath = preflightMarkerPath();
		mkdirSync(path.dirname(markerPath), {recursive: true});
		writeFileSync(
			markerPath,
			JSON.stringify({
				version: PREFLIGHT_MARKER_VERSION,
				fingerprint,
				completedAt: new Date().toISOString(),
				degraded,
			}),
			'utf-8',
		);
	} catch (error) {
		log.warn('[NativeModulePreflight] Failed to persist preflight marker', error);
	}
}

function clearPreflightMarker(): void {
	try {
		const markerPath = preflightMarkerPath();
		if (existsSync(markerPath)) writeFileSync(markerPath, '', 'utf-8');
	} catch {}
}

interface ResolvedSpec {
	spec: NativeModulePreflightSpec;
	modulePath: string;
}

function resolveSpecs(): {resolved: Array<ResolvedSpec>; failures: Array<NativeModulePreflightFailure>} {
	const resolved: Array<ResolvedSpec> = [];
	const failures: Array<NativeModulePreflightFailure> = [];
	for (const spec of currentPlatformSpecs()) {
		try {
			resolved.push({spec, modulePath: requireModule.resolve(spec.name)});
		} catch (error) {
			failures.push({
				name: spec.name,
				severity: spec.severity,
				reason: `failed to resolve module: ${error instanceof Error ? error.message : String(error)}`,
			});
		}
	}
	return {resolved, failures};
}

interface ProbeOutcome {
	readonly inconclusive: string | null;
	readonly failures: Array<NativeModulePreflightFailure>;
}

function probeAllModules(resolved: Array<ResolvedSpec>): ProbeOutcome {
	if (resolved.length === 0) return {inconclusive: null, failures: []};
	const result = spawnSync(process.execPath, ['-e', PROBE_SCRIPT, ...resolved.map((entry) => entry.modulePath)], {
		env: {
			...process.env,
			ELECTRON_RUN_AS_NODE: '1',
			FLUXER_NATIVE_MODULE_PREFLIGHT_CHILD: '1',
		},
		encoding: 'utf8',
		timeout: PREFLIGHT_TIMEOUT_MS,
		maxBuffer: 32 * 1024 * 1024,
	});
	if (result.error) {
		return {inconclusive: result.error.message, failures: []};
	}
	const stdout = result.stdout ?? '';
	const stderr = result.stderr ?? '';
	const records = parseProbeRecords(stdout);
	if (records.length === 0 && result.status !== 0) {
		return {
			inconclusive: result.signal
				? `preflight child terminated by signal ${result.signal} without running any probe`
				: `preflight child exited with code ${result.status} without running any probe`,
			failures: [],
		};
	}
	const byIndex = new Map<number, Array<ProbeRecord>>();
	for (const record of records) {
		const list = byIndex.get(record.i) ?? [];
		list.push(record);
		byIndex.set(record.i, list);
	}
	const failures: Array<NativeModulePreflightFailure> = [];
	for (let i = 0; i < resolved.length; i++) {
		const {spec, modulePath} = resolved[i];
		const recordsForModule = byIndex.get(i) ?? [];
		const startedRecord = recordsForModule.find((r) => r.phase === 'start');
		const completionRecord = recordsForModule.find(
			(r) => r.phase === 'done' || r.phase === 'load-error' || r.phase === 'throw',
		);
		if (!startedRecord) {
			const earlierMissing = Array.from({length: i}).some(
				(_, j) => !(byIndex.get(j) ?? []).some((r) => r.phase === 'start'),
			);
			if (earlierMissing) continue;
			failures.push({
				name: spec.name,
				severity: spec.severity,
				modulePath,
				reason: result.signal
					? `child process terminated by signal ${result.signal} before this probe ran`
					: result.status !== null && result.status !== 0
						? `child process exited with code ${result.status} before this probe ran`
						: 'child process did not start probe',
				stdout: trimOutput(stdout),
				stderr: trimOutput(stderr),
			});
			continue;
		}
		if (!completionRecord) {
			failures.push({
				name: spec.name,
				severity: spec.severity,
				modulePath,
				reason: result.signal
					? `module require crashed: child terminated by signal ${result.signal}`
					: `module require crashed: child exited with code ${result.status ?? '<none>'}`,
				stdout: trimOutput(stdout),
				stderr: trimOutput(stderr),
			});
			continue;
		}
		if (completionRecord.phase === 'load-error' || completionRecord.phase === 'throw') {
			failures.push({
				name: spec.name,
				severity: spec.severity,
				modulePath,
				reason: completionRecord.phase === 'load-error' ? 'module reported loadError' : 'module require threw',
				stdout: completionRecord.message ?? undefined,
				stderr: completionRecord.stack ?? trimOutput(stderr),
			});
		}
	}
	return {inconclusive: null, failures};
}

function formatNativeModulePreflightFailure(failure: NativeModulePreflightFailure): string {
	return [
		`- ${failure.name}: ${failure.reason}`,
		failure.modulePath ? `  module: ${failure.modulePath}` : '',
		failure.stderr ? `  stderr:\n${failure.stderr}` : '',
		failure.stdout ? `  stdout:\n${failure.stdout}` : '',
	]
		.filter(Boolean)
		.join('\n');
}

export function runNativeModulePreflight(): NativeModulePreflightResult {
	if (!shouldRunNativeModulePreflight()) {
		return {degraded: []};
	}
	const {resolved, failures: resolveFailures} = resolveSpecs();
	const fingerprint = preflightFingerprint(resolved);
	if (!resolveFailures.some((failure) => failure.severity === 'fatal')) {
		const marker = readPreflightMarker();
		if (marker && marker.fingerprint === fingerprint) {
			log.info('[NativeModulePreflight] Skipped: install fingerprint matches successful previous run', {
				completedAt: marker.completedAt,
				degraded: marker.degraded,
			});
			return {degraded: marker.degraded};
		}
	}
	const outcome = probeAllModules(resolved);
	if (outcome.inconclusive !== null && resolveFailures.length === 0) {
		log.warn('[NativeModulePreflight] Skipped: preflight could not be completed on this machine', {
			reason: outcome.inconclusive,
		});
		return {degraded: []};
	}
	const failures = [...resolveFailures, ...outcome.failures];
	const fatal = failures.filter((failure) => failure.severity === 'fatal');
	const degraded = failures.filter((failure) => failure.severity === 'degraded');
	if (degraded.length > 0) {
		log.warn(
			'[NativeModulePreflight] Optional native modules are unavailable. The app continues without them',
			degraded.map(formatNativeModulePreflightFailure).join('\n'),
		);
	}
	if (fatal.length > 0) {
		clearPreflightMarker();
		const details = fatal.map(formatNativeModulePreflightFailure).join('\n');
		throw new Error(`Fluxer native module preflight failed on ${process.platform}/${process.arch}.\n${details}`);
	}
	const degradedModules = degraded.map((failure) => failure.name);
	writePreflightMarker(fingerprint, degradedModules);
	return {degraded: degradedModules};
}
