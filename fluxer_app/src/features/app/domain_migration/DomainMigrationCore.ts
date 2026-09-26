// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import type {StoredAccount} from '@app/features/auth/state/AccountStorage';
import {isIOSMobileOrTabletUserAgent} from '@app/features/platform/notifications/NotificationAlertOptions';
import {PASSKEY_MIGRATION_RP_ID} from '@fluxer/constants/src/PasskeyConstants';
import type {DomainMigrationDiscoveryResponse} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';

export const DOMAIN_MIGRATION_SOURCE_TO_TARGET: Readonly<Record<string, string>> = {
	'https://web.fluxer.app': 'https://fluxer.com',
	'https://web.canary.fluxer.app': 'https://canary.fluxer.com',
};

export const DOMAIN_MIGRATION_TARGET_TO_SOURCE: Readonly<Record<string, string>> = Object.fromEntries(
	Object.entries(DOMAIN_MIGRATION_SOURCE_TO_TARGET).map(([source, target]) => [target, source]),
);

export const DOMAIN_MIGRATION_MARKER_KEY = 'fluxer:domain-migration';
export const DOMAIN_MIGRATION_DEVICE_KEY = 'fluxer:domain-migration:device';
export const DOMAIN_MIGRATION_PENDING_KEY = 'fluxer:domain-migration:pending';
export const DOMAIN_MIGRATION_INTENT_KEY = 'fluxer:domain-migration:intent';
export const DOMAIN_MIGRATION_NOTIFICATIONS_KEY = 'fluxer:domain-migration:notifications';
export const DOMAIN_MIGRATION_MOVED_DISMISSED_KEY = 'fluxer:domain-migration:moved-dismissed-at';

export const DOMAIN_MIGRATION_PAYLOAD_VERSION = 1;
export const DOMAIN_MIGRATION_DEFAULT_NEXT_PATH = '/channels/@me';
export const DOMAIN_MIGRATION_MAX_FAILED_ATTEMPTS = 3;
export const DOMAIN_MIGRATION_FAILED_RETRY_DELAY_MS = 24 * 60 * 60 * 1000;
export const DOMAIN_MIGRATION_PENDING_MAX_AGE_MS = 10 * 60 * 1000;
export const DOMAIN_MIGRATION_CUSTOM_SOUNDS_MAX_BYTES = 4 * 1024 * 1024;
export const DOMAIN_MIGRATION_THEME_ASSETS_MAX_BYTES = 2 * 1024 * 1024;
export const DOMAIN_MIGRATION_MOVED_DISMISS_MS = 7 * 24 * 60 * 60 * 1000;

const NEXT_PATH_BASE = 'https://next.invalid';

function hasUnsafeNextPathCharacter(value: string): boolean {
	for (let index = 0; index < value.length; index++) {
		const code = value.charCodeAt(index);
		if (code <= 0x1f || code === 0x7f || code === 0x5c) {
			return true;
		}
	}
	return false;
}

const DENIED_LOCAL_STORAGE_KEYS: ReadonlySet<string> = new Set([
	'fluxer.lastPushEndpoint',
	'__test__',
	'runtimeConfig',
]);
const PUSH_SUBSCRIPTION_KEY_PATTERN = /push[-_.:]?(?:subscription|endpoint)/iu;

export interface StorageLike {
	getItem(key: string): string | null;
	setItem(key: string, value: string): void;
	removeItem(key: string): void;
	key(index: number): string | null;
	readonly length: number;
}

export type DomainMigrationSide =
	| {role: 'source'; source: string; target: string}
	| {role: 'target'; source: string; target: string};

export function resolveDomainMigrationSide(origin: string): DomainMigrationSide | null {
	const target = DOMAIN_MIGRATION_SOURCE_TO_TARGET[origin];
	if (target !== undefined) {
		return {role: 'source', source: origin, target};
	}
	const source = DOMAIN_MIGRATION_TARGET_TO_SOURCE[origin];
	if (source !== undefined) {
		return {role: 'target', source, target: origin};
	}
	return null;
}

export function sanitizeNextPath(value: unknown): string {
	if (typeof value !== 'string' || !value.startsWith('/') || hasUnsafeNextPathCharacter(value)) {
		return DOMAIN_MIGRATION_DEFAULT_NEXT_PATH;
	}
	let url: URL;
	try {
		url = new URL(value, NEXT_PATH_BASE);
	} catch {
		return DOMAIN_MIGRATION_DEFAULT_NEXT_PATH;
	}
	if (url.origin !== NEXT_PATH_BASE || url.pathname.startsWith('/migrate')) {
		return DOMAIN_MIGRATION_DEFAULT_NEXT_PATH;
	}
	const pathname = url.pathname === '/' ? '/app' : url.pathname;
	return `${pathname}${url.search}${url.hash}`;
}

export function buildTargetUrl(target: string, pathname: string, search: string, hash: string): string {
	return `${target}${sanitizeNextPath(`${pathname}${search}${hash}`)}`;
}

export type DomainMigrationMarker =
	| {state: 'completed'; target: string; at: number}
	| {state: 'failed'; at: number; attempts: number};

export function parseDomainMigrationMarker(raw: string | null): DomainMigrationMarker | null {
	if (!raw) {
		return null;
	}
	try {
		const value = JSON.parse(raw) as Record<string, unknown>;
		if (typeof value !== 'object' || value === null || typeof value.at !== 'number') {
			return null;
		}
		if (value.state === 'completed' && typeof value.target === 'string') {
			return {state: 'completed', target: value.target, at: value.at};
		}
		if (value.state === 'failed') {
			const attempts = typeof value.attempts === 'number' && value.attempts > 0 ? value.attempts : 1;
			return {state: 'failed', at: value.at, attempts};
		}
		return null;
	} catch {
		return null;
	}
}

export function readDomainMigrationMarker(storage: StorageLike | null): DomainMigrationMarker | null {
	try {
		return parseDomainMigrationMarker(storage?.getItem(DOMAIN_MIGRATION_MARKER_KEY) ?? null);
	} catch {
		return null;
	}
}

function writeDomainMigrationMarker(storage: StorageLike | null, marker: DomainMigrationMarker): void {
	try {
		storage?.setItem(DOMAIN_MIGRATION_MARKER_KEY, JSON.stringify(marker));
	} catch {}
}

export function markDomainMigrationCompleted(storage: StorageLike | null, target: string, now: number): void {
	writeDomainMigrationMarker(storage, {state: 'completed', target, at: now});
}

export function markDomainMigrationFailed(storage: StorageLike | null, now: number): void {
	const previous = readDomainMigrationMarker(storage);
	const attempts = previous?.state === 'failed' ? previous.attempts + 1 : 1;
	writeDomainMigrationMarker(storage, {state: 'failed', at: now, attempts});
}

export function markerAllowsDomainMigration(marker: DomainMigrationMarker | null, now: number): boolean {
	if (marker === null) {
		return true;
	}
	if (marker.state === 'completed') {
		return false;
	}
	return (
		marker.attempts < DOMAIN_MIGRATION_MAX_FAILED_ATTEMPTS && now - marker.at >= DOMAIN_MIGRATION_FAILED_RETRY_DELAY_MS
	);
}

export interface DomainMigrationIntent {
	at: number;
	handoff_id?: string;
}

export function readDomainMigrationIntent(storage: StorageLike | null, now: number): DomainMigrationIntent | null {
	try {
		const raw = storage?.getItem(DOMAIN_MIGRATION_INTENT_KEY) ?? null;
		if (!raw) {
			return null;
		}
		const value = JSON.parse(raw) as unknown;
		if (!isRecord(value) || typeof value.at !== 'number' || now - value.at > DOMAIN_MIGRATION_PENDING_MAX_AGE_MS) {
			return null;
		}
		return typeof value.handoff_id === 'string' ? {at: value.at, handoff_id: value.handoff_id} : {at: value.at};
	} catch {
		return null;
	}
}

export function writeDomainMigrationIntent(storage: StorageLike | null, intent: DomainMigrationIntent): void {
	storage?.setItem(DOMAIN_MIGRATION_INTENT_KEY, JSON.stringify(intent));
}

export function clearDomainMigrationIntent(storage: StorageLike | null): void {
	try {
		storage?.removeItem(DOMAIN_MIGRATION_INTENT_KEY);
	} catch {}
}

export function intentConfirmsCompletion(intent: DomainMigrationIntent | null, handoffId: string | null): boolean {
	if (intent === null) {
		return false;
	}
	return intent.handoff_id === undefined || intent.handoff_id === handoffId;
}

export type DomainMigrationInstallKind =
	| 'none'
	| 'chromium-desktop'
	| 'chromium-android'
	| 'webkit'
	| 'firefox'
	| 'other';

export type DomainMigrationDisplayMode = 'browser' | 'minimal-ui' | 'standalone' | 'window-controls-overlay';

export interface DomainMigrationInstallSignals {
	displayMode: DomainMigrationDisplayMode;
	navigatorStandalone: boolean;
	userAgent: string;
	userAgentData: {brands?: ReadonlyArray<{brand: string}>; mobile?: boolean; platform?: string} | null;
	maxTouchPoints: number;
	electron: boolean;
}

const INSTALLED_DISPLAY_MODES: ReadonlySet<DomainMigrationDisplayMode> = new Set([
	'standalone',
	'window-controls-overlay',
]);
const CHROMIUM_USER_AGENT_PATTERN = /\b(?:Chrome|Chromium|CriOS|EdgA|Edg|OPR|SamsungBrowser)\//u;
const ANDROID_USER_AGENT_PATTERN = /\bAndroid\b/u;

export function classifyDomainMigrationInstallKind(signals: DomainMigrationInstallSignals): DomainMigrationInstallKind {
	if (signals.electron) {
		return 'none';
	}
	const installed = signals.navigatorStandalone || INSTALLED_DISPLAY_MODES.has(signals.displayMode);
	if (!installed) {
		return 'none';
	}
	const {userAgent} = signals;
	if (isIOSMobileOrTabletUserAgent(userAgent, signals.maxTouchPoints)) {
		return 'webkit';
	}
	const android =
		signals.userAgentData?.platform === 'Android' ||
		signals.userAgentData?.mobile === true ||
		ANDROID_USER_AGENT_PATTERN.test(userAgent);
	if (signals.userAgentData?.brands?.some((entry) => entry.brand === 'Chromium')) {
		return android ? 'chromium-android' : 'chromium-desktop';
	}
	if (/\bFirefox\//u.test(userAgent)) {
		return 'firefox';
	}
	if (CHROMIUM_USER_AGENT_PATTERN.test(userAgent)) {
		return android ? 'chromium-android' : 'chromium-desktop';
	}
	if (/\bMacintosh\b/u.test(userAgent) && /\bSafari\//u.test(userAgent)) {
		return 'webkit';
	}
	return 'other';
}

export interface DomainMigrationEnvironment {
	installKind: DomainMigrationInstallKind;
	electron: boolean;
	electronMigrationVersion: number | null;
	electronPasskeyRpIds: ReadonlyArray<string>;
}

export function environmentAllowsDomainMigration(environment: DomainMigrationEnvironment): boolean {
	if (environment.installKind !== 'none' && environment.installKind !== 'chromium-desktop') {
		return false;
	}
	if (environment.electron) {
		return (
			environment.electronMigrationVersion !== null &&
			environment.electronMigrationVersion >= 1 &&
			environment.electronPasskeyRpIds.includes(PASSKEY_MIGRATION_RP_ID)
		);
	}
	return true;
}

export function environmentMayForward(
	environment: DomainMigrationEnvironment,
	discovery: DomainMigrationDiscoveryResponse | null,
): boolean {
	if (!environmentAllowsDomainMigration(environment)) {
		return false;
	}
	return environment.installKind === 'none' || discovery?.standalone_forwarding === true;
}

export interface DomainMigrationGateInput {
	environment: DomainMigrationEnvironment;
	assignmentEnabled: boolean;
	discovery: DomainMigrationDiscoveryResponse | null;
	marker: DomainMigrationMarker | null;
	now: number;
	voiceActive: boolean;
	oneShotRoute: boolean;
}

export function shouldStartDomainMigration(input: DomainMigrationGateInput): boolean {
	return (
		input.assignmentEnabled &&
		input.discovery?.enabled === true &&
		markerAllowsDomainMigration(input.marker, input.now) &&
		environmentAllowsDomainMigration(input.environment) &&
		!input.voiceActive &&
		!input.oneShotRoute
	);
}

export function shouldForwardCompletedSource(
	discovery: DomainMigrationDiscoveryResponse | null,
	marker: DomainMigrationMarker | null,
	environment: DomainMigrationEnvironment,
): boolean {
	return discovery?.enabled === true && marker?.state === 'completed' && environmentMayForward(environment, discovery);
}

export interface DomainMovedNoticeInput {
	side: DomainMigrationSide | null;
	installKind: DomainMigrationInstallKind;
	discovery: DomainMigrationDiscoveryResponse | null;
	assignmentEnabled: boolean;
	marker: DomainMigrationMarker | null;
	dismissedAt: number | null;
	now: number;
}

export function shouldShowDomainMovedNotice(input: DomainMovedNoticeInput): boolean {
	if (input.side?.role !== 'source' || input.installKind === 'none' || input.discovery?.enabled !== true) {
		return false;
	}
	if (input.dismissedAt !== null && input.now - input.dismissedAt < DOMAIN_MIGRATION_MOVED_DISMISS_MS) {
		return false;
	}
	const completed = input.marker?.state === 'completed';
	if (input.installKind === 'chromium-desktop') {
		return completed;
	}
	return completed || input.assignmentEnabled;
}

export function domainMovedInstallUrl(target: string): string {
	return `${target}/app`;
}

export function domainMovedManifestId(target: string): string {
	return `${target}/`;
}

export function domainMovedBrowserMigrationUrl(target: string): string {
	return `${target}/migrate/begin?start=1&next=${encodeURIComponent('/app')}`;
}

export function anonymousRolloutIsOpen(discovery: DomainMigrationDiscoveryResponse | null): boolean {
	return discovery?.enabled === true && discovery.anonymous_rollout_basis_points > 0;
}

export function deviceIsInAnonymousRollout(discovery: DomainMigrationDiscoveryResponse, deviceId: string): boolean {
	return experimentBucket(deviceId, discovery.rollout_salt) < discovery.anonymous_rollout_basis_points;
}

export function isExportableLocalStorageKey(key: string): boolean {
	return (
		!DENIED_LOCAL_STORAGE_KEYS.has(key) &&
		!key.startsWith(DOMAIN_MIGRATION_MARKER_KEY) &&
		!PUSH_SUBSCRIPTION_KEY_PATTERN.test(key)
	);
}

export function collectExportableLocalStorage(storage: StorageLike | null): Record<string, string> {
	const entries: Record<string, string> = {};
	if (!storage) {
		return entries;
	}
	for (let index = 0; index < storage.length; index++) {
		const key = storage.key(index);
		if (key === null || !isExportableLocalStorageKey(key)) {
			continue;
		}
		const value = storage.getItem(key);
		if (value !== null) {
			entries[key] = value;
		}
	}
	return entries;
}

export interface DomainMigrationCustomSound {
	sound_type: string;
	file_name: string;
	mime_type: string;
	data: string;
}

export interface DomainMigrationThemeAsset {
	id: string;
	name: string;
	mime_type: string;
	size: number;
	data?: string;
	desktop_path?: string;
	created_at: number;
	updated_at: number;
}

export interface DomainMigrationThemeLibrary {
	themes: Array<Record<string, unknown>>;
	assets: Array<DomainMigrationThemeAsset>;
	local_files: Array<Record<string, unknown>>;
	enabled_theme_ids: Array<string>;
}

export interface DomainMigrationPayload {
	version: typeof DOMAIN_MIGRATION_PAYLOAD_VERSION;
	source_origin: string;
	exported_at: number;
	local_storage: Record<string, string>;
	accounts: Array<StoredAccount>;
	custom_sounds?: Array<DomainMigrationCustomSound>;
	theme_library?: DomainMigrationThemeLibrary;
	notification_permission: string;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function isStringRecord(value: unknown): value is Record<string, string> {
	return isRecord(value) && Object.values(value).every((entry) => typeof entry === 'string');
}

function isStoredAccount(value: unknown): value is StoredAccount {
	return (
		isRecord(value) &&
		typeof value.userId === 'string' &&
		value.userId.length > 0 &&
		(typeof value.token === 'string' || value.token === null) &&
		typeof value.lastActive === 'number'
	);
}

function isCustomSound(value: unknown): value is DomainMigrationCustomSound {
	return (
		isRecord(value) &&
		typeof value.sound_type === 'string' &&
		typeof value.file_name === 'string' &&
		typeof value.mime_type === 'string' &&
		typeof value.data === 'string'
	);
}

function isIdentifiedRecord(value: unknown): value is Record<string, unknown> {
	return isRecord(value) && typeof value.id === 'string';
}

function isThemeAsset(value: unknown): value is DomainMigrationThemeAsset {
	return (
		isIdentifiedRecord(value) &&
		typeof value.name === 'string' &&
		typeof value.mime_type === 'string' &&
		typeof value.size === 'number' &&
		(value.data === undefined || typeof value.data === 'string') &&
		(value.desktop_path === undefined || typeof value.desktop_path === 'string') &&
		typeof value.created_at === 'number' &&
		typeof value.updated_at === 'number'
	);
}

function isThemeLibrary(value: unknown): value is DomainMigrationThemeLibrary {
	return (
		isRecord(value) &&
		Array.isArray(value.themes) &&
		value.themes.every(isIdentifiedRecord) &&
		Array.isArray(value.assets) &&
		value.assets.every(isThemeAsset) &&
		Array.isArray(value.local_files) &&
		value.local_files.every(isIdentifiedRecord) &&
		Array.isArray(value.enabled_theme_ids) &&
		value.enabled_theme_ids.every((id) => typeof id === 'string')
	);
}

export function withoutOptionalPayloadData(payload: DomainMigrationPayload): DomainMigrationPayload {
	return {
		...payload,
		custom_sounds: undefined,
		theme_library: payload.theme_library && {...payload.theme_library, assets: []},
	};
}

export function parseDomainMigrationPayload(value: unknown, expectedSource: string): DomainMigrationPayload | null {
	if (
		!isRecord(value) ||
		value.version !== DOMAIN_MIGRATION_PAYLOAD_VERSION ||
		value.source_origin !== expectedSource ||
		typeof value.exported_at !== 'number' ||
		!isStringRecord(value.local_storage) ||
		!Array.isArray(value.accounts) ||
		!value.accounts.every(isStoredAccount) ||
		typeof value.notification_permission !== 'string'
	) {
		return null;
	}
	if (
		value.custom_sounds !== undefined &&
		!(Array.isArray(value.custom_sounds) && value.custom_sounds.every(isCustomSound))
	) {
		return null;
	}
	if (value.theme_library !== undefined && !isThemeLibrary(value.theme_library)) {
		return null;
	}
	return value as unknown as DomainMigrationPayload;
}

function withoutRuntimeConfig(snapshot: Record<string, string> | undefined): Record<string, string> {
	const {runtimeConfig: _runtimeConfig, ...rest} = snapshot ?? {};
	return rest;
}

export function rewriteImportedAccount(record: StoredAccount, instance: RuntimeConfigSnapshot): StoredAccount {
	const managed = withoutRuntimeConfig(record.managedStorageData ?? record.localStorageData);
	return {
		...record,
		localStorageData: managed,
		managedStorageData: managed,
		instance,
	};
}
