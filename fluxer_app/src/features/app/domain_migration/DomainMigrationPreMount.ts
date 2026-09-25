// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	hasStoredAccount,
	readActiveSessionToken,
	readDomainMigrationDiscovery,
	readDomainMigrationEnvironment,
} from '@app/features/app/domain_migration/DomainMigrationBrowser';
import {
	anonymousRolloutIsOpen,
	buildTargetUrl,
	clearDomainMigrationIntent,
	collectExportableLocalStorage,
	DOMAIN_MIGRATION_CUSTOM_SOUNDS_MAX_BYTES,
	DOMAIN_MIGRATION_DEFAULT_NEXT_PATH,
	DOMAIN_MIGRATION_DEVICE_KEY,
	DOMAIN_MIGRATION_MARKER_KEY,
	DOMAIN_MIGRATION_NOTIFICATIONS_KEY,
	DOMAIN_MIGRATION_PAYLOAD_VERSION,
	DOMAIN_MIGRATION_PENDING_KEY,
	DOMAIN_MIGRATION_PENDING_MAX_AGE_MS,
	DOMAIN_MIGRATION_THEME_ASSETS_MAX_BYTES,
	type DomainMigrationCustomSound,
	type DomainMigrationPayload,
	type DomainMigrationSide,
	type DomainMigrationThemeLibrary,
	deviceIsInAnonymousRollout,
	environmentAllowsDomainMigration,
	environmentMayForward,
	intentConfirmsCompletion,
	isExportableLocalStorageKey,
	markDomainMigrationCompleted,
	markDomainMigrationFailed,
	parseDomainMigrationMarker,
	parseDomainMigrationPayload,
	readDomainMigrationIntent,
	readDomainMigrationMarker,
	resolveDomainMigrationSide,
	rewriteImportedAccount,
	type StorageLike,
	sanitizeNextPath,
	shouldForwardCompletedSource,
	withoutOptionalPayloadData,
	writeDomainMigrationIntent,
} from '@app/features/app/domain_migration/DomainMigrationCore';
import {
	base64UrlToBytes,
	bytesToBase64Url,
	decryptDomainMigrationPayload,
	encryptDomainMigrationPayload,
	randomBase64Url,
	sha256Hex,
} from '@app/features/app/domain_migration/DomainMigrationCrypto';
import type {SoundType} from '@app/features/notification/utils/SoundUtils';
import {getProtectedLocalStorage, getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {
	ThemeLibraryAsset,
	ThemeLibraryLocalFileReference,
	ThemeLibraryTheme,
} from '@app/features/theme/state/ThemeLibrary';
import {when} from 'mobx';

const logger = new Logger('DomainMigration');

const NONCE_BYTES = 32;
const DEVICE_ID_BYTES = 16;
const BASE64URL_TOKEN_PATTERN = /^[A-Za-z0-9_-]{43}$/u;
const MAX_HANDOFF_PAYLOAD_LENGTH = 8 * 1024 * 1024;

type DomainMigrationFailureReason =
	| 'disabled'
	| 'no_pending'
	| 'missing_handoff'
	| 'handoff_expired'
	| 'nonce_mismatch'
	| 'redeem_failed'
	| 'invalid_payload'
	| 'import_failed'
	| 'target_error';

class DomainMigrationImportError extends Error {
	constructor(readonly reason: DomainMigrationFailureReason) {
		super(`Domain migration import failed: ${reason}`);
		this.name = 'DomainMigrationImportError';
	}
}

interface PendingHandoff {
	nonce: string;
	next: string;
	at: number;
}

function navigate(url: string): true {
	window.location.replace(url);
	return true;
}

function readCurrentPath(): {pathname: string; search: string; hash: string} {
	return {pathname: window.location.pathname, search: window.location.search, hash: window.location.hash};
}

function readNotificationPermission(): string {
	return typeof Notification === 'undefined' ? 'unsupported' : Notification.permission;
}

function readOrCreateDeviceId(): string {
	const storage = getProtectedLocalStorage();
	const existing = storage?.getItem(DOMAIN_MIGRATION_DEVICE_KEY);
	if (existing) {
		return existing;
	}
	const deviceId = randomBase64Url(DEVICE_ID_BYTES);
	try {
		storage?.setItem(DOMAIN_MIGRATION_DEVICE_KEY, deviceId);
	} catch {}
	return deviceId;
}

async function collectCustomSounds(): Promise<Array<DomainMigrationCustomSound> | undefined> {
	try {
		const {getAllCustomSounds} = await import('@app/features/notification/utils/CustomSoundDB');
		const sounds = await getAllCustomSounds();
		const totalBytes = sounds.reduce((sum, sound) => sum + sound.blob.size, 0);
		if (totalBytes > DOMAIN_MIGRATION_CUSTOM_SOUNDS_MAX_BYTES) {
			return undefined;
		}
		return await Promise.all(
			sounds.map(async (sound) => ({
				sound_type: sound.soundType,
				file_name: sound.fileName,
				mime_type: sound.blob.type,
				data: bytesToBase64Url(new Uint8Array(await sound.blob.arrayBuffer())),
			})),
		);
	} catch (err) {
		logger.warn('Skipping custom sounds in the domain migration export:', err);
		return undefined;
	}
}

async function restoreCustomSounds(sounds: ReadonlyArray<DomainMigrationCustomSound> | undefined): Promise<void> {
	if (!sounds || sounds.length === 0) {
		return;
	}
	const {saveCustomSound} = await import('@app/features/notification/utils/CustomSoundDB');
	for (const sound of sounds) {
		try {
			const blob = new Blob([base64UrlToBytes(sound.data)], {type: sound.mime_type});
			await saveCustomSound(sound.sound_type as SoundType, blob, sound.file_name);
		} catch (err) {
			logger.warn(`Failed to restore custom sound ${sound.sound_type}:`, err);
		}
	}
}

async function collectThemeLibrary(): Promise<DomainMigrationThemeLibrary | undefined> {
	try {
		const db = await import('@app/features/theme/utils/ThemeLibraryDb');
		const [themes, assets, localFiles, enabledThemeIds] = await Promise.all([
			db.listThemeLibraryThemes(),
			db.listThemeLibraryAssets(),
			db.listThemeLibraryLocalFiles(),
			db.getEnabledThemeIds(),
		]);
		const assetBytes = assets.reduce((sum, asset) => sum + (asset.data?.size ?? 0), 0);
		const portableAssets = assetBytes > DOMAIN_MIGRATION_THEME_ASSETS_MAX_BYTES ? [] : assets;
		return {
			themes: themes.map((theme) => ({...theme})),
			assets: await Promise.all(
				portableAssets.map(async (asset) => ({
					id: asset.id,
					name: asset.name,
					mime_type: asset.mimeType,
					size: asset.size,
					data: asset.data ? bytesToBase64Url(new Uint8Array(await asset.data.arrayBuffer())) : undefined,
					desktop_path: asset.desktopPath,
					created_at: asset.createdAt,
					updated_at: asset.updatedAt,
				})),
			),
			local_files: localFiles.map((file) => ({...file})),
			enabled_theme_ids: enabledThemeIds,
		};
	} catch (err) {
		logger.warn('Skipping the theme library in the domain migration export:', err);
		return undefined;
	}
}

async function restoreThemeLibrary(library: DomainMigrationThemeLibrary | undefined): Promise<void> {
	if (!library) {
		return;
	}
	try {
		const db = await import('@app/features/theme/utils/ThemeLibraryDb');
		for (const theme of library.themes) {
			await db.saveThemeLibraryTheme(theme as unknown as ThemeLibraryTheme);
		}
		for (const asset of library.assets) {
			const restored: ThemeLibraryAsset = {
				id: asset.id,
				name: asset.name,
				mimeType: asset.mime_type,
				size: asset.size,
				data: asset.data === undefined ? undefined : new Blob([base64UrlToBytes(asset.data)], {type: asset.mime_type}),
				desktopPath: asset.desktop_path,
				createdAt: asset.created_at,
				updatedAt: asset.updated_at,
			};
			await db.saveThemeLibraryAsset(restored);
		}
		for (const file of library.local_files) {
			await db.saveThemeLibraryLocalFile(file as unknown as ThemeLibraryLocalFileReference);
		}
		if (library.enabled_theme_ids.length > 0) {
			await db.setEnabledThemeIds(library.enabled_theme_ids);
		}
	} catch (err) {
		logger.warn('Failed to restore the theme library:', err);
	}
}

async function createHandoff(apiEndpoint: string, token: string, nonceHash: string, payload: string): Promise<string> {
	const response = await fetch(`${apiEndpoint}/v1/auth/origin-handoff`, {
		method: 'POST',
		credentials: 'omit',
		headers: {'Content-Type': 'application/json', Authorization: token},
		body: JSON.stringify({nonce_hash: nonceHash, payload}),
	});
	if (!response.ok) {
		throw new Error(`Origin handoff was rejected with status ${response.status}`);
	}
	const body = (await response.json()) as {handoff_id?: unknown};
	if (typeof body.handoff_id !== 'string' || !BASE64URL_TOKEN_PATTERN.test(body.handoff_id)) {
		throw new Error('Origin handoff response is malformed');
	}
	return body.handoff_id;
}

async function redeemHandoff(target: string, handoffId: string, nonce: string): Promise<string> {
	let response: Response;
	try {
		response = await fetch(`${target}/api/v1/auth/origin-handoff/redeem`, {
			method: 'POST',
			credentials: 'omit',
			headers: {'Content-Type': 'application/json'},
			body: JSON.stringify({handoff_id: handoffId, nonce}),
		});
	} catch {
		throw new DomainMigrationImportError('redeem_failed');
	}
	if (response.status === 404) {
		throw new DomainMigrationImportError('handoff_expired');
	}
	if (response.status === 400) {
		throw new DomainMigrationImportError('nonce_mismatch');
	}
	if (!response.ok) {
		throw new DomainMigrationImportError('redeem_failed');
	}
	const body = (await response.json()) as {payload?: unknown};
	if (typeof body.payload !== 'string') {
		throw new DomainMigrationImportError('redeem_failed');
	}
	return body.payload;
}

function sourceUrl(side: DomainMigrationSide, next: unknown): string {
	return `${side.source}${sanitizeNextPath(next)}`;
}

function discoveryAllowsMigration(): boolean {
	return (
		readDomainMigrationDiscovery()?.enabled === true &&
		environmentAllowsDomainMigration(readDomainMigrationEnvironment())
	);
}

function revokeCompletedMarker(storage: StorageLike | null): void {
	if (readDomainMigrationMarker(storage)?.state === 'completed') {
		markDomainMigrationFailed(storage, Date.now());
	}
}

async function exportFromSource(side: DomainMigrationSide, nonce: string | null): Promise<boolean> {
	const storage = getProtectedLocalStorage();
	const sessionStorage = getProtectedSessionStorage();
	const intent = readDomainMigrationIntent(sessionStorage, Date.now());
	if (intent === null || intent.handoff_id !== undefined) {
		clearDomainMigrationIntent(sessionStorage);
		return navigate(sourceUrl(side, DOMAIN_MIGRATION_DEFAULT_NEXT_PATH));
	}
	try {
		if (nonce === null || !BASE64URL_TOKEN_PATTERN.test(nonce)) {
			throw new Error('Missing or malformed nonce');
		}
		const [{default: accountStorage}, {default: RuntimeConfig}] = await Promise.all([
			import('@app/features/auth/state/AccountStorage'),
			import('@app/features/app/state/RuntimeConfig'),
		]);
		const accounts = (await accountStorage.getAllAccounts()).filter((account) => Boolean(account.token));
		const token = readActiveSessionToken() ?? accounts.find((account) => account.isValid !== false)?.token ?? null;
		if (!token) {
			throw new Error('No stored account to export');
		}
		const payload: DomainMigrationPayload = {
			version: DOMAIN_MIGRATION_PAYLOAD_VERSION,
			source_origin: side.source,
			exported_at: Date.now(),
			local_storage: collectExportableLocalStorage(storage),
			accounts,
			custom_sounds: await collectCustomSounds(),
			theme_library: await collectThemeLibrary(),
			notification_permission: readNotificationPermission(),
		};
		let sealed = await encryptDomainMigrationPayload(payload);
		if (sealed.payload.length > MAX_HANDOFF_PAYLOAD_LENGTH) {
			sealed = await encryptDomainMigrationPayload(withoutOptionalPayloadData(payload));
		}
		const handoffId = await createHandoff(RuntimeConfig.apiEndpoint, token, await sha256Hex(nonce), sealed.payload);
		writeDomainMigrationIntent(sessionStorage, {at: intent.at, handoff_id: handoffId});
		return navigate(`${side.target}/migrate/complete#h=${handoffId}&k=${sealed.key}`);
	} catch (err) {
		logger.warn('Domain migration export failed:', err);
		clearDomainMigrationIntent(sessionStorage);
		revokeCompletedMarker(storage);
		return navigate(sourceUrl(side, DOMAIN_MIGRATION_DEFAULT_NEXT_PATH));
	}
}

async function forwardFromSource(side: DomainMigrationSide): Promise<boolean> {
	const environment = readDomainMigrationEnvironment();
	const discovery = readDomainMigrationDiscovery();
	if (!environmentMayForward(environment, discovery)) {
		return false;
	}
	const {pathname, search, hash} = readCurrentPath();
	if (shouldForwardCompletedSource(discovery, readDomainMigrationMarker(getProtectedLocalStorage()), environment)) {
		if (!(await hasStoredAccount())) {
			return navigate(buildTargetUrl(side.target, pathname, search, hash));
		}
		writeDomainMigrationIntent(getProtectedSessionStorage(), {at: Date.now()});
		const next = sanitizeNextPath(`${pathname}${search}${hash}`);
		return navigate(`${side.target}/migrate/begin?resume=1&next=${encodeURIComponent(next)}`);
	}
	if (
		discovery !== null &&
		anonymousRolloutIsOpen(discovery) &&
		!(await hasStoredAccount()) &&
		deviceIsInAnonymousRollout(discovery, readOrCreateDeviceId())
	) {
		return navigate(buildTargetUrl(side.target, pathname, search, hash));
	}
	return false;
}

async function forwardWhenIdle(side: DomainMigrationSide): Promise<void> {
	const {default: MediaEngine} = await import('@app/features/voice/engine/MediaEngineFacade');
	await when(() => !MediaEngine.connected && !MediaEngine.connecting);
	if (
		!shouldForwardCompletedSource(
			readDomainMigrationDiscovery(),
			readDomainMigrationMarker(getProtectedLocalStorage()),
			readDomainMigrationEnvironment(),
		)
	) {
		return;
	}
	const {pathname, search, hash} = readCurrentPath();
	navigate(buildTargetUrl(side.target, pathname, search, hash));
}

function installSourceMarkerListener(side: DomainMigrationSide): void {
	let waiting = false;
	window.addEventListener('storage', (event) => {
		if (event.key !== DOMAIN_MIGRATION_MARKER_KEY || waiting) {
			return;
		}
		if (
			!shouldForwardCompletedSource(
				readDomainMigrationDiscovery(),
				parseDomainMigrationMarker(event.newValue),
				readDomainMigrationEnvironment(),
			)
		) {
			return;
		}
		waiting = true;
		forwardWhenIdle(side)
			.catch((err) => {
				logger.warn('Failed to forward a migrated source tab:', err);
			})
			.finally(() => {
				waiting = false;
			});
	});
}

function completeOnSource(side: DomainMigrationSide, params: URLSearchParams): boolean {
	const sessionStorage = getProtectedSessionStorage();
	const intent = readDomainMigrationIntent(sessionStorage, Date.now());
	clearDomainMigrationIntent(sessionStorage);
	if (!intentConfirmsCompletion(intent, params.get('h'))) {
		return navigate(sourceUrl(side, params.get('next')));
	}
	markDomainMigrationCompleted(getProtectedLocalStorage(), side.target, Date.now());
	if (!environmentMayForward(readDomainMigrationEnvironment(), readDomainMigrationDiscovery())) {
		return navigate(sourceUrl(side, params.get('next')));
	}
	return navigate(`${side.target}${sanitizeNextPath(params.get('next'))}`);
}

function startOnSource(side: DomainMigrationSide, params: URLSearchParams): boolean {
	writeDomainMigrationIntent(getProtectedSessionStorage(), {at: Date.now()});
	return navigate(`${side.target}/migrate/begin?next=${encodeURIComponent(sanitizeNextPath(params.get('next')))}`);
}

function failOnSource(side: DomainMigrationSide, params: URLSearchParams): boolean {
	const sessionStorage = getProtectedSessionStorage();
	const intent = readDomainMigrationIntent(sessionStorage, Date.now());
	clearDomainMigrationIntent(sessionStorage);
	if (intent !== null) {
		logger.warn(`Domain migration failed on ${side.target}: ${params.get('reason') ?? 'unknown'}`);
		revokeCompletedMarker(getProtectedLocalStorage());
	}
	return navigate(sourceUrl(side, params.get('next')));
}

async function handleSource(side: DomainMigrationSide): Promise<boolean> {
	const params = new URLSearchParams(window.location.search);
	const {pathname} = window.location;
	if (pathname.startsWith('/migrate/') && !discoveryAllowsMigration()) {
		clearDomainMigrationIntent(getProtectedSessionStorage());
		return navigate(sourceUrl(side, params.get('next')));
	}
	switch (pathname) {
		case '/migrate/export':
			return exportFromSource(side, params.get('n'));
		case '/migrate/start':
			return startOnSource(side, params);
		case '/migrate/done':
			return completeOnSource(side, params);
		case '/migrate/failed':
			return failOnSource(side, params);
	}
	if (await forwardFromSource(side)) {
		return true;
	}
	installSourceMarkerListener(side);
	return false;
}

function takePendingHandoff(): PendingHandoff | null {
	const storage = getProtectedSessionStorage();
	try {
		const raw = storage?.getItem(DOMAIN_MIGRATION_PENDING_KEY) ?? null;
		storage?.removeItem(DOMAIN_MIGRATION_PENDING_KEY);
		if (!raw) {
			return null;
		}
		const value = JSON.parse(raw) as Partial<PendingHandoff>;
		if (typeof value.nonce !== 'string' || typeof value.next !== 'string' || typeof value.at !== 'number') {
			return null;
		}
		return {nonce: value.nonce, next: value.next, at: value.at};
	} catch {
		return null;
	}
}

async function importHandoff(side: DomainMigrationSide, fragment: URLSearchParams, nonce: string): Promise<void> {
	const handoffId = fragment.get('h');
	const key = fragment.get('k');
	if (!handoffId || !key || !BASE64URL_TOKEN_PATTERN.test(handoffId)) {
		throw new DomainMigrationImportError('missing_handoff');
	}
	const sealed = await redeemHandoff(side.target, handoffId, nonce);
	let payload: DomainMigrationPayload | null;
	try {
		payload = parseDomainMigrationPayload(await decryptDomainMigrationPayload(sealed, key), side.source);
	} catch {
		payload = null;
	}
	if (payload === null) {
		throw new DomainMigrationImportError('invalid_payload');
	}
	try {
		const [{default: accountStorage}, {default: RuntimeConfig}] = await Promise.all([
			import('@app/features/auth/state/AccountStorage'),
			import('@app/features/app/state/RuntimeConfig'),
		]);
		const instance = RuntimeConfig.getSnapshot();
		await accountStorage.importAccounts(payload.accounts.map((account) => rewriteImportedAccount(account, instance)));
	} catch (err) {
		logger.warn('Failed to import migrated accounts:', err);
		throw new DomainMigrationImportError('import_failed');
	}
	const storage = getProtectedLocalStorage();
	for (const [storageKey, value] of Object.entries(payload.local_storage)) {
		if (!isExportableLocalStorageKey(storageKey)) {
			continue;
		}
		try {
			storage?.setItem(storageKey, value);
		} catch (err) {
			logger.warn(`Failed to import localStorage key ${storageKey}:`, err);
		}
	}
	await restoreCustomSounds(payload.custom_sounds);
	await restoreThemeLibrary(payload.theme_library);
	if (payload.notification_permission === 'granted') {
		try {
			storage?.setItem(DOMAIN_MIGRATION_NOTIFICATIONS_KEY, 'granted');
		} catch {}
	}
	await persistDesktopAppOrigin();
}

async function persistDesktopAppOrigin(): Promise<void> {
	try {
		await window.electron?.domainMigration?.setAppOrigin(window.location.origin);
	} catch (err) {
		logger.warn('Failed to persist the desktop app origin:', err);
	}
}

function doneUrl(side: DomainMigrationSide, next: string, handoffId: string | null): string {
	const handoff = handoffId === null ? '' : `h=${encodeURIComponent(handoffId)}&`;
	return `${side.source}/migrate/done?${handoff}next=${encodeURIComponent(next)}`;
}

function failedUrl(side: DomainMigrationSide, reason: DomainMigrationFailureReason, next: string): string {
	return `${side.source}/migrate/failed?reason=${reason}&next=${encodeURIComponent(next)}`;
}

async function beginOnTarget(side: DomainMigrationSide, params: URLSearchParams): Promise<boolean> {
	const next = sanitizeNextPath(params.get('next'));
	if (readDomainMigrationDiscovery()?.enabled !== true) {
		return navigate(failedUrl(side, 'disabled', next));
	}
	const started = params.get('start') === '1';
	if (await hasStoredAccount()) {
		await persistDesktopAppOrigin();
		return navigate(params.get('resume') === '1' || started ? `${side.target}${next}` : doneUrl(side, next, null));
	}
	if (started) {
		return navigate(`${side.source}/migrate/start?next=${encodeURIComponent(next)}`);
	}
	const nonce = randomBase64Url(NONCE_BYTES);
	const pending: PendingHandoff = {nonce, next, at: Date.now()};
	getProtectedSessionStorage()?.setItem(DOMAIN_MIGRATION_PENDING_KEY, JSON.stringify(pending));
	return navigate(`${side.source}/migrate/export?n=${nonce}`);
}

async function completeOnTarget(side: DomainMigrationSide): Promise<boolean> {
	const fragment = new URLSearchParams(window.location.hash.slice(1));
	window.history.replaceState(window.history.state, '', `${window.location.pathname}${window.location.search}`);
	const pending = takePendingHandoff();
	if (pending === null || Date.now() - pending.at > DOMAIN_MIGRATION_PENDING_MAX_AGE_MS) {
		return navigate(failedUrl(side, 'no_pending', DOMAIN_MIGRATION_DEFAULT_NEXT_PATH));
	}
	const next = sanitizeNextPath(pending.next);
	const handoffId = fragment.get('h');
	if (await hasStoredAccount()) {
		await persistDesktopAppOrigin();
		return navigate(doneUrl(side, next, handoffId));
	}
	try {
		await importHandoff(side, fragment, pending.nonce);
	} catch (err) {
		const reason = err instanceof DomainMigrationImportError ? err.reason : 'import_failed';
		logger.warn('Domain migration import failed:', err);
		return navigate(failedUrl(side, reason, next));
	}
	return navigate(doneUrl(side, next, handoffId));
}

async function handleTarget(side: DomainMigrationSide): Promise<boolean> {
	switch (window.location.pathname) {
		case '/migrate/begin':
			return beginOnTarget(side, new URLSearchParams(window.location.search));
		case '/migrate/complete':
			return completeOnTarget(side);
	}
	return false;
}

export async function runDomainMigrationPreMount(): Promise<boolean> {
	if (typeof window === 'undefined') {
		return false;
	}
	const side = resolveDomainMigrationSide(window.location.origin);
	if (side === null) {
		return false;
	}
	try {
		return side.role === 'source' ? await handleSource(side) : await handleTarget(side);
	} catch (err) {
		logger.error('Domain migration pre-mount step failed:', err);
		if (side.role === 'target' && window.location.pathname.startsWith('/migrate/')) {
			return navigate(failedUrl(side, 'target_error', DOMAIN_MIGRATION_DEFAULT_NEXT_PATH));
		}
		return false;
	}
}
