// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {DESKTOP_APP_NAME, LINUX_DESKTOP_ENTRY_ID} from '@electron/common/DesktopIdentity';
import {createChildLogger} from '@electron/common/Logger';
import type {NotificationOptions} from '@electron/common/Types';
import {getNativeNotificationsMode} from '@electron/main/LaunchOptions';
import {resolveNotificationIcon} from '@electron/main/NotificationIcon';
import {shouldPlayNotificationSound} from '@electron/main/NotificationState';
import {requirePrivilegedRendererDocumentSender} from '@electron/main/PrivilegedRendererDocuments';
import {type BrowserWindow, ipcMain, Notification, nativeImage} from 'electron';

const logger = createChildLogger('Notifications');
const requireModule = createRequire(import.meta.url);

interface NativeNotifyImageData {
	width: number;
	height: number;
	rowstride: number;
	hasAlpha: boolean;
	bitsPerSample: number;
	channels: number;
	data: Buffer | Uint8Array;
}

interface NativeNotifyPayload {
	appName: string;
	replacesId?: number;
	appIcon?: string;
	summary: string;
	body: string;
	expireTimeoutMs?: number;
	hints?: {
		urgency?: 'low' | 'normal' | 'critical';
		category?: string;
		desktopEntry?: string;
		soundFile?: string;
		suppressSound?: boolean;
		transient?: boolean;
		actionIcons?: boolean;
		imageData?: NativeNotifyImageData;
	};
	actions?: ReadonlyArray<{key: string; label: string}>;
}

type NativeFreedesktopNotificationEvent =
	| {kind: 'actionInvoked'; id: number; actionKey: string}
	| {kind: 'closed'; id: number; reason: number};
interface NativeNotificationClient {
	notify(payload: NativeNotifyPayload): Promise<number>;
	closeNotification(id: number): Promise<void>;
	getServerCapabilities(): Promise<Array<string>>;
	close(): Promise<void>;
}
type NativeNotificationsCtor = new (
	onEvent: (event: NativeFreedesktopNotificationEvent) => void,
) => NativeNotificationClient;

interface NativeNotificationsModule {
	FreedesktopNotifications: NativeNotificationsCtor | null;
	getServerInformation: (() => Promise<{name: string; vendor: string; version: string; specVersion: string}>) | null;
	loadError: Error | null;
}

const REQUIRED_LINUX_NOTIFICATION_METHODS = ['notify', 'closeNotification', 'getServerCapabilities', 'close'] as const;

function formatError(error: unknown): string {
	if (error instanceof Error) {
		return `${error.name}: ${error.message}${error.stack ? `\n${error.stack}` : ''}`;
	}
	return String(error);
}

function listOwnProperties(value: unknown): Array<string> {
	if ((typeof value !== 'object' && typeof value !== 'function') || value == null) return [];
	return Object.getOwnPropertyNames(value).sort();
}

function describeLinuxNotificationsSurface(mod: NativeNotificationsModule, client?: unknown): Record<string, unknown> {
	const notificationConstructor = mod.FreedesktopNotifications;
	const prototype = notificationConstructor ? notificationConstructor.prototype : null;
	return {
		moduleKeys: listOwnProperties(mod),
		constructorType: typeof notificationConstructor,
		constructorName: notificationConstructor ? notificationConstructor.name : null,
		prototypeKeys: listOwnProperties(prototype),
		instanceKeys: client === undefined ? undefined : listOwnProperties(client),
		loadError: mod.loadError ? formatError(mod.loadError) : null,
	};
}

function assertLinuxNotificationsModule(
	mod: NativeNotificationsModule,
): asserts mod is NativeNotificationsModule & {FreedesktopNotifications: NativeNotificationsCtor} {
	if (!mod.FreedesktopNotifications) {
		throw new Error(
			`@fluxer/linux-notifications loaded but exports no FreedesktopNotifications. ` +
				`Surface: ${JSON.stringify(describeLinuxNotificationsSurface(mod))}`,
		);
	}
	if (typeof mod.FreedesktopNotifications !== 'function') {
		throw new Error(
			`@fluxer/linux-notifications FreedesktopNotifications export is not constructable. ` +
				`Surface: ${JSON.stringify(describeLinuxNotificationsSurface(mod))}`,
		);
	}
}

function assertLinuxNotificationClient(
	mod: NativeNotificationsModule,
	client: unknown,
): asserts client is NativeNotificationClient {
	const record = client as Partial<Record<(typeof REQUIRED_LINUX_NOTIFICATION_METHODS)[number], unknown>>;
	const missingMethods = REQUIRED_LINUX_NOTIFICATION_METHODS.filter((method) => typeof record[method] !== 'function');
	if (missingMethods.length === 0) return;
	throw new Error(
		`@fluxer/linux-notifications FreedesktopNotifications ABI mismatch: missing instance method(s) ` +
			`${missingMethods.join(', ')}. Surface: ${JSON.stringify(describeLinuxNotificationsSurface(mod, client))}`,
	);
}

function loadLinuxNotificationsModule(): NativeNotificationsModule | null {
	if (process.platform !== 'linux') return null;
	const mode = getNativeNotificationsMode(process.argv);
	if (mode === 'off' || mode === 'electron') {
		throw new Error(
			`Linux native notifications are disabled by launch mode (${mode}); refusing Electron Notification fallback.`,
		);
	}
	let mod: NativeNotificationsModule;
	try {
		mod = requireModule('@fluxer/linux-notifications') as NativeNotificationsModule;
	} catch (error) {
		throw new Error(
			`@fluxer/linux-notifications failed to load on Linux — this is a packaging bug, not a runtime fallback case. ` +
				`Original error: ${error instanceof Error ? error.message : String(error)}`,
		);
	}
	assertLinuxNotificationsModule(mod);
	return mod;
}

let linuxNotificationsModuleCache: NativeNotificationsModule | null | undefined;

function getLinuxNotificationsModule(): NativeNotificationsModule | null {
	if (linuxNotificationsModuleCache !== undefined) return linuxNotificationsModuleCache;
	linuxNotificationsModuleCache = loadLinuxNotificationsModule();
	return linuxNotificationsModuleCache;
}

interface NotificationHandle {
	type: 'electron';
	notification: Notification;
}

interface LinuxNotificationHandle {
	type: 'linux-native';
	nativeId: number;
}

type ActiveNotificationHandle = NotificationHandle | LinuxNotificationHandle;

const activeNotifications = new Map<string, ActiveNotificationHandle>();

let notificationIdCounter = 0;
let linuxNativeClient: NativeNotificationClient | null = null;
let linuxNativeWindowGetter: (() => BrowserWindow | null) | null = null;

const linuxNativeIdToString = new Map<number, string>();
const linuxNativeStringToOptions = new Map<string, NotificationOptions>();

function getNotificationId(options: NotificationOptions): string {
	return options.id && options.id.length > 0 ? options.id : `notification-${++notificationIdCounter}`;
}

function deleteActiveNotification(id: string, handle: ActiveNotificationHandle): void {
	if (activeNotifications.get(id) === handle) {
		activeNotifications.delete(id);
	}
}

function ensureLinuxNativeClient(getMainWindow: () => BrowserWindow | null): NativeNotificationClient {
	if (process.platform !== 'linux') {
		throw new Error(`Linux native notifications requested on ${process.platform}`);
	}
	const linuxNotificationsModule = getLinuxNotificationsModule();
	if (!linuxNotificationsModule?.FreedesktopNotifications) {
		throw new Error('@fluxer/linux-notifications is unavailable on Linux; refusing Electron Notification fallback.');
	}
	if (linuxNativeClient) {
		linuxNativeWindowGetter = getMainWindow;
		return linuxNativeClient;
	}
	linuxNativeWindowGetter = getMainWindow;
	try {
		linuxNativeClient = new linuxNotificationsModule.FreedesktopNotifications((event) => {
			const stringId = linuxNativeIdToString.get(event.id);
			if (event.kind === 'actionInvoked') {
				if (!stringId) return;
				const options = linuxNativeStringToOptions.get(stringId);
				const mainWindow = linuxNativeWindowGetter ? linuxNativeWindowGetter() : null;
				if (mainWindow) {
					if (mainWindow.isMinimized()) mainWindow.restore();
					mainWindow.show();
					mainWindow.focus();
					mainWindow.webContents.send('notification-click', stringId, options?.url);
				}
				cleanupLinuxNativeNotification(stringId, event.id);
			} else if (event.kind === 'closed') {
				if (stringId) cleanupLinuxNativeNotification(stringId, event.id);
			}
		});
		assertLinuxNotificationClient(linuxNotificationsModule, linuxNativeClient);
	} catch (error) {
		linuxNativeClient = null;
		throw new Error(
			`Failed to construct FreedesktopNotifications client; refusing Electron Notification fallback. ` +
				`Original error: ${formatError(error)}`,
		);
	}
	return linuxNativeClient;
}

function cleanupLinuxNativeNotification(stringId: string, nativeId: number): void {
	const handle = activeNotifications.get(stringId);
	if (handle?.type === 'linux-native' && handle.nativeId === nativeId) {
		activeNotifications.delete(stringId);
		linuxNativeStringToOptions.delete(stringId);
	}
	linuxNativeIdToString.delete(nativeId);
}

function imageToNotifyImageData(image: Electron.NativeImage): NativeNotifyImageData | null {
	if (image.isEmpty()) return null;
	const size = image.getSize();
	if (size.width <= 0 || size.height <= 0) return null;
	const bitmap = image.toBitmap();
	if (bitmap.length === 0) return null;
	const rgba = Buffer.allocUnsafe(bitmap.length);
	for (let i = 0; i < bitmap.length; i += 4) {
		rgba[i] = bitmap[i + 2] ?? 0;
		rgba[i + 1] = bitmap[i + 1] ?? 0;
		rgba[i + 2] = bitmap[i] ?? 0;
		rgba[i + 3] = bitmap[i + 3] ?? 0;
	}
	return {
		width: size.width,
		height: size.height,
		rowstride: size.width * 4,
		hasAlpha: true,
		bitsPerSample: 8,
		channels: 4,
		data: rgba,
	};
}

async function resolveImageDataForLinux(iconSource: string | undefined): Promise<NativeNotifyImageData | null> {
	if (!iconSource) return null;
	try {
		const resolved = await resolveNotificationIcon(iconSource);
		if (!resolved) return null;
		const image: Electron.NativeImage = typeof resolved === 'string' ? nativeImage.createFromPath(resolved) : resolved;
		return imageToNotifyImageData(image);
	} catch (error) {
		logger.warn('Failed to resolve notification icon for Linux native client', {error});
		return null;
	}
}

async function showLinuxNativeNotification(
	id: string,
	options: NotificationOptions,
	getMainWindow: () => BrowserWindow | null,
): Promise<void> {
	const client = ensureLinuxNativeClient(getMainWindow);
	const previousHandle = activeNotifications.get(id);
	const replacesId = previousHandle?.type === 'linux-native' ? previousHandle.nativeId : undefined;
	const imageData = await resolveImageDataForLinux(options.icon);
	const payload: NativeNotifyPayload = {
		appName: DESKTOP_APP_NAME,
		...(replacesId !== undefined ? {replacesId} : {}),
		summary: options.title,
		body: options.body,
		expireTimeoutMs: -1,
		hints: {
			urgency: 'normal',
			category: 'im',
			desktopEntry: LINUX_DESKTOP_ENTRY_ID,
			suppressSound: true,
			transient: false,
			actionIcons: false,
			...(imageData ? {imageData} : {}),
		},
		actions: [{key: 'default', label: 'Open'}],
	};
	try {
		const nativeId = await client.notify(payload);
		if (replacesId !== undefined && replacesId !== nativeId) {
			linuxNativeIdToString.delete(replacesId);
		}
		linuxNativeIdToString.set(nativeId, id);
		linuxNativeStringToOptions.set(id, options);
		activeNotifications.set(id, {type: 'linux-native', nativeId});
	} catch (error) {
		throw new Error(
			`FreedesktopNotifications.notify failed; refusing Electron Notification fallback. ` +
				`Payload summary: ${JSON.stringify({
					appName: payload.appName,
					summary: payload.summary,
					hasBody: payload.body.length > 0,
					hasImageData: Boolean(payload.hints?.imageData),
					desktopEntry: payload.hints?.desktopEntry,
				})}. Original error: ${formatError(error)}`,
		);
	}
}

export function registerNotificationIpcHandlers(getMainWindow: () => BrowserWindow | null): void {
	ipcMain.handle('notification-sound-allowed', async (): Promise<boolean> => shouldPlayNotificationSound());
	ipcMain.handle(
		'show-notification',
		async (
			event,
			options: NotificationOptions,
		): Promise<{
			id: string;
		}> => {
			requirePrivilegedRendererDocumentSender(event, 'show-notification');
			const id = getNotificationId(options);
			if (process.platform === 'linux') {
				await showLinuxNativeNotification(id, options, getMainWindow);
				return {id};
			}
			const previousHandle = activeNotifications.get(id);
			if (previousHandle) {
				closeNotificationHandle(id, previousHandle);
			}
			if (!Notification.isSupported()) {
				return {id};
			}
			const notificationOpts: Electron.NotificationConstructorOptions = {
				title: options.title,
				body: options.body,
				silent: true,
			};
			if (process.platform === 'darwin' && options.subtitle) {
				notificationOpts.subtitle = options.subtitle;
			}
			if (options.icon) {
				try {
					const icon = await resolveNotificationIcon(options.icon);
					if (icon) notificationOpts.icon = icon;
				} catch (error) {
					logger.warn('Failed to load notification icon:', error);
				}
			}
			const notification = new Notification(notificationOpts);
			const handle: NotificationHandle = {type: 'electron', notification};
			activeNotifications.set(id, handle);
			notification.on('click', () => {
				const mainWindow = getMainWindow();
				if (mainWindow) {
					if (mainWindow.isMinimized()) {
						mainWindow.restore();
					}
					mainWindow.show();
					mainWindow.focus();
					mainWindow.webContents.send('notification-click', id, options.url);
				}
				deleteActiveNotification(id, handle);
			});
			notification.on('close', () => {
				deleteActiveNotification(id, handle);
			});
			notification.show();
			return {id};
		},
	);
	ipcMain.on('close-notification', (_event, id: string) => {
		const handle = activeNotifications.get(id);
		if (!handle) return;
		closeNotificationHandle(id, handle);
	});
	ipcMain.on('close-notifications', (_event, ids: Array<string>) => {
		for (const id of ids) {
			const handle = activeNotifications.get(id);
			if (handle) closeNotificationHandle(id, handle);
		}
	});
}

function closeNotificationHandle(id: string, handle: ActiveNotificationHandle): void {
	if (handle.type === 'electron') {
		handle.notification.close();
		deleteActiveNotification(id, handle);
		return;
	}
	if (linuxNativeClient) {
		void linuxNativeClient.closeNotification(handle.nativeId).catch((error) => {
			logger.warn('FreedesktopNotifications.closeNotification failed', {error});
		});
	}
	cleanupLinuxNativeNotification(id, handle.nativeId);
}
