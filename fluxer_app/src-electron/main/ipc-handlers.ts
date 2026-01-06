/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import child_process from 'node:child_process';
import fs from 'node:fs';
import http from 'node:http';
import https from 'node:https';
import {createRequire} from 'node:module';
import os from 'node:os';
import type {
	PublicKeyCredential,
	PublicKeyCredentialCreationOptions,
	PublicKeyCredentialDescriptor,
	PublicKeyCredentialRequestOptions,
} from '@electron-webauthn/native';
import {create as nativeCreate, get as nativeGet, isSupported as nativeIsSupported} from '@electron-webauthn/native';
import type {
	AuthenticationExtensionsClientOutputs,
	AuthenticationResponseJSON,
	AuthenticatorAssertionResponseJSON,
	AuthenticatorAttestationResponseJSON,
	PublicKeyCredentialCreationOptionsJSON,
	PublicKeyCredentialDescriptorJSON,
	PublicKeyCredentialRequestOptionsJSON,
	RegistrationResponseJSON,
} from '@simplewebauthn/browser';
import {
	app,
	BrowserWindow,
	clipboard,
	dialog,
	globalShortcut,
	ipcMain,
	Notification,
	nativeImage,
	shell,
	systemPreferences,
} from 'electron';
import type {
	AssertionCredential,
	AuthenticatorType,
	CreateCredentialOptions,
	CredentialDescriptor,
	GetCredentialOptions,
	RegistrationCredential,
	WebAuthnMacAddon,
} from 'electron-webauthn-mac';
import {BUILD_CHANNEL} from '../common/build-channel.js';
import type {
	DesktopInfo,
	DownloadFileResult,
	GlobalShortcutOptions,
	MediaAccessType,
	NotificationOptions,
} from '../common/types.js';
import {getMediaProxyToken} from './media-proxy-server.js';
import {getMainWindow} from './window.js';
import {setWindowsBadgeOverlay} from './windows-badge.js';

const registeredShortcuts = new Map<string, string>();

interface ActiveNotification {
	notification: Notification;
	url?: string;
}
const activeNotifications = new Map<string, ActiveNotification>();
const requireModule = createRequire(import.meta.url);
let notificationIdCounter = 0;

const base64UrlToBuffer = (value: string): Buffer => {
	const normalized = value.replace(/-/g, '+').replace(/_/g, '/');
	const padLength = (4 - (normalized.length % 4)) % 4;
	return Buffer.from(`${normalized}${'='.repeat(padLength)}`, 'base64');
};

const bufferToBase64Url = (value: Buffer): string =>
	value.toString('base64').replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');

const convertDescriptorList = (
	list?: Array<PublicKeyCredentialDescriptorJSON>,
): Array<PublicKeyCredentialDescriptor> | undefined =>
	list?.map((descriptor) => ({
		id: base64UrlToBuffer(descriptor.id),
		type: descriptor.type,
		transports: descriptor.transports,
	}));

const convertRequestOptions = (options: PublicKeyCredentialRequestOptionsJSON): PublicKeyCredentialRequestOptions => ({
	...options,
	challenge: base64UrlToBuffer(options.challenge),
	allowCredentials: convertDescriptorList(options.allowCredentials),
});

const convertCreationOptions = (
	options: PublicKeyCredentialCreationOptionsJSON,
): PublicKeyCredentialCreationOptions => ({
	attestation: options.attestation,
	authenticatorSelection: options.authenticatorSelection,
	challenge: base64UrlToBuffer(options.challenge),
	excludeCredentials: convertDescriptorList(options.excludeCredentials),
	extensions: options.extensions,
	pubKeyCredParams: options.pubKeyCredParams,
	rp: options.rp,
	timeout: options.timeout,
	user: {...options.user, id: base64UrlToBuffer(options.user.id)},
});

const emptyExtensions: AuthenticationExtensionsClientOutputs = {};

const parseCredentialResponse = <T>(credential: PublicKeyCredential): T => {
	const payload = credential.response.toString('utf-8');
	if (!payload) {
		throw new Error('Passkey response payload is empty');
	}
	try {
		return JSON.parse(payload) as T;
	} catch (error) {
		throw new Error(
			`Failed to parse passkey response payload: ${error instanceof Error ? error.message : 'unknown error'}`,
		);
	}
};

const normalizeAuthenticatorAttachment = (
	attachment: string | null | undefined,
): AuthenticatorAttachment | undefined => (attachment == null ? undefined : (attachment as AuthenticatorAttachment));

const buildAuthenticationResponse = (credential: PublicKeyCredential): AuthenticationResponseJSON => ({
	id: bufferToBase64Url(credential.rawId),
	rawId: bufferToBase64Url(credential.rawId),
	response: parseCredentialResponse<AuthenticatorAssertionResponseJSON>(credential),
	clientExtensionResults: emptyExtensions,
	type: 'public-key',
	authenticatorAttachment: normalizeAuthenticatorAttachment(credential.authenticatorAttachment),
});

const buildRegistrationResponse = (credential: PublicKeyCredential): RegistrationResponseJSON => ({
	id: bufferToBase64Url(credential.rawId),
	rawId: bufferToBase64Url(credential.rawId),
	response: parseCredentialResponse<AuthenticatorAttestationResponseJSON>(credential),
	clientExtensionResults: emptyExtensions,
	type: 'public-key',
	authenticatorAttachment: normalizeAuthenticatorAttachment(credential.authenticatorAttachment),
});

const base64ToBase64Url = (value: string): string => value.replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');

const base64UrlToBase64 = (value: string): string => base64UrlToBuffer(value).toString('base64');

const convertDescriptorForMac = (descriptor: PublicKeyCredentialDescriptorJSON): CredentialDescriptor => ({
	id: base64UrlToBase64(descriptor.id),
	transports: descriptor.transports,
});

const deriveAuthenticatorTypesFromSelection = (
	selection?: PublicKeyCredentialCreationOptionsJSON['authenticatorSelection'],
): Array<AuthenticatorType> | undefined => {
	const attachment = selection?.authenticatorAttachment;
	if (!attachment) {
		return undefined;
	}

	if (attachment === 'platform') {
		return ['platform'];
	}

	if (attachment === 'cross-platform') {
		return ['securityKey'];
	}

	return undefined;
};

const ensureRpId = (value: string | undefined, context: string): string => {
	if (!value) {
		throw new Error(`Passkey ${context} operation requires rpId`);
	}
	return value;
};

const runSysctl = (query: string): string | null => {
	try {
		return child_process
			.execSync(query, {
				encoding: 'utf8',
				stdio: ['ignore', 'pipe', 'ignore'],
			})
			.trim();
	} catch {
		return null;
	}
};

const detectRosettaMode = (): boolean => {
	if (process.platform !== 'darwin') {
		return false;
	}

	const translated = runSysctl('sysctl -n sysctl.proc_translated');
	return translated === '1';
};

const detectHardwareArch = (): string => {
	if (process.platform !== 'darwin') {
		return os.arch();
	}
	const optionalArm64 = runSysctl('sysctl -n hw.optional.arm64');
	if (optionalArm64 === '1') {
		return 'arm64';
	}
	return os.arch();
};

const convertMacCreationOptions = (options: PublicKeyCredentialCreationOptionsJSON): CreateCredentialOptions => ({
	rpId: ensureRpId(options.rp.id, 'registration'),
	userId: base64UrlToBuffer(options.user.id).toString('base64'),
	name: options.user.name,
	displayName: options.user.displayName,
	authenticators: deriveAuthenticatorTypesFromSelection(options.authenticatorSelection),
	excludeCredentials: options.excludeCredentials?.map(convertDescriptorForMac),
	userVerification: options.authenticatorSelection?.userVerification,
	attestation: options.attestation,
});

const convertMacRequestOptions = (options: PublicKeyCredentialRequestOptionsJSON): GetCredentialOptions => ({
	rpId: ensureRpId(options.rpId, 'assertion'),
	allowCredentials: options.allowCredentials?.map(convertDescriptorForMac),
	userVerification: options.userVerification,
});

const MAC_APP_IDENTIFIER_SEARCH = 'application identifier';
const hasMacApplicationIdentifier = (): boolean => process.platform === 'darwin' && app.isPackaged;

const isMissingApplicationIdentifierError = (error: unknown): boolean => {
	const message =
		error instanceof Error
			? error.message
			: typeof error === 'object' && error !== null && 'message' in error
				? String((error as {message?: unknown}).message ?? '')
				: '';
	return message.toLowerCase().includes(MAC_APP_IDENTIFIER_SEARCH);
};

const buildRegistrationResponseFromMac = (credential: RegistrationCredential): RegistrationResponseJSON => {
	const id = base64ToBase64Url(credential.credentialID);
	return {
		id,
		rawId: id,
		response: {
			clientDataJSON: base64ToBase64Url(credential.clientDataJSON),
			attestationObject: base64ToBase64Url(credential.attestationObject),
			transports:
				'transports' in credential
					? (credential.transports as RegistrationResponseJSON['response']['transports'])
					: undefined,
		},
		clientExtensionResults: emptyExtensions,
		type: 'public-key',
		authenticatorAttachment:
			'attachment' in credential ? (credential.attachment as AuthenticatorAttachment) : undefined,
	};
};

const buildAuthenticationResponseFromMac = (credential: AssertionCredential): AuthenticationResponseJSON => {
	const id = base64ToBase64Url(credential.credentialID);
	return {
		id,
		rawId: id,
		response: {
			clientDataJSON: base64ToBase64Url(credential.clientDataJSON),
			authenticatorData: base64ToBase64Url(credential.authenticatorData),
			signature: base64ToBase64Url(credential.signature),
			userHandle: credential.userID ? base64ToBase64Url(credential.userID) : undefined,
		},
		clientExtensionResults: emptyExtensions,
		type: 'public-key',
		authenticatorAttachment:
			'attachment' in credential ? (credential.attachment as AuthenticatorAttachment) : undefined,
	};
};

interface PasskeyProvider {
	isSupported: () => Promise<boolean>;
	authenticate: (options: PublicKeyCredentialRequestOptionsJSON) => Promise<AuthenticationResponseJSON>;
	register: (options: PublicKeyCredentialCreationOptionsJSON) => Promise<RegistrationResponseJSON>;
}

const passkeyProvider = createPasskeyProvider();

function createPasskeyProvider(): PasskeyProvider {
	const macAddon = loadMacWebAuthnAddon();
	if (macAddon) {
		return createMacPasskeyProvider(macAddon);
	}
	return createNativePasskeyProvider();
}

function createNativePasskeyProvider(): PasskeyProvider {
	return {
		isSupported: nativeIsSupported,
		authenticate: async (options) => {
			const requestOptions = convertRequestOptions(options);
			const credential = await nativeGet(requestOptions);
			return buildAuthenticationResponse(credential);
		},
		register: async (options) => {
			const creationOptions = convertCreationOptions(options);
			const credential = await nativeCreate(creationOptions);
			return buildRegistrationResponse(credential);
		},
	};
}

function createMacPasskeyProvider(addon: WebAuthnMacAddon): PasskeyProvider {
	const fallbackProvider = createNativePasskeyProvider();
	let useAddon = true;

	const disableAddon = (): void => {
		useAddon = false;
	};

	const callWithFallback = async <T>(
		addonOperation: () => Promise<T>,
		nativeOperation: () => Promise<T>,
	): Promise<T> => {
		if (!useAddon) {
			return nativeOperation();
		}

		try {
			return await addonOperation();
		} catch (error) {
			if (isMissingApplicationIdentifierError(error)) {
				console.warn('electron-webauthn-mac disabled: missing application identifier', error);
				disableAddon();
				return nativeOperation();
			}
			throw error;
		}
	};

	return {
		isSupported: async () => {
			if (!useAddon) {
				return fallbackProvider.isSupported();
			}
			return true;
		},
		authenticate: async (options) =>
			callWithFallback(
				async () => {
					const requestOptions = convertMacRequestOptions(options);
					const credential = await addon.getCredential(requestOptions);
					return buildAuthenticationResponseFromMac(credential);
				},
				() => fallbackProvider.authenticate(options),
			),
		register: async (options) =>
			callWithFallback(
				async () => {
					const creationOptions = convertMacCreationOptions(options);
					const credential = await addon.createCredential(creationOptions);
					return buildRegistrationResponseFromMac(credential);
				},
				() => fallbackProvider.register(options),
			),
	};
}

function loadMacWebAuthnAddon(): WebAuthnMacAddon | null {
	if (process.platform !== 'darwin' || !hasMacApplicationIdentifier()) {
		if (process.platform === 'darwin') {
			console.info(
				'electron-webauthn-mac disabled: macOS build lacks an application identifier (likely unsigned dev bundle).',
			);
		}
		return null;
	}

	try {
		return requireModule('electron-webauthn-mac') as WebAuthnMacAddon;
	} catch (error) {
		console.warn('Failed to initialize electron-webauthn-mac:', error);
		return null;
	}
}

export function registerIpcHandlers(): void {
	ipcMain.handle(
		'get-desktop-info',
		(): DesktopInfo => ({
			version: app.getVersion(),
			channel: BUILD_CHANNEL,
			arch: process.arch,
			hardwareArch: detectHardwareArch(),
			runningUnderRosetta: detectRosettaMode(),
			os: process.platform,
			osVersion: os.release(),
		}),
	);

	ipcMain.handle('get-media-proxy-token', (): string => {
		return getMediaProxyToken();
	});
	ipcMain.on('get-media-proxy-token', (event) => {
		event.returnValue = getMediaProxyToken();
	});

	ipcMain.on('window-minimize', (event) => {
		BrowserWindow.fromWebContents(event.sender)?.minimize();
	});

	ipcMain.on('window-maximize', (event) => {
		const win = BrowserWindow.fromWebContents(event.sender);
		if (win) {
			if (win.isMaximized()) {
				win.unmaximize();
			} else {
				win.maximize();
			}
		}
	});

	ipcMain.on('window-close', (event) => {
		BrowserWindow.fromWebContents(event.sender)?.close();
	});

	ipcMain.handle('window-is-maximized', (event): boolean => {
		return BrowserWindow.fromWebContents(event.sender)?.isMaximized() ?? false;
	});

	ipcMain.handle('open-external', async (_event, url: string): Promise<void> => {
		const allowedProtocols = ['http:', 'https:', 'mailto:'];
		if (process.platform === 'darwin') {
			allowedProtocols.push('x-apple.systempreferences:');
		}

		try {
			const parsed = new URL(url);
			if (allowedProtocols.includes(parsed.protocol)) {
				await shell.openExternal(url);
			} else {
				throw new Error('Invalid URL protocol');
			}
		} catch (error) {
			if (error instanceof TypeError) {
				throw new Error('Invalid URL');
			}
			throw error;
		}
	});

	ipcMain.handle('clipboard-write-text', (_event, text: string): void => {
		clipboard.writeText(text);
	});

	ipcMain.handle('clipboard-read-text', (): string => {
		return clipboard.readText();
	});

	ipcMain.handle('app-set-badge', (_event, payload: {count: number; text?: string}) => {
		const count = Math.max(0, Math.floor(payload?.count ?? 0));
		const label = payload?.text ?? String(count);

		app.setBadgeCount(count);

		if (process.platform === 'darwin' && app.dock) {
			app.dock.setBadge(count > 0 ? label : '');
		}

		if (process.platform === 'win32') {
			setWindowsBadgeOverlay(getMainWindow(), count);
		}
	});

	ipcMain.handle(
		'download-file',
		async (event, options: {url: string; defaultPath: string}): Promise<DownloadFileResult> => {
			const win = BrowserWindow.fromWebContents(event.sender);
			if (!win) {
				return {success: false, error: 'No window found'};
			}

			try {
				const result = await dialog.showSaveDialog(win, {
					defaultPath: options.defaultPath,
				});

				if (result.canceled || !result.filePath) {
					return {success: false};
				}

				await downloadFile(options.url, result.filePath);
				return {success: true, path: result.filePath};
			} catch (error) {
				return {success: false, error: error instanceof Error ? error.message : 'Unknown error'};
			}
		},
	);

	ipcMain.on('toggle-devtools', (event) => {
		const win = BrowserWindow.fromWebContents(event.sender);
		if (win) {
			if (win.webContents.isDevToolsOpened()) {
				win.webContents.closeDevTools();
			} else {
				win.webContents.openDevTools();
			}
		}
	});

	ipcMain.handle('register-global-shortcut', (_event, options: GlobalShortcutOptions): boolean => {
		const {accelerator, id} = options;

		if (registeredShortcuts.has(accelerator)) {
			globalShortcut.unregister(accelerator);
		}

		const success = globalShortcut.register(accelerator, () => {
			const mainWindow = getMainWindow();
			if (mainWindow) {
				mainWindow.webContents.send('global-shortcut-triggered', id);
			}
		});

		if (success) {
			registeredShortcuts.set(accelerator, id);
		}

		return success;
	});

	ipcMain.handle('unregister-global-shortcut', (_event, accelerator: string): void => {
		if (registeredShortcuts.has(accelerator)) {
			globalShortcut.unregister(accelerator);
			registeredShortcuts.delete(accelerator);
		}
	});

	ipcMain.handle('unregister-all-global-shortcuts', (): void => {
		globalShortcut.unregisterAll();
		registeredShortcuts.clear();
	});

	ipcMain.handle('check-media-access', (_event, type: MediaAccessType): string => {
		if (process.platform !== 'darwin') {
			return 'granted';
		}
		return systemPreferences.getMediaAccessStatus(type);
	});

	ipcMain.handle('request-media-access', async (_event, type: MediaAccessType): Promise<boolean> => {
		if (process.platform !== 'darwin') {
			return true;
		}
		if (type === 'screen') {
			return systemPreferences.getMediaAccessStatus('screen') === 'granted';
		}
		return systemPreferences.askForMediaAccess(type);
	});

	ipcMain.handle('open-media-access-settings', async (_event, type: MediaAccessType): Promise<void> => {
		if (process.platform !== 'darwin') {
			return;
		}
		const privacyKeys: Record<MediaAccessType, string> = {
			microphone: 'Privacy_Microphone',
			camera: 'Privacy_Camera',
			screen: 'Privacy_ScreenCapture',
		};
		await shell.openExternal(`x-apple.systempreferences:com.apple.preference.security?${privacyKeys[type]}`);
	});

	ipcMain.handle('check-accessibility', (_event, prompt: boolean): boolean => {
		if (process.platform !== 'darwin') {
			return true;
		}
		return systemPreferences.isTrustedAccessibilityClient(prompt);
	});

	ipcMain.handle('open-accessibility-settings', async (): Promise<void> => {
		if (process.platform !== 'darwin') {
			return;
		}
		await shell.openExternal('x-apple.systempreferences:com.apple.preference.security?Privacy_Accessibility');
	});

	ipcMain.handle('open-input-monitoring-settings', async (): Promise<void> => {
		if (process.platform !== 'darwin') {
			return;
		}
		await shell.openExternal('x-apple.systempreferences:com.apple.preference.security?Privacy_ListenEvent');
	});

	ipcMain.handle('show-notification', async (_event, options: NotificationOptions): Promise<{id: string}> => {
		const id = `notification-${++notificationIdCounter}`;

		if (!Notification.isSupported()) {
			return {id};
		}

		const notificationOpts: Electron.NotificationConstructorOptions = {
			title: options.title,
			body: options.body,
			silent: true,
		};

		if (options.icon) {
			try {
				if (options.icon.startsWith('http://') || options.icon.startsWith('https://')) {
					const iconBuffer = await downloadToBuffer(options.icon);
					notificationOpts.icon = nativeImage.createFromBuffer(iconBuffer);
				} else if (options.icon.startsWith('data:')) {
					const base64Data = options.icon.split(',')[1];
					if (base64Data) {
						const iconBuffer = Buffer.from(base64Data, 'base64');
						notificationOpts.icon = nativeImage.createFromBuffer(iconBuffer);
					}
				} else {
					notificationOpts.icon = options.icon;
				}
			} catch (error) {
				console.warn('[Notification] Failed to load icon:', error);
			}
		}

		const notification = new Notification(notificationOpts);

		activeNotifications.set(id, {notification, url: options.url});

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
			activeNotifications.delete(id);
		});

		notification.on('close', () => {
			activeNotifications.delete(id);
		});

		notification.show();
		return {id};
	});

	ipcMain.on('close-notification', (_event, id: string) => {
		const active = activeNotifications.get(id);
		if (active) {
			active.notification.close();
			activeNotifications.delete(id);
		}
	});

	ipcMain.on('close-notifications', (_event, ids: Array<string>) => {
		for (const id of ids) {
			const active = activeNotifications.get(id);
			if (active) {
				active.notification.close();
				activeNotifications.delete(id);
			}
		}
	});

	ipcMain.on('set-badge-count', (_event, count: number) => {
		if (process.platform === 'darwin') {
			app.setBadgeCount(count);
		} else if (process.platform === 'win32') {
			setWindowsBadgeOverlay(getMainWindow(), count);
		} else {
			app.setBadgeCount(count);
		}
	});

	ipcMain.handle('get-badge-count', (): number => {
		return app.getBadgeCount();
	});

	ipcMain.on('bounce-dock', (event, type: 'critical' | 'informational') => {
		if (process.platform === 'darwin' && app.dock) {
			const id = app.dock.bounce(type);
			event.returnValue = id;
		} else {
			event.returnValue = -1;
		}
	});

	ipcMain.on('cancel-bounce-dock', (_event, id: number) => {
		if (process.platform === 'darwin' && app.dock && id >= 0) {
			app.dock.cancelBounce(id);
		}
	});

	ipcMain.on('set-zoom-factor', (event, factor: number) => {
		const win = BrowserWindow.fromWebContents(event.sender);
		if (win && factor > 0) {
			win.webContents.setZoomFactor(factor);
		}
	});

	ipcMain.handle('get-zoom-factor', (event): number => {
		const win = BrowserWindow.fromWebContents(event.sender);
		return win?.webContents.getZoomFactor() ?? 1;
	});

	ipcMain.handle('passkey-is-supported', (): Promise<boolean> => {
		return passkeyProvider.isSupported();
	});

	ipcMain.handle(
		'passkey-authenticate',
		async (_event, options: PublicKeyCredentialRequestOptionsJSON): Promise<AuthenticationResponseJSON> => {
			return passkeyProvider.authenticate(options);
		},
	);

	ipcMain.handle(
		'passkey-register',
		async (_event, options: PublicKeyCredentialCreationOptionsJSON): Promise<RegistrationResponseJSON> => {
			return passkeyProvider.register(options);
		},
	);
}

function downloadToBuffer(url: string): Promise<Buffer> {
	return new Promise((resolve, reject) => {
		const protocol = url.startsWith('https://') ? https : http;
		protocol
			.get(url, (response) => {
				if (response.statusCode === 301 || response.statusCode === 302) {
					const redirectUrl = response.headers.location;
					if (redirectUrl) {
						downloadToBuffer(redirectUrl).then(resolve).catch(reject);
						return;
					}
				}

				if (response.statusCode !== 200) {
					reject(new Error(`HTTP ${response.statusCode}`));
					return;
				}

				const chunks: Array<Buffer> = [];
				response.on('data', (chunk: Buffer) => chunks.push(chunk));
				response.on('end', () => resolve(Buffer.concat(chunks)));
				response.on('error', reject);
			})
			.on('error', reject);
	});
}

function downloadFile(url: string, destPath: string): Promise<void> {
	return new Promise((resolve, reject) => {
		const protocol = url.startsWith('https://') ? https : http;
		const file = fs.createWriteStream(destPath);

		protocol
			.get(url, (response) => {
				if (response.statusCode === 301 || response.statusCode === 302) {
					const redirectUrl = response.headers.location;
					if (redirectUrl) {
						file.close();
						fs.unlinkSync(destPath);
						downloadFile(redirectUrl, destPath).then(resolve).catch(reject);
						return;
					}
				}

				if (response.statusCode !== 200) {
					file.close();
					fs.unlinkSync(destPath);
					reject(new Error(`HTTP ${response.statusCode}`));
					return;
				}

				response.pipe(file);
				file.on('finish', () => {
					file.close();
					resolve();
				});
			})
			.on('error', (err) => {
				file.close();
				fs.unlink(destPath, () => {});
				reject(err);
			});
	});
}

export function cleanupIpcHandlers(): void {
	globalShortcut.unregisterAll();
	registeredShortcuts.clear();
}
