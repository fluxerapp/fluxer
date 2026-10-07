// SPDX-License-Identifier: AGPL-3.0-or-later

import {APP_PROTOCOL} from '@electron/common/Constants';
import {parseJumpListTaskFromArgv} from '@electron/main/JumpList';
import {recordRecentDeepLink} from '@electron/main/RecentDocuments';
import {getMainWindow, showWindow} from '@electron/main/Window';
import {app, ipcMain} from 'electron';
import log from 'electron-log';

let initialDeepLink: string | null = null;

const DUPLICATE_URL_SUPPRESS_MS = 1500;
const APP_PROTOCOL_SCHEME = `${APP_PROTOCOL}:`;
const DEEP_LINK_RENDERER_PAYLOAD_BLOCKLIST = /["'<>\\|\t\r\n]/;
const INSTANCE_DESIGNATING_KEYS = new Set([
	'api',
	'apibase',
	'apiendpoint',
	'domain',
	'endpoint',
	'gateway',
	'host',
	'instance',
	'instancekey',
	'origin',
]);

function designationKey(name: string): string {
	return name.toLowerCase().replace(/[^a-z0-9]/gu, '');
}

export function findInstanceDesignation(url: URL): string | null {
	if (INSTANCE_DESIGNATING_KEYS.has(designationKey(url.hostname))) {
		return url.hostname;
	}
	for (const name of url.searchParams.keys()) {
		if (INSTANCE_DESIGNATING_KEYS.has(designationKey(name))) {
			return name;
		}
	}
	return null;
}

function findRawInstanceDesignation(rawUrl: string): string | null {
	for (const match of rawUrl.matchAll(/[?&]([^=&#]+)=/gu)) {
		const name = match[1];
		if (INSTANCE_DESIGNATING_KEYS.has(designationKey(name))) {
			return name;
		}
	}
	return null;
}

function rejectInstanceDesignation(designation: string): null {
	log.warn(
		`[DeepLinks] Rejected a deep link that tries to designate an instance through "${designation}" because deep links resolve against the foreground account only`,
	);
	return null;
}

let lastDispatchedUrl: string | null = null;
let lastDispatchedAt = 0;

function shouldSuppressAsDuplicate(url: string): boolean {
	const now = Date.now();
	if (lastDispatchedUrl === url && now - lastDispatchedAt < DUPLICATE_URL_SUPPRESS_MS) {
		return true;
	}
	lastDispatchedUrl = url;
	lastDispatchedAt = now;
	return false;
}

function isAppProtocolUrl(value: string): boolean {
	if (value.length <= APP_PROTOCOL_SCHEME.length) return false;
	try {
		return new URL(value).protocol.toLowerCase() === APP_PROTOCOL_SCHEME;
	} catch {
		return value.toLowerCase().startsWith(APP_PROTOCOL_SCHEME);
	}
}

function extractDeepLinkFromArgv(argv: ReadonlyArray<string>): string | null {
	const urlFlagIndex = argv.indexOf('--url');
	if (urlFlagIndex !== -1) {
		const separatorIndex = argv.indexOf('--', urlFlagIndex + 1);
		const urlArgs = separatorIndex === -1 ? argv.slice(urlFlagIndex + 1) : argv.slice(separatorIndex + 1);
		const url = urlArgs.find(isAppProtocolUrl);
		if (url) return url;
	}
	return argv.find(isAppProtocolUrl) ?? null;
}

function normalizeDeepLinkForRenderer(rawUrl: string): string | null {
	try {
		const parsed = new URL(rawUrl);
		if (parsed.protocol.toLowerCase() !== APP_PROTOCOL_SCHEME) {
			return null;
		}
		const designation = findInstanceDesignation(parsed);
		if (designation !== null) {
			return rejectInstanceDesignation(designation);
		}
		const host = parsed.hostname;
		const path = host && host !== '-' ? `/${host}${parsed.pathname}` : parsed.pathname || '/';
		const payload = `${path.startsWith('/') ? path : `/${path}`}${parsed.search}${parsed.hash}`;
		return DEEP_LINK_RENDERER_PAYLOAD_BLOCKLIST.test(payload) ? null : payload;
	} catch {
		const designation = findRawInstanceDesignation(rawUrl);
		if (designation !== null) {
			return rejectInstanceDesignation(designation);
		}
		return isAppProtocolUrl(rawUrl) && !DEEP_LINK_RENDERER_PAYLOAD_BLOCKLIST.test(rawUrl) ? rawUrl : null;
	}
}

export function initializeDeepLinks(): void {
	if (process.platform === 'linux') {
		registerInitialDeepLinkHandler();
		return;
	}
	if (app.isPackaged) {
		app.setAsDefaultProtocolClient(APP_PROTOCOL);
	}
	registerInitialDeepLinkHandler();
}

function registerInitialDeepLinkHandler(): void {
	const deepLinkArg = extractDeepLinkFromArgv(process.argv);
	if (deepLinkArg) {
		const normalized = normalizeDeepLinkForRenderer(deepLinkArg);
		if (normalized) {
			initialDeepLink = normalized;
			shouldSuppressAsDuplicate(normalized);
		}
	}
	ipcMain.handle('get-initial-deep-link', (): string | null => {
		const url = initialDeepLink;
		initialDeepLink = null;
		return url;
	});
}

function dispatchDeepLink(url: string): void {
	const normalized = normalizeDeepLinkForRenderer(url);
	if (!normalized || shouldSuppressAsDuplicate(normalized)) return;
	recordRecentDeepLink(url);
	const mainWindow = getMainWindow();
	if (mainWindow && !mainWindow.isDestroyed()) {
		mainWindow.webContents.send('deep-link', normalized);
		showWindow();
	} else {
		initialDeepLink = normalized;
	}
}

function dispatchJumpListTask(taskId: 'open-settings' | 'new-dm'): void {
	const mainWindow = getMainWindow();
	if (!mainWindow || mainWindow.isDestroyed()) return;
	showWindow();
	if (taskId === 'open-settings') {
		mainWindow.webContents.send('open-settings');
	} else {
		mainWindow.webContents.send('jump-list-new-dm');
	}
}

export function handleOpenUrl(url: string): void {
	dispatchDeepLink(url);
}

function isSquirrelOrSyntheticArg(arg: string): boolean {
	return arg.startsWith('--squirrel-');
}

export function handleSecondInstance(argv: Array<string>): void {
	const task = parseJumpListTaskFromArgv(argv);
	if (task) {
		dispatchJumpListTask(task);
		return;
	}
	const url = extractDeepLinkFromArgv(argv);
	if (url) {
		dispatchDeepLink(url);
		return;
	}
	if (argv.some(isSquirrelOrSyntheticArg)) {
		return;
	}
	showWindow();
}

export function consumeInitialJumpListTask(): 'open-settings' | 'new-dm' | null {
	return parseJumpListTaskFromArgv(process.argv);
}
