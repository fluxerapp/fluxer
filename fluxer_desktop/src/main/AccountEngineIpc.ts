// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';
import type {AdmittedGateway, EngineTokenPersistOutcome} from '@electron/engine/HostProtocol';
import {
	type AccountEngineAccountEntry,
	parseAccountEngineEntries,
	parseProvidedAccountEngineEntries,
	rendererHoldsAccount,
	rotateStoredAccountToken,
} from '@electron/main/AccountEngineAccounts';
import {
	AccountEngineHostController,
	type AccountEngineHttpResult,
	type AccountEngineProcess,
	type AccountEngineRendererLink,
	installAccountEngineHost,
} from '@electron/main/AccountEngineHost';
import {getDesktopAppStorage} from '@electron/main/DesktopAppStorage';
import {getDesktopDistributionPath} from '@electron/main/DesktopDistributionPath';
import {
	DesktopOriginTrust,
	getDesktopOutboundHTTP,
	readBoundedMessage,
	readMessageContentLength,
} from '@electron/main/DesktopOutboundHTTP';
import {resolveDesktopGatewayTrustedCertificates} from '@electron/main/DesktopSystemTrustVerifier';
import {getGatewayOriginRegistry} from '@electron/main/GatewayOriginRegistry';
import {prefersAccountEngineWebSocketTransport} from '@electron/main/LaunchOptions';
import {
	createPrivilegedRendererDocumentOwners,
	requirePrivilegedRendererDocumentSender,
} from '@electron/main/PrivilegedRendererDocuments';
import {websocketHTTPOrigin} from '@electron/main/WebSocketOrigin';
import {getMainWindow} from '@electron/main/Window';
import {ACCOUNT_ENGINE_CHANNELS} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import {
	DESKTOP_LEGACY_AUTHORITY_MARKER_KEY,
	DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE,
} from '@fluxer/desktop_ipc/src/StorageContract';
import {
	app,
	ipcMain,
	MessageChannelMain,
	type MessagePortMain,
	net,
	powerMonitor,
	session,
	utilityProcess,
} from 'electron';
import log from 'electron-log';

const ENGINE_CACHE_FILE_NAME = 'account-engine-cache.sqlite3';
const HTTP_TIMEOUT_MS = 15_000;
const HTTP_RESPONSE_MAX_BYTES = 4 * 1024 * 1024;
const HTTP_RESPONSE_MAX_CHUNKS = 4096;
const NET_POLL_INTERVAL_MS = 5_000;

let controller: AccountEngineHostController | null = null;
let providedAccounts: ReadonlyArray<AccountEngineAccountEntry> = [];
let netPollTimer: ReturnType<typeof setInterval> | null = null;
const powerSubscriptions: Array<() => void> = [];

function forkEngine(): AccountEngineProcess {
	const child = utilityProcess.fork(getDesktopDistributionPath('engine', 'AccountEngine.cjs'), [], {
		serviceName: 'Fluxer account engine',
		stdio: 'pipe',
		...(process.platform === 'darwin' && !app.isPackaged ? {allowLoadingUnsignedLibraries: true} : {}),
	});
	child.stdout?.on('data', (chunk: Buffer) => log.info('[AccountEngine]', chunk.toString('utf8').trimEnd()));
	child.stderr?.on('data', (chunk: Buffer) => log.warn('[AccountEngine]', chunk.toString('utf8').trimEnd()));
	return {
		postMessage: (message, transfer) => child.postMessage(message, (transfer ?? []) as Array<MessagePortMain>),
		onMessage: (listener) => {
			child.on('message', listener);
		},
		onExit: (listener) => {
			child.on('exit', listener);
		},
		kill: () => {
			child.kill();
		},
	};
}

async function desktopStoreIsAuthoritative(): Promise<boolean> {
	const storage = getDesktopAppStorage();
	return (
		storage !== null &&
		(await storage.getMarker(DESKTOP_LEGACY_AUTHORITY_MARKER_KEY)) === DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE
	);
}

async function readEngineAccounts(): Promise<ReadonlyArray<AccountEngineAccountEntry>> {
	const storage = getDesktopAppStorage();
	if (storage === null) {
		return [];
	}
	if (!(await desktopStoreIsAuthoritative())) {
		return providedAccounts;
	}
	return parseAccountEngineEntries(await storage.getAllAccounts());
}

function engineGatewayProxy(route: string): string | null {
	for (const entry of route.split(';')) {
		const [scheme = '', hostPort = ''] = entry.trim().split(/\s+/u);
		switch (scheme.toUpperCase()) {
			case 'DIRECT':
				return null;
			case 'PROXY':
				return `http://${hostPort}`;
			case 'HTTPS':
				return `https://${hostPort}`;
		}
	}
	throw new Error(`The gateway resolves through a proxy the account engine cannot use: ${route}`);
}

async function admitEngineGateway(entry: AccountEngineAccountEntry, url: string): Promise<AdmittedGateway> {
	await getGatewayOriginRegistry().admitAnchored({url, anchor: entry.record.apiEndpoint});
	const proxy = engineGatewayProxy(await session.defaultSession.resolveProxy(url.replace(/^ws/u, 'http')));
	if (new URL(url).protocol !== 'ws:') {
		return {url, address: null, proxy};
	}
	const httpOrigin = websocketHTTPOrigin(url);
	if (httpOrigin == null) {
		throw new Error('The gateway has no HTTP origin');
	}
	const address = await getDesktopOutboundHTTP().requireCleartextTransportAddress(httpOrigin);
	return {url, address: proxy === null ? address : null, proxy};
}

async function engineHttp(
	entry: AccountEngineAccountEntry,
	method: 'POST',
	requestPath: string,
	body: string | null,
): Promise<AccountEngineHttpResult> {
	const outbound = getDesktopOutboundHTTP();
	const apiOrigin = new URL(entry.record.apiEndpoint).origin;
	await outbound.registerAnchoredOrigins({anchorOrigin: apiOrigin, origins: []});
	const base = entry.record.apiEndpoint.replace(/\/+$/u, '');
	const response = await outbound.request({
		body: body === null ? null : new TextEncoder().encode(body),
		expectedOrigin: apiOrigin,
		headers: {
			Authorization: entry.record.token,
			Accept: 'application/json',
			...(body === null ? {} : {'Content-Type': 'application/json'}),
		},
		method,
		originTrust: DesktopOriginTrust.REGISTERED,
		signal: null,
		timeoutMs: HTTP_TIMEOUT_MS,
		url: `${base}/v${entry.apiCodeVersion}${requestPath}`,
	});
	const description = 'Account engine response';
	const payload = await readBoundedMessage({
		declaredBytes: readMessageContentLength(response, description),
		description,
		maxBytes: HTTP_RESPONSE_MAX_BYTES,
		maxChunks: HTTP_RESPONSE_MAX_CHUNKS,
		message: response.message,
	});
	return {ok: response.status >= 200 && response.status < 300, status: response.status, body: payload.toString('utf8')};
}

async function persistRotatedToken(
	accountKey: string,
	previousToken: string,
	token: string,
): Promise<EngineTokenPersistOutcome> {
	const storage = getDesktopAppStorage();
	if (storage === null) {
		return 'rejected';
	}
	if (!(await desktopStoreIsAuthoritative())) {
		return rendererHoldsAccount(providedAccounts, accountKey) ? 'renderer' : 'rejected';
	}
	const stored = await storage.getAccount(accountKey);
	if (stored === null) {
		return 'rejected';
	}
	const replacement = rotateStoredAccountToken(stored, previousToken, token);
	if (replacement === null) {
		return 'rejected';
	}
	return (await storage.compareAndSwapAccount({expected: stored, replacement})) ? 'persisted' : 'rejected';
}

function registerConnectHandler(host: AccountEngineHostController): void {
	const owners = createPrivilegedRendererDocumentOwners('AccountEngine');
	ipcMain.handle(ACCOUNT_ENGINE_CHANNELS.connect, (event) => {
		requirePrivilegedRendererDocumentSender(event, ACCOUNT_ENGINE_CHANNELS.connect);
		if (getMainWindow()?.webContents !== event.sender) {
			return false;
		}
		const owner = owners.capture(event, ACCOUNT_ENGINE_CHANNELS.connect);
		const link: AccountEngineRendererLink = {
			createPortPair: () => {
				const {port1, port2} = new MessageChannelMain();
				return {engine: port1, renderer: port2};
			},
			deliver: (port) => {
				if (!owner.isCurrent()) {
					return false;
				}
				owner.frame.postMessage(ACCOUNT_ENGINE_CHANNELS.port, null, [port as MessagePortMain]);
				return true;
			},
			isCurrent: () => owner.isCurrent(),
		};
		const watcher = owner.watchInvalidation(() => host.detachRenderer(link));
		const attached = host.attachRenderer(link);
		if (!attached) {
			watcher.dispose();
		}
		return attached;
	});
}

function registerAccountsHandler(host: AccountEngineHostController): void {
	ipcMain.handle(ACCOUNT_ENGINE_CHANNELS.accounts, async (event, accounts: unknown) => {
		requirePrivilegedRendererDocumentSender(event, ACCOUNT_ENGINE_CHANNELS.accounts);
		if (getMainWindow()?.webContents !== event.sender || (await desktopStoreIsAuthoritative())) {
			return false;
		}
		const parsed = parseProvidedAccountEngineEntries(accounts);
		if (parsed === null) {
			return false;
		}
		providedAccounts = parsed;
		host.accountsChanged();
		return true;
	});
}

function watchPower(host: AccountEngineHostController): void {
	for (const event of ['suspend', 'resume', 'lock-screen', 'unlock-screen'] as const) {
		const listener = (): void => host.power(event);
		powerMonitor.on(event as 'suspend', listener);
		powerSubscriptions.push(() => powerMonitor.off(event as 'suspend', listener));
	}
	let online = net.isOnline();
	netPollTimer = setInterval(() => {
		const next = net.isOnline();
		if (next !== online) {
			online = next;
			host.power(next ? 'online' : 'offline');
		}
	}, NET_POLL_INTERVAL_MS);
	netPollTimer.unref();
}

export function startAccountEngine(userDataPath: string): void {
	if (controller !== null || getDesktopAppStorage() === null) {
		return;
	}
	const host = new AccountEngineHostController({
		fork: forkEngine,
		readAccounts: readEngineAccounts,
		admit: admitEngineGateway,
		http: engineHttp,
		persistRotatedToken,
		log: (level, message, detail) => (detail === undefined ? log[level](message) : log[level](message, detail)),
		now: () => Date.now(),
		setTimeout: (callback, delayMs) => setTimeout(callback, delayMs),
		clearTimeout: (timer) => clearTimeout(timer as ReturnType<typeof setTimeout>),
		cachePath: path.join(userDataPath, ENGINE_CACHE_FILE_NAME),
		preferWebSocket: prefersAccountEngineWebSocketTransport(),
		trustedCertificates: () => resolveDesktopGatewayTrustedCertificates(),
	});
	controller = host;
	installAccountEngineHost(host);
	registerConnectHandler(host);
	registerAccountsHandler(host);
	watchPower(host);
	void host.start().catch((error: unknown) => log.error('[AccountEngine] Failed to start', error));
}

export async function stopAccountEngine(): Promise<void> {
	const host = controller;
	if (host === null) {
		return;
	}
	controller = null;
	ipcMain.removeHandler(ACCOUNT_ENGINE_CHANNELS.connect);
	ipcMain.removeHandler(ACCOUNT_ENGINE_CHANNELS.accounts);
	providedAccounts = [];
	for (const unsubscribe of powerSubscriptions.splice(0)) {
		unsubscribe();
	}
	if (netPollTimer !== null) {
		clearInterval(netPollTimer);
		netPollTimer = null;
	}
	await host.shutdown();
	installAccountEngineHost(null);
}
