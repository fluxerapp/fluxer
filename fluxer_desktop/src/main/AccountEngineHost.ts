// SPDX-License-Identifier: AGPL-3.0-or-later

import {errorMessage} from '@electron/common/ErrorMessage';

import {
	ACCOUNT_ENGINE_HOST_PROTOCOL_VERSION,
	type AdmittedGateway,
	type EngineAccountRecord,
	type EnginePowerEvent,
	type EngineToHostMessage,
	type EngineTokenPersistOutcome,
	type HostToEngineMessage,
} from '@electron/engine/HostProtocol';
import type {AccountEngineAccountEntry, AccountEngineAccountLabel} from '@electron/main/AccountEngineAccounts';
import {
	type AccountEngineAccountSummary,
	AccountEngineConnectionState,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';

const ACCOUNT_ENGINE_RESTART_DELAYS_MS = Object.freeze([1_000, 2_000, 5_000, 10_000, 30_000]);
const ACCOUNT_ENGINE_CRASH_WINDOW_MS = 10 * 60 * 1000;
const ACCOUNT_ENGINE_CRASH_LIMIT = 5;
const ACCOUNT_ENGINE_ACCOUNTS_DEBOUNCE_MS = 100;
const ACCOUNT_ENGINE_ACCOUNTS_RETRY_MS = 5_000;
const ACCOUNT_ENGINE_SHUTDOWN_TIMEOUT_MS = 3_000;

export interface AccountEngineProcess {
	postMessage(message: HostToEngineMessage, transfer?: ReadonlyArray<unknown>): void;
	onMessage(listener: (message: EngineToHostMessage) => void): void;
	onExit(listener: (code: number) => void): void;
	kill(): void;
}

export interface AccountEngineRendererLink {
	createPortPair(): {readonly engine: unknown; readonly renderer: unknown};
	deliver(port: unknown): boolean;
	isCurrent(): boolean;
}

export interface AccountEngineHttpResult {
	readonly ok: boolean;
	readonly status: number;
	readonly body: string | null;
}

export interface AccountEngineHostDependencies {
	readonly fork: () => AccountEngineProcess;
	readonly readAccounts: () => Promise<ReadonlyArray<AccountEngineAccountEntry>>;
	readonly admit: (entry: AccountEngineAccountEntry, url: string) => Promise<AdmittedGateway>;
	readonly http: (
		entry: AccountEngineAccountEntry,
		method: 'POST',
		path: string,
		body: string | null,
	) => Promise<AccountEngineHttpResult>;
	readonly persistRotatedToken: (
		accountKey: string,
		previousToken: string,
		token: string,
	) => Promise<EngineTokenPersistOutcome>;
	readonly log: (level: 'debug' | 'info' | 'warn' | 'error', message: string, detail?: unknown) => void;
	readonly now: () => number;
	readonly setTimeout: (callback: () => void, delayMs: number) => unknown;
	readonly clearTimeout: (timer: unknown) => void;
	readonly cachePath: string;
	readonly preferWebSocket: boolean;
	readonly trustedCertificates: () => ReadonlyArray<string>;
}

export interface AccountEngineLabel extends AccountEngineAccountLabel {
	readonly isForeground: boolean;
}

const ENGINE_HTTP_ROUTES: ReadonlyArray<{readonly method: 'POST'; readonly path: RegExp}> = [
	{method: 'POST', path: /^\/channels\/\d{1,20}\/messages\/\d{1,20}\/ack$/u},
];

function isAllowedEngineHttpRequest(method: string, path: string): boolean {
	return ENGINE_HTTP_ROUTES.some((route) => route.method === method && route.path.test(path));
}

type SummaryListener = (summaries: ReadonlyArray<AccountEngineAccountSummary>) => void;

function foregroundHint(entries: ReadonlyArray<AccountEngineAccountEntry>): string | null {
	let best: EngineAccountRecord | null = null;
	for (const {record} of entries) {
		if (record.isValid && (best === null || record.lastActive > best.lastActive)) {
			best = record;
		}
	}
	return best?.accountKey ?? null;
}

export class AccountEngineHostController {
	private readonly deps: AccountEngineHostDependencies;
	private process: AccountEngineProcess | null = null;
	private entries = new Map<string, AccountEngineAccountEntry>();
	private readonly summaries = new Map<string, AccountEngineAccountSummary>();
	private readonly listeners = new Set<SummaryListener>();
	private readonly crashes: Array<number> = [];
	private renderer: AccountEngineRendererLink | null = null;
	private restartTimer: unknown = null;
	private accountsTimer: unknown = null;
	private readonly accountsRetryTimers = new Set<unknown>();
	private stopping = false;
	private available = true;
	private started = false;
	private shutdownResolve: (() => void) | null = null;

	constructor(deps: AccountEngineHostDependencies) {
		this.deps = deps;
	}

	get isAvailable(): boolean {
		return this.available;
	}

	async start(): Promise<void> {
		if (this.started) {
			return;
		}
		this.started = true;
		await this.spawn();
	}

	accountsChanged(): void {
		if (!this.started || this.stopping || this.accountsTimer !== null) {
			return;
		}
		this.accountsTimer = this.deps.setTimeout(() => {
			this.accountsTimer = null;
			void this.pushAccounts();
		}, ACCOUNT_ENGINE_ACCOUNTS_DEBOUNCE_MS);
	}

	attachRenderer(link: AccountEngineRendererLink): boolean {
		if (!this.available || this.process === null || this.stopping) {
			return false;
		}
		this.renderer = link;
		return this.connectRenderer(link);
	}

	detachRenderer(link: AccountEngineRendererLink): void {
		if (this.renderer !== link) {
			return;
		}
		this.renderer = null;
		this.process?.postMessage({type: 'detachRenderer'});
	}

	power(event: EnginePowerEvent): void {
		this.process?.postMessage({type: 'power', event});
	}

	getSummaries(): ReadonlyArray<AccountEngineAccountSummary> {
		return Array.from(this.summaries.values());
	}

	getLabels(): ReadonlyArray<AccountEngineLabel> {
		return Array.from(this.entries.values(), ({label}) => ({
			...label,
			isForeground: this.summaries.get(label.accountKey)?.state === AccountEngineConnectionState.FOREGROUND,
		}));
	}

	onSummaries(listener: SummaryListener): () => void {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}

	shutdown(): Promise<void> {
		this.stopping = true;
		if (this.restartTimer !== null) {
			this.deps.clearTimeout(this.restartTimer);
			this.restartTimer = null;
		}
		if (this.accountsTimer !== null) {
			this.deps.clearTimeout(this.accountsTimer);
			this.accountsTimer = null;
		}
		for (const timer of this.accountsRetryTimers) {
			this.deps.clearTimeout(timer);
		}
		this.accountsRetryTimers.clear();
		const engine = this.process;
		if (engine === null) {
			return Promise.resolve();
		}
		return new Promise<void>((resolve) => {
			const timer = this.deps.setTimeout(() => {
				this.deps.log('warn', 'The account engine did not exit in time, killing it');
				engine.kill();
				finish();
			}, ACCOUNT_ENGINE_SHUTDOWN_TIMEOUT_MS);
			const finish = (): void => {
				this.deps.clearTimeout(timer);
				this.shutdownResolve = null;
				resolve();
			};
			this.shutdownResolve = finish;
			engine.postMessage({type: 'shutdown'});
		});
	}

	private async spawn(): Promise<void> {
		let entries: ReadonlyArray<AccountEngineAccountEntry> = Array.from(this.entries.values());
		let accountsComplete = true;
		try {
			entries = await this.deps.readAccounts();
		} catch (error) {
			accountsComplete = false;
			this.deps.log('warn', 'Account engine could not read accounts', error);
		}
		if (this.stopping) {
			return;
		}
		this.entries = new Map(entries.map((entry) => [entry.record.accountKey, entry]));
		let engine: AccountEngineProcess;
		try {
			engine = this.deps.fork();
		} catch (error) {
			this.deps.log('error', 'Account engine could not start', error);
			this.handleExit(null, -1);
			return;
		}
		this.process = engine;
		engine.onMessage((message) => this.handleMessage(engine, message));
		engine.onExit((code) => this.handleExit(engine, code));
		engine.postMessage({
			type: 'init',
			protocol: ACCOUNT_ENGINE_HOST_PROTOCOL_VERSION,
			cachePath: this.deps.cachePath,
			foregroundHint: this.currentForeground() ?? foregroundHint(entries),
			accounts: entries.map((entry) => entry.record),
			accountsComplete,
			preferWebSocket: this.deps.preferWebSocket,
			trustedCertificates: this.deps.trustedCertificates(),
		});
		if (!accountsComplete) {
			this.accountsChanged();
		}
		const renderer = this.renderer;
		if (renderer?.isCurrent()) {
			this.connectRenderer(renderer);
		}
	}

	private currentForeground(): string | null {
		for (const summary of this.summaries.values()) {
			if (summary.state === AccountEngineConnectionState.FOREGROUND && this.entries.has(summary.accountKey)) {
				return summary.accountKey;
			}
		}
		return null;
	}

	private connectRenderer(link: AccountEngineRendererLink): boolean {
		const engine = this.process;
		if (engine === null) {
			return false;
		}
		const pair = link.createPortPair();
		engine.postMessage({type: 'attachRenderer'}, [pair.engine]);
		return link.deliver(pair.renderer);
	}

	private async pushAccounts(): Promise<void> {
		let entries: ReadonlyArray<AccountEngineAccountEntry>;
		try {
			entries = await this.deps.readAccounts();
		} catch (error) {
			this.deps.log('warn', 'Account engine could not read accounts', error);
			if (this.stopping) {
				return;
			}
			const timer = this.deps.setTimeout(() => {
				this.accountsRetryTimers.delete(timer);
				this.accountsChanged();
			}, ACCOUNT_ENGINE_ACCOUNTS_RETRY_MS);
			this.accountsRetryTimers.add(timer);
			return;
		}
		this.entries = new Map(entries.map((entry) => [entry.record.accountKey, entry]));
		for (const accountKey of Array.from(this.summaries.keys())) {
			if (!this.entries.has(accountKey)) {
				this.summaries.delete(accountKey);
			}
		}
		this.process?.postMessage({type: 'accounts', accounts: entries.map((entry) => entry.record)});
	}

	private handleMessage(engine: AccountEngineProcess, message: EngineToHostMessage): void {
		if (engine !== this.process || typeof message !== 'object' || message === null) {
			return;
		}
		switch (message.type) {
			case 'ready':
				this.deps.log('info', 'Account engine gateway transport', {transport: message.transport});
				if (message.transportError !== null) {
					this.deps.log('warn', 'Account engine uses the WebSocket fallback transport', message.transportError);
				}
				return;
			case 'admit':
				void this.answerAdmit(engine, message.requestId, message.accountKey, message.url);
				return;
			case 'http':
				void this.answerHttp(engine, message);
				return;
			case 'tokenRotated':
				void this.persistToken(engine, message.accountKey, message.previousToken, message.token);
				return;
			case 'summaries':
				this.storeSummaries(message.summaries);
				return;
			case 'removed':
				for (const accountKey of message.accountKeys) {
					this.summaries.delete(accountKey);
				}
				this.notify();
				return;
			case 'health':
				this.deps.log('info', 'Account engine health', {
					rss: message.rss,
					heapUsed: message.heapUsed,
					...message.counts,
				});
				return;
			case 'log':
				this.deps.log(message.level, `[AccountEngine] ${message.message}`, message.detail ?? undefined);
				return;
			case 'shutdownComplete':
				return;
		}
	}

	private storeSummaries(summaries: ReadonlyArray<AccountEngineAccountSummary>): void {
		for (const summary of summaries) {
			if (this.entries.has(summary.accountKey)) {
				this.summaries.set(summary.accountKey, summary);
			}
		}
		this.notify();
	}

	private notify(): void {
		const summaries = this.getSummaries();
		for (const listener of Array.from(this.listeners)) {
			try {
				listener(summaries);
			} catch (error) {
				this.deps.log('warn', 'Account engine summary listener failed', error);
			}
		}
	}

	private async answerAdmit(
		engine: AccountEngineProcess,
		requestId: number,
		accountKey: string,
		url: string,
	): Promise<void> {
		const entry = this.entries.get(accountKey);
		try {
			if (entry === undefined) {
				throw new Error('Unknown account');
			}
			if (url !== entry.record.gatewayEndpoint) {
				throw new Error('The gateway does not match the account record');
			}
			const admitted = await this.deps.admit(entry, url);
			engine.postMessage({
				type: 'admitResult',
				requestId,
				ok: true,
				url: admitted.url,
				address: admitted.address,
				proxy: admitted.proxy,
				error: null,
			});
		} catch (error) {
			engine.postMessage({
				type: 'admitResult',
				requestId,
				ok: false,
				url: null,
				address: null,
				proxy: null,
				error: errorMessage(error),
			});
		}
	}

	private async answerHttp(
		engine: AccountEngineProcess,
		message: Extract<EngineToHostMessage, {type: 'http'}>,
	): Promise<void> {
		const entry = this.entries.get(message.accountKey);
		try {
			if (entry === undefined) {
				throw new Error('Unknown account');
			}
			if (!isAllowedEngineHttpRequest(message.method, message.path)) {
				throw new Error('Invalid request path');
			}
			const result = await this.deps.http(entry, message.method, message.path, message.body);
			engine.postMessage({
				type: 'httpResult',
				requestId: message.requestId,
				ok: result.ok,
				status: result.status,
				body: result.body,
				error: null,
			});
		} catch (error) {
			engine.postMessage({
				type: 'httpResult',
				requestId: message.requestId,
				ok: false,
				status: 0,
				body: null,
				error: errorMessage(error),
			});
		}
	}

	private async persistToken(
		engine: AccountEngineProcess,
		accountKey: string,
		previousToken: string,
		token: string,
	): Promise<void> {
		let outcome: EngineTokenPersistOutcome = 'rejected';
		try {
			outcome = await this.deps.persistRotatedToken(accountKey, previousToken, token);
		} catch (error) {
			this.deps.log('warn', 'Account engine token rotation could not be persisted', error);
		}
		if (engine === this.process) {
			engine.postMessage({type: 'tokenPersisted', accountKey, previousToken, token, outcome});
		}
		if (outcome === 'persisted') {
			this.accountsChanged();
		}
	}

	private handleExit(engine: AccountEngineProcess | null, code: number): void {
		if (engine !== null && engine !== this.process) {
			return;
		}
		this.process = null;
		if (this.stopping) {
			this.shutdownResolve?.();
			return;
		}
		const now = this.deps.now();
		this.crashes.push(now);
		while (this.crashes.length > 0 && now - this.crashes[0] > ACCOUNT_ENGINE_CRASH_WINDOW_MS) {
			this.crashes.shift();
		}
		this.deps.log('warn', 'The account engine exited unexpectedly', {code, recentCrashes: this.crashes.length});
		if (this.crashes.length >= ACCOUNT_ENGINE_CRASH_LIMIT) {
			this.available = false;
			this.deps.log('error', 'The account engine keeps crashing and stays off for this session');
			return;
		}
		const delay =
			ACCOUNT_ENGINE_RESTART_DELAYS_MS[Math.min(this.crashes.length - 1, ACCOUNT_ENGINE_RESTART_DELAYS_MS.length - 1)];
		this.restartTimer = this.deps.setTimeout(() => {
			this.restartTimer = null;
			void this.spawn();
		}, delay);
	}
}

let host: AccountEngineHostController | null = null;

export function installAccountEngineHost(controller: AccountEngineHostController | null): void {
	host = controller;
}

export function isAccountEngineAvailable(): boolean {
	return host?.isAvailable === true;
}

export function notifyAccountEngineAccountsChanged(): void {
	host?.accountsChanged();
}

export function getAccountEngineSummaries(): ReadonlyArray<AccountEngineAccountSummary> {
	return host?.getSummaries() ?? [];
}

export function onAccountEngineSummaries(listener: SummaryListener): () => void {
	return host?.onSummaries(listener) ?? (() => undefined);
}

export function getAccountEngineLabels(): ReadonlyArray<AccountEngineLabel> {
	return host?.getLabels() ?? [];
}
