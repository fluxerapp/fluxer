// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GlobalShortcutsPortalConsent} from '@electron/common/DesktopConfig';
import {
	GLOBAL_SHORTCUT_ACTIONS,
	GLOBAL_SHORTCUT_HOLD_ACTIONS,
	type GlobalShortcutAction,
} from '@electron/common/GlobalShortcutActions';
import type {GlobalShortcutsPortalState, GlobalShortcutsPortalStatus, LinuxDesktopKind} from '@electron/common/Types';

export interface PortalShortcutDefinition {
	id: string;
	description: string;
	preferredTrigger?: string | null;
}

export interface PortalShortcutBinding {
	id: string;
	description: string | null;
	triggerDescription: string | null;
}

export type PortalEvent =
	| {type: 'activated'; id: string}
	| {type: 'deactivated'; id: string}
	| {type: 'shortcuts-changed'; shortcuts: Array<PortalShortcutBinding>}
	| {type: 'session-lost'; reason: PortalSessionLostReason}
	| {type: 'portal-available'};

type PortalSessionLostReason = 'closed' | 'portal-restarted' | 'bus-error';

export interface PortalClientOptions {
	portalAppId: string | null;
	sandboxed: boolean;
	sessionToken: string;
	desktop: LinuxDesktopKind;
}

export interface PortalOpenResult {
	version: number;
	appIdSource: 'sandbox' | 'registered' | 'unregistered';
	uniqueName: string;
	listed: Array<PortalShortcutBinding>;
}

export type PortalBindOutcome =
	| {outcome: 'bound'; shortcuts: Array<PortalShortcutBinding>}
	| {outcome: 'cancelled'}
	| {outcome: 'denied'}
	| {outcome: 'failed'; code: number};

export interface PortalClient {
	open(): Promise<PortalOpenResult>;
	bind(shortcuts: Array<PortalShortcutDefinition>, parentWindow: string): Promise<PortalBindOutcome>;
	configure(parentWindow: string): Promise<void>;
	close(): void;
}

export interface PortalTimer {
	cancel(): void;
}

export interface LinuxPortalShortcutsDeps {
	createPortal: (onEvent: (event: PortalEvent) => void) => PortalClient;
	desktop: LinuxDesktopKind;
	plasma5: boolean;
	portalAppId: string | null;
	getConsent: () => GlobalShortcutsPortalConsent;
	setConsent: (consent: GlobalShortcutsPortalConsent) => void;
	getDefinitions: () => Array<PortalShortcutDefinition>;
	getParentWindow: () => string;
	onShortcut: (action: GlobalShortcutAction, phase: 'press' | 'release') => void;
	onStatusChanged: () => void;
	schedule: (callback: () => void, delayMs: number) => PortalTimer;
	now: () => number;
	log: (message: string, details?: Record<string, unknown>) => void;
}

export const PORTAL_RECOVERY_BACKOFF_MS: ReadonlyArray<number> = [1000, 2000, 5000, 10000, 30000];
export const PORTAL_NO_PORTAL_RETRY_WINDOW_MS = 30000;
export const PORTAL_TOGGLE_REARM_MS = 1500;
const PORTAL_CLOSED_RECOVERY_WINDOW_MS = 60000;
const NO_PORTAL_ERROR = 'unsupported:no-portal';
const CONFIGURE_UNSUPPORTED_ERROR = 'unsupported:version';
const RESPONSE_CODE_ERROR = /^unsupported:\d+$/;

type OpenFailure = {kind: 'no-portal' | 'unsupported' | 'identity' | 'timeout' | 'failed'; message: string};

function classifyOpenFailure(error: unknown): OpenFailure {
	const message = error instanceof Error ? error.message : String(error);
	if (message === NO_PORTAL_ERROR || RESPONSE_CODE_ERROR.test(message)) return {kind: 'no-portal', message};
	if (message.startsWith('unsupported:')) return {kind: 'unsupported', message};
	if (message.startsWith('identity:')) return {kind: 'identity', message};
	if (message === 'error:timeout') return {kind: 'timeout', message};
	return {kind: 'failed', message};
}

function errorMessage(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

function isKnownAction(id: string): id is GlobalShortcutAction {
	return (GLOBAL_SHORTCUT_ACTIONS as ReadonlyArray<string>).includes(id);
}

function normalizeTrigger(value: string | null): string | null {
	return value !== null && value.trim().length > 0 ? value : null;
}

export class LinuxPortalShortcutsManager {
	private state: GlobalShortcutsPortalState = 'unknown';
	private error: string | null = null;
	private version: number | null = null;
	private registered = true;
	private configureUnsupported = false;
	private readonly triggers = new Map<GlobalShortcutAction, string | null>();
	private readonly listedActions = new Set<GlobalShortcutAction>();
	private retainedTriggers = false;
	private readonly activeIds = new Set<GlobalShortcutAction>();
	private readonly lastActivatedAt = new Map<GlobalShortcutAction, number>();
	private portal: PortalClient | null = null;
	private opened = false;
	private generation = 0;
	private active = false;
	private queue: Promise<unknown> = Promise.resolve();
	private recoveryTimer: PortalTimer | null = null;
	private recoveryAttempt = 0;
	private retry: 'session' | 'startup' | null = null;
	private recovering = false;
	private retrySince: number | null = null;
	private lastClosedRecoveryAt: number | null = null;
	private binding = false;
	private pendingLoss: PortalSessionLostReason | null = null;
	private setUpTask: Promise<void> | null = null;

	constructor(private readonly deps: LinuxPortalShortcutsDeps) {}

	getState(): GlobalShortcutsPortalState {
		return this.state;
	}

	getStatus(): GlobalShortcutsPortalStatus {
		return {
			state: this.state,
			version: this.version,
			canConfigure:
				this.state === 'bound' &&
				this.opened &&
				!this.recovering &&
				this.version !== null &&
				this.version >= 2 &&
				!this.configureUnsupported,
			recovering: this.recovering,
			canRecheck: !this.deps.plasma5 && (this.state === 'unsupported' || this.state === 'error'),
			portalAppId: this.registered ? this.deps.portalAppId : null,
			shortcuts: GLOBAL_SHORTCUT_ACTIONS.map((action) => ({
				action,
				triggerDescription: this.triggers.get(action) ?? null,
			})),
			error: this.error,
		};
	}

	activate(): Promise<void> {
		if (this.active) return this.queue.then(() => undefined);
		this.active = true;
		if (this.deps.plasma5) {
			this.setState('unsupported', null);
			return Promise.resolve();
		}
		if (this.deps.getConsent() !== 'granted') return Promise.resolve();
		return this.enqueue(async () => {
			if (!this.active) return;
			await this.runStartupSequence();
		});
	}

	probe(): Promise<void> {
		if (!this.active || this.state !== 'unknown') return this.queue.then(() => undefined);
		return this.enqueue(async () => {
			if (!this.active || this.state !== 'unknown') return;
			await this.runStartupSequence();
		});
	}

	deactivate(): void {
		if (!this.active) return;
		this.active = false;
		this.cancelRecovery();
		this.resetRecovery();
		this.pendingLoss = null;
		this.closePortal();
		this.clearTriggers();
		this.lastActivatedAt.clear();
		this.version = null;
		this.setState('unknown', null);
	}

	releaseAll(): void {
		for (const action of [...this.activeIds]) {
			this.activeIds.delete(action);
			this.deps.onShortcut(action, 'release');
		}
	}

	recheck(): Promise<void> {
		if (!this.active || this.deps.plasma5) return this.queue.then(() => undefined);
		return this.enqueue(async () => {
			if (!this.active || (this.state !== 'unsupported' && this.state !== 'error')) return;
			this.cancelRecovery();
			this.resetRecovery();
			this.lastClosedRecoveryAt = null;
			this.setState('probing', null);
			await this.runStartupSequence();
		});
	}

	setUp(): Promise<void> {
		if (!this.active || this.deps.plasma5) return this.queue.then(() => undefined);
		if (this.state === 'unsupported') return this.recheck();
		if (this.setUpTask !== null) return this.setUpTask;
		if (this.binding) return this.queue.then(() => undefined);
		const task = this.enqueue(async () => {
			if (!this.active) return;
			this.cancelRecovery();
			this.resetRecovery();
			this.lastClosedRecoveryAt = null;
			if (!this.opened && !(await this.openSession())) return;
			const outcome = await this.bind();
			if (outcome?.outcome === 'bound' && this.everyTriggerEmpty() && this.deps.desktop !== 'hyprland') {
				await this.runConfigure();
			}
		});
		this.setUpTask = task;
		void task.finally(() => {
			if (this.setUpTask === task) this.setUpTask = null;
		});
		return task;
	}

	configure(): Promise<void> {
		return this.enqueue(async () => {
			if (!this.active || !this.opened || this.recovering || this.portal === null) return;
			await this.runConfigure();
		});
	}

	private async runConfigure(): Promise<void> {
		const portal = this.portal;
		if (portal === null || this.version === null || this.version < 2 || this.configureUnsupported) return;
		try {
			await portal.configure(this.deps.getParentWindow());
		} catch (error) {
			const message = errorMessage(error);
			this.deps.log('Global shortcuts portal configure failed', {message});
			if (message !== CONFIGURE_UNSUPPORTED_ERROR) return;
			this.configureUnsupported = true;
			this.deps.onStatusChanged();
		}
	}

	private everyTriggerEmpty(): boolean {
		return GLOBAL_SHORTCUT_ACTIONS.every((action) => (this.triggers.get(action) ?? null) === null);
	}

	private async runStartupSequence(): Promise<void> {
		await this.runStartupSteps();
		if (!this.recovering || this.retry !== null) return;
		this.recovering = false;
		this.deps.onStatusChanged();
	}

	private async runStartupSteps(): Promise<void> {
		if (!(await this.openSession())) return;
		const consent = this.deps.getConsent();
		if (consent === 'declined') {
			this.setState('declined', null);
			return;
		}
		if (consent === 'unset') {
			this.setState('not-set-up', null);
			return;
		}
		const version = this.version ?? 0;
		if (version < 2) {
			if (GLOBAL_SHORTCUT_ACTIONS.every((action) => this.listedActions.has(action))) {
				this.setState('bound', null);
				return;
			}
			if (this.deps.desktop === 'kde' && this.listedActions.size === 0) {
				this.setState('not-set-up', null);
				return;
			}
		}
		await this.bind();
	}

	private async openSession(): Promise<boolean> {
		this.closePortal();
		this.pendingLoss = null;
		const generation = ++this.generation;
		if (this.state === 'unknown') this.setState('probing', null);
		let result: PortalOpenResult;
		try {
			const portal = this.deps.createPortal((event) => this.handleEvent(generation, event));
			this.portal = portal;
			result = await portal.open();
		} catch (error) {
			if (generation !== this.generation) return false;
			this.pendingLoss = null;
			this.handleOpenFailure(classifyOpenFailure(error));
			return false;
		}
		if (generation !== this.generation) return false;
		const lost = this.pendingLoss;
		this.pendingLoss = null;
		if (lost !== null) {
			this.handleSessionLost(lost);
			return false;
		}
		this.deps.log('Global shortcuts portal opened', {
			version: result.version,
			appIdSource: result.appIdSource,
			uniqueName: result.uniqueName,
		});
		this.opened = true;
		this.version = result.version;
		this.registered = result.appIdSource !== 'unregistered';
		this.configureUnsupported = false;
		const recovering = this.recovering;
		this.resetRecovery();
		this.recovering = recovering;
		this.listedActions.clear();
		for (const binding of result.listed) {
			if (isKnownAction(binding.id)) this.listedActions.add(binding.id);
		}
		if (!this.retainedTriggers || this.listedActions.size > 0) this.replaceTriggers(result.listed);
		this.deps.onStatusChanged();
		return true;
	}

	private handleOpenFailure(failure: OpenFailure): void {
		this.deps.log('Global shortcuts portal open failed', {kind: failure.kind, message: failure.message});
		if (failure.kind === 'identity' || failure.kind === 'unsupported') {
			this.closePortal();
			this.cancelRecovery();
			this.resetRecovery();
			if (failure.kind === 'identity') this.setState('error', 'identity');
			else this.setState('unsupported', null);
			this.deps.onStatusChanged();
			return;
		}
		if (this.retry === 'session') {
			if (this.recovering && this.recoveryAttempt >= PORTAL_RECOVERY_BACKOFF_MS.length) {
				this.recovering = false;
				this.setState('error', 'portal-unavailable');
				this.deps.onStatusChanged();
			}
			this.scheduleRecovery();
			return;
		}
		const now = this.deps.now();
		this.retry = 'startup';
		this.retrySince ??= now;
		const remaining = this.retrySince + PORTAL_NO_PORTAL_RETRY_WINDOW_MS - now;
		const retryable = failure.kind === 'no-portal' || this.deps.getConsent() === 'granted';
		this.recovering = retryable && remaining > 0;
		if (failure.kind === 'no-portal') this.setState('unsupported', null);
		else this.setState('error', failure.kind === 'timeout' ? 'timeout' : 'open-failed');
		this.deps.onStatusChanged();
		if (this.recovering) this.scheduleRecovery(remaining);
	}

	private async bind(): Promise<PortalBindOutcome | null> {
		const portal = this.portal;
		if (portal === null) return null;
		const generation = this.generation;
		this.setState('binding', null);
		this.binding = true;
		let outcome: PortalBindOutcome | null = null;
		let failure: unknown = null;
		try {
			outcome = await portal.bind(this.deps.getDefinitions(), this.deps.getParentWindow());
		} catch (error) {
			failure = error;
		} finally {
			this.binding = false;
		}
		const lost = this.pendingLoss;
		this.pendingLoss = null;
		if (generation !== this.generation) return null;
		if (outcome === null) {
			this.deps.log('Global shortcuts portal bind failed', {message: errorMessage(failure)});
			if (lost !== null) {
				this.handleSessionLost(lost);
				return null;
			}
			this.closePortal();
			this.setState('error', 'bind-failed');
			await this.openSession();
			return null;
		}
		this.applyBindOutcome(outcome);
		if (lost !== null) this.handleSessionLost(lost);
		return outcome;
	}

	private applyBindOutcome(outcome: PortalBindOutcome): void {
		switch (outcome.outcome) {
			case 'bound':
				this.replaceTriggers(outcome.shortcuts);
				this.deps.setConsent('granted');
				this.setState('bound', null);
				return;
			case 'cancelled':
			case 'denied':
				this.deps.setConsent('declined');
				this.setState('declined', null);
				return;
			case 'failed':
				this.deps.log('Global shortcuts portal bind returned a failure response', {code: outcome.code});
				this.setState('error', 'bind-failed');
				return;
		}
	}

	private handleEvent(generation: number, event: PortalEvent): void {
		if (generation !== this.generation) return;
		switch (event.type) {
			case 'activated':
				this.handleActivated(event.id);
				return;
			case 'deactivated':
				if (!isKnownAction(event.id) || !this.activeIds.delete(event.id)) return;
				this.deps.onShortcut(event.id, 'release');
				return;
			case 'shortcuts-changed':
				this.handleShortcutsChanged(event.shortcuts);
				return;
			case 'session-lost':
				if (!this.opened) {
					if (this.portal !== null) this.pendingLoss ??= event.reason;
					return;
				}
				if (this.binding) {
					this.pendingLoss ??= event.reason;
					return;
				}
				this.handleSessionLost(event.reason);
				return;
			case 'portal-available':
				this.handlePortalAvailable();
				return;
		}
	}

	private handleActivated(id: string): void {
		if (!isKnownAction(id)) return;
		const now = this.deps.now();
		const previous = this.lastActivatedAt.get(id);
		this.lastActivatedAt.set(id, now);
		if (this.activeIds.has(id)) {
			if (GLOBAL_SHORTCUT_HOLD_ACTIONS.has(id)) return;
			if (previous !== undefined && now - previous < PORTAL_TOGGLE_REARM_MS) return;
			this.deps.onShortcut(id, 'release');
		} else {
			this.activeIds.add(id);
		}
		this.deps.onShortcut(id, 'press');
	}

	private handleShortcutsChanged(shortcuts: ReadonlyArray<PortalShortcutBinding>): void {
		const previous = new Map(this.triggers);
		if (this.retainedTriggers) this.replaceTriggers(shortcuts);
		else this.applyBindings(shortcuts);
		for (const action of [...this.activeIds]) {
			const trigger = this.triggers.get(action) ?? null;
			if (trigger !== null && trigger === (previous.get(action) ?? null)) continue;
			this.activeIds.delete(action);
			this.deps.onShortcut(action, 'release');
		}
		this.deps.onStatusChanged();
	}

	private handlePortalAvailable(): void {
		if (!this.active || this.opened || this.error === 'identity') return;
		if (this.state === 'unsupported' && this.retrySince === null) return;
		this.deps.log('Global shortcuts portal appeared on the bus');
		this.cancelRecovery();
		void this.enqueue(async () => {
			if (!this.active || this.opened) return;
			await this.runStartupSequence();
		});
	}

	private handleSessionLost(reason: PortalSessionLostReason): void {
		this.deps.log('Global shortcuts portal session lost', {reason});
		this.closePortal();
		this.retainedTriggers = this.triggers.size > 0;
		if (!this.active) return;
		if (reason === 'closed') {
			const now = this.deps.now();
			const recentlyRecovered =
				this.lastClosedRecoveryAt !== null && now - this.lastClosedRecoveryAt < PORTAL_CLOSED_RECOVERY_WINDOW_MS;
			if (recentlyRecovered) {
				this.cancelRecovery();
				this.resetRecovery();
				this.setState('error', 'session-closed');
				this.deps.onStatusChanged();
				return;
			}
			this.lastClosedRecoveryAt = now;
		}
		this.retry = 'session';
		this.recovering = true;
		this.deps.onStatusChanged();
		this.scheduleRecovery();
	}

	private scheduleRecovery(maxDelayMs = Number.POSITIVE_INFINITY): void {
		this.cancelRecovery();
		const index = Math.min(this.recoveryAttempt, PORTAL_RECOVERY_BACKOFF_MS.length - 1);
		const delay = Math.min(PORTAL_RECOVERY_BACKOFF_MS[index] ?? 30000, maxDelayMs);
		this.recoveryAttempt += 1;
		this.recoveryTimer = this.deps.schedule(() => {
			this.recoveryTimer = null;
			void this.enqueue(async () => {
				if (!this.active || this.opened) return;
				await this.runStartupSequence();
			});
		}, delay);
	}

	private cancelRecovery(): void {
		this.recoveryTimer?.cancel();
		this.recoveryTimer = null;
	}

	private resetRecovery(): void {
		this.retry = null;
		this.recovering = false;
		this.recoveryAttempt = 0;
		this.retrySince = null;
	}

	private replaceTriggers(bindings: ReadonlyArray<PortalShortcutBinding>): void {
		this.clearTriggers();
		this.applyBindings(bindings);
	}

	private clearTriggers(): void {
		this.triggers.clear();
		this.retainedTriggers = false;
	}

	private applyBindings(bindings: ReadonlyArray<PortalShortcutBinding>): void {
		for (const binding of bindings) {
			if (!isKnownAction(binding.id)) continue;
			this.triggers.set(binding.id, normalizeTrigger(binding.triggerDescription));
		}
	}

	private closePortal(): void {
		this.generation += 1;
		this.opened = false;
		this.releaseAll();
		const portal = this.portal;
		this.portal = null;
		if (portal === null) return;
		try {
			portal.close();
		} catch (error) {
			this.deps.log('Global shortcuts portal close threw', {message: errorMessage(error)});
		}
	}

	private setState(state: GlobalShortcutsPortalState, error: string | null): void {
		if (this.state === state && this.error === error) return;
		if (this.retainedTriggers && state !== 'bound' && state !== 'binding') this.clearTriggers();
		this.state = state;
		this.error = error;
		this.deps.onStatusChanged();
	}

	private enqueue(task: () => Promise<void>): Promise<void> {
		const run = this.queue.then(task);
		this.queue = run.catch((error: unknown) => {
			this.deps.log('Global shortcuts portal task failed', {message: errorMessage(error)});
		});
		return this.queue.then(() => undefined);
	}
}
