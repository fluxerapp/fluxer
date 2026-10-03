// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GLOBAL_SHORTCUT_ACTIONS,
	GLOBAL_SHORTCUT_DEFAULT_DESCRIPTIONS,
	GLOBAL_SHORTCUT_DESCRIPTION_MAX_LENGTH,
	type GlobalShortcutAction,
	isGlobalShortcutAction,
} from '@electron/common/GlobalShortcutActions';
import type {
	GlobalShortcutCaptureEvent,
	GlobalShortcutCombo,
	GlobalShortcutEvent,
	GlobalShortcutsBackend,
	GlobalShortcutsStatus,
	LinuxDesktopKind,
	LinuxSessionType,
} from '@electron/common/Types';
import {comboToXdgTrigger} from '@electron/main/GlobalShortcutKeys';
import {
	bindingFromCombo,
	GlobalShortcutMatcher,
	type HookKeyEvent,
	type HookMouseEvent,
	type ShortcutBinding,
	type ShortcutTransition,
} from '@electron/main/GlobalShortcutMatcher';
import type {LinuxPortalShortcutsManager, PortalShortcutDefinition} from '@electron/main/LinuxGlobalShortcutsPortal';

export type HookBackendKind = 'x11' | 'evdev' | 'windows' | 'macos';
export type HookStartResult = 'ok' | 'permission' | 'start-failed';
export type InputHookMode = 'auto' | 'off' | 'evdev' | 'native';

export interface NativeKeyIdentity {
	keycode: number;
	keyName: string;
}

export type HookEvent = (HookKeyEvent & {native: NativeKeyIdentity}) | (HookMouseEvent & {native: null});

export interface HookBackend {
	readonly kind: HookBackendKind;
	start(onEvent: (event: HookEvent) => void): Promise<HookStartResult>;
	stop(): void;
}

export interface StoredGlobalShortcutAction {
	action: GlobalShortcutAction;
	description: string;
	preferredTrigger: string | null;
}

export interface GlobalShortcutsSettingsStore {
	getDirectInputEnabled(): boolean;
	setDirectInputEnabled(enabled: boolean): void;
	isMigrated(): boolean;
	setMigrated(): void;
	getLastActions(): Array<StoredGlobalShortcutAction>;
	setLastActions(actions: Array<StoredGlobalShortcutAction>): void;
}

export interface GlobalShortcutsLinuxEnvironment {
	session: LinuxSessionType;
	sandboxed: boolean;
	desktop: LinuxDesktopKind;
	inputHookMode: InputHookMode;
}

export interface GlobalShortcutsConsumer {
	wantsHooks(): boolean;
	handleHookEvent(event: HookEvent, backend: HookBackendKind): void;
	releaseHookPresses(): void;
}

export interface GlobalShortcutsTimer {
	cancel(): void;
}

export interface InputDeviceWatch {
	close(): void;
}

export interface GlobalShortcutsEngineDeps {
	platform: 'linux' | 'windows' | 'macos';
	linux: GlobalShortcutsLinuxEnvironment | null;
	settings: GlobalShortcutsSettingsStore;
	portal: LinuxPortalShortcutsManager | null;
	createHookBackend: (kind: HookBackendKind) => HookBackend;
	isDirectInputAvailable: () => boolean;
	watchInputDevices: (onChange: () => void) => InputDeviceWatch | null;
	schedule: (callback: () => void, delayMs: number) => GlobalShortcutsTimer;
	log: (message: string, details?: Record<string, unknown>) => void;
}

export interface SanitizedSyncBinding {
	sourceId: string;
	action: string;
	combo: GlobalShortcutCombo;
}

export interface SanitizedSyncAction {
	action: GlobalShortcutAction;
	description: string;
	preferredCombo: GlobalShortcutCombo | null;
}

export interface SanitizedSyncPayload {
	bindings: Array<SanitizedSyncBinding>;
	actions: Array<SanitizedSyncAction>;
}

const XDG_TRIGGER_PATTERN = /^(?:(?:CTRL|ALT|SHIFT|LOGO)\+)*[A-Za-z0-9_]+$/;
const MAX_SYNC_BINDINGS = 256;
const MAX_SOURCE_ID_LENGTH = 200;
const MAX_ACTION_LENGTH = 100;
const MAX_KEY_LENGTH = 64;
const MAX_MOUSE_BUTTON = 31;
const INPUT_DEVICE_REPROBE_DELAY_MS = 500;
export const GLOBAL_SHORTCUT_CAPTURE_TIMEOUT_MS = 60000;

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function sanitizeBoundedString(value: unknown, maxLength: number): string | null {
	if (typeof value !== 'string') return null;
	if (value.length === 0 || value.length > maxLength) return null;
	return value;
}

function sanitizeGlobalShortcutCombo(value: unknown): GlobalShortcutCombo | null {
	if (!isRecord(value)) return null;
	if (typeof value.key !== 'string' || value.key.length > MAX_KEY_LENGTH) return null;
	const combo: GlobalShortcutCombo = {
		key: value.key,
		ctrl: value.ctrl === true,
		alt: value.alt === true,
		shift: value.shift === true,
		meta: value.meta === true,
	};
	if (value.code !== undefined) {
		const code = sanitizeBoundedString(value.code, MAX_KEY_LENGTH);
		if (code === null) return null;
		combo.code = code;
	}
	if (value.mouseButton !== undefined) {
		const button = value.mouseButton;
		if (typeof button !== 'number' || !Number.isInteger(button) || button < 0 || button > MAX_MOUSE_BUTTON) {
			return null;
		}
		combo.mouseButton = button;
	}
	if (value.modifierOnly === true) combo.modifierOnly = true;
	if (value.bothSides === true) combo.bothSides = true;
	return combo;
}

export function sanitizeGlobalShortcutsSyncPayload(value: unknown): SanitizedSyncPayload | null {
	if (!isRecord(value) || !Array.isArray(value.bindings) || !Array.isArray(value.actions)) return null;
	if (value.bindings.length > MAX_SYNC_BINDINGS) return null;
	const bindings: Array<SanitizedSyncBinding> = [];
	for (const entry of value.bindings) {
		if (!isRecord(entry)) return null;
		const sourceId = sanitizeBoundedString(entry.sourceId, MAX_SOURCE_ID_LENGTH);
		const action = sanitizeBoundedString(entry.action, MAX_ACTION_LENGTH);
		const combo = sanitizeGlobalShortcutCombo(entry.combo);
		if (sourceId === null || action === null || combo === null) return null;
		bindings.push({sourceId, action, combo});
	}
	const actions: Array<SanitizedSyncAction> = [];
	const seenActions = new Set<string>();
	for (const entry of value.actions) {
		if (!isRecord(entry) || !isGlobalShortcutAction(entry.action) || seenActions.has(entry.action)) continue;
		const description = typeof entry.description === 'string' ? entry.description.trim() : '';
		if (description.length === 0 || description.length > GLOBAL_SHORTCUT_DESCRIPTION_MAX_LENGTH) continue;
		const preferredCombo = entry.preferredCombo === null ? null : sanitizeGlobalShortcutCombo(entry.preferredCombo);
		seenActions.add(entry.action);
		actions.push({action: entry.action, description, preferredCombo});
	}
	return {bindings, actions};
}

export function storedActionsFromSync(actions: ReadonlyArray<SanitizedSyncAction>): Array<StoredGlobalShortcutAction> {
	return actions.map((entry) => ({
		action: entry.action,
		description: entry.description,
		preferredTrigger: entry.preferredCombo === null ? null : comboToXdgTrigger(entry.preferredCombo),
	}));
}

export function portalDefinitionsFromStored(
	stored: ReadonlyArray<StoredGlobalShortcutAction>,
): Array<PortalShortcutDefinition> {
	const byAction = new Map(stored.map((entry) => [entry.action, entry]));
	return GLOBAL_SHORTCUT_ACTIONS.map((action) => {
		const entry = byAction.get(action);
		const description = entry?.description.trim() || GLOBAL_SHORTCUT_DEFAULT_DESCRIPTIONS[action];
		const preferredTrigger = entry?.preferredTrigger ?? null;
		return preferredTrigger !== null && XDG_TRIGGER_PATTERN.test(preferredTrigger)
			? {id: action, description, preferredTrigger}
			: {id: action, description};
	});
}

function portalSourceId(action: GlobalShortcutAction): string {
	return `portal:${action}`;
}

function isHookBackend(backend: GlobalShortcutsBackend): backend is HookBackendKind {
	return backend !== 'portal' && backend !== 'none';
}

function captureEventFromHook(event: HookEvent): GlobalShortcutCaptureEvent {
	const modifiers = {ctrl: event.ctrlKey, alt: event.altKey, shift: event.shiftKey, meta: event.metaKey};
	if (event.native === null) return {type: event.type, code: null, key: null, button: event.button, ...modifiers};
	return {type: event.type, code: event.code, key: event.key, button: null, ...modifiers};
}

interface SyncClient {
	send: (channel: string, payload: unknown) => void;
	matcher: GlobalShortcutMatcher;
	paused: boolean;
	portalPressed: Set<string>;
}

interface CaptureSession {
	clientId: number;
	captureId: number;
	timer: GlobalShortcutsTimer;
}

export class GlobalShortcutsEngine {
	private readonly clients = new Map<number, SyncClient>();
	private readonly consumers = new Set<GlobalShortcutsConsumer>();
	private hook: HookBackend | null = null;
	private hookGeneration = 0;
	private hookError: 'permission' | 'start-failed' | null = null;
	private hookQueue: Promise<void> = Promise.resolve();
	private directInputAvailable = false;
	private inputDeviceWatch: InputDeviceWatch | null = null;
	private inputDeviceReprobe: GlobalShortcutsTimer | null = null;
	private portalRequested = false;
	private capture: CaptureSession | null = null;
	private captureSeq = 0;
	private lastStatusJson: string | null = null;
	private disposed = false;

	constructor(private readonly deps: GlobalShortcutsEngineDeps) {
		this.directInputAvailable = deps.linux !== null && deps.isDirectInputAvailable();
	}

	start(): void {
		this.syncPortalActivity();
		this.syncInputDeviceWatch();
	}

	dispose(): void {
		if (this.disposed) return;
		this.disposed = true;
		this.capture?.timer.cancel();
		this.capture = null;
		this.deps.portal?.deactivate();
		this.syncInputDeviceWatch();
		this.stopHook();
		this.clients.clear();
		this.consumers.clear();
	}

	addConsumer(consumer: GlobalShortcutsConsumer): void {
		this.consumers.add(consumer);
	}

	attachClient(id: number, send: (channel: string, payload: unknown) => void): void {
		if (this.clients.has(id)) return;
		this.clients.set(id, {send, matcher: new GlobalShortcutMatcher(), paused: false, portalPressed: new Set()});
		this.portalRequested = true;
		this.syncPortalActivity();
	}

	hasClient(id: number): boolean {
		return this.clients.has(id);
	}

	detachClient(id: number): void {
		if (!this.clients.delete(id)) return;
		if (this.capture?.clientId === id) {
			this.capture.timer.cancel();
			this.capture = null;
		}
		if (this.clients.size === 0) {
			this.portalRequested = false;
			this.syncPortalActivity();
		}
		void this.refreshHooks();
	}

	sync(id: number, payload: SanitizedSyncPayload): void {
		const client = this.clients.get(id);
		if (!client) return;
		const bindings: Array<ShortcutBinding> = [];
		for (const entry of payload.bindings) {
			const binding = bindingFromCombo(entry.sourceId, entry.action, entry.combo);
			if (binding !== null) bindings.push(binding);
		}
		this.emitTransitions(client, client.matcher.setBindings(bindings));
		if (payload.actions.length > 0) this.storeActions(storedActionsFromSync(payload.actions));
		if (bindings.length > 0) this.migrateDirectInput();
		if (!this.directInputAvailable) this.reprobeDirectInput();
		void this.refreshHooks();
	}

	setPaused(id: number, paused: boolean): void {
		const client = this.clients.get(id);
		if (client) client.paused = paused;
	}

	refreshDirectInputAvailability(): void {
		if (this.deps.linux === null) return;
		this.directInputAvailable = this.deps.isDirectInputAvailable();
		this.syncPortalActivity();
		this.syncInputDeviceWatch();
		void this.refreshHooks();
		this.notifyStatus();
	}

	async setUp(): Promise<void> {
		const portal = this.deps.portal;
		if (portal === null || !this.portalApplicable()) return;
		if (portal.getState() === 'unsupported') {
			await portal.recheck();
			return;
		}
		if (this.getBackendSelection() !== 'portal') return;
		await portal.setUp();
	}

	async recheck(): Promise<void> {
		const portal = this.deps.portal;
		if (portal === null || !this.portalApplicable()) return;
		await portal.recheck();
	}

	async startCapture(id: number): Promise<number | null> {
		if (!this.clients.has(id) || !isHookBackend(this.getBackendSelection())) return null;
		this.capture?.timer.cancel();
		const captureId = ++this.captureSeq;
		const timer = this.deps.schedule(() => this.stopCapture(id, captureId), GLOBAL_SHORTCUT_CAPTURE_TIMEOUT_MS);
		const session: CaptureSession = {clientId: id, captureId, timer};
		this.capture = session;
		await this.refreshHooks();
		if (this.capture !== session) return null;
		if (this.hook !== null) return captureId;
		this.stopCapture(id, captureId);
		return null;
	}

	stopCapture(id: number, captureId: number): void {
		if (this.capture?.clientId !== id || this.capture.captureId !== captureId) return;
		this.capture.timer.cancel();
		this.capture = null;
		void this.refreshHooks();
	}

	async configure(): Promise<void> {
		const portal = this.deps.portal;
		if (portal === null || this.getBackendSelection() !== 'portal') return;
		await portal.configure();
	}

	setDirectInputEnabled(enabled: boolean): void {
		if (this.deps.linux === null) return;
		this.deps.settings.setDirectInputEnabled(enabled);
		this.refreshDirectInputAvailability();
	}

	isDirectInputActive(): boolean {
		const linux = this.deps.linux;
		if (linux === null || linux.inputHookMode === 'off' || linux.inputHookMode === 'native') return false;
		if (!this.directInputAvailable) return false;
		return linux.inputHookMode === 'evdev' || this.deps.settings.getDirectInputEnabled();
	}

	hooksActive(): boolean {
		return this.hook !== null;
	}

	getBackendSelection(): GlobalShortcutsBackend {
		const linux = this.deps.linux;
		if (linux === null) return this.deps.platform === 'windows' ? 'windows' : 'macos';
		if (linux.inputHookMode === 'off') return 'none';
		if (this.isDirectInputActive()) return 'evdev';
		if (linux.session === 'wayland') {
			const portal = this.deps.portal;
			if (linux.inputHookMode === 'native' || portal === null) return 'none';
			return portal.getState() === 'unsupported' ? 'none' : 'portal';
		}
		return 'x11';
	}

	getStatus(): GlobalShortcutsStatus {
		const backend = this.getBackendSelection();
		const linux = this.deps.linux;
		const hookBackend = isHookBackend(backend);
		return {
			backend,
			platform: this.deps.platform,
			linux:
				linux === null
					? null
					: {
							session: linux.session,
							sandbox: linux.sandboxed ? 'flatpak' : 'none',
							desktop: linux.desktop,
							portal: linux.session === 'wayland' && this.deps.portal !== null ? this.deps.portal.getStatus() : null,
							directInput: {
								available: this.directInputAvailable,
								enabled: this.deps.settings.getDirectInputEnabled(),
								locked: linux.inputHookMode !== 'auto',
							},
						},
			hooksActive: this.hook !== null,
			hookError: this.hookError,
			supportsMouseButtons: hookBackend,
			supportsModifierOnly: hookBackend,
		};
	}

	notifyStatus(): void {
		if (this.disposed) return;
		const status = this.getStatus();
		const json = JSON.stringify(status);
		if (json === this.lastStatusJson) return;
		this.lastStatusJson = json;
		for (const client of this.clients.values()) client.send('global-shortcuts:status', status);
	}

	handlePortalShortcut(action: GlobalShortcutAction, phase: 'press' | 'release'): void {
		const sourceId = portalSourceId(action);
		const event: GlobalShortcutEvent = {action, sourceId, phase};
		for (const client of this.clients.values()) {
			if (phase === 'press') {
				if (client.paused) continue;
				client.portalPressed.add(sourceId);
			} else if (!client.portalPressed.delete(sourceId)) {
				continue;
			}
			client.send('global-shortcut-event', event);
		}
	}

	releaseAllPressed(): void {
		this.releaseHookSources();
		this.deps.portal?.releaseAll();
	}

	refreshHooks(): Promise<void> {
		const run = this.hookQueue.then(() => this.reconcileHooks());
		this.hookQueue = run.catch((error: unknown) => {
			this.deps.log('Global shortcut hook reconcile failed', {
				message: error instanceof Error ? error.message : String(error),
			});
		});
		return this.hookQueue;
	}

	private desiredHookKind(): HookBackendKind | null {
		if (this.disposed) return null;
		const backend = this.getBackendSelection();
		return isHookBackend(backend) && this.hooksWanted() ? backend : null;
	}

	private hooksWanted(): boolean {
		return (
			this.capture !== null ||
			[...this.clients.values()].some((client) => client.matcher.hasBindings()) ||
			[...this.consumers].some((consumer) => consumer.wantsHooks())
		);
	}

	private reprobeDirectInput(): void {
		const linux = this.deps.linux;
		if (linux === null || linux.inputHookMode === 'off' || linux.inputHookMode === 'native') return;
		if (linux.inputHookMode !== 'evdev' && !this.deps.settings.getDirectInputEnabled()) return;
		const available = this.deps.isDirectInputAvailable();
		if (available === this.directInputAvailable) return;
		this.deps.log('Direct input availability changed', {available});
		this.directInputAvailable = available;
		this.syncPortalActivity();
		this.syncInputDeviceWatch();
		this.notifyStatus();
	}

	private async reconcileHooks(): Promise<void> {
		if (this.hook === null && !this.disposed && this.hooksWanted()) this.reprobeDirectInput();
		const desired = this.desiredHookKind();
		if (this.hook !== null && this.hook.kind !== desired) this.stopHook();
		if (desired === null) {
			this.hookError = null;
			this.notifyStatus();
			return;
		}
		if (this.hook !== null) return;
		const backend = this.deps.createHookBackend(desired);
		const generation = ++this.hookGeneration;
		let result: HookStartResult;
		try {
			result = await backend.start((event) => {
				if (generation === this.hookGeneration) this.dispatchHookEvent(event, backend.kind);
			});
		} catch (error) {
			this.deps.log('Global shortcut hook start threw', {
				backend: desired,
				message: error instanceof Error ? error.message : String(error),
			});
			result = 'start-failed';
		}
		if (result !== 'ok' || generation !== this.hookGeneration || this.desiredHookKind() !== desired) {
			if (result === 'ok') {
				this.stopBackendQuietly(backend);
			} else {
				this.deps.log('Global shortcut hook did not start', {backend: desired, result});
			}
			if (generation === this.hookGeneration) {
				this.hookGeneration += 1;
				this.releaseHookSources();
			}
			this.hookError = result === 'ok' ? null : result;
			this.notifyStatus();
			return;
		}
		this.hook = backend;
		this.hookError = null;
		this.notifyStatus();
	}

	private stopHook(): void {
		const hook = this.hook;
		this.hook = null;
		this.hookGeneration += 1;
		if (hook !== null) this.stopBackendQuietly(hook);
		this.releaseHookSources();
	}

	private releaseHookSources(): void {
		for (const client of this.clients.values()) this.emitTransitions(client, client.matcher.releaseAll());
		for (const consumer of this.consumers) consumer.releaseHookPresses();
	}

	private stopBackendQuietly(backend: HookBackend): void {
		try {
			backend.stop();
		} catch (error) {
			this.deps.log('Global shortcut hook stop threw', {
				backend: backend.kind,
				message: error instanceof Error ? error.message : String(error),
			});
		}
	}

	private dispatchHookEvent(event: HookEvent, backend: HookBackendKind): void {
		for (const client of this.clients.values()) {
			const transitions =
				event.native === null
					? client.matcher.handleMouse(event, !client.paused)
					: client.matcher.handleKey(event, !client.paused);
			this.emitTransitions(client, transitions);
		}
		for (const consumer of this.consumers) consumer.handleHookEvent(event, backend);
		const capture = this.capture;
		if (capture !== null)
			this.clients.get(capture.clientId)?.send('global-shortcuts:capture', captureEventFromHook(event));
	}

	private emitTransitions(client: SyncClient, transitions: ReadonlyArray<ShortcutTransition>): void {
		for (const transition of transitions) {
			const event: GlobalShortcutEvent = {
				action: transition.action,
				sourceId: transition.sourceId,
				phase: transition.phase,
			};
			client.send('global-shortcut-event', event);
		}
	}

	private storeActions(actions: Array<StoredGlobalShortcutAction>): void {
		const previous = this.deps.settings.getLastActions();
		if (JSON.stringify(previous) === JSON.stringify(actions)) return;
		this.deps.settings.setLastActions(actions);
	}

	migrateLegacyDirectInput(): void {
		if (this.deps.linux === null || this.deps.settings.isMigrated() || !this.deps.isDirectInputAvailable()) return;
		this.migrateDirectInput();
	}

	private migrateDirectInput(): void {
		if (this.deps.linux === null || this.deps.settings.isMigrated()) return;
		this.directInputAvailable = this.deps.isDirectInputAvailable();
		if (this.directInputAvailable) {
			this.deps.log('Enabling direct input access for an existing global shortcut setup');
			this.deps.settings.setDirectInputEnabled(true);
		}
		this.deps.settings.setMigrated();
		this.syncPortalActivity();
		this.syncInputDeviceWatch();
		void this.refreshHooks();
		this.notifyStatus();
	}

	private wantsInputDeviceWatch(): boolean {
		const linux = this.deps.linux;
		if (linux === null || this.disposed || this.directInputAvailable) return false;
		if (linux.inputHookMode === 'off' || linux.inputHookMode === 'native') return false;
		return linux.inputHookMode === 'evdev' || this.deps.settings.getDirectInputEnabled();
	}

	private syncInputDeviceWatch(): void {
		if (!this.wantsInputDeviceWatch()) {
			this.inputDeviceReprobe?.cancel();
			this.inputDeviceReprobe = null;
			this.inputDeviceWatch?.close();
			this.inputDeviceWatch = null;
			return;
		}
		if (this.inputDeviceWatch !== null) return;
		this.inputDeviceWatch = this.deps.watchInputDevices(() => this.scheduleInputDeviceReprobe());
	}

	private scheduleInputDeviceReprobe(): void {
		if (this.inputDeviceWatch === null) return;
		this.inputDeviceReprobe?.cancel();
		this.inputDeviceReprobe = this.deps.schedule(() => {
			this.inputDeviceReprobe = null;
			this.refreshDirectInputAvailability();
		}, INPUT_DEVICE_REPROBE_DELAY_MS);
	}

	private portalApplicable(): boolean {
		const linux = this.deps.linux;
		if (linux === null || this.deps.portal === null || this.disposed) return false;
		return (
			linux.session === 'wayland' &&
			linux.inputHookMode !== 'off' &&
			linux.inputHookMode !== 'native' &&
			!this.isDirectInputActive()
		);
	}

	syncPortalActivity(): void {
		const portal = this.deps.portal;
		if (portal === null) return;
		if (this.portalRequested && this.portalApplicable()) {
			void portal.activate();
			void portal.probe();
		} else {
			portal.deactivate();
		}
	}
}
