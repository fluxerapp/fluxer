// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {
	GlobalShortcutsApi,
	GlobalShortcutsBackend,
	GlobalShortcutsLinuxStatus,
	GlobalShortcutsPortalState,
	GlobalShortcutsPortalStatus,
	GlobalShortcutsStatus,
} from '@app/types/electron.d';
import {makeAutoObservable, runInAction} from 'mobx';

const KNOWN_BACKENDS: ReadonlySet<GlobalShortcutsBackend> = new Set([
	'portal',
	'x11',
	'evdev',
	'windows',
	'macos',
	'none',
]);
const KNOWN_PORTAL_STATES: ReadonlySet<GlobalShortcutsPortalState> = new Set([
	'unknown',
	'probing',
	'unsupported',
	'not-set-up',
	'binding',
	'bound',
	'declined',
	'error',
]);

type GlobalShortcutsPendingAction = 'set-up' | 'configure' | 'direct-input' | 'recheck';

const PORTAL_ASSIGNED_ACTIONS_KEY = 'GlobalShortcuts:portalAssignedActions:v1';

const logger = new Logger('GlobalShortcuts');

function isTransientPortal(portal: GlobalShortcutsPortalStatus): boolean {
	return portal.recovering === true || portal.state === 'probing' || portal.state === 'binding';
}

function settledPortalAssignment(status: GlobalShortcutsStatus): ReadonlyArray<string> | null {
	if (status.platform !== 'linux' || status.linux === null) return [];
	const portal = status.linux.portal;
	if (portal === null) return [];
	if (portal.recovering === true) return null;
	if (status.backend !== 'portal' && !(status.backend === 'none' && portal.state === 'unsupported')) return [];
	switch (portal.state) {
		case 'bound':
			return portal.shortcuts
				.filter((entry) => status.linux?.desktop === 'hyprland' || entry.triggerDescription !== null)
				.map((entry) => entry.action);
		case 'declined':
		case 'unsupported':
			return [];
		default:
			return null;
	}
}

function readStoredPortalAssignment(): ReadonlyArray<string> {
	try {
		const stored = AppStorage.getJSON<unknown>(PORTAL_ASSIGNED_ACTIONS_KEY);
		if (!Array.isArray(stored)) return [];
		return stored.filter((action): action is string => typeof action === 'string');
	} catch (error) {
		logger.warn('Failed to read the stored system-wide shortcut assignment', error);
		return [];
	}
}

function storePortalAssignment(actions: ReadonlyArray<string>): void {
	try {
		AppStorage.setJSON(PORTAL_ASSIGNED_ACTIONS_KEY, actions);
	} catch (error) {
		logger.warn('Failed to store the system-wide shortcut assignment', error);
	}
}

function sameActions(a: ReadonlyArray<string>, b: ReadonlyArray<string>): boolean {
	return a.length === b.length && a.every((action) => b.includes(action));
}

function findPortalTrigger(portal: GlobalShortcutsPortalStatus, action: string): string | null {
	return portal.shortcuts.find((entry) => entry.action === action)?.triggerDescription ?? null;
}

export function getGlobalShortcutsApi(): GlobalShortcutsApi | null {
	const api = getElectronAPI()?.globalShortcuts;
	if (!api || typeof api.sync !== 'function') return null;
	return api;
}

class GlobalShortcuts {
	status: GlobalShortcutsStatus | null = null;
	legacyWaylandNeedsUpdate = false;
	pendingAction: GlobalShortcutsPendingAction | null = null;
	private settledPortal: GlobalShortcutsPortalStatus | null = null;
	private portalAssignedActions: ReadonlyArray<string> = [];
	private attached = false;
	private statusRevision = 0;
	private statusUnsubscribe: (() => void) | null = null;

	constructor() {
		makeAutoObservable<this, 'attached' | 'statusRevision' | 'statusUnsubscribe'>(
			this,
			{attached: false, statusRevision: false, statusUnsubscribe: false},
			{autoBind: true},
		);
	}

	attach(): void {
		if (this.attached) return;
		this.attached = true;
		const api = getGlobalShortcutsApi();
		if (!api) {
			void this.probeLegacyDesktop();
			return;
		}
		this.portalAssignedActions = readStoredPortalAssignment();
		this.statusUnsubscribe = api.onStatus((status) => {
			this.statusRevision += 1;
			this.applyStatus(status);
		});
		const revision = this.statusRevision;
		void api.getStatus().then(
			(status) => {
				if (!this.attached || this.statusRevision !== revision) return;
				this.applyStatus(status);
			},
			(error) => {
				logger.warn('Failed to read global shortcuts status', error);
			},
		);
	}

	detach(): void {
		if (!this.attached) return;
		this.attached = false;
		this.statusRevision += 1;
		this.statusUnsubscribe?.();
		this.statusUnsubscribe = null;
	}

	private applyStatus(status: GlobalShortcutsStatus): void {
		runInAction(() => {
			this.status = status;
			const portal = status.platform === 'linux' ? status.linux?.portal : null;
			if (!portal || !KNOWN_PORTAL_STATES.has(portal.state) || portal.state === 'unknown') {
				this.settledPortal = null;
			} else if (!isTransientPortal(portal) || (this.settledPortal === null && portal.state === 'bound')) {
				this.settledPortal = portal;
			}
			const assigned = settledPortalAssignment(status);
			if (assigned === null || sameActions(assigned, this.portalAssignedActions)) return;
			this.portalAssignedActions = assigned;
			storePortalAssignment(assigned);
		});
	}

	private async probeLegacyDesktop(): Promise<void> {
		const electronApi = getElectronAPI();
		if (!electronApi) return;
		try {
			const desktopInfo = await electronApi.getDesktopInfo();
			if (!desktopInfo.waylandSession) return;
			const evdevStatus = await electronApi.linuxEvdevStatus?.();
			const needsUpdate = evdevStatus?.hasAccess !== true;
			runInAction(() => {
				this.legacyWaylandNeedsUpdate = needsUpdate;
			});
		} catch (error) {
			logger.warn('Failed to read legacy desktop input status', error);
		}
	}

	get backend(): GlobalShortcutsBackend | null {
		const backend = this.status?.backend;
		if (backend === undefined || !KNOWN_BACKENDS.has(backend)) return null;
		return backend;
	}

	get hooksActive(): boolean {
		return this.status?.hooksActive === true;
	}

	get hookError(): 'permission' | 'start-failed' | null {
		return this.status?.hookError ?? null;
	}

	get linux(): GlobalShortcutsLinuxStatus | null {
		if (this.status?.platform !== 'linux') return null;
		return this.status.linux;
	}

	private get livePortal(): GlobalShortcutsPortalStatus | null {
		const portal = this.linux?.portal;
		if (!portal || !KNOWN_PORTAL_STATES.has(portal.state)) return null;
		return portal;
	}

	get portal(): GlobalShortcutsPortalStatus | null {
		const portal = this.livePortal;
		if (portal === null) return null;
		if ((portal.recovering === true || portal.state === 'probing') && this.settledPortal !== null) {
			return this.settledPortal;
		}
		return portal;
	}

	get portalRecovering(): boolean {
		return this.livePortal?.recovering === true;
	}

	get isPortalBackend(): boolean {
		return this.backend === 'portal' && this.portal !== null;
	}

	getPortalTrigger(action: string): string | null {
		const portal = this.portal;
		if (!this.isPortalBackend || portal?.state !== 'bound') return null;
		return findPortalTrigger(portal, action);
	}

	isPortalActionAssigned(action: string): boolean {
		return this.portalAssignedActions.includes(action);
	}

	async setUp(): Promise<void> {
		await this.runAction('set-up', (api) => api.setUp());
	}

	async configure(): Promise<void> {
		await this.runAction('configure', async (api) => {
			await api.configure();
			return null;
		});
	}

	async setDirectInputEnabled(enabled: boolean): Promise<void> {
		await this.runAction('direct-input', (api) => api.setDirectInputEnabled(enabled));
	}

	async recheck(): Promise<void> {
		await this.runAction('recheck', (api) => api.recheck());
	}

	private async runAction(
		action: GlobalShortcutsPendingAction,
		run: (api: GlobalShortcutsApi) => Promise<GlobalShortcutsStatus | null>,
	): Promise<void> {
		const api = getGlobalShortcutsApi();
		if (!api || this.pendingAction !== null) return;
		runInAction(() => {
			this.pendingAction = action;
		});
		try {
			const status = await run(api);
			if (status && this.attached) {
				this.statusRevision += 1;
				this.applyStatus(status);
			}
		} catch (error) {
			logger.error(`Global shortcuts action ${action} failed`, error);
		} finally {
			runInAction(() => {
				this.pendingAction = null;
			});
		}
	}
}

export default new GlobalShortcuts();
