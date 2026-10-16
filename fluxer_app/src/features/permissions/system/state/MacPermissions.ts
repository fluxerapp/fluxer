// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	checkNativePermission,
	MAC_PERMISSION_KINDS,
	type NativePermissionResult,
	openNativePermissionSettings,
	type PermissionKind,
	requestNativePermission,
	settledPermissionKinds,
} from '@app/features/permissions/system/utils/NativePermissions';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {getNativePlatformSync, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {makeAutoObservable, runInAction} from 'mobx';

export type MacPermissionKind = PermissionKind;

const logger = new Logger('MacPermissions');

const DEFAULT_STATUSES: Record<MacPermissionKind, NativePermissionResult> = {
	microphone: 'not-determined',
	camera: 'not-determined',
	screen: 'not-determined',
	'input-monitoring': 'not-determined',
};

const KINDS_WITH_MACOS_QUIT_PROMPT: ReadonlySet<MacPermissionKind> = new Set(['screen', 'input-monitoring']);

const reapplyGlobalShortcuts = async (): Promise<void> => {
	const module = await import('@app/features/app/keybindings/KeybindManager');
	await module.default.reapplyGlobalShortcuts();
};

class MacPermissions {
	statuses: Record<MacPermissionKind, NativePermissionResult> = {...DEFAULT_STATUSES};
	isHydrated = false;
	private changeRequested: Record<MacPermissionKind, boolean> = {
		microphone: false,
		camera: false,
		screen: false,
		'input-monitoring': false,
	};
	screenStillBlockedAfterReturn = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initialize());
	}

	private async initialize(): Promise<void> {
		await this.refreshAll();
		runInAction(() => {
			this.isHydrated = true;
		});
		if (this.isNativeMacDesktop) {
			window.addEventListener('focus', () => void this.refreshAfterReturn());
		}
	}

	get isNativeMacDesktop(): boolean {
		return isDesktop() && getNativePlatformSync() === 'macos';
	}

	get allGranted(): boolean {
		return MAC_PERMISSION_KINDS.every((kind) => this.statuses[kind] === 'granted');
	}

	get settledKinds(): Array<MacPermissionKind> {
		return settledPermissionKinds(this.statuses);
	}

	showsQuitPromptAdvice(kind: MacPermissionKind): boolean {
		return KINDS_WITH_MACOS_QUIT_PROMPT.has(kind) && this.changeRequested[kind] && this.statuses[kind] !== 'granted';
	}

	get anyQuitPromptAdvice(): boolean {
		return MAC_PERMISSION_KINDS.some((kind) => this.showsQuitPromptAdvice(kind));
	}

	private recordStatus(kind: MacPermissionKind, status: NativePermissionResult): void {
		const previous = this.statuses[kind];
		this.statuses[kind] = status;
		if (status !== 'granted') return;
		this.changeRequested[kind] = false;
		if (kind === 'screen') this.screenStillBlockedAfterReturn = false;
		if (kind === 'input-monitoring' && previous !== 'granted' && this.isHydrated) {
			reapplyGlobalShortcuts().catch((error) => {
				logger.warn('Failed to reapply global shortcuts after an Input Monitoring grant', error);
			});
		}
	}

	async refreshAll(): Promise<void> {
		if (!this.isNativeMacDesktop) {
			runInAction(() => {
				for (const kind of MAC_PERMISSION_KINDS) {
					this.statuses[kind] = 'unsupported';
				}
			});
			return;
		}
		try {
			const entries = await Promise.all(
				MAC_PERMISSION_KINDS.map(async (kind) => [kind, await checkNativePermission(kind)] as const),
			);
			runInAction(() => {
				for (const [kind, status] of entries) {
					this.recordStatus(kind, status);
				}
			});
		} catch (error) {
			logger.warn('Failed to refresh macOS permissions', error);
		}
	}

	async refreshKind(kind: MacPermissionKind): Promise<NativePermissionResult> {
		if (!this.isNativeMacDesktop) return 'unsupported';
		const status = await checkNativePermission(kind);
		runInAction(() => {
			this.recordStatus(kind, status);
		});
		return status;
	}

	async refreshAfterReturn(): Promise<void> {
		if (this.allGranted) return;
		await this.refreshAll();
		runInAction(() => {
			this.screenStillBlockedAfterReturn = this.changeRequested.screen && this.statuses.screen !== 'granted';
		});
	}

	applyPermissionResult(kind: MacPermissionKind, status: NativePermissionResult): void {
		this.recordStatus(kind, status);
	}

	async request(kind: MacPermissionKind): Promise<void> {
		const status = await requestNativePermission(kind);
		runInAction(() => {
			this.changeRequested[kind] = true;
			this.recordStatus(kind, status);
		});
	}

	async openSettings(kind: MacPermissionKind): Promise<void> {
		await openNativePermissionSettings(kind);
		runInAction(() => {
			if (this.statuses[kind] !== 'granted') this.changeRequested[kind] = true;
		});
	}
}

export default new MacPermissions();
