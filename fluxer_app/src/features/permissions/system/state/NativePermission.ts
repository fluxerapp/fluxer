// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import MacPermissions from '@app/features/permissions/system/state/MacPermissions';
import type {NativePermissionResult} from '@app/features/permissions/system/utils/NativePermissions';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI, getNativePlatform, isDesktop, type NativePlatform} from '@app/features/ui/utils/NativeUtils';
import {makeAutoObservable, runInAction} from 'mobx';

export type LinuxInputAccessNagbarReason = 'global-hotkeys' | 'push-to-talk' | 'settings';
export type LinuxInputAccessStatus = 'unknown' | 'granted' | 'blocked';

const LINUX_INPUT_ACCESS_NAGBAR_DISMISSED_STORAGE_KEY = 'NativePermission:linuxInputAccessNagbarDismissed';
const logger = new Logger('NativePermission');

class NativePermission {
	private _initialized = false;
	private _isDesktop = false;
	private _platform: NativePlatform = 'unknown';
	private _waylandSession = false;
	private _linuxFlatpak = false;
	private _linuxInputAccessStatus: LinuxInputAccessStatus = 'unknown';
	private _linuxInputAccessNagbarRequested = false;
	private _linuxInputAccessNagbarDismissed =
		AppStorage.getItem(LINUX_INPUT_ACCESS_NAGBAR_DISMISSED_STORAGE_KEY) === 'true';
	private _linuxInputAccessNagbarReason: LinuxInputAccessNagbarReason | null = null;
	private _linuxInputAccessGrantNeedsRelogin = false;
	private _linuxInputAccessGrantError: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initialize();
	}

	private async initialize(): Promise<void> {
		const desktop = isDesktop();
		const platform = await getNativePlatform();
		let waylandSession = false;
		let linuxFlatpak = false;
		let linuxInputAccessStatus: LinuxInputAccessStatus = 'unknown';
		if (desktop && platform === 'linux') {
			try {
				const electronApi = getElectronAPI();
				const desktopInfo = await electronApi?.getDesktopInfo();
				waylandSession = Boolean(desktopInfo?.waylandSession);
				linuxFlatpak = Boolean(desktopInfo?.flatpak);
				const linuxInputAccess = await electronApi?.linuxEvdevStatus?.();
				linuxInputAccessStatus = linuxInputAccess?.hasAccess ? 'granted' : 'blocked';
			} catch (error) {
				logger.warn('Failed to read desktop session type', error);
			}
		}
		logger.debug('Initialized', {
			desktop,
			platform,
			waylandSession,
			linuxFlatpak,
		});
		runInAction(() => {
			this._isDesktop = desktop;
			this._platform = platform;
			this._waylandSession = waylandSession;
			this._linuxFlatpak = linuxFlatpak;
			this._linuxInputAccessStatus = linuxInputAccessStatus;
			if (linuxInputAccessStatus === 'granted') {
				this.resolveLinuxInputAccessNagbar();
			}
			this._initialized = true;
		});
	}

	get initialized(): boolean {
		return this._initialized;
	}

	get isDesktop(): boolean {
		return this._isDesktop;
	}

	get isMacOS(): boolean {
		return this._platform === 'macos';
	}

	get isNativeMacDesktop(): boolean {
		return this._isDesktop && this._platform === 'macos';
	}

	get isLinuxWaylandDesktop(): boolean {
		return this._isDesktop && this._platform === 'linux' && this._waylandSession;
	}

	get isLinuxFlatpakDesktop(): boolean {
		return this._isDesktop && this._platform === 'linux' && this._linuxFlatpak;
	}

	get platform(): NativePlatform {
		return this._platform;
	}

	get inputMonitoringStatus(): NativePermissionResult {
		return MacPermissions.statuses['input-monitoring'];
	}

	get isInputMonitoringGranted(): boolean {
		return MacPermissions.statuses['input-monitoring'] === 'granted';
	}

	get linuxInputAccessStatus(): LinuxInputAccessStatus {
		return this._linuxInputAccessStatus;
	}

	get linuxInputAccessGrantNeedsRelogin(): boolean {
		return this._linuxInputAccessGrantNeedsRelogin;
	}

	get linuxInputAccessGrantError(): string | null {
		return this._linuxInputAccessGrantError;
	}

	get linuxInputAccessNagbarReason(): LinuxInputAccessNagbarReason | null {
		return this._linuxInputAccessNagbarReason;
	}

	get shouldShowLinuxInputAccessNagbar(): boolean {
		return (
			this._linuxInputAccessNagbarRequested &&
			!this._linuxInputAccessNagbarDismissed &&
			this._isDesktop &&
			this._platform === 'linux' &&
			this._waylandSession &&
			this._linuxInputAccessStatus !== 'granted'
		);
	}

	requestLinuxInputAccessNagbar(reason: LinuxInputAccessNagbarReason): void {
		if (this._linuxInputAccessNagbarDismissed) return;
		this._linuxInputAccessNagbarRequested = true;
		this._linuxInputAccessNagbarReason = reason;
	}

	dismissLinuxInputAccessNagbar(): void {
		this._linuxInputAccessNagbarDismissed = true;
		AppStorage.setItem(LINUX_INPUT_ACCESS_NAGBAR_DISMISSED_STORAGE_KEY, 'true');
		this.clearLinuxInputAccessNagbarRequest();
	}

	private clearLinuxInputAccessNagbarRequest(): void {
		this._linuxInputAccessNagbarRequested = false;
		this._linuxInputAccessNagbarReason = null;
	}

	private resolveLinuxInputAccessNagbar(): void {
		this.clearLinuxInputAccessNagbarRequest();
		this._linuxInputAccessNagbarDismissed = false;
		AppStorage.removeItem(LINUX_INPUT_ACCESS_NAGBAR_DISMISSED_STORAGE_KEY);
	}

	async recheckInputMonitoring(): Promise<NativePermissionResult> {
		return MacPermissions.refreshKind('input-monitoring');
	}

	setInputMonitoringStatus(status: NativePermissionResult): void {
		MacPermissions.applyPermissionResult('input-monitoring', status);
	}

	async recheckLinuxInputAccess(): Promise<LinuxInputAccessStatus> {
		if (!this._isDesktop || this._platform !== 'linux' || !this._waylandSession) {
			return 'granted';
		}
		const status = await getElectronAPI()?.linuxEvdevStatus?.();
		const nextStatus: LinuxInputAccessStatus = status?.hasAccess ? 'granted' : 'blocked';
		runInAction(() => {
			this._linuxInputAccessStatus = nextStatus;
			if (nextStatus === 'granted') {
				this._linuxInputAccessGrantNeedsRelogin = false;
				this._linuxInputAccessGrantError = null;
				this.resolveLinuxInputAccessNagbar();
			}
		});
		logger.debug('Rechecked Linux input access', {status: nextStatus});
		return nextStatus;
	}

	async grantLinuxInputAccess(): Promise<{success: boolean; needsRelogin: boolean; error?: string}> {
		if (!this._isDesktop || this._platform !== 'linux' || !this._waylandSession) {
			return {success: false, needsRelogin: false, error: 'Not a Linux Wayland session'};
		}
		if (this._linuxFlatpak) {
			return {
				success: false,
				needsRelogin: false,
				error: `Enable Flatpak input device access, then restart ${PRODUCT_NAME}`,
			};
		}
		const electronApi = getElectronAPI();
		if (!electronApi?.linuxEvdevGrantAccess) {
			return {success: false, needsRelogin: false, error: 'Input access helper is unavailable'};
		}
		runInAction(() => {
			this._linuxInputAccessGrantNeedsRelogin = false;
			this._linuxInputAccessGrantError = null;
		});
		const result = await electronApi.linuxEvdevGrantAccess();
		const nextStatus = result.success && !result.needsRelogin ? await this.recheckLinuxInputAccess() : 'blocked';
		runInAction(() => {
			this._linuxInputAccessGrantNeedsRelogin = Boolean(result.success && result.needsRelogin);
			this._linuxInputAccessGrantError = result.success ? null : (result.error ?? 'Input access could not be enabled');
			this._linuxInputAccessStatus = nextStatus;
			if (nextStatus === 'granted') {
				this.resolveLinuxInputAccessNagbar();
			}
		});
		return result;
	}
}

export default new NativePermission();
