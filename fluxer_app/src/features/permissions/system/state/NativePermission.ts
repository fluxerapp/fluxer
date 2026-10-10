// SPDX-License-Identifier: AGPL-3.0-or-later

import MacPermissions from '@app/features/permissions/system/state/MacPermissions';
import type {NativePermissionResult} from '@app/features/permissions/system/utils/NativePermissions';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {getElectronAPI, getNativePlatform, isDesktop, type NativePlatform} from '@app/features/ui/utils/NativeUtils';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('NativePermission');

class NativePermission {
	private _isDesktop = false;
	private _platform: NativePlatform = 'unknown';
	private _waylandSession = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initialize());
	}

	private async initialize(): Promise<void> {
		const desktop = isDesktop();
		const platform = await getNativePlatform();
		let waylandSession = false;
		if (desktop && platform === 'linux') {
			try {
				const desktopInfo = await getElectronAPI()?.getDesktopInfo();
				waylandSession = Boolean(desktopInfo?.waylandSession);
			} catch (error) {
				logger.warn('Failed to read desktop session type', error);
			}
		}
		logger.debug('Initialized', {
			desktop,
			platform,
			waylandSession,
		});
		runInAction(() => {
			this._isDesktop = desktop;
			this._platform = platform;
			this._waylandSession = waylandSession;
		});
	}

	get isDesktop(): boolean {
		return this._isDesktop;
	}

	get isMacOS(): boolean {
		return this._platform === 'macos';
	}

	get isLinuxWaylandDesktop(): boolean {
		return this._isDesktop && this._platform === 'linux' && this._waylandSession;
	}

	get isInputMonitoringGranted(): boolean {
		return MacPermissions.statuses['input-monitoring'] === 'granted';
	}

	setInputMonitoringStatus(status: NativePermissionResult): void {
		MacPermissions.applyPermissionResult('input-monitoring', status);
	}
}

export default new NativePermission();
