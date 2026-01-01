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

import {makeAutoObservable, runInAction} from 'mobx';
import {Logger} from '~/lib/Logger';
import {checkNativePermission, type NativePermissionResult} from '~/utils/NativePermissions';
import {getNativePlatform, isDesktop, type NativePlatform} from '~/utils/NativeUtils';

const logger = new Logger('NativePermissionStore');

class NativePermissionStore {
	private _initialized = false;
	private _isDesktop = false;
	private _platform: NativePlatform = 'unknown';
	private _inputMonitoringStatus: NativePermissionResult = 'granted';

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initialize();
	}

	private async initialize(): Promise<void> {
		const desktop = isDesktop();
		const platform = await getNativePlatform();

		let inputMonitoringStatus: NativePermissionResult = 'granted';

		if (desktop && platform === 'macos') {
			inputMonitoringStatus = await checkNativePermission('input-monitoring');
		}

		logger.debug('Initialized', {
			desktop,
			platform,
			inputMonitoringStatus,
		});

		runInAction(() => {
			this._isDesktop = desktop;
			this._platform = platform;
			this._inputMonitoringStatus = inputMonitoringStatus;
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

	get platform(): NativePlatform {
		return this._platform;
	}

	get inputMonitoringStatus(): NativePermissionResult {
		return this._inputMonitoringStatus;
	}

	get isInputMonitoringGranted(): boolean {
		return this._inputMonitoringStatus === 'granted';
	}

	get shouldShowInputMonitoringBanner(): boolean {
		return this._isDesktop && this._platform === 'macos' && this._inputMonitoringStatus !== 'granted';
	}

	async recheckInputMonitoring(): Promise<NativePermissionResult> {
		if (!this._isDesktop || this._platform !== 'macos') {
			return 'granted';
		}

		const status = await checkNativePermission('input-monitoring');

		runInAction(() => {
			this._inputMonitoringStatus = status;
		});

		logger.debug('Rechecked input monitoring', {status});
		return status;
	}

	setInputMonitoringStatus(status: NativePermissionResult): void {
		this._inputMonitoringStatus = status;
	}
}

export default new NativePermissionStore();
