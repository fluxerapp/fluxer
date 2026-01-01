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

import {makeAutoObservable, reaction, runInAction} from 'mobx';
import {Logger} from '~/lib/Logger';
import {MediaDeviceRefreshType, refreshMediaDeviceLists} from '~/utils/MediaDeviceRefresh';
import {checkNativePermission} from '~/utils/NativePermissions';

const logger = new Logger('MediaPermissionStore');

class MediaPermissionStore {
	microphoneExplicitlyDenied = false;
	cameraExplicitlyDenied = false;
	screenRecordingExplicitlyDenied = false;
	microphonePermissionState: PermissionState | null = null;
	cameraPermissionState: PermissionState | null = null;
	screenRecordingPermissionState: PermissionState | null = null;
	initialized = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initializePermissionState();
	}

	private async initializePermissionState(): Promise<void> {
		if (await this.tryInitializeNativePermissions()) {
			return;
		}

		if (!navigator.permissions) {
			logger.debug('Permissions API not available');
			runInAction(() => {
				this.initialized = true;
			});
			return;
		}

		try {
			const micPermission = await navigator.permissions.query({name: 'microphone' as PermissionName});
			const micDenied = micPermission.state === 'denied';

			const cameraPermission = await navigator.permissions.query({name: 'camera' as PermissionName});
			const cameraDenied = cameraPermission.state === 'denied';

			logger.debug('Initial permission state', {
				microphone: micPermission.state,
				camera: cameraPermission.state,
				micDenied,
				cameraDenied,
			});

			runInAction(() => {
				this.microphoneExplicitlyDenied = micDenied;
				this.cameraExplicitlyDenied = cameraDenied;
				this.microphonePermissionState = micPermission.state;
				this.cameraPermissionState = cameraPermission.state;
				this.initialized = true;
			});

			micPermission.onchange = () => {
				const isDenied = micPermission.state === 'denied';
				logger.debug('Microphone permission changed', {state: micPermission.state, isDenied});
				runInAction(() => {
					this.microphoneExplicitlyDenied = isDenied;
					this.microphonePermissionState = micPermission.state;
				});
			};

			cameraPermission.onchange = () => {
				const isDenied = cameraPermission.state === 'denied';
				logger.debug('Camera permission changed', {state: cameraPermission.state, isDenied});
				runInAction(() => {
					this.cameraExplicitlyDenied = isDenied;
					this.cameraPermissionState = cameraPermission.state;
				});
			};
		} catch (error) {
			logger.debug('Failed to query permissions', error);
			runInAction(() => {
				this.initialized = true;
			});
		}
	}

	private async tryInitializeNativePermissions(): Promise<boolean> {
		const [micState, cameraState, screenState] = await Promise.all([
			checkNativePermission('microphone'),
			checkNativePermission('camera'),
			checkNativePermission('screen'),
		]);

		const handled = micState !== 'unsupported' || cameraState !== 'unsupported';
		if (!handled) return false;

		runInAction(() => {
			if (micState !== 'unsupported') {
				this.microphoneExplicitlyDenied = micState === 'denied';
				this.microphonePermissionState = micState === 'granted' ? 'granted' : 'denied';
			}
			if (cameraState !== 'unsupported') {
				this.cameraExplicitlyDenied = cameraState === 'denied';
				this.cameraPermissionState = cameraState === 'granted' ? 'granted' : 'denied';
			}
			if (screenState !== 'unsupported') {
				this.screenRecordingExplicitlyDenied = screenState === 'denied';
				this.screenRecordingPermissionState = screenState === 'granted' ? 'granted' : 'denied';
			}
			this.initialized = true;
		});

		return true;
	}

	markMicrophoneExplicitlyDenied(): void {
		this.microphoneExplicitlyDenied = true;
		this.microphonePermissionState = 'denied';
		logger.debug('Marked microphone as explicitly denied');
	}

	markCameraExplicitlyDenied(): void {
		this.cameraExplicitlyDenied = true;
		this.cameraPermissionState = 'denied';
		logger.debug('Marked camera as explicitly denied');
	}

	markScreenRecordingExplicitlyDenied(): void {
		this.screenRecordingExplicitlyDenied = true;
		this.screenRecordingPermissionState = 'denied';
		logger.debug('Marked screen recording as explicitly denied');
	}

	clearMicrophoneDenial(): void {
		this.microphoneExplicitlyDenied = false;
		logger.debug('Cleared microphone denial');
	}

	clearCameraDenial(): void {
		this.cameraExplicitlyDenied = false;
		logger.debug('Cleared camera denial');
	}

	clearScreenRecordingDenial(): void {
		this.screenRecordingExplicitlyDenied = false;
		logger.debug('Cleared screen recording denial');
	}

	updateMicrophonePermissionGranted(): void {
		this.microphoneExplicitlyDenied = false;
		this.microphonePermissionState = 'granted';
		logger.debug('Updated microphone permission to granted');
		void refreshMediaDeviceLists({type: MediaDeviceRefreshType.audio});
	}

	updateCameraPermissionGranted(): void {
		this.cameraExplicitlyDenied = false;
		this.cameraPermissionState = 'granted';
		logger.debug('Updated camera permission to granted');
		void refreshMediaDeviceLists({type: MediaDeviceRefreshType.video});
	}

	updateScreenRecordingPermissionGranted(): void {
		this.screenRecordingExplicitlyDenied = false;
		this.screenRecordingPermissionState = 'granted';
		logger.debug('Updated screen recording permission to granted');
	}

	reset(): void {
		this.microphoneExplicitlyDenied = false;
		this.cameraExplicitlyDenied = false;
		this.screenRecordingExplicitlyDenied = false;
		this.microphonePermissionState = null;
		this.cameraPermissionState = null;
		this.screenRecordingPermissionState = null;
		this.initialized = false;
		logger.debug('Reset all permissions');
	}

	isInitialized(): boolean {
		return this.initialized;
	}

	isMicrophoneExplicitlyDenied(): boolean {
		return this.microphoneExplicitlyDenied;
	}

	isCameraExplicitlyDenied(): boolean {
		return this.cameraExplicitlyDenied;
	}

	isScreenRecordingExplicitlyDenied(): boolean {
		return this.screenRecordingExplicitlyDenied;
	}

	isMicrophoneGranted(): boolean {
		return this.microphonePermissionState === 'granted';
	}

	isCameraGranted(): boolean {
		return this.cameraPermissionState === 'granted';
	}

	isScreenRecordingGranted(): boolean {
		return this.screenRecordingPermissionState === 'granted';
	}

	getMicrophonePermissionState(): PermissionState | null {
		return this.microphonePermissionState;
	}

	getCameraPermissionState(): PermissionState | null {
		return this.cameraPermissionState;
	}

	getScreenRecordingPermissionState(): PermissionState | null {
		return this.screenRecordingPermissionState;
	}

	addChangeListener(callback: () => void): () => void {
		const dispose = reaction(
			() => ({
				mic: this.microphonePermissionState,
				camera: this.cameraPermissionState,
				screen: this.screenRecordingPermissionState,
			}),
			() => {
				callback();
			},
			{fireImmediately: true},
		);
		return dispose;
	}
}

export default new MediaPermissionStore();
