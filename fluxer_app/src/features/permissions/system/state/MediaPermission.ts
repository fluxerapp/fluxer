// SPDX-License-Identifier: AGPL-3.0-or-later

import MacPermissions from '@app/features/permissions/system/state/MacPermissions';
import {
	checkNativePermission,
	type NativePermissionResult,
} from '@app/features/permissions/system/utils/NativePermissions';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MediaDeviceRefreshType, refreshMediaDeviceLists} from '@app/features/voice/utils/MediaDeviceRefresh';
import {makeAutoObservable, reaction, runInAction} from 'mobx';

const logger = new Logger('MediaPermission');

interface MediaPermissionUpdateOptions {
	refreshDevices?: boolean;
}

class MediaPermission {
	microphoneExplicitlyDenied = false;
	cameraExplicitlyDenied = false;
	screenRecordingExplicitlyDenied = false;
	microphonePermissionState: PermissionState | null = null;
	cameraPermissionState: PermissionState | null = null;
	screenRecordingPermissionState: PermissionState | null = null;
	initialized = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.bindMacPermissions();
		void this.initializePermissionState();
	}

	private bindMacPermissions(): void {
		reaction(
			() => ({
				isMac: MacPermissions.isNativeMacDesktop,
				isHydrated: MacPermissions.isHydrated,
				microphone: MacPermissions.statuses.microphone,
				camera: MacPermissions.statuses.camera,
				screen: MacPermissions.statuses.screen,
			}),
			(snapshot) => {
				if (!snapshot.isMac) return;
				if (!snapshot.isHydrated) return;
				this.applyNativeStatus('microphone', snapshot.microphone);
				this.applyNativeStatus('camera', snapshot.camera);
				this.applyNativeStatus('screen', snapshot.screen);
				this.initialized = true;
			},
			{fireImmediately: true},
		);
	}

	private permissionStateFromNativeStatus(status: NativePermissionResult): PermissionState {
		switch (status) {
			case 'granted':
				return 'granted';
			case 'denied':
				return 'denied';
			case 'not-determined':
				return 'prompt';
			case 'unsupported':
				throw new Error('Unsupported native permission status cannot be mapped to a browser permission state');
		}
	}

	private applyNativeStatus(kind: 'microphone' | 'camera' | 'screen', status: NativePermissionResult): void {
		if (status === 'unsupported') return;
		const permissionState = this.permissionStateFromNativeStatus(status);
		const denied = status === 'denied';
		if (kind === 'microphone') {
			this.microphoneExplicitlyDenied = denied;
			this.microphonePermissionState = permissionState;
		} else if (kind === 'camera') {
			this.cameraExplicitlyDenied = denied;
			this.cameraPermissionState = permissionState;
		} else {
			this.screenRecordingExplicitlyDenied = denied;
			this.screenRecordingPermissionState = permissionState;
		}
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
			this.applyNativeStatus('microphone', micState);
			this.applyNativeStatus('camera', cameraState);
			this.applyNativeStatus('screen', screenState);
			this.initialized = true;
		});
		return true;
	}

	markMicrophoneExplicitlyDenied(): void {
		this.microphoneExplicitlyDenied = true;
		this.microphonePermissionState = 'denied';
		MacPermissions.applyPermissionResult('microphone', 'denied');
		logger.debug('Marked microphone as explicitly denied');
	}

	markCameraExplicitlyDenied(): void {
		this.cameraExplicitlyDenied = true;
		this.cameraPermissionState = 'denied';
		MacPermissions.applyPermissionResult('camera', 'denied');
		logger.debug('Marked camera as explicitly denied');
	}

	markScreenRecordingExplicitlyDenied(): void {
		this.screenRecordingExplicitlyDenied = true;
		this.screenRecordingPermissionState = 'denied';
		MacPermissions.applyPermissionResult('screen', 'denied');
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

	updateMicrophonePermissionGranted(options: MediaPermissionUpdateOptions = {}): void {
		const shouldRefresh =
			options.refreshDevices !== false &&
			(this.microphoneExplicitlyDenied || this.microphonePermissionState !== 'granted');
		this.microphoneExplicitlyDenied = false;
		this.microphonePermissionState = 'granted';
		MacPermissions.applyPermissionResult('microphone', 'granted');
		logger.debug('Updated microphone permission to granted');
		if (shouldRefresh) {
			void refreshMediaDeviceLists({type: MediaDeviceRefreshType.audio});
		}
	}

	updateCameraPermissionGranted(options: MediaPermissionUpdateOptions = {}): void {
		const shouldRefresh =
			options.refreshDevices !== false && (this.cameraExplicitlyDenied || this.cameraPermissionState !== 'granted');
		this.cameraExplicitlyDenied = false;
		this.cameraPermissionState = 'granted';
		MacPermissions.applyPermissionResult('camera', 'granted');
		logger.debug('Updated camera permission to granted');
		if (shouldRefresh) {
			void refreshMediaDeviceLists({type: MediaDeviceRefreshType.video});
		}
	}

	updateScreenRecordingPermissionGranted(): void {
		this.screenRecordingExplicitlyDenied = false;
		this.screenRecordingPermissionState = 'granted';
		MacPermissions.applyPermissionResult('screen', 'granted');
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
				initialized: this.initialized,
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

export default new MediaPermission();
