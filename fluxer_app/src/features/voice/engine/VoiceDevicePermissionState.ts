// SPDX-License-Identifier: AGPL-3.0-or-later

import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	type EnsureVoiceDevicesOptions,
	type VoiceDeviceState,
	voiceDeviceManager,
} from '@app/features/voice/utils/VoiceDeviceManager';

const logger = new Logger('VoiceDevicePermissionState');

type DeviceListener = (state: VoiceDeviceState) => void;

class VoiceDevicePermissionState {
	deviceState: VoiceDeviceState = voiceDeviceManager.getState();
	private deviceListeners = new Set<DeviceListener>();
	private permissionRequestsInFlight = new Map<'audio' | 'video', Promise<boolean>>();

	constructor() {
		voiceDeviceManager.subscribe((state) => this.handleDeviceStateChange(state));
	}

	private handleDeviceStateChange(state: VoiceDeviceState): void {
		this.deviceState = state;
		this.deviceListeners.forEach((listener) => {
			try {
				listener(state);
			} catch (error) {
				logger.error('Voice device listener threw', {error});
			}
		});
	}

	getState(): VoiceDeviceState {
		return this.deviceState;
	}

	subscribe(listener: DeviceListener): () => void {
		this.deviceListeners.add(listener);
		listener(this.deviceState);
		return () => {
			this.deviceListeners.delete(listener);
		};
	}

	async ensureDevices(options: EnsureVoiceDevicesOptions = {}): Promise<VoiceDeviceState> {
		const state = await voiceDeviceManager.ensureDevices(options);
		if (this.deviceState !== state) {
			this.handleDeviceStateChange(state);
		}
		return state;
	}

	async refreshDevices(requestPermissions?: boolean): Promise<VoiceDeviceState> {
		return this.ensureDevices({requestPermissions, forceRefresh: true});
	}

	async requestPermissionFor(type: 'audio' | 'video'): Promise<boolean> {
		const permissionGranted =
			type === 'audio' ? MediaPermission.isMicrophoneGranted() : MediaPermission.isCameraGranted();
		const hasPrimaryDevice =
			type === 'audio'
				? this.deviceState.inputDevices.some((device) => device.kind === 'audioinput')
				: this.deviceState.videoDevices.some((device) => device.kind === 'videoinput');
		if (permissionGranted && hasPrimaryDevice) return true;
		const inFlightRequest = this.permissionRequestsInFlight.get(type);
		if (inFlightRequest) {
			return inFlightRequest;
		}
		const requestPromise = (async (): Promise<boolean> => {
			const state = await this.ensureDevices({requestPermissionTypes: [type], forceRefresh: true});
			if (state.permissionStatus[type] === 'granted') {
				if (type === 'audio') {
					MediaPermission.updateMicrophonePermissionGranted({refreshDevices: false});
				} else {
					MediaPermission.updateCameraPermissionGranted({refreshDevices: false});
				}
				return true;
			}
			if (state.permissionStatus[type] === 'denied') {
				if (type === 'audio') {
					MediaPermission.markMicrophoneExplicitlyDenied();
				} else {
					MediaPermission.markCameraExplicitlyDenied();
				}
				return false;
			}
			return false;
		})()
			.catch((error) => {
				logger.error('Failed to request media permission', {type, error});
				return false;
			})
			.finally(() => {
				this.permissionRequestsInFlight.delete(type);
			});
		this.permissionRequestsInFlight.set(type, requestPromise);
		return requestPromise;
	}
}

export default new VoiceDevicePermissionState();
