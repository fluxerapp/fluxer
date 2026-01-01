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
import MediaPermissionStore from '~/stores/MediaPermissionStore';
import {type VoiceDeviceState, voiceDeviceManager} from '~/utils/VoiceDeviceManager';

const logger = new Logger('VoiceDevicePermissionStore');

type DeviceListener = (state: VoiceDeviceState) => void;

class VoiceDevicePermissionStore {
	deviceState: VoiceDeviceState = voiceDeviceManager.getState();
	private deviceListeners = new Set<DeviceListener>();
	private permissionRequestInFlight: Promise<boolean> | null = null;

	constructor() {
		makeAutoObservable<this, 'deviceListeners' | 'permissionRequestInFlight'>(
			this,
			{
				deviceListeners: false,
				permissionRequestInFlight: false,
			},
			{autoBind: true},
		);

		voiceDeviceManager.subscribe(this.handleDeviceStateChange);
	}

	private handleDeviceStateChange(state: VoiceDeviceState): void {
		runInAction(() => {
			this.deviceState = state;
		});

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

	async ensureDevices(options: {requestPermissions?: boolean} = {}): Promise<VoiceDeviceState> {
		const state = await voiceDeviceManager.ensureDevices(options);
		this.handleDeviceStateChange(state);
		return state;
	}

	async refreshDevices(requestPermissions?: boolean): Promise<VoiceDeviceState> {
		return this.ensureDevices({requestPermissions});
	}

	async requestPermissionFor(type: 'audio' | 'video'): Promise<boolean> {
		if (this.permissionRequestInFlight) {
			return this.permissionRequestInFlight;
		}

		const requestPromise = (async (): Promise<boolean> => {
			const state = await this.ensureDevices({requestPermissions: true});

			if (state.permissionStatus === 'granted') {
				if (type === 'audio') {
					MediaPermissionStore.updateMicrophonePermissionGranted();
				} else {
					MediaPermissionStore.updateCameraPermissionGranted();
				}
				return true;
			}

			if (state.permissionStatus === 'denied') {
				if (type === 'audio') {
					MediaPermissionStore.markMicrophoneExplicitlyDenied();
				} else {
					MediaPermissionStore.markCameraExplicitlyDenied();
				}
				return false;
			}

			return type === 'audio' ? MediaPermissionStore.isMicrophoneGranted() : MediaPermissionStore.isCameraGranted();
		})()
			.catch((error) => {
				logger.error('Failed to request media permission', {type, error});
				return false;
			})
			.finally(() => {
				this.permissionRequestInFlight = null;
			});

		this.permissionRequestInFlight = requestPromise;
		return requestPromise;
	}
}

export default new VoiceDevicePermissionStore();
export type {VoiceDeviceState};
