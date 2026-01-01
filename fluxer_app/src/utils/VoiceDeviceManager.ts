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

import {Logger} from '~/lib/Logger';
import {Platform} from '~/lib/Platform';
import {ensureNativePermission} from '~/utils/NativePermissions';
import {isDesktop} from '~/utils/NativeUtils';

const logger = new Logger('VoiceDeviceManager');

export function resolveEffectiveDeviceId(
	storedDeviceId: string,
	devices: ReadonlyArray<MediaDeviceInfo>,
): string | null {
	if (devices.length === 0) {
		return null;
	}

	const deviceExists = devices.some((d) => d.deviceId === storedDeviceId);
	if (deviceExists) {
		return storedDeviceId;
	}

	return devices[0].deviceId;
}

export function hasDeviceLabels(devices: ReadonlyArray<MediaDeviceInfo>): boolean {
	return devices.some((d) => d.label && d.label.trim().length > 0);
}

type PermissionStatus = 'idle' | 'loading' | 'granted' | 'denied';

export interface VoiceDeviceState {
	inputDevices: Array<MediaDeviceInfo>;
	outputDevices: Array<MediaDeviceInfo>;
	videoDevices: Array<MediaDeviceInfo>;
	permissionStatus: PermissionStatus;
}

type Listener = (state: VoiceDeviceState) => void;

const sortDevices = (devices: Array<MediaDeviceInfo>): Array<MediaDeviceInfo> => {
	return [...devices].sort((a, b) => {
		const aIsDefault = a.deviceId === 'default';
		const bIsDefault = b.deviceId === 'default';
		if (aIsDefault && !bIsDefault) return -1;
		if (!aIsDefault && bIsDefault) return 1;
		return a.label.localeCompare(b.label);
	});
};

class VoiceDeviceManager {
	private state: VoiceDeviceState = {
		inputDevices: [],
		outputDevices: [],
		videoDevices: [],
		permissionStatus: 'idle',
	};

	private listeners = new Set<Listener>();
	private enumeratingPromise: Promise<VoiceDeviceState> | null = null;
	private shouldRequestPermissions = false;

	constructor() {
		if (navigator.mediaDevices?.addEventListener) {
			navigator.mediaDevices.addEventListener('devicechange', this.handleDeviceChange);
		}
	}

	public getState(): VoiceDeviceState {
		return this.state;
	}

	public subscribe(listener: Listener): () => void {
		this.listeners.add(listener);
		listener(this.state);
		return () => {
			this.listeners.delete(listener);
		};
	}

	public async ensureDevices(options: {requestPermissions?: boolean} = {}): Promise<VoiceDeviceState> {
		const requestPermissions = options.requestPermissions ?? false;

		logger.debug('ensureDevices called', {
			requestPermissions,
			shouldRequestPermissions: this.shouldRequestPermissions,
			hasEnumeratingPromise: !!this.enumeratingPromise,
			currentState: {
				inputDeviceCount: this.state.inputDevices.length,
				permissionStatus: this.state.permissionStatus,
			},
		});

		if (requestPermissions) {
			this.shouldRequestPermissions = true;
		}

		const shouldRequest = this.shouldRequestPermissions || requestPermissions;

		const runEnumeration = () =>
			this.enumerateDevices(shouldRequest).catch((error) => {
				logger.debug('Failed to enumerate media devices:', error);
				throw error;
			});

		if (this.enumeratingPromise) {
			logger.debug('Chaining to existing enumeration promise');
			this.enumeratingPromise = this.enumeratingPromise.catch(() => this.state).then(runEnumeration);
		} else {
			logger.debug('Creating new enumeration promise');
			this.enumeratingPromise = runEnumeration();
		}

		const pendingPromise = this.enumeratingPromise;
		return pendingPromise.finally(() => {
			if (this.enumeratingPromise === pendingPromise) {
				logger.debug('Enumeration promise completed');
				this.enumeratingPromise = null;
			}
		});
	}

	private async enumerateDevices(requestPermissions: boolean): Promise<VoiceDeviceState> {
		logger.debug('enumerateDevices started', {requestPermissions});

		if (!navigator.mediaDevices?.enumerateDevices) {
			logger.debug('Navigator or mediaDevices API not available');
			return this.state;
		}

		if (requestPermissions) {
			logger.debug('Setting permission status to loading');
			this.updateState({permissionStatus: 'loading'});
		}

		try {
			logger.debug('Calling navigator.mediaDevices.enumerateDevices()');
			let devices = await navigator.mediaDevices.enumerateDevices();
			let permissionStatus = this.state.permissionStatus;

			logger.debug('Initial enumeration result', {
				deviceCount: devices.length,
				devices: devices.map((d) => ({
					kind: d.kind,
					deviceId: d.deviceId,
					label: d.label,
					hasLabel: !!d.label,
				})),
			});

			const hasLabels = devices.some((device) => device.label && device.label !== '');
			let usedNativeFlow = false;
			if (hasLabels) {
				logger.debug('Devices have labels, permissions already granted');
				permissionStatus = 'granted';
			} else if (requestPermissions && isDesktop()) {
				logger.debug('No labels detected; attempting native permission flow');
				const [nativeMic, nativeCamera] = await Promise.all([
					ensureNativePermission('microphone'),
					ensureNativePermission('camera'),
				]);
				usedNativeFlow = nativeMic !== 'unsupported' || nativeCamera !== 'unsupported';
				if (nativeMic === 'denied' || nativeCamera === 'denied') {
					permissionStatus = 'denied';
				} else if (nativeMic === 'granted' || nativeCamera === 'granted') {
					permissionStatus = 'granted';
				}
			}

			if (!hasLabels && requestPermissions && (!usedNativeFlow || permissionStatus !== 'granted')) {
				const isIOSPWA = Platform.isIOSWeb && Platform.isPWA;
				let skipGetUserMedia = false;

				if (isIOSPWA && navigator.permissions) {
					try {
						const micPermission = await navigator.permissions.query({name: 'microphone' as PermissionName});
						if (micPermission.state === 'granted') {
							logger.debug('iOS PWA: microphone permission already granted via Permissions API, skipping getUserMedia');
							permissionStatus = 'granted';
							devices = await navigator.mediaDevices.enumerateDevices();
							skipGetUserMedia = devices.some((d) => d.label && d.label !== '');
						}
					} catch {}
				}

				if (!skipGetUserMedia) {
					logger.debug('No labels found, requesting permissions via getUserMedia');
					try {
						const stream = await navigator.mediaDevices.getUserMedia({audio: true, video: true});
						logger.debug('getUserMedia succeeded, stopping tracks');
						stream.getTracks().forEach((track) => {
							logger.debug('Stopping track', {kind: track.kind, label: track.label});
							track.stop();
						});
						permissionStatus = 'granted';
						logger.debug('Re-enumerating devices after permission grant');
						devices = await navigator.mediaDevices.enumerateDevices();
						logger.debug('Re-enumeration result', {
							deviceCount: devices.length,
							devices: devices.map((d) => ({
								kind: d.kind,
								deviceId: d.deviceId,
								label: d.label,
							})),
						});
					} catch (error) {
						logger.debug('getUserMedia failed', {
							error,
							errorName: error instanceof DOMException ? error.name : 'unknown',
							errorMessage: error instanceof Error ? error.message : String(error),
						});
						if (
							error instanceof DOMException &&
							(error.name === 'NotAllowedError' || error.name === 'PermissionDeniedError')
						) {
							permissionStatus = 'denied';
						} else {
							permissionStatus = 'granted';
						}
					}
				}
			}

			const inputDevices = sortDevices(devices.filter((device) => device.kind === 'audioinput'));
			const outputDevices = sortDevices(devices.filter((device) => device.kind === 'audiooutput'));
			const videoDevices = sortDevices(devices.filter((device) => device.kind === 'videoinput'));

			const nextState: VoiceDeviceState = {
				inputDevices,
				outputDevices,
				videoDevices,
				permissionStatus: this.resolvePermissionStatus(requestPermissions, permissionStatus),
			};

			logger.debug('Final device state', {
				inputDeviceCount: inputDevices.length,
				outputDeviceCount: outputDevices.length,
				videoDeviceCount: videoDevices.length,
				permissionStatus: nextState.permissionStatus,
			});

			this.updateState(nextState);
			return this.state;
		} catch (_error) {
			logger.debug('enumerateDevices failed with exception', _error);
			if (requestPermissions) {
				this.updateState({permissionStatus: 'denied'});
			}
			return this.state;
		}
	}

	private resolvePermissionStatus(requestPermissions: boolean, computedStatus: PermissionStatus): PermissionStatus {
		if (!requestPermissions) {
			if (this.state.permissionStatus === 'denied') {
				return 'denied';
			}
			if (this.state.permissionStatus === 'granted') {
				return 'granted';
			}
		}
		return computedStatus;
	}

	private updateState(partial: Partial<VoiceDeviceState>) {
		this.state = {
			...this.state,
			...partial,
		};
		this.listeners.forEach((listener) => listener(this.state));
	}

	private handleDeviceChange = () => {
		void this.ensureDevices({requestPermissions: this.shouldRequestPermissions});
	};
}

export const voiceDeviceManager = new VoiceDeviceManager();
