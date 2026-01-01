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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import {makeAutoObservable, runInAction} from 'mobx';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import {Logger} from '~/lib/Logger';
import {makePersistent} from '~/lib/MobXPersistence';
import VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import VoiceDevicePermissionStore, {type VoiceDeviceState} from '~/stores/voice/VoiceDevicePermissionStore';

const logger = new Logger('NewDeviceMonitoringStore');

type DeviceType = 'input' | 'output';

interface PendingDevicePrompt {
	deviceId: string;
	deviceName: string;
	deviceType: DeviceType;
}

class NewDeviceMonitoringStore {
	knownDeviceIds: Array<string> = [];
	ignoredDeviceIds: Array<string> = [];
	suppressAlerts = false;

	private isInitialized = false;
	private isStarted = false;
	private startPromise: Promise<void> | null = null;
	private startEpoch = 0;
	private pendingPrompts: Array<PendingDevicePrompt> = [];
	private isShowingPrompt = false;
	private unsubscribe: (() => void) | null = null;
	private i18n: I18n | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setI18n(i18n: I18n): void {
		this.i18n = i18n;
	}

	private startMonitoring(): void {
		if (this.unsubscribe) return;
		this.unsubscribe = VoiceDevicePermissionStore.subscribe(this.handleDeviceStateChange);
	}

	async start(): Promise<void> {
		if (this.startPromise) return this.startPromise;

		this.isStarted = true;
		const epoch = ++this.startEpoch;

		this.startPromise = (async () => {
			await makePersistent(this, 'NewDeviceMonitoringStore', ['knownDeviceIds', 'ignoredDeviceIds', 'suppressAlerts']);

			if (!this.isStarted || epoch !== this.startEpoch) return;

			this.startMonitoring();
		})();

		return this.startPromise;
	}

	private handleDeviceStateChange(state: VoiceDeviceState): void {
		if (!this.isStarted) return;

		if (state.permissionStatus !== 'granted') {
			return;
		}

		if (this.suppressAlerts) {
			return;
		}

		const currentInputIds = state.inputDevices.map((d) => d.deviceId);
		const currentOutputIds = state.outputDevices.map((d) => d.deviceId);
		const allCurrentIds = [...currentInputIds, ...currentOutputIds];

		if (!this.isInitialized) {
			runInAction(() => {
				this.knownDeviceIds = [...new Set([...this.knownDeviceIds, ...allCurrentIds])];
				this.isInitialized = true;
			});
			logger.debug('Initialized with known devices', {count: this.knownDeviceIds.length});
			return;
		}

		const newInputDevices = state.inputDevices.filter(
			(device) =>
				device.deviceId !== 'default' &&
				!this.knownDeviceIds.includes(device.deviceId) &&
				!this.ignoredDeviceIds.includes(device.deviceId) &&
				device.label,
		);

		const newOutputDevices = state.outputDevices.filter(
			(device) =>
				device.deviceId !== 'default' &&
				!this.knownDeviceIds.includes(device.deviceId) &&
				!this.ignoredDeviceIds.includes(device.deviceId) &&
				device.label,
		);

		if (newInputDevices.length > 0 || newOutputDevices.length > 0) {
			logger.debug('New devices detected', {
				inputs: newInputDevices.map((d) => d.label),
				outputs: newOutputDevices.map((d) => d.label),
			});

			runInAction(() => {
				for (const device of newInputDevices) {
					this.pendingPrompts.push({
						deviceId: device.deviceId,
						deviceName: device.label,
						deviceType: 'input',
					});
					this.knownDeviceIds.push(device.deviceId);
				}

				for (const device of newOutputDevices) {
					this.pendingPrompts.push({
						deviceId: device.deviceId,
						deviceName: device.label,
						deviceType: 'output',
					});
					this.knownDeviceIds.push(device.deviceId);
				}
			});

			this.processNextPrompt();
		}
	}

	private processNextPrompt(): void {
		if (!this.isStarted) return;

		if (this.isShowingPrompt || this.pendingPrompts.length === 0) {
			return;
		}

		const prompt = this.pendingPrompts.shift();
		if (!prompt) {
			return;
		}

		this.isShowingPrompt = true;
		this.showNewDeviceModal(prompt);
	}

	private showNewDeviceModal(prompt: PendingDevicePrompt): void {
		if (!this.i18n) {
			throw new Error('NewDeviceMonitoringStore: i18n not initialized');
		}
		const i18n = this.i18n;
		const {deviceId, deviceName, deviceType} = prompt;

		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(msg`New audio device detected!`)}
					description={
						deviceType === 'input' ? (
							<Trans>
								Fluxer has found a new audio input device named <strong>{deviceName}</strong>. Do you want to switch to
								it?
							</Trans>
						) : (
							<Trans>
								Fluxer has found a new audio output device named <strong>{deviceName}</strong>. Do you want to switch to
								it?
							</Trans>
						)
					}
					primaryText={i18n._(msg`Switch Device`)}
					primaryVariant="primary"
					secondaryText={i18n._(msg`Not Now`)}
					checkboxContent={
						<Checkbox>
							<Trans>
								Don't ask me this again for <strong>{deviceName}</strong>
							</Trans>
						</Checkbox>
					}
					onPrimary={(dontAskAgain) => {
						if (deviceType === 'input') {
							VoiceSettingsStore.updateSettings({inputDeviceId: deviceId});
						} else {
							VoiceSettingsStore.updateSettings({outputDeviceId: deviceId});
						}

						if (dontAskAgain) {
							this.addToIgnored(deviceId);
						}

						queueMicrotask(() => this.onModalClosed());
					}}
					onSecondary={(dontAskAgain) => {
						if (dontAskAgain) {
							this.addToIgnored(deviceId);
						}

						queueMicrotask(() => this.onModalClosed());
					}}
				/>
			)),
		);
	}

	private onModalClosed(): void {
		this.isShowingPrompt = false;
		this.processNextPrompt();
	}

	private addToIgnored(deviceId: string): void {
		if (!this.ignoredDeviceIds.includes(deviceId)) {
			runInAction(() => {
				this.ignoredDeviceIds.push(deviceId);
			});
			logger.debug('Added device to ignore list', {deviceId});
		}
	}

	clearIgnoredDevices(): void {
		this.ignoredDeviceIds = [];
		logger.debug('Cleared all ignored devices');
	}

	removeFromIgnored(deviceId: string): void {
		const index = this.ignoredDeviceIds.indexOf(deviceId);
		if (index !== -1) {
			this.ignoredDeviceIds.splice(index, 1);
			logger.debug('Removed device from ignore list', {deviceId});
		}
	}

	getIgnoredDeviceIds(): ReadonlyArray<string> {
		return this.ignoredDeviceIds;
	}

	setSuppressAlerts(suppress: boolean): void {
		this.suppressAlerts = suppress;
		logger.debug('Suppress alerts setting changed', {suppress});
	}

	showTestModal(): void {
		this.showNewDeviceModal({
			deviceId: 'test-device-id',
			deviceName: 'Test Audio Device',
			deviceType: 'input',
		});
	}

	dispose(): void {
		this.isStarted = false;
		this.startPromise = null;
		this.startEpoch++;

		this.pendingPrompts = [];
		this.isShowingPrompt = false;

		if (this.unsubscribe) {
			this.unsubscribe();
			this.unsubscribe = null;
		}

		this.isInitialized = false;
	}
}

export default new NewDeviceMonitoringStore();
