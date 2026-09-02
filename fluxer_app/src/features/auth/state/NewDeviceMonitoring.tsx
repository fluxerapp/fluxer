// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/auth/state/NewDeviceMonitoring.module.css';
import {
	getNewDevicePromptCandidates,
	type PendingDevicePrompt,
} from '@app/features/auth/state/NewDeviceMonitoringDevices';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import type {VoiceDeviceState} from '@app/features/voice/utils/VoiceDeviceManager';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import {makeAutoObservable, runInAction} from 'mobx';
import type React from 'react';

const NEW_AUDIO_DEVICE_DETECTED_DESCRIPTOR = msg({
	message: 'New audio device detected!',
	comment: 'Body text in the authentication new device monitoring. Keep the tone plain and specific.',
});
const SWITCH_DEVICE_DESCRIPTOR = msg({
	message: 'Switch device',
	comment: 'Short label in the authentication new device monitoring. Keep the tone plain and specific.',
});
const NOT_NOW_DESCRIPTOR = msg({
	message: 'Not now',
	comment: 'Short label in the authentication new device monitoring. Keep the tone plain and specific.',
});
const logger = new Logger('NewDeviceMonitoring');

interface IgnoreDeviceLinkProps {
	deviceName: string;
	onClick: () => void;
	checked?: boolean;
	onChange?: (checked: boolean) => void;
}

const IgnoreDeviceLink: React.FC<IgnoreDeviceLinkProps> = ({deviceName, onClick}) => (
	<button
		type="button"
		className={styles.ignoreDeviceLink}
		onClick={onClick}
		data-flx="auth.new-device-monitoring.ignore-device-link.ignore-device-link.click.button"
	>
		<Trans>Don't suggest {deviceName} again</Trans>
	</button>
);

class NewDeviceMonitoring {
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
		this.unsubscribe = VoiceDevicePermissionState.subscribe(this.handleDeviceStateChange);
	}

	private async refreshDeviceSnapshot(): Promise<void> {
		try {
			await VoiceDevicePermissionState.ensureDevices({requestPermissions: false});
		} catch (error) {
			logger.warn('Failed to refresh devices for new-device monitoring startup', {error});
		}
	}

	async start(): Promise<void> {
		if (this.startPromise) return this.startPromise;
		this.isStarted = true;
		const epoch = ++this.startEpoch;
		this.startPromise = (async () => {
			await makePersistent(this, 'NewDeviceMonitoring', ['knownDeviceIds', 'ignoredDeviceIds', 'suppressAlerts']);
			if (!this.isStarted || epoch !== this.startEpoch) return;
			await this.refreshDeviceSnapshot();
			if (!this.isStarted || epoch !== this.startEpoch) return;
			this.startMonitoring();
		})();
		return this.startPromise;
	}

	private handleDeviceStateChange(state: VoiceDeviceState): void {
		if (!this.isStarted) return;
		if (state.permissionStatus.audio !== 'granted') {
			return;
		}
		if (this.suppressAlerts) {
			return;
		}
		const currentInputIds = state.inputDevices.map((d) => d.deviceId);
		const currentOutputIds = state.outputDevices.map((d) => d.deviceId);
		const allCurrentIds = [...currentInputIds, ...currentOutputIds];
		if (!this.isInitialized) {
			const promptCandidates =
				this.knownDeviceIds.length > 0
					? getNewDevicePromptCandidates(state, this.knownDeviceIds, this.ignoredDeviceIds, {
							inputDeviceId: VoiceSettings.getInputDeviceId(),
							outputDeviceId: VoiceSettings.getOutputDeviceId(),
						})
					: [];
			runInAction(() => {
				if (promptCandidates.length > 0) {
					this.pendingPrompts.push(...promptCandidates);
				}
				this.knownDeviceIds = [...new Set([...this.knownDeviceIds, ...allCurrentIds])];
				this.isInitialized = true;
			});
			logger.debug('Initialized with known devices', {
				count: this.knownDeviceIds.length,
				promptCount: promptCandidates.length,
			});
			if (promptCandidates.length > 0) {
				this.processNextPrompt();
			}
			return;
		}
		const promptCandidates = getNewDevicePromptCandidates(state, this.knownDeviceIds, this.ignoredDeviceIds, {
			inputDeviceId: VoiceSettings.getInputDeviceId(),
			outputDeviceId: VoiceSettings.getOutputDeviceId(),
		});
		runInAction(() => {
			if (promptCandidates.length > 0) {
				this.pendingPrompts.push(...promptCandidates);
			}
			this.knownDeviceIds = [...new Set([...this.knownDeviceIds, ...allCurrentIds])];
		});
		if (promptCandidates.length > 0) {
			logger.debug('New devices detected', {
				devices: promptCandidates.map((prompt) => ({
					deviceIds: prompt.deviceIds,
					deviceName: prompt.deviceName,
					deviceType: prompt.deviceType,
				})),
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
			throw new Error('NewDeviceMonitoring: i18n not initialized');
		}
		const i18n = this.i18n;
		const {deviceIds, deviceName, deviceType, inputDeviceId, outputDeviceId} = prompt;
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(NEW_AUDIO_DEVICE_DETECTED_DESCRIPTOR)}
					description={
						deviceType === 'input' ? (
							<Trans>
								{PRODUCT_NAME} has found a new audio input device named{' '}
								<strong data-flx="auth.new-device-monitoring.strong">{deviceName}</strong>. Do you want to switch to it?
							</Trans>
						) : deviceType === 'output' ? (
							<Trans>
								{PRODUCT_NAME} has found a new audio output device named{' '}
								<strong data-flx="auth.new-device-monitoring.strong--2">{deviceName}</strong>. Do you want to switch to
								it?
							</Trans>
						) : (
							<Trans>
								{PRODUCT_NAME} has found a new audio device named{' '}
								<strong data-flx="auth.new-device-monitoring.strong--3">{deviceName}</strong>. Do you want to switch to
								it?
							</Trans>
						)
					}
					primaryText={i18n._(SWITCH_DEVICE_DESCRIPTOR)}
					primaryVariant="primary"
					secondaryText={i18n._(NOT_NOW_DESCRIPTOR)}
					checkboxContent={
						<IgnoreDeviceLink
							deviceName={deviceName}
							onClick={() => {
								this.addToIgnored(deviceIds);
								ModalCommands.pop();
								setTimeout(() => this.onModalClosed(), 0);
							}}
							data-flx="auth.new-device-monitoring.ignore-device-link.add-to-ignored"
						/>
					}
					onPrimary={(dontAskAgain) => {
						if (inputDeviceId !== undefined && outputDeviceId !== undefined) {
							VoiceSettings.updateSettings({inputDeviceId, outputDeviceId});
						} else if (inputDeviceId !== undefined) {
							VoiceSettings.updateSettings({inputDeviceId});
						} else if (outputDeviceId !== undefined) {
							VoiceSettings.updateSettings({outputDeviceId});
						}
						if (dontAskAgain) {
							this.addToIgnored(deviceIds);
						}
						setTimeout(() => this.onModalClosed(), 0);
					}}
					onSecondary={(dontAskAgain) => {
						if (dontAskAgain) {
							this.addToIgnored(deviceIds);
						}
						setTimeout(() => this.onModalClosed(), 0);
					}}
					data-flx="auth.new-device-monitoring.confirm-modal"
				/>
			)),
		);
	}

	private onModalClosed(): void {
		this.isShowingPrompt = false;
		this.processNextPrompt();
	}

	private addToIgnored(deviceIds: string | ReadonlyArray<string>): void {
		const ids = typeof deviceIds === 'string' ? [deviceIds] : deviceIds;
		const newDeviceIds = ids.filter((deviceId) => !this.ignoredDeviceIds.includes(deviceId));
		if (newDeviceIds.length > 0) {
			runInAction(() => {
				this.ignoredDeviceIds.push(...newDeviceIds);
			});
			logger.debug('Added device to ignore list', {deviceIds: newDeviceIds});
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
			deviceIds: ['test-device-id'],
			deviceName: 'Test Audio Device',
			deviceType: 'input',
			inputDeviceId: 'test-device-id',
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

export default new NewDeviceMonitoring();
