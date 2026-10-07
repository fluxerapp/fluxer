// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import type {DomainMigrationMediaDeviceKind} from '@app/features/app/domain_migration/DomainMigrationCore';
import {remapMigratedDeviceIds} from '@app/features/app/domain_migration/DomainMigrationDeviceRemap';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/auth/state/NewDeviceMonitoring.module.css';
import {
	getNewDevicePromptCandidates,
	getRealDeviceIds,
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
import {msg, ph} from '@lingui/core/macro';
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
const NEW_DEVICE_MODAL_KEY = 'new-audio-device';
const MAX_PROMPTABLE_NEW_DEVICE_GROUPS = 2;
const MIGRATED_AUDIO_DEVICE_KINDS: ReadonlyArray<DomainMigrationMediaDeviceKind> = ['audioinput', 'audiooutput'];
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
			await makePersistent(this, 'NewDeviceMonitoring', ['knownDeviceIds', 'ignoredDeviceIds', 'suppressAlerts'], {
				syncAcrossTabs: true,
			});
			if (!this.isStarted || epoch !== this.startEpoch) return;
			await this.refreshDeviceSnapshot();
			if (!this.isStarted || epoch !== this.startEpoch) return;
			this.startMonitoring();
		})();
		return this.startPromise;
	}

	private handleDeviceStateChange(state: VoiceDeviceState): void {
		if (!this.isStarted) return;
		const ignoredDeviceIds = remapMigratedDeviceIds(this.ignoredDeviceIds, MIGRATED_AUDIO_DEVICE_KINDS);
		if (ignoredDeviceIds !== this.ignoredDeviceIds) {
			runInAction(() => {
				this.ignoredDeviceIds = ignoredDeviceIds;
			});
		}
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
			const knownDeviceIdSet = new Set(this.knownDeviceIds);
			const realDeviceIds = getRealDeviceIds(state);
			const isFreshIdSpace =
				realDeviceIds.length > 0 && !realDeviceIds.some((deviceId) => knownDeviceIdSet.has(deviceId));
			const knownDeviceIds = isFreshIdSpace
				? this.knownDeviceIds
				: remapMigratedDeviceIds(this.knownDeviceIds, MIGRATED_AUDIO_DEVICE_KINDS);
			const promptCandidates = isFreshIdSpace
				? []
				: getNewDevicePromptCandidates(state, knownDeviceIds, this.ignoredDeviceIds, {
						inputDeviceId: VoiceSettings.getInputDeviceId(),
						outputDeviceId: VoiceSettings.getOutputDeviceId(),
					});
			runInAction(() => {
				this.knownDeviceIds = isFreshIdSpace
					? [...new Set(allCurrentIds)]
					: [...new Set([...knownDeviceIds, ...allCurrentIds])];
				if (ignoredDeviceIds !== this.ignoredDeviceIds) {
					this.ignoredDeviceIds = ignoredDeviceIds;
				}
				this.isInitialized = true;
			});
			logger.debug('Initialized with known devices', {
				count: this.knownDeviceIds.length,
				isFreshIdSpace,
				promptCount: promptCandidates.length,
			});
			this.promptForNewDevice(promptCandidates);
			return;
		}
		const promptCandidates = getNewDevicePromptCandidates(state, this.knownDeviceIds, this.ignoredDeviceIds, {
			inputDeviceId: VoiceSettings.getInputDeviceId(),
			outputDeviceId: VoiceSettings.getOutputDeviceId(),
		});
		runInAction(() => {
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
		}
		this.promptForNewDevice(promptCandidates);
	}

	private promptForNewDevice(promptCandidates: ReadonlyArray<PendingDevicePrompt>): void {
		if (promptCandidates.length === 0 || promptCandidates.length > MAX_PROMPTABLE_NEW_DEVICE_GROUPS) {
			return;
		}
		this.showNewDeviceModal(
			promptCandidates.find((prompt) => prompt.inputDeviceId !== undefined) ?? promptCandidates[0],
		);
	}

	private showNewDeviceModal(prompt: PendingDevicePrompt): void {
		if (!this.i18n) {
			throw new Error('NewDeviceMonitoring: i18n not initialized');
		}
		const i18n = this.i18n;
		const {deviceIds, deviceName, deviceType, inputDeviceId, outputDeviceId} = prompt;
		ModalCommands.pushWithKey(
			modal(() => (
				<ConfirmModal
					title={i18n._(NEW_AUDIO_DEVICE_DETECTED_DESCRIPTOR)}
					description={
						deviceType === 'input' ? (
							<Trans>
								{ph({PRODUCT_NAME: RuntimeConfig.productName})} has found a new audio input device named{' '}
								<strong data-flx="auth.new-device-monitoring.strong">{deviceName}</strong>. Do you want to switch to it?
							</Trans>
						) : deviceType === 'output' ? (
							<Trans>
								{ph({PRODUCT_NAME: RuntimeConfig.productName})} has found a new audio output device named{' '}
								<strong data-flx="auth.new-device-monitoring.strong--2">{deviceName}</strong>. Do you want to switch to
								it?
							</Trans>
						) : (
							<Trans>
								{ph({PRODUCT_NAME: RuntimeConfig.productName})} has found a new audio device named{' '}
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
								ModalCommands.popWithKey(NEW_DEVICE_MODAL_KEY);
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
					}}
					onSecondary={(dontAskAgain) => {
						if (dontAskAgain) {
							this.addToIgnored(deviceIds);
						}
					}}
					data-flx="auth.new-device-monitoring.confirm-modal"
				/>
			)),
			NEW_DEVICE_MODAL_KEY,
		);
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
		if (this.unsubscribe) {
			this.unsubscribe();
			this.unsubscribe = null;
		}
		this.isInitialized = false;
	}
}

export default new NewDeviceMonitoring();
