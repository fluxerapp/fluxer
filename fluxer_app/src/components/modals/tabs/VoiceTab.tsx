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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as VoiceSettingsActionCreators from '~/actions/VoiceSettingsActionCreators';
import {Select} from '~/components/form/Select';
import {Switch} from '~/components/form/Switch';
import {KeybindRecorder} from '~/components/keybinds/KeybindRecorder';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {InputMonitoringCTAModal} from '~/components/modals/InputMonitoringCTAModal';
import {Button} from '~/components/uikit/Button/Button';
import {RadioGroup, type RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {Slider} from '~/components/uikit/Slider';
import {WarningAlert} from '~/components/uikit/WarningAlert/WarningAlert';
import KeybindStore, {getDefaultKeybind} from '~/stores/KeybindStore';
import NativePermissionStore from '~/stores/NativePermissionStore';
import NewDeviceMonitoringStore from '~/stores/NewDeviceMonitoringStore';
import type VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import {openExternalUrl} from '~/utils/NativeUtils';
import {hasDeviceLabels, resolveEffectiveDeviceId} from '~/utils/VoiceDeviceManager';
import {EntranceSoundSection} from './components/EntranceSoundSection';
import {MicTestSection} from './components/MicTestSection';
import {useMediaPermission} from './hooks/useMediaPermission';
import styles from './VoiceTab.module.css';

type TransmitMode = 'voice_activity' | 'push_to_talk';

interface VoiceTabProps {
	voiceSettings: typeof VoiceSettingsStore;
	hasPremium: boolean;
	autoRequestPermission?: boolean;
}

export const VoiceTab: React.FC<VoiceTabProps> = observer(
	({voiceSettings, hasPremium, autoRequestPermission = true}) => {
		const {t, i18n} = useLingui();
		const {
			inputDeviceId,
			outputDeviceId,
			inputVolume,
			outputVolume,
			echoCancellation,
			noiseSuppression,
			autoGainControl,
		} = voiceSettings;

		const {
			devices,
			status: permissionStatus,
			requestPermission,
		} = useMediaPermission('audio', {
			autoRequest: autoRequestPermission,
		});

		const isNativeDesktop = NativePermissionStore.isDesktop;
		const isNativeMac = NativePermissionStore.isMacOS;
		const inputMonitoringGranted = NativePermissionStore.isInputMonitoringGranted;

		const inputDevices = React.useMemo(() => devices.filter((d) => d.kind === 'audioinput'), [devices]);
		const outputDevices = React.useMemo(() => devices.filter((d) => d.kind === 'audiooutput'), [devices]);
		const transmitMode = KeybindStore.transmitMode;
		const isPushToTalk = transmitMode === 'push_to_talk';
		const pttKeybind = KeybindStore.getByAction('push_to_talk');
		const pttReleaseDelay = KeybindStore.pushToTalkReleaseDelay;
		const pttLatching = KeybindStore.pushToTalkLatching;

		const isPttLimited = !isNativeDesktop || (isNativeMac && !inputMonitoringGranted);
		const defaultPttCombo = getDefaultKeybind('push_to_talk', i18n);

		const inputHasLabels = hasDeviceLabels(inputDevices);
		const outputHasLabels = hasDeviceLabels(outputDevices);

		const effectiveInputDeviceId = resolveEffectiveDeviceId(inputDeviceId, inputDevices) ?? 'default';
		const effectiveOutputDeviceId = resolveEffectiveDeviceId(outputDeviceId, outputDevices) ?? 'default';

		const inputDeviceOptions =
			inputDevices.length > 0 && inputHasLabels
				? inputDevices.map((device) => {
						const shortDeviceId = device.deviceId.slice(0, 8);
						return {
							value: device.deviceId,
							label: device.label || t`Microphone ${shortDeviceId}`,
						};
					})
				: [{value: 'default', label: t`Default`}];

		const outputDeviceOptions =
			outputDevices.length > 0 && outputHasLabels
				? outputDevices.map((device) => {
						const shortDeviceId = device.deviceId.slice(0, 8);
						return {
							value: device.deviceId,
							label: device.label || t`Speaker ${shortDeviceId}`,
						};
					})
				: [{value: 'default', label: t`Default`}];

		const transmitModeOptions: Array<RadioOption<TransmitMode>> = [
			{
				value: 'voice_activity',
				name: t`Voice Activity`,
				desc: t`Automatically transmit when you speak`,
			},
			{
				value: 'push_to_talk',
				name: isPttLimited ? t`Push-to-Talk (Limited)` : t`Push-to-Talk`,
				desc: isPttLimited
					? t`Hold a key to transmit — only works when the app has focus`
					: t`Hold a key to transmit — works system-wide`,
			},
		];

		const handleTransmitModeChange = (mode: TransmitMode) => {
			if (mode === 'push_to_talk' && !isNativeDesktop) {
				ModalActionCreators.push(
					modal(() => (
						<ConfirmModal
							title={t`Push-to-Talk (Limited)`}
							description={
								<p>
									<Trans>
										In your browser, Push-to-Talk will only work when the Fluxer tab is focused. For system-wide
										Push-to-Talk that works even when gaming or using other apps, download the desktop app.
									</Trans>
								</p>
							}
							primaryText={t`Download Desktop App`}
							primaryVariant="primary"
							secondaryText={t`I understand`}
							onPrimary={() => {
								void openExternalUrl('https://fluxer.app/download');
							}}
							onSecondary={() => {
								KeybindStore.setTransmitMode(mode);
								MediaEngineStore.handlePushToTalkModeChange();
							}}
						/>
					)),
				);
			} else if (mode === 'push_to_talk' && isNativeMac && !inputMonitoringGranted) {
				ModalActionCreators.push(
					modal(() => (
						<InputMonitoringCTAModal
							onComplete={() => {
								void NativePermissionStore.recheckInputMonitoring();
								KeybindStore.setTransmitMode(mode);
								MediaEngineStore.handlePushToTalkModeChange();
							}}
						/>
					)),
				);
			} else {
				KeybindStore.setTransmitMode(mode);
				MediaEngineStore.handlePushToTalkModeChange();
			}
		};

		const handleOpenInputMonitoringModal = () => {
			ModalActionCreators.push(
				modal(() => (
					<InputMonitoringCTAModal
						onComplete={() => {
							void NativePermissionStore.recheckInputMonitoring();
						}}
					/>
				)),
			);
		};

		return (
			<>
				{devices.length === 0 && permissionStatus !== 'loading' && permissionStatus !== 'granted' ? (
					<div className={styles.deviceNotice}>
						<div className={styles.deviceNoticeText}>
							<div className={styles.deviceNoticeTitle}>
								<Trans>No microphones detected</Trans>
							</div>
							<p className={styles.deviceNoticeDescription}>
								{permissionStatus === 'denied' ? (
									isNativeDesktop ? (
										<Trans>
											Allow Fluxer to access your microphone in System Settings → Privacy &amp; Security → Microphone.
										</Trans>
									) : (
										<Trans>
											Allow Fluxer to access your microphone. Check your browser address bar or settings to enable
											permissions.
										</Trans>
									)
								) : (
									<Trans>Fluxer needs access to list the available microphones and speakers.</Trans>
								)}
							</p>
						</div>
						<Button
							variant="secondary"
							small={true}
							onClick={() => {
								void requestPermission();
							}}
						>
							<Trans>Allow Microphone</Trans>
						</Button>
					</div>
				) : null}

				<div className={styles.inputModeSection}>
					<div className={styles.inputModeLabel}>
						<Trans>Input Mode</Trans>
					</div>
					<div className={styles.inputModeCard}>
						<RadioGroup
							options={transmitModeOptions}
							value={transmitMode}
							onChange={handleTransmitModeChange}
							aria-label={t`Select input mode for voice transmission`}
						/>

						{isPushToTalk && (
							<div className={styles.pttSettings}>
								<div className={styles.pttSettingRow}>
									<div className={styles.pttSettingLabel}>
										<Trans>Shortcut</Trans>
									</div>
									<div className={styles.pttKeybindRow}>
										<KeybindRecorder
											action="push_to_talk"
											value={pttKeybind.combo}
											defaultValue={defaultPttCombo}
											disabled={isPttLimited}
											onChange={(combo) => {
												KeybindStore.setKeybind('push_to_talk', {
													...combo,
													global: pttKeybind.combo.global,
												});
											}}
											onReset={() => {
												if (defaultPttCombo) {
													KeybindStore.setKeybind('push_to_talk', {
														...defaultPttCombo,
														global: pttKeybind.combo.global,
													});
												}
											}}
										/>
									</div>
								</div>

								<div className={styles.pttSettingRow}>
									<div className={styles.pttSettingLabel}>
										<Trans>Push-to-Talk Release Delay</Trans>
									</div>
									<Slider
										defaultValue={pttReleaseDelay}
										factoryDefaultValue={20}
										minValue={20}
										maxValue={2000}
										step={10}
										markers={[20, 500, 1000, 1500, 2000]}
										stickToMarkers={false}
										onMarkerRender={(value) => `${value}ms`}
										onValueRender={(value) => <Trans>{value}ms</Trans>}
										onValueChange={(value) => KeybindStore.setPushToTalkReleaseDelay(value)}
									/>
								</div>

								<div className={styles.pttLatchingRow}>
									<div className={styles.pttLatchingText}>
										<div className={styles.pttSettingLabel}>
											<Trans>Push-to-Talk Latching</Trans>
										</div>
										<p className={styles.pttSettingDescription}>
											<Trans>
												When enabled, quickly tapping your push-to-talk shortcut will keep your microphone on until
												pressed again.
											</Trans>
										</p>
									</div>
									<Switch
										value={pttLatching}
										onChange={(value) => KeybindStore.setPushToTalkLatching(value)}
										ariaLabel={t`Push-to-Talk Latching`}
									/>
								</div>
							</div>
						)}

						{isPttLimited && (
							<WarningAlert
								link={
									!isNativeDesktop
										? {
												label: <Trans>Download the desktop app for system-wide Push-to-Talk</Trans>,
												onClick: () => void openExternalUrl('https://fluxer.app/download'),
											}
										: {
												label: <Trans>Enable Input Monitoring permission</Trans>,
												onClick: handleOpenInputMonitoringModal,
											}
								}
							>
								{!isNativeDesktop ? (
									<Trans>Push-to-Talk will only work when the browser tab has focus.</Trans>
								) : (
									<Trans>
										Push-to-Talk will only work when the app has focus until you enable Input Monitoring permission. To
										use any key or mouse button for PTT, enable it below.
									</Trans>
								)}
							</WarningAlert>
						)}
					</div>
				</div>

				<div>
					<Select
						label={t`Input Device`}
						value={effectiveInputDeviceId}
						options={inputDeviceOptions}
						onChange={(value) => VoiceSettingsActionCreators.update({inputDeviceId: value})}
					/>
				</div>

				<div>
					<Select
						label={t`Output Device`}
						value={effectiveOutputDeviceId}
						options={outputDeviceOptions}
						onChange={(value) => VoiceSettingsActionCreators.update({outputDeviceId: value})}
					/>
				</div>

				<Switch
					label={<Trans>Show new device alerts</Trans>}
					description={<Trans>Get prompted when new audio devices are connected</Trans>}
					value={!NewDeviceMonitoringStore.suppressAlerts}
					onChange={(value) => NewDeviceMonitoringStore.setSuppressAlerts(!value)}
				/>

				<div>
					<div className={styles.sliderLabel}>
						<Trans>Input Volume</Trans>
					</div>
					<Slider
						defaultValue={inputVolume}
						factoryDefaultValue={100}
						minValue={0}
						maxValue={100}
						step={1}
						markers={[0, 25, 50, 75, 100]}
						stickToMarkers={false}
						onMarkerRender={(value) => `${value}%`}
						onValueRender={(value) => <Trans>{value}%</Trans>}
						onValueChange={(value) => VoiceSettingsActionCreators.update({inputVolume: value})}
					/>
				</div>

				<div>
					<div className={styles.sliderLabel}>
						<Trans>Output Volume</Trans>
					</div>
					<Slider
						defaultValue={outputVolume}
						factoryDefaultValue={100}
						minValue={0}
						maxValue={100}
						step={1}
						markers={[0, 25, 50, 75, 100]}
						stickToMarkers={false}
						onMarkerRender={(value) => `${value}%`}
						onValueRender={(value) => <Trans>{value}%</Trans>}
						onValueChange={(value) => VoiceSettingsActionCreators.update({outputVolume: value})}
					/>
				</div>

				<div className={styles.audioProcessing}>
					<div className={styles.audioProcessingLabel}>
						<Trans>Audio Processing</Trans>
					</div>
					<div className={styles.audioProcessingCard}>
						<p className={styles.audioProcessingDescription}>
							<Trans>
								These settings use browser-native audio processing to improve call quality. For best results, keep all
								options enabled.
							</Trans>
						</p>
						<div className={styles.audioProcessingOptions}>
							<Switch
								label={<Trans>Echo Cancellation</Trans>}
								description={<Trans>Reduces echo and feedback from speakers</Trans>}
								value={echoCancellation}
								onChange={(value) => VoiceSettingsActionCreators.update({echoCancellation: value})}
							/>
							<Switch
								label={<Trans>Noise Suppression</Trans>}
								description={<Trans>Filters out background noise like fans and keyboard typing</Trans>}
								value={noiseSuppression}
								onChange={(value) => VoiceSettingsActionCreators.update({noiseSuppression: value})}
							/>
							<Switch
								label={<Trans>Auto Gain Control</Trans>}
								description={<Trans>Automatically adjusts microphone volume for consistent levels</Trans>}
								value={autoGainControl}
								onChange={(value) => VoiceSettingsActionCreators.update({autoGainControl: value})}
							/>
						</div>
					</div>
				</div>

				<MicTestSection settings={voiceSettings} />

				<EntranceSoundSection hasPremium={hasPremium} />
			</>
		);
	},
);
