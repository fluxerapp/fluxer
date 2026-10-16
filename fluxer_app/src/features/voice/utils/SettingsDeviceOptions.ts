// SPDX-License-Identifier: AGPL-3.0-or-later

import {CAMERA_DESCRIPTOR as CAMERA_DEVICE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {hasDeviceLabels, type VoiceDeviceState} from '@app/features/voice/utils/VoiceDeviceManager';
import {formatVoiceAudioDeviceLabel} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {
	DEFAULT_3_DESCRIPTOR as DEFAULT_DEVICE_DESCRIPTOR,
	MICROPHONE_DESCRIPTOR as MICROPHONE_DEVICE_DESCRIPTOR,
	SPEAKER_2_DESCRIPTOR as SPEAKER_DEVICE_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceSharedMessageDescriptors';
import type {I18n} from '@lingui/core';

export type SettingsDeviceKind = 'audioinput' | 'audiooutput' | 'videoinput';

function devicesForKind(deviceState: VoiceDeviceState, kind: SettingsDeviceKind): Array<MediaDeviceInfo> {
	switch (kind) {
		case 'audioinput':
			return deviceState.inputDevices;
		case 'audiooutput':
			return deviceState.outputDevices;
		case 'videoinput':
			return deviceState.videoDevices;
	}
}

function buildAudioDeviceOptions(
	devices: ReadonlyArray<MediaDeviceInfo>,
	fallbackLabel: string,
	i18n: I18n,
): Array<ComboboxOption> {
	return devices.map((device) => ({
		value: device.deviceId,
		label: formatVoiceAudioDeviceLabel(i18n, device, fallbackLabel),
	}));
}

function buildVideoDeviceOptions(
	devices: ReadonlyArray<MediaDeviceInfo>,
	defaultLabel: string,
	fallbackLabel: string,
): Array<ComboboxOption> {
	return devices.map((device) => ({
		value: device.deviceId,
		label: device.deviceId === 'default' ? defaultLabel : device.label || fallbackLabel,
	}));
}

export function buildSettingsDeviceOptions(
	deviceState: VoiceDeviceState,
	kind: SettingsDeviceKind,
	i18n: I18n,
): Array<ComboboxOption> {
	const defaultLabel = i18n._(DEFAULT_DEVICE_DESCRIPTOR);
	const lockedFallback: Array<ComboboxOption> = [{value: 'default', label: defaultLabel}];
	const devices = devicesForKind(deviceState, kind);
	if (devices.length === 0) {
		return lockedFallback;
	}
	if (kind === 'videoinput') {
		return buildVideoDeviceOptions(devices, defaultLabel, i18n._(CAMERA_DEVICE_DESCRIPTOR));
	}
	if (!hasDeviceLabels(devices)) {
		return lockedFallback;
	}
	const fallbackLabel = i18n._(kind === 'audioinput' ? MICROPHONE_DEVICE_DESCRIPTOR : SPEAKER_DEVICE_DESCRIPTOR);
	return buildAudioDeviceOptions(devices, fallbackLabel, i18n);
}
