// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getVoiceAudioDeviceMetadata,
	resolveEffectiveDeviceId,
	type VoiceDeviceState,
} from '@app/features/voice/utils/VoiceDeviceManager';

export type DeviceType = 'input' | 'output' | 'input_output';

export interface PendingDevicePrompt {
	deviceIds: Array<string>;
	deviceName: string;
	deviceType: DeviceType;
	inputDeviceId?: string;
	outputDeviceId?: string;
}

export interface DevicePromptSelection {
	inputDeviceId: string;
	outputDeviceId: string;
}

interface PendingDevicePromptAccumulator {
	deviceIds: Set<string>;
	deviceName: string;
	inputDeviceId?: string;
	outputDeviceId?: string;
}

interface PromptableDeviceContext {
	effectiveDeviceId: string | null;
	defaultEndpointLabel: string;
	knownDeviceIds: ReadonlySet<string>;
	ignoredDeviceIds: ReadonlySet<string>;
}

function getDevicePromptKey(device: MediaDeviceInfo): string {
	const groupId = device.groupId?.trim() ?? '';
	if (groupId.length > 0) {
		return `group:${groupId}`;
	}
	return `device:${device.deviceId}`;
}

function getDefaultRouteEndpointLabel(devices: ReadonlyArray<MediaDeviceInfo>): string {
	const defaultRoute = devices.find((device) => device.deviceId === 'default');
	if (!defaultRoute) {
		return '';
	}
	const metadata = getVoiceAudioDeviceMetadata(defaultRoute);
	return metadata?.endpointLabel || defaultRoute.label.trim();
}

function isVirtualRouteDevice(device: MediaDeviceInfo): boolean {
	if (device.deviceId === 'default' || device.deviceId === 'communications') {
		return true;
	}
	return getVoiceAudioDeviceMetadata(device) !== null;
}

export function getRealDeviceIds(state: VoiceDeviceState): Array<string> {
	return [...state.inputDevices, ...state.outputDevices]
		.filter((device) => !isVirtualRouteDevice(device) && device.deviceId.trim().length > 0)
		.map((device) => device.deviceId);
}

function isPromptableDevice(device: MediaDeviceInfo, context: PromptableDeviceContext): boolean {
	if (isVirtualRouteDevice(device)) {
		return false;
	}
	if (device.deviceId.trim().length === 0 || device.label.trim().length === 0) {
		return false;
	}
	if (device.deviceId === context.effectiveDeviceId) {
		return false;
	}
	if (context.effectiveDeviceId === 'default' && device.label.trim() === context.defaultEndpointLabel) {
		return false;
	}
	return !context.knownDeviceIds.has(device.deviceId) && !context.ignoredDeviceIds.has(device.deviceId);
}

function getDeviceType(candidate: PendingDevicePromptAccumulator): DeviceType {
	if (candidate.inputDeviceId !== undefined && candidate.outputDeviceId !== undefined) {
		return 'input_output';
	}
	if (candidate.inputDeviceId !== undefined) {
		return 'input';
	}
	return 'output';
}

export function getNewDevicePromptCandidates(
	state: VoiceDeviceState,
	knownDeviceIds: ReadonlyArray<string>,
	ignoredDeviceIds: ReadonlyArray<string>,
	selection: DevicePromptSelection,
): Array<PendingDevicePrompt> {
	const knownDeviceIdSet = new Set(knownDeviceIds);
	const ignoredDeviceIdSet = new Set(ignoredDeviceIds);
	const inputContext: PromptableDeviceContext = {
		effectiveDeviceId: resolveEffectiveDeviceId(selection.inputDeviceId, state.inputDevices),
		defaultEndpointLabel: getDefaultRouteEndpointLabel(state.inputDevices),
		knownDeviceIds: knownDeviceIdSet,
		ignoredDeviceIds: ignoredDeviceIdSet,
	};
	const outputContext: PromptableDeviceContext = {
		effectiveDeviceId: resolveEffectiveDeviceId(selection.outputDeviceId, state.outputDevices),
		defaultEndpointLabel: getDefaultRouteEndpointLabel(state.outputDevices),
		knownDeviceIds: knownDeviceIdSet,
		ignoredDeviceIds: ignoredDeviceIdSet,
	};
	const candidates = new Map<string, PendingDevicePromptAccumulator>();
	const candidatesByDeviceId = new Map<string, PendingDevicePromptAccumulator>();

	const addDevice = (device: MediaDeviceInfo, deviceType: 'input' | 'output'): void => {
		const context = deviceType === 'input' ? inputContext : outputContext;
		if (!isPromptableDevice(device, context)) {
			return;
		}
		const key = getDevicePromptKey(device);
		const candidate =
			candidatesByDeviceId.get(device.deviceId) ??
			candidates.get(key) ??
			(() => {
				const newCandidate: PendingDevicePromptAccumulator = {
					deviceIds: new Set(),
					deviceName: device.label,
				};
				candidates.set(key, newCandidate);
				return newCandidate;
			})();
		candidate.deviceIds.add(device.deviceId);
		candidatesByDeviceId.set(device.deviceId, candidate);
		if (deviceType === 'input') {
			candidate.inputDeviceId ??= device.deviceId;
		} else {
			candidate.outputDeviceId ??= device.deviceId;
		}
	};

	for (const device of state.inputDevices) {
		addDevice(device, 'input');
	}
	for (const device of state.outputDevices) {
		addDevice(device, 'output');
	}

	return [...candidates.values()].map((candidate) => ({
		deviceIds: [...candidate.deviceIds],
		deviceName: candidate.deviceName,
		deviceType: getDeviceType(candidate),
		inputDeviceId: candidate.inputDeviceId,
		outputDeviceId: candidate.outputDeviceId,
	}));
}
