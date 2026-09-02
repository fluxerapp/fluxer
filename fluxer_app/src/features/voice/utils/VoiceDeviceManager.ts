// SPDX-License-Identifier: AGPL-3.0-or-later

import {ensureNativePermission} from '@app/features/permissions/system/utils/NativePermissions';
import {Platform} from '@app/features/platform/types/Platform';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getNativePlatformSync, isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {VoiceEngineV2AudioDeviceRole} from '@fluxer/voice_engine_v2';

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

export type VoiceMediaPermissionType = 'audio' | 'video';
export type VoiceMediaPermissionStatus = 'idle' | 'loading' | 'granted' | 'denied';

const MAX_ENUMERATION_CHAIN_PASSES = 3;

export interface EnsureVoiceDevicesOptions {
	requestPermissions?: boolean;
	requestPermissionTypes?: ReadonlyArray<VoiceMediaPermissionType>;
	forceRefresh?: boolean;
}

export interface VoiceDeviceState {
	inputDevices: Array<MediaDeviceInfo>;
	outputDevices: Array<MediaDeviceInfo>;
	videoDevices: Array<MediaDeviceInfo>;
	permissionStatus: Record<VoiceMediaPermissionType, VoiceMediaPermissionStatus>;
}

type Listener = (state: VoiceDeviceState) => void;

const INTERNAL_VIRTUAL_AUDIO_DEVICE_LABELS = [
	'fluxer-screen-share',
	'fluxer screen share',
	'fluxer-direct-capture',
	'fluxer direct capture',
];

export function isInternalVirtualAudioDevice(device: MediaDeviceInfo): boolean {
	const label = device.label.trim().toLowerCase();
	return INTERNAL_VIRTUAL_AUDIO_DEVICE_LABELS.some((internalLabel) => label.includes(internalLabel));
}

const sortDevices = (devices: Array<MediaDeviceInfo>): Array<MediaDeviceInfo> => {
	return [...devices].sort((a, b) => {
		const aMetadata = getVoiceAudioDeviceMetadata(a);
		const bMetadata = getVoiceAudioDeviceMetadata(b);
		const aIsDefaultRoute = a.deviceId === 'default' || aMetadata?.role === 'default';
		const bIsDefaultRoute = b.deviceId === 'default' || bMetadata?.role === 'default';
		if (aIsDefaultRoute && !bIsDefaultRoute) return -1;
		if (!aIsDefaultRoute && bIsDefaultRoute) return 1;
		const aIsDefault = aIsDefaultRoute || (a as {isDefault?: boolean}).isDefault === true;
		const bIsDefault = bIsDefaultRoute || (b as {isDefault?: boolean}).isDefault === true;
		if (aIsDefault && !bIsDefault) return -1;
		if (!aIsDefault && bIsDefault) return 1;
		return a.label.localeCompare(b.label);
	});
};

export type VoiceAudioDefaultDevicePlatform = 'windows' | 'macos' | 'linux' | 'browser';
type VoiceAudioDeviceRole = 'default' | 'communications';

export interface VoiceAudioDeviceMetadata {
	role: VoiceAudioDeviceRole;
	endpointLabel: string;
	defaultPlatform?: VoiceAudioDefaultDevicePlatform;
}

export type VoiceMediaDeviceInfo = MediaDeviceInfo & {
	isDefault?: boolean;
	fluxerVoiceAudioDevice?: VoiceAudioDeviceMetadata;
};

interface NormalizedAudioDeviceLabel {
	role: VoiceAudioDeviceRole | null;
	endpointLabel: string;
	label: string;
	isDefaultRoute: boolean;
}

interface AudioDeviceShapeInput {
	deviceId: string;
	groupId: string;
	kind: MediaDeviceKind;
	label: string;
	isDefault?: boolean;
	role?: VoiceEngineV2AudioDeviceRole;
	endpointLabel?: string;
	isDefaultRoute?: boolean;
}

interface ShapeAudioDevicesOptions {
	synthesizeDefaultRoute?: boolean;
}

const WINDOWS_DEFAULT_DEVICE_PREFIX = /^Default\s*-\s*(.+)$/i;
const WINDOWS_COMMUNICATIONS_DEVICE_PREFIX = /^Communications\s*-\s*(.+)$/i;
const ADM_DEFAULT_DEVICE_WRAPPER = /^default\s*\((.+)\)$/i;
const USB_HARDWARE_ID_SUFFIX = /\s*\([0-9a-f]{4}:[0-9a-f]{4}\)\s*$/i;
const USB_HARDWARE_ID_LABEL = /^[0-9a-f]{4}:[0-9a-f]{4}$/i;
const UUID_DEVICE_LABEL = /^[{(]?[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}[)}]?$/i;
const OPAQUE_DEVICE_LABEL = /^[a-z0-9+/=_:-]{24,}$/i;
const DEVICE_PATH_LABEL = /^(?:\\\\\?\\|[a-z]+:\/)/i;

function getAudioDefaultDevicePlatform(): VoiceAudioDefaultDevicePlatform {
	if (!isDesktop()) {
		return 'browser';
	}
	const platform = getNativePlatformSync();
	switch (platform) {
		case 'macos':
		case 'windows':
		case 'linux':
			return platform;
		case 'unknown':
			return 'browser';
	}
}

function cleanAudioDeviceLabel(rawLabel: string): string {
	return rawLabel.replace(USB_HARDWARE_ID_SUFFIX, '').trim();
}

function stripDefaultRouteLabelWrapper(label: string): string {
	const windowsMatch = label.match(WINDOWS_DEFAULT_DEVICE_PREFIX);
	if (windowsMatch?.[1]) {
		return cleanAudioDeviceLabel(windowsMatch[1]);
	}
	const admMatch = label.match(ADM_DEFAULT_DEVICE_WRAPPER);
	if (admMatch?.[1]) {
		return cleanAudioDeviceLabel(admMatch[1]);
	}
	if (label.toLowerCase() === 'default') {
		return '';
	}
	return cleanAudioDeviceLabel(label);
}

function cleanVideoDeviceLabel(rawLabel: string, deviceId: string): string {
	const label = rawLabel.replace(USB_HARDWARE_ID_SUFFIX, '').trim();
	const trimmedDeviceId = deviceId.trim();
	if (!label) return '';
	if (
		trimmedDeviceId &&
		(label === trimmedDeviceId || (trimmedDeviceId.length >= 8 && label.includes(trimmedDeviceId)))
	) {
		return '';
	}
	if (
		USB_HARDWARE_ID_LABEL.test(label) ||
		UUID_DEVICE_LABEL.test(label) ||
		OPAQUE_DEVICE_LABEL.test(label) ||
		DEVICE_PATH_LABEL.test(label)
	) {
		return '';
	}
	return label;
}

function normalizeAudioDeviceLabel(device: AudioDeviceShapeInput): NormalizedAudioDeviceLabel {
	const deviceId = device.deviceId.trim();
	const baseLabel = cleanAudioDeviceLabel(device.label);
	const metadataEndpointLabel = device.endpointLabel ? cleanAudioDeviceLabel(device.endpointLabel) : '';
	if (device.role === 'default' || device.isDefaultRoute === true || deviceId === 'default') {
		const endpointLabel = stripDefaultRouteLabelWrapper(metadataEndpointLabel || baseLabel);
		return {
			role: 'default',
			endpointLabel,
			label: endpointLabel,
			isDefaultRoute: true,
		};
	}
	if (device.role === 'communications' || deviceId === 'communications') {
		const communicationsMatch = baseLabel.match(WINDOWS_COMMUNICATIONS_DEVICE_PREFIX);
		const endpointLabel = metadataEndpointLabel || communicationsMatch?.[1]?.trim() || baseLabel;
		return {
			role: 'communications',
			endpointLabel: cleanAudioDeviceLabel(endpointLabel),
			label: cleanAudioDeviceLabel(endpointLabel),
			isDefaultRoute: false,
		};
	}
	const defaultMatch = baseLabel.match(WINDOWS_DEFAULT_DEVICE_PREFIX);
	if (defaultMatch?.[1]) {
		const endpointLabel = defaultMatch[1].trim();
		return {
			role: 'default',
			endpointLabel: cleanAudioDeviceLabel(endpointLabel),
			label: cleanAudioDeviceLabel(endpointLabel),
			isDefaultRoute: true,
		};
	}
	const communicationsMatch = baseLabel.match(WINDOWS_COMMUNICATIONS_DEVICE_PREFIX);
	if (communicationsMatch?.[1]) {
		const endpointLabel = communicationsMatch[1].trim();
		return {
			role: 'communications',
			endpointLabel: cleanAudioDeviceLabel(endpointLabel),
			label: cleanAudioDeviceLabel(endpointLabel),
			isDefaultRoute: false,
		};
	}
	const admDefaultMatch = baseLabel.match(ADM_DEFAULT_DEVICE_WRAPPER);
	if (admDefaultMatch?.[1]) {
		const endpointLabel = cleanAudioDeviceLabel(admDefaultMatch[1]);
		return {
			role: 'default',
			endpointLabel,
			label: endpointLabel,
			isDefaultRoute: true,
		};
	}
	return {
		role: null,
		endpointLabel: baseLabel,
		label: baseLabel,
		isDefaultRoute: false,
	};
}

function createAudioDeviceInfo(
	device: AudioDeviceShapeInput,
	normalized: NormalizedAudioDeviceLabel,
): VoiceMediaDeviceInfo {
	const deviceId = normalized.role === 'default' && normalized.isDefaultRoute ? 'default' : device.deviceId.trim();
	const groupId = device.groupId;
	const kind = device.kind;
	const label = normalized.label;
	const metadata: VoiceAudioDeviceMetadata | undefined =
		normalized.role === null
			? undefined
			: {
					role: normalized.role,
					endpointLabel: normalized.endpointLabel,
					...(normalized.role === 'default' ? {defaultPlatform: getAudioDefaultDevicePlatform()} : {}),
				};
	const shaped: VoiceMediaDeviceInfo = {
		deviceId,
		groupId,
		kind,
		label,
		isDefault: device.isDefault,
		toJSON: () => ({deviceId, groupId, kind, label}),
	} as VoiceMediaDeviceInfo;
	if (metadata) {
		shaped.fluxerVoiceAudioDevice = metadata;
	}
	return shaped;
}

export function getVoiceAudioDeviceMetadata(device: MediaDeviceInfo): VoiceAudioDeviceMetadata | null {
	return (device as VoiceMediaDeviceInfo).fluxerVoiceAudioDevice ?? null;
}

function createVideoDeviceInfo(deviceId: string, groupId: string, label: string): MediaDeviceInfo {
	return {
		deviceId,
		groupId,
		kind: 'videoinput',
		label,
		toJSON: () => ({deviceId, groupId, kind: 'videoinput', label}),
	} as MediaDeviceInfo;
}

export function shapeVideoDevices(devices: ReadonlyArray<MediaDeviceInfo>): Array<MediaDeviceInfo> {
	const endpointDevices = devices
		.filter((device) => device.kind === 'videoinput' && device.deviceId.trim().length > 0)
		.map((device) =>
			createVideoDeviceInfo(
				device.deviceId.trim(),
				device.groupId,
				cleanVideoDeviceLabel(device.label, device.deviceId),
			),
		);
	const explicitDefaultDevice = endpointDevices.find((device) => device.deviceId === 'default') ?? null;
	const physicalDevices = endpointDevices.filter((device) => device.deviceId !== 'default');
	if (physicalDevices.length === 0) {
		return explicitDefaultDevice ? sortDevices([explicitDefaultDevice]) : [];
	}
	const defaultDevice =
		explicitDefaultDevice ?? createVideoDeviceInfo('default', physicalDevices[0].groupId, physicalDevices[0].label);
	return sortDevices([defaultDevice, ...physicalDevices]);
}

function shapeAudioDevices(
	devices: ReadonlyArray<AudioDeviceShapeInput>,
	options: ShapeAudioDevicesOptions = {},
): Array<MediaDeviceInfo> {
	let normalizedDevices = devices
		.filter(
			(device) => device.deviceId.trim().length > 0 || device.isDefaultRoute === true || device.role === 'default',
		)
		.map((device) => ({
			device,
			label: normalizeAudioDeviceLabel(device),
		}));
	if (
		options.synthesizeDefaultRoute === true &&
		!normalizedDevices.some(({label}) => label.role === 'default' && label.isDefaultRoute)
	) {
		const defaultEndpoint =
			normalizedDevices.find(
				({device, label}) =>
					device.isDefault === true && label.role !== 'communications' && label.endpointLabel.length > 0,
			) ?? normalizedDevices.find(({label}) => label.role !== 'communications');
		if (defaultEndpoint) {
			const syntheticDefaultDevice: AudioDeviceShapeInput = {
				deviceId: 'default',
				groupId: defaultEndpoint.device.groupId,
				kind: defaultEndpoint.device.kind,
				label: defaultEndpoint.label.endpointLabel,
				isDefault: true,
				role: 'default',
				endpointLabel: defaultEndpoint.label.endpointLabel,
				isDefaultRoute: true,
			};
			normalizedDevices = [
				{device: syntheticDefaultDevice, label: normalizeAudioDeviceLabel(syntheticDefaultDevice)},
				...normalizedDevices,
			];
		}
	}
	const representedEndpointLabels = new Set(
		normalizedDevices
			.filter(({label}) => label.role !== 'communications')
			.map(({label}) => label.endpointLabel)
			.filter((label) => label.length > 0),
	);
	return sortDevices(
		dedupeAudioDeviceInfos(
			normalizedDevices
				.filter(({label}) => label.role !== 'communications' || !representedEndpointLabels.has(label.endpointLabel))
				.map(({device, label}) => createAudioDeviceInfo(device, label)),
		),
	);
}

function dedupeAudioDeviceInfos(devices: ReadonlyArray<VoiceMediaDeviceInfo>): Array<VoiceMediaDeviceInfo> {
	const devicesByDeviceId = new Map<string, VoiceMediaDeviceInfo>();
	for (const device of devices) {
		const existing = devicesByDeviceId.get(device.deviceId);
		if (existing === undefined) {
			devicesByDeviceId.set(device.deviceId, device);
			continue;
		}
		if (existing.label.length === 0 && device.label.length > 0) {
			devicesByDeviceId.set(device.deviceId, device);
		}
	}
	return [...devicesByDeviceId.values()];
}

export function shapeBrowserAudioDevices(devices: ReadonlyArray<MediaDeviceInfo>): Array<MediaDeviceInfo> {
	return shapeAudioDevices(
		devices.map((device) => ({
			deviceId: device.deviceId,
			groupId: device.groupId,
			kind: device.kind,
			label: device.label,
			isDefault: (device as VoiceMediaDeviceInfo).isDefault === true,
			isDefaultRoute: device.deviceId === 'default',
		})),
		{synthesizeDefaultRoute: true},
	);
}

class VoiceDeviceManager {
	private state: VoiceDeviceState = {
		inputDevices: [],
		outputDevices: [],
		videoDevices: [],
		permissionStatus: {
			audio: 'idle',
			video: 'idle',
		},
	};
	private listeners = new Set<Listener>();
	private enumerationChainPromise: Promise<VoiceDeviceState> | null = null;
	private scheduledEnumerationPermissionTypes = new Set<VoiceMediaPermissionType>();
	private enumerationChainPermissionTypes = new Set<VoiceMediaPermissionType>();
	private hasEnumeratedDevices = false;

	constructor() {
		if (typeof navigator !== 'undefined' && navigator.mediaDevices?.addEventListener) {
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

	public async ensureDevices(options: EnsureVoiceDevicesOptions = {}): Promise<VoiceDeviceState> {
		const requestPermissionTypes = this.resolveRequestedPermissionTypes(options);
		const requestPermissions = requestPermissionTypes.length > 0;
		const forceRefresh = options.forceRefresh ?? false;
		logger.debug('ensureDevices called', {
			requestPermissionTypes,
			forceRefresh,
			hasEnumeratingPromise: !!this.enumerationChainPromise,
			currentState: {
				inputDeviceCount: this.state.inputDevices.length,
				permissionStatus: this.state.permissionStatus,
			},
		});
		if (!forceRefresh && !this.enumerationChainPromise && this.canUseCachedState(requestPermissions)) {
			logger.debug('Using cached device state');
			return this.state;
		}
		if (this.enumerationChainPromise) {
			this.scheduleMissingPermissionTypes(requestPermissionTypes);
			logger.debug('Joining existing enumeration promise');
			return this.enumerationChainPromise;
		}
		logger.debug('Creating new enumeration promise');
		return this.startEnumerationChain(requestPermissionTypes);
	}

	private scheduleMissingPermissionTypes(requestPermissionTypes: ReadonlyArray<VoiceMediaPermissionType>): void {
		for (const type of requestPermissionTypes) {
			if (this.enumerationChainPermissionTypes.has(type)) continue;
			this.enumerationChainPermissionTypes.add(type);
			this.scheduledEnumerationPermissionTypes.add(type);
		}
	}

	private resolveRequestedPermissionTypes(options: EnsureVoiceDevicesOptions): Array<VoiceMediaPermissionType> {
		if (options.requestPermissionTypes) {
			return [...new Set(options.requestPermissionTypes)];
		}
		return options.requestPermissions === true ? ['audio', 'video'] : [];
	}

	private canUseCachedState(requestPermissions: boolean): boolean {
		if (!this.hasEnumeratedDevices) {
			return false;
		}
		return !requestPermissions;
	}

	private updatePermissionStatusForTypes(
		requestPermissionTypes: ReadonlyArray<VoiceMediaPermissionType>,
		status: VoiceMediaPermissionStatus,
	): void {
		const permissionStatus = {...this.state.permissionStatus};
		for (const type of requestPermissionTypes) {
			permissionStatus[type] = status;
		}
		this.updateState({permissionStatus});
	}

	private startEnumerationChain(
		requestPermissionTypes: ReadonlyArray<VoiceMediaPermissionType>,
	): Promise<VoiceDeviceState> {
		this.enumerationChainPermissionTypes = new Set(requestPermissionTypes);
		const pendingPromise = this.runEnumerationChain(requestPermissionTypes).finally(() => {
			if (this.enumerationChainPromise !== pendingPromise) return;
			logger.debug('Enumeration promise completed');
			this.enumerationChainPromise = null;
			this.scheduledEnumerationPermissionTypes.clear();
			this.enumerationChainPermissionTypes.clear();
		});
		this.enumerationChainPromise = pendingPromise;
		return pendingPromise;
	}

	private async runEnumerationChain(
		initialPermissionTypes: ReadonlyArray<VoiceMediaPermissionType>,
	): Promise<VoiceDeviceState> {
		let requestPermissionTypes = [...initialPermissionTypes];
		let state = this.state;
		for (let pass = 0; pass < MAX_ENUMERATION_CHAIN_PASSES; pass += 1) {
			state = await this.enumerateDevices(requestPermissionTypes);
			if (this.scheduledEnumerationPermissionTypes.size === 0) return state;
			requestPermissionTypes = [...this.scheduledEnumerationPermissionTypes];
			this.scheduledEnumerationPermissionTypes.clear();
		}
		if (this.scheduledEnumerationPermissionTypes.size > 0) {
			throw new Error(`Voice device enumeration exceeded ${MAX_ENUMERATION_CHAIN_PASSES} bounded passes`);
		}
		return state;
	}

	private async enumerateDevices(
		requestPermissionTypes: ReadonlyArray<VoiceMediaPermissionType>,
	): Promise<VoiceDeviceState> {
		const requestPermissions = requestPermissionTypes.length > 0;
		logger.debug('enumerateDevices started', {requestPermissionTypes});
		if (!navigator.mediaDevices?.enumerateDevices) {
			logger.debug('Navigator or mediaDevices API not available');
			return this.state;
		}
		const permissionTypesToLoad = requestPermissionTypes.filter(
			(type) => this.state.permissionStatus[type] !== 'granted',
		);
		if (permissionTypesToLoad.length > 0) {
			logger.debug('Setting permission status to loading');
			this.updatePermissionStatusForTypes(permissionTypesToLoad, 'loading');
		}
		const permissionStatus = {...this.state.permissionStatus};
		try {
			logger.debug('Calling navigator.mediaDevices.enumerateDevices()');
			let devices = await navigator.mediaDevices.enumerateDevices();
			logger.debug('Initial enumeration result', {
				deviceCount: devices.length,
				devices: devices.map((d) => ({
					kind: d.kind,
					hasDeviceId: d.deviceId.trim().length > 0,
					label: d.label,
					hasLabel: !!d.label,
				})),
			});
			const permissionTypesWithLabels = requestPermissionTypes.filter((type) => {
				const requiredKind = type === 'audio' ? 'audioinput' : 'videoinput';
				return devices.some((device) => device.kind === requiredKind && device.label !== '');
			});
			for (const type of permissionTypesWithLabels) {
				permissionStatus[type] = 'granted';
			}
			if (requestPermissions && permissionTypesWithLabels.length === requestPermissionTypes.length) {
				logger.debug('Devices have labels, permissions already granted');
			} else if (requestPermissions && isDesktop()) {
				logger.debug('No labels detected; attempting native permission flow');
				const unresolvedNativeTypes = requestPermissionTypes.filter((type) => permissionStatus[type] !== 'granted');
				const nativeResults = await Promise.all(
					unresolvedNativeTypes.map(async (type) => {
						try {
							return {
								type,
								result: await ensureNativePermission(type === 'audio' ? 'microphone' : 'camera'),
							};
						} catch (error) {
							logger.warn('Native media permission request failed', {type, error});
							return {type, result: 'not-determined' as const};
						}
					}),
				);
				for (const {type, result} of nativeResults) {
					if (result === 'granted') permissionStatus[type] = 'granted';
					if (result === 'denied') permissionStatus[type] = 'denied';
				}
				if (nativeResults.some(({result}) => result === 'granted')) {
					try {
						devices = await navigator.mediaDevices.enumerateDevices();
					} catch (error) {
						logger.warn('Device re-enumeration after native permission grant failed', {error});
					}
				}
			}
			const browserPermissionTypes = requestPermissionTypes.filter(
				(type) => permissionStatus[type] !== 'granted' && permissionStatus[type] !== 'denied',
			);
			if (browserPermissionTypes.length > 0) {
				const isIOSPWA =
					browserPermissionTypes.length === 1 &&
					browserPermissionTypes[0] === 'audio' &&
					Platform.isIOSWeb &&
					Platform.isPWA;
				let skipGetUserMedia = false;
				if (isIOSPWA && navigator.permissions) {
					try {
						const micPermission = await navigator.permissions.query({name: 'microphone' as PermissionName});
						if (micPermission.state === 'granted') {
							logger.debug('iOS PWA: microphone permission already granted via Permissions API, skipping getUserMedia');
							permissionStatus.audio = 'granted';
							try {
								devices = await navigator.mediaDevices.enumerateDevices();
								skipGetUserMedia = devices.some((device) => device.kind === 'audioinput' && device.label !== '');
							} catch (error) {
								logger.warn('iOS PWA device re-enumeration after permission grant failed', {error});
							}
						}
					} catch (error) {
						logger.debug('iOS PWA microphone permission query failed', {error});
					}
				}
				if (!skipGetUserMedia) {
					logger.debug('No labels found, requesting permissions via getUserMedia');
					try {
						const stream = await navigator.mediaDevices.getUserMedia({
							audio: browserPermissionTypes.includes('audio'),
							video: browserPermissionTypes.includes('video'),
						});
						logger.debug('getUserMedia succeeded, stopping tracks');
						stream.getTracks().forEach((track) => {
							logger.debug('Stopping track', {kind: track.kind, label: track.label});
							track.stop();
						});
						for (const type of browserPermissionTypes) {
							permissionStatus[type] = 'granted';
						}
						logger.debug('Re-enumerating devices after permission grant');
						try {
							devices = await navigator.mediaDevices.enumerateDevices();
							logger.debug('Re-enumeration result', {
								deviceCount: devices.length,
								devices: devices.map((d) => ({
									kind: d.kind,
									hasDeviceId: d.deviceId.trim().length > 0,
									label: d.label,
								})),
							});
						} catch (error) {
							logger.warn('Device re-enumeration after browser permission grant failed', {error});
						}
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
							for (const type of browserPermissionTypes) {
								permissionStatus[type] = 'denied';
							}
						} else {
							for (const type of browserPermissionTypes) {
								permissionStatus[type] = 'idle';
							}
						}
					}
				}
			}
			const inputDevices = shapeBrowserAudioDevices(
				devices.filter((device) => device.kind === 'audioinput' && !isInternalVirtualAudioDevice(device)),
			);
			const outputDevices = shapeBrowserAudioDevices(
				devices.filter((device) => device.kind === 'audiooutput' && !isInternalVirtualAudioDevice(device)),
			);
			const videoDevices = shapeVideoDevices(devices.filter((device) => device.kind === 'videoinput'));
			const nextState: VoiceDeviceState = {
				inputDevices,
				outputDevices,
				videoDevices,
				permissionStatus,
			};
			this.hasEnumeratedDevices = true;
			logger.debug('Final device state', {
				inputDeviceCount: inputDevices.length,
				outputDeviceCount: outputDevices.length,
				videoDeviceCount: videoDevices.length,
				permissionStatus: nextState.permissionStatus,
			});
			this.updateState(nextState);
			return this.state;
		} catch (error) {
			logger.warn('Voice device enumeration failed', {error, requestPermissionTypes});
			for (const type of requestPermissionTypes) {
				if (permissionStatus[type] === 'loading') permissionStatus[type] = 'idle';
			}
			this.updateState({permissionStatus});
			return this.state;
		}
	}

	private updateState(partial: Partial<VoiceDeviceState>) {
		this.state = {
			...this.state,
			...partial,
		};
		this.listeners.forEach((listener) => listener(this.state));
	}

	private handleDeviceChange = () => {
		this.hasEnumeratedDevices = false;
		void this.ensureDevices({requestPermissions: false}).catch((error) => {
			logger.warn('Voice device refresh after device change failed', {error});
		});
	};
}

export const voiceDeviceManager = new VoiceDeviceManager();
