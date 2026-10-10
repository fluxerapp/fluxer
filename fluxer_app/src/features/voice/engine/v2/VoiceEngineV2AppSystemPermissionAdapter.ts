// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {MediaAccessStatus, MediaAccessType} from '@app/types/electron.d';
import {
	createVoiceEngineV2PermissionResult,
	type PermissionPort,
	type VoiceEngineV2PermissionName,
	type VoiceEngineV2PermissionResult,
	type VoiceEngineV2PermissionStatus,
} from '@fluxer/voice_engine_v2';

export interface VoiceEngineV2SystemPermissionsApi {
	checkMediaAccess(type: MediaAccessType): Promise<MediaAccessStatus>;
	requestMediaAccess(type: MediaAccessType): Promise<boolean>;
}

const VOICE_ENGINE_V2_PERMISSION_STATUSES: ReadonlySet<VoiceEngineV2PermissionStatus> = new Set([
	'unknown',
	'prompt',
	'granted',
	'denied',
	'restricted',
	'unsupported',
]);

function assertVoiceEngineV2PermissionStatus(status: VoiceEngineV2PermissionStatus): void {
	if (!VOICE_ENGINE_V2_PERMISSION_STATUSES.has(status)) {
		throw new Error(`VoiceEngineV2AppSystemPermissionAdapter: invalid status ${String(status)}`);
	}
}

function mapPermissionNameToMediaAccessType(name: VoiceEngineV2PermissionName): MediaAccessType | null {
	switch (name) {
		case 'microphone':
			return 'microphone';
		case 'camera':
			return 'camera';
		case 'screen':
			return 'screen';
		case 'screenAudio':
		case 'systemAudio':
			return 'audio-capture';
		default:
			return null;
	}
}

function mapMediaAccessStatusToPermissionStatus(status: MediaAccessStatus): VoiceEngineV2PermissionStatus {
	switch (status) {
		case 'granted':
			return 'granted';
		case 'denied':
			return 'denied';
		case 'restricted':
			return 'restricted';
		case 'not-determined':
			return 'prompt';
		case 'unknown':
			return 'unknown';
		default:
			return 'unknown';
	}
}

function defaultVoiceEngineV2SystemPermissionsApi(): VoiceEngineV2SystemPermissionsApi {
	const electron = getElectronAPI();
	return {
		async checkMediaAccess(type): Promise<MediaAccessStatus> {
			if (!electron) return 'unknown';
			return electron.checkMediaAccess(type);
		},
		async requestMediaAccess(type): Promise<boolean> {
			if (!electron) return false;
			return electron.requestMediaAccess(type);
		},
	};
}

export class VoiceEngineV2AppSystemPermissionAdapter implements PermissionPort {
	private readonly api: VoiceEngineV2SystemPermissionsApi;

	constructor(api: VoiceEngineV2SystemPermissionsApi = defaultVoiceEngineV2SystemPermissionsApi()) {
		if (!api) {
			throw new Error('VoiceEngineV2AppSystemPermissionAdapter: api is required');
		}
		if (typeof api.checkMediaAccess !== 'function') {
			throw new Error('VoiceEngineV2AppSystemPermissionAdapter: api.checkMediaAccess is required');
		}
		if (typeof api.requestMediaAccess !== 'function') {
			throw new Error('VoiceEngineV2AppSystemPermissionAdapter: api.requestMediaAccess is required');
		}
		this.api = api;
	}

	async checkPermission(name: VoiceEngineV2PermissionName): Promise<VoiceEngineV2PermissionResult> {
		if (typeof name !== 'string' || name.length === 0) {
			throw new Error('VoiceEngineV2AppSystemPermissionAdapter.checkPermission: name must be a non-empty string');
		}
		const mediaType = mapPermissionNameToMediaAccessType(name);
		if (mediaType === null) {
			const result = createVoiceEngineV2PermissionResult(name, 'unknown');
			assertVoiceEngineV2PermissionStatus(result.status);
			return result;
		}
		let status: VoiceEngineV2PermissionStatus;
		try {
			const raw = await this.api.checkMediaAccess(mediaType);
			status = mapMediaAccessStatusToPermissionStatus(raw);
		} catch {
			status = 'unknown';
		}
		assertVoiceEngineV2PermissionStatus(status);
		const result = createVoiceEngineV2PermissionResult(name, status);
		assertVoiceEngineV2PermissionStatus(result.status);
		return result;
	}

	async requestPermission(name: VoiceEngineV2PermissionName): Promise<VoiceEngineV2PermissionResult> {
		if (typeof name !== 'string' || name.length === 0) {
			throw new Error('VoiceEngineV2AppSystemPermissionAdapter.requestPermission: name must be a non-empty string');
		}
		const mediaType = mapPermissionNameToMediaAccessType(name);
		if (mediaType === null) {
			const result = createVoiceEngineV2PermissionResult(name, 'unknown');
			assertVoiceEngineV2PermissionStatus(result.status);
			return result;
		}
		let status: VoiceEngineV2PermissionStatus;
		try {
			const granted = await this.api.requestMediaAccess(mediaType);
			status = granted ? 'granted' : 'denied';
		} catch {
			status = 'unknown';
		}
		assertVoiceEngineV2PermissionStatus(status);
		const result = createVoiceEngineV2PermissionResult(name, status);
		assertVoiceEngineV2PermissionStatus(result.status);
		return result;
	}
}

export function createVoiceEngineV2AppSystemPermissionAdapter(
	api?: VoiceEngineV2SystemPermissionsApi,
): VoiceEngineV2AppSystemPermissionAdapter {
	return new VoiceEngineV2AppSystemPermissionAdapter(api);
}
