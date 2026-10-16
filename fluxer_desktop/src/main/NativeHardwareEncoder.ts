// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {createChildLogger} from '@electron/common/Logger';
import {VOICE_ENGINE_V2_HARDWARE_ENCODER_IPC_CHANNEL} from '@fluxer/voice_engine_v2/bridge';
import {
	normalizeVoiceEngineV2HardwareEncoderCapabilities,
	unavailableVoiceEngineV2HardwareEncoderCapabilities,
} from '@fluxer/voice_engine_v2/src/policies/hardwareEncoderCapabilities';
import type {VoiceEngineV2HardwareEncoderCapabilities} from '@fluxer/voice_engine_v2/src/protocol/types';
import {ipcMain} from 'electron';

const logger = createChildLogger('NativeHardwareEncoder');
const requireModule = createRequire(import.meta.url);

interface HardwareEncoderModule {
	getHardwareEncoderCapabilities?: () => unknown;
}

let cachedModule: HardwareEncoderModule | null | undefined;
let cachedModuleLoadErrorDetail: string | undefined;

function loadHardwareEncoderModule(): HardwareEncoderModule | null {
	if (cachedModule !== undefined) return cachedModule;
	cachedModuleLoadErrorDetail = undefined;
	try {
		cachedModule = requireModule('@fluxer/hardware-encoder') as HardwareEncoderModule;
	} catch (error) {
		logger.warn('Failed to load @fluxer/hardware-encoder', error);
		cachedModuleLoadErrorDetail = error instanceof Error ? error.message : String(error);
		cachedModule = null;
	}
	return cachedModule;
}

function getHardwareEncoderCapabilities(): VoiceEngineV2HardwareEncoderCapabilities {
	const mod = loadHardwareEncoderModule();
	if (!mod) {
		return unavailableVoiceEngineV2HardwareEncoderCapabilities('load-failed', cachedModuleLoadErrorDetail);
	}
	if (typeof mod.getHardwareEncoderCapabilities !== 'function') {
		return unavailableVoiceEngineV2HardwareEncoderCapabilities('unsupported-addon-version');
	}
	try {
		return normalizeVoiceEngineV2HardwareEncoderCapabilities(mod.getHardwareEncoderCapabilities());
	} catch (error) {
		logger.warn('Native hardware encoder getHardwareEncoderCapabilities() threw', error);
		const detail = error instanceof Error ? error.message : String(error);
		return unavailableVoiceEngineV2HardwareEncoderCapabilities('query-failed', detail);
	}
}

let handlersRegistered = false;

export function registerNativeHardwareEncoderHandlers(): void {
	if (handlersRegistered) return;
	handlersRegistered = true;
	ipcMain.handle(
		VOICE_ENGINE_V2_HARDWARE_ENCODER_IPC_CHANNEL,
		(): VoiceEngineV2HardwareEncoderCapabilities => getHardwareEncoderCapabilities(),
	);
}

export function cleanupNativeHardwareEncoderHandlers(): void {
	if (!handlersRegistered) return;
	ipcMain.removeHandler(VOICE_ENGINE_V2_HARDWARE_ENCODER_IPC_CHANNEL);
	handlersRegistered = false;
}
