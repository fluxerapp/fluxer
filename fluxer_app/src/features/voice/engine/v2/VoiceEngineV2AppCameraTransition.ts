// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import {handleMediaPermissionBlocked} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import {isPermissionDeniedError} from '@app/features/voice/engine/v2/VoiceEngineV2AppAdapterAssertions';
import VoiceEngineV2AppMediaStateAdapter from '@app/features/voice/engine/v2/VoiceEngineV2AppMediaStateAdapter';
import {ensureNativeMediaPermission} from '@app/features/voice/engine/voice_screen_share_manager/NativePermissionGate';

export const CAMERA_PERMISSION_DENIED_MODAL_DATA_FLX =
	'voice.engine.voice-media-manager.camera-permission-denied-modal';

export type VoiceEngineV2AppCameraTransitionOutcome = 'applied' | 'denied' | 'failed';

export interface VoiceEngineV2AppCameraTransitionArgs {
	enabled: boolean;
	sendUpdate: boolean;
	publish: () => Promise<void>;
	readActualEnabled: () => boolean;
	onPermissionDenied: (() => void) | null;
	onSuccessSettled: (() => void) | null;
	onFailure: ((actual: boolean, error: unknown) => void) | null;
	rethrowOnFailure: boolean;
}

function playCameraSettleSound(attemptedEnabled: boolean, actual: boolean): void {
	assert.equal(typeof attemptedEnabled, 'boolean', 'attempted camera state must be a boolean');
	assert.equal(typeof actual, 'boolean', 'actual camera state must be a boolean');
	if (actual) {
		SoundCommands.playSound(SoundType.CameraOn);
		return;
	}
	if (!attemptedEnabled) {
		SoundCommands.playSound(SoundType.CameraOff);
	}
}

export async function runCameraTransition(
	args: VoiceEngineV2AppCameraTransitionArgs,
): Promise<VoiceEngineV2AppCameraTransitionOutcome> {
	assert.equal(typeof args.enabled, 'boolean', 'camera transition enabled must be a boolean');
	assert.equal(typeof args.sendUpdate, 'boolean', 'camera transition sendUpdate must be a boolean');
	assert.equal(typeof args.publish, 'function', 'camera transition requires a publish thunk');
	assert.equal(typeof args.readActualEnabled, 'function', 'camera transition requires an actual-state reader');
	if (args.enabled) {
		const permissionResult = await ensureNativeMediaPermission({
			kind: 'camera',
			onDenied: args.sendUpdate ? 'modal' : 'none',
			deniedModalDataFlx: CAMERA_PERMISSION_DENIED_MODAL_DATA_FLX,
		});
		if (permissionResult === 'denied') {
			args.onPermissionDenied?.();
			return 'denied';
		}
	}
	try {
		const syncSettledState = args.sendUpdate;
		if (!args.enabled) {
			VoiceEngineV2AppMediaStateAdapter.applyCameraState(false, {
				forceSync: syncSettledState,
				sendUpdate: args.sendUpdate,
			});
		}
		await args.publish();
		if (args.enabled) {
			MediaPermission.updateCameraPermissionGranted();
			VoiceEngineV2AppMediaStateAdapter.applyCameraState(true, {
				forceSync: syncSettledState,
				sendUpdate: args.sendUpdate,
			});
		}
		args.onSuccessSettled?.();
		SoundCommands.playSound(args.enabled ? SoundType.CameraOn : SoundType.CameraOff);
		return 'applied';
	} catch (error) {
		if (args.enabled && isPermissionDeniedError(error)) {
			MediaPermission.markCameraExplicitlyDenied();
			if (args.sendUpdate) handleMediaPermissionBlocked('camera');
			args.onPermissionDenied?.();
			return 'denied';
		}
		const actual = args.readActualEnabled();
		assert.equal(typeof actual, 'boolean', 'actual-state reader must return a boolean');
		args.onFailure?.(actual, error);
		VoiceEngineV2AppMediaStateAdapter.applyCameraState(actual, {
			forceSync: args.sendUpdate,
			sendUpdate: args.sendUpdate,
		});
		playCameraSettleSound(args.enabled, actual);
		if (args.rethrowOnFailure) {
			throw error;
		}
		return 'failed';
	}
}
