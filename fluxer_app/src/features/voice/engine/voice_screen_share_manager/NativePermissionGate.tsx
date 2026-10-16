// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {handleMediaPermissionBlocked} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import {
	ensureNativePermission,
	type NativePermissionResult,
} from '@app/features/permissions/system/utils/NativePermissions';
import {ScreenRecordingPermissionDeniedError} from '@app/features/permissions/system/utils/ScreenRecordingPermissionDeniedError';
import {Logger} from '@app/features/platform/utils/AppLogger';

export type ScreenShareDevicePermissionContext = 'start' | 'replace';

type NativeMediaPermissionKind = 'camera' | 'microphone' | 'screen';
type NativeMediaPermissionDeniedPolicy = 'modal' | 'throw' | 'modal-then-throw' | 'none';

export interface EnsureNativeMediaPermissionArgs {
	kind: NativeMediaPermissionKind;
	onDenied: NativeMediaPermissionDeniedPolicy;
	deniedModalDataFlx?: string;
}

const logger = new Logger('NativePermissionGate');

function getCameraDeniedDataFlx(context: ScreenShareDevicePermissionContext): string {
	return context === 'start'
		? 'voice.engine.voice-screen-share-manager.camera-permission-denied-modal'
		: 'voice.engine.voice-screen-share-manager.camera-permission-denied-modal--2';
}

function getMicrophoneDeniedDataFlx(context: ScreenShareDevicePermissionContext): string {
	return context === 'start'
		? 'voice.engine.voice-screen-share-manager.microphone-permission-denied-modal'
		: 'voice.engine.voice-screen-share-manager.microphone-permission-denied-modal--2';
}

function markExplicitlyDenied(kind: NativeMediaPermissionKind): void {
	if (kind === 'camera') {
		MediaPermission.markCameraExplicitlyDenied();
		return;
	}
	if (kind === 'microphone') {
		MediaPermission.markMicrophoneExplicitlyDenied();
		return;
	}
	MediaPermission.markScreenRecordingExplicitlyDenied();
}

function updatePermissionGranted(kind: NativeMediaPermissionKind): void {
	if (kind === 'camera') {
		MediaPermission.updateCameraPermissionGranted();
		return;
	}
	if (kind === 'microphone') {
		MediaPermission.updateMicrophonePermissionGranted();
		return;
	}
	MediaPermission.updateScreenRecordingPermissionGranted();
}

function pushDeniedModal(kind: NativeMediaPermissionKind, dataFlx: string): void {
	assert.ok(kind === 'camera' || kind === 'microphone', 'denied modal is only available for camera and microphone');
	assert.ok(dataFlx.length > 0, 'denied modal requires a data-flx identifier');
	handleMediaPermissionBlocked(kind);
}

function buildDeniedError(kind: NativeMediaPermissionKind): Error {
	if (kind === 'screen') {
		return new ScreenRecordingPermissionDeniedError();
	}
	const label = kind === 'camera' ? 'Camera' : 'Microphone';
	return Object.assign(new Error(`${label} permission denied`), {name: 'NotAllowedError'});
}

export async function ensureNativeMediaPermission(
	args: EnsureNativeMediaPermissionArgs,
): Promise<NativePermissionResult> {
	assert.ok(args.kind === 'camera' || args.kind === 'microphone' || args.kind === 'screen');
	if (args.kind === 'screen') {
		assert.equal(args.onDenied, 'throw', 'screen permission denial has no modal; use the throw policy');
		if (MediaPermission.isScreenRecordingExplicitlyDenied()) {
			logger.info('Screen recording permission previously denied; re-querying the OS');
		}
	}
	const result = await ensureNativePermission(args.kind);
	if (result === 'denied') {
		logger.warn('Native media permission denied', {kind: args.kind});
		markExplicitlyDenied(args.kind);
		if (args.onDenied === 'modal' || args.onDenied === 'modal-then-throw') {
			pushDeniedModal(args.kind, args.deniedModalDataFlx ?? '');
		}
		if (args.onDenied === 'throw' || args.onDenied === 'modal-then-throw') {
			throw buildDeniedError(args.kind);
		}
		return result;
	}
	if (result === 'granted') {
		updatePermissionGranted(args.kind);
	}
	return result;
}

export async function ensureNativeCameraPermissionForDeviceShare(
	context: ScreenShareDevicePermissionContext,
): Promise<void> {
	await ensureNativeMediaPermission({
		kind: 'camera',
		onDenied: 'modal-then-throw',
		deniedModalDataFlx: getCameraDeniedDataFlx(context),
	});
}

export async function ensureNativeMicrophonePermissionForDeviceShare(
	context: ScreenShareDevicePermissionContext,
): Promise<void> {
	await ensureNativeMediaPermission({
		kind: 'microphone',
		onDenied: 'modal-then-throw',
		deniedModalDataFlx: getMicrophoneDeniedDataFlx(context),
	});
}
