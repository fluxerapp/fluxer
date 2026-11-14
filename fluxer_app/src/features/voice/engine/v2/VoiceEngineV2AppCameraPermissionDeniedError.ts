// SPDX-License-Identifier: AGPL-3.0-or-later

const CAMERA_PERMISSION_DENIED_ERROR_NAME = 'VoiceEngineV2AppCameraPermissionDeniedError';

export function buildVoiceEngineV2AppCameraPermissionDeniedError(): Error {
	const error = new Error('Camera permission denied') as Error & {code: string; capability: string};
	error.name = CAMERA_PERMISSION_DENIED_ERROR_NAME;
	error.code = 'permissionDenied';
	error.capability = 'camera';
	return error;
}
