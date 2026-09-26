// SPDX-License-Identifier: AGPL-3.0-or-later

import {handleMediaPermissionBlocked} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import {ScreenRecordingPermissionDeniedError} from '@app/features/permissions/system/utils/ScreenRecordingPermissionDeniedError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {ScreenShareFailedModal} from '@app/features/voice/components/alerts/ScreenShareFailedModal';
import {ScreenShareRollbackIncompleteModal} from '@app/features/voice/components/alerts/ScreenShareRollbackIncompleteModal';
import {ScreenShareUnsupportedModal} from '@app/features/voice/components/alerts/ScreenShareUnsupportedModal';
import {isScreenShareAudioCaptureError} from '@app/features/voice/utils/ScreenShareAudioCaptureError';
import {isScreenSharePortalUnavailableError} from '@app/features/voice/utils/ScreenSharePortalUnavailableError';
import {isScreenShareRollbackIncompleteError} from '@app/features/voice/utils/ScreenShareRollbackIncompleteError';

const logger = new Logger('ScreenShareUtils');
const SCREEN_SHARE_ROLLBACK_INCOMPLETE_MODAL_KEY = 'screen-share-rollback-incomplete';
const isScreenShareUnsupportedError = (error: unknown): boolean => {
	if (!(error instanceof Error)) return false;
	return (
		error.name === 'DeviceUnsupportedError' || error.name === 'NotSupportedError' || error.name === 'NotAllowedError'
	);
};
export const handleScreenShareError = (error: unknown): void => {
	if (error instanceof ScreenRecordingPermissionDeniedError) {
		handleMediaPermissionBlocked('screen');
		return;
	}
	if (isScreenSharePortalUnavailableError(error)) {
		logger.warn('Wayland screen share portal unavailable; portal modal is surfaced by the picker IPC handler', {
			reason: error.reason,
		});
		return;
	}
	if (isScreenShareAudioCaptureError(error)) {
		logger.warn('Screen share audio capture failed; the picker will surface the targeted error', error.debugInfo);
		return;
	}
	if (isScreenShareRollbackIncompleteError(error)) {
		logger.error('Screen share rollback was incomplete', {errors: error.errors});
		ModalCommands.pushWithKey(
			modal(() => (
				<ScreenShareRollbackIncompleteModal data-flx="voice.screen-share-utils.handle-screen-share-error.rollback-incomplete-modal" />
			)),
			SCREEN_SHARE_ROLLBACK_INCOMPLETE_MODAL_KEY,
		);
		return;
	}
	if (isScreenShareUnsupportedError(error)) {
		ModalCommands.push(
			modal(() => (
				<ScreenShareUnsupportedModal data-flx="voice.screen-share-utils.handle-screen-share-error.screen-share-unsupported-modal" />
			)),
		);
	} else {
		logger.error('Failed to start screen share:', error);
		ModalCommands.push(
			modal(() => (
				<ScreenShareFailedModal data-flx="voice.screen-share-utils.handle-screen-share-error.screen-share-failed-modal" />
			)),
		);
	}
};

export async function executeScreenShareOperation(
	operation: () => Promise<void>,
	onError?: (error: unknown) => void,
): Promise<void> {
	try {
		await operation();
	} catch (error) {
		handleScreenShareError(error);
		if (onError) {
			onError(error);
		}
		throw error;
	}
}
