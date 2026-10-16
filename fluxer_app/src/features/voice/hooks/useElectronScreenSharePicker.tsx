// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {DisplayMediaRequestInfo} from '@app/types/electron.d';
import {useEffect} from 'react';

const logger = new Logger('ElectronScreenSharePicker');

async function hasScreenRecordingPermission(requestId: string): Promise<boolean> {
	const [{checkNativePermission}, {default: MediaPermission}] = await Promise.all([
		import('@app/features/permissions/system/utils/NativePermissions'),
		import('@app/features/permissions/system/state/MediaPermission'),
	]);
	const permission = await checkNativePermission('screen');
	if (permission !== 'denied') {
		return true;
	}
	logger.warn('Screen recording permission denied');
	MediaPermission.markScreenRecordingExplicitlyDenied();
	const {handleMediaPermissionBlocked} = await import(
		'@app/features/permissions/system/commands/MacPermissionsModalCommands'
	);
	handleMediaPermissionBlocked('screen');
	logger.warn('Rejecting display media request because screen recording permission is denied', {requestId});
	return false;
}

export const useElectronScreenSharePicker = (): void => {
	useEffect(() => {
		const electronApi = getElectronAPI();
		if (!electronApi?.onDisplayMediaRequested) {
			logger.info('Screen share picker unavailable (missing platform handler)');
			return;
		}
		let handlingRequest = false;
		const handleRequest = async (requestId: string, info: DisplayMediaRequestInfo) => {
			if (handlingRequest) {
				electronApi.selectDisplayMediaSource(requestId, null, false);
				return;
			}
			if (!info.videoRequested) {
				logger.warn('Rejecting display media request without a video stream', {
					requestId,
					audioRequested: info.audioRequested,
				});
				electronApi.selectDisplayMediaSource(requestId, null, false);
				return;
			}
			handlingRequest = true;
			try {
				if (electronApi.platform === 'darwin' && !(await hasScreenRecordingPermission(requestId))) {
					electronApi.selectDisplayMediaSource(requestId, null, false);
					return;
				}
				const {consumeDesktopSourceIntent} = await import('@app/features/voice/state/DesktopSourceIntent');
				const intent = consumeDesktopSourceIntent();
				if (!intent) {
					logger.warn('No desktop source intent pending for display media request; cancelling', {requestId});
					electronApi.selectDisplayMediaSource(requestId, null, false);
					return;
				}
				electronApi.selectDisplayMediaSource(requestId, intent.sourceId, false);
			} catch (error) {
				logger.error('Failed to handle display media request', error);
				electronApi.selectDisplayMediaSource(requestId, null, false);
			} finally {
				handlingRequest = false;
			}
		};
		const unsubscribe = electronApi.onDisplayMediaRequested((requestId, info) => {
			void handleRequest(requestId, info);
		});
		const unsubscribePortalEmpty = electronApi.onDisplayMediaPortalEmpty?.((requestId) => {
			logger.warn('Linux screen share portal returned no sources', {requestId});
			void Promise.all([
				import('@app/features/ui/commands/ModalCommands'),
				import('@app/features/voice/components/alerts/ScreenShareUnsupportedModal'),
			]).then(([ModalCommands, {ScreenShareUnsupportedModal}]) => {
				ModalCommands.push(
					ModalCommands.modal(() => (
						<ScreenShareUnsupportedModal
							variant="portal-empty"
							data-flx="voice.use-electron-screen-share-picker.screen-share-unsupported-modal"
						/>
					)),
				);
			});
		});
		return () => {
			unsubscribe();
			unsubscribePortalEmpty?.();
		};
	}, []);
};
