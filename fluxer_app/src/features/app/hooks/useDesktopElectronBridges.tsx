// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {isClientBooting} from '@app/features/app/state/ClientReadiness';
import Authentication from '@app/features/auth/state/Authentication';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {initializeDesktopTrayBridge} from '@app/features/platform/utils/DesktopTrayBridge';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import {broadcastThemeStudioMessage} from '@app/features/theme_studio/state/ThemeStudioBroadcast';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {startLiveResizeTracking} from '@app/features/window/state/LiveResize';
import {useEffect} from 'react';

const logger = new Logger('useDesktopElectronBridges');

export function useDesktopElectronBridges(): void {
	useEffect(() => {
		void Accessibility.applyStoredZoom().catch((error: unknown) => {
			logger.error('Failed to apply the stored zoom level', error);
		});
		void ThemeLibrary.init().catch((error: unknown) => {
			logger.error('Failed to initialize the theme library', error);
		});
		const electronApi = getElectronAPI();
		if (!electronApi) return;
		const disposeLinkedFileSync = ThemeLibrary.startLinkedFileSync({
			onThemesChanged: () => broadcastThemeStudioMessage({type: 'themeLibrary', revision: ThemeLibrary.revision}),
		});
		const adjustZoom = (direction: 1 | -1) => {
			void Accessibility.adjustZoom(direction).catch((error: unknown) => {
				logger.error('Failed to adjust the zoom level', error);
			});
		};
		const unsubZoomIn = electronApi.onZoomIn?.(() => adjustZoom(1));
		const unsubZoomOut = electronApi.onZoomOut?.(() => adjustZoom(-1));
		const unsubZoomReset = electronApi.onZoomReset?.(() => Accessibility.updateSettings({zoomLevel: 1.0}));
		const unsubOpenSettings = electronApi.onOpenSettings?.(() => {
			if (!Authentication.isAuthenticated) return;
			if (isClientBooting()) return;
			void loadLazyModule(() => import('@app/features/user/components/modals/UserSettingsModal'))
				.then(({UserSettingsModal}) => {
					ModalCommands.push(
						ModalCommands.modal(
							() => <UserSettingsModal data-flx="app.app.use-desktop-electron-bridges.user-settings-modal" />,
							'user-settings',
						),
					);
				})
				.catch((error: unknown) => {
					logger.error('Failed to open user settings', error);
				});
		});
		startLiveResizeTracking();
		const disposeTrayBridge = initializeDesktopTrayBridge();
		return () => {
			unsubZoomIn?.();
			unsubZoomOut?.();
			unsubZoomReset?.();
			unsubOpenSettings?.();
			disposeTrayBridge?.();
			disposeLinkedFileSync();
		};
	}, []);
}
