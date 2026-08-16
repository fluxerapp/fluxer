// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {isClientBooting} from '@app/features/app/state/ClientReadiness';
import Authentication from '@app/features/auth/state/Authentication';
import {initializeDesktopTrayBridge} from '@app/features/platform/utils/DesktopTrayBridge';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {useEffect} from 'react';

export function useDesktopElectronBridges(): void {
	useEffect(() => {
		void Accessibility.applyStoredZoom();
		void ThemeLibrary.init();
		const electronApi = getElectronAPI();
		if (!electronApi) return;
		const unsubZoomIn = electronApi.onZoomIn?.(() => void Accessibility.adjustZoom(1));
		const unsubZoomOut = electronApi.onZoomOut?.(() => void Accessibility.adjustZoom(-1));
		const unsubZoomReset = electronApi.onZoomReset?.(() => Accessibility.updateSettings({zoomLevel: 1.0}));
		const unsubOpenSettings = electronApi.onOpenSettings?.(() => {
			if (!Authentication.isAuthenticated) return;
			if (isClientBooting()) return;
			void import('@app/features/user/components/modals/UserSettingsModal').then(({UserSettingsModal}) => {
				ModalCommands.push(
					ModalCommands.modal(() => (
						<UserSettingsModal data-flx="app.app.use-desktop-electron-bridges.user-settings-modal" />
					)),
				);
			});
		});
		const disposeTrayBridge = initializeDesktopTrayBridge();
		return () => {
			unsubZoomIn?.();
			unsubZoomOut?.();
			unsubZoomReset?.();
			unsubOpenSettings?.();
			disposeTrayBridge?.();
		};
	}, []);
}
