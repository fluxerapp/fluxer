// SPDX-License-Identifier: AGPL-3.0-or-later

import ScreenReader from '@app/features/accessibility/state/ScreenReader';
import {attachExternalLinkInterceptor} from '@app/features/messaging/utils/ExternalLinkUtils';
import {startDeepLinkHandling} from '@app/features/navigation/utils/DeepLinkUtils';
import {ensureAutostartDefaultEnabled} from '@app/features/platform/utils/Autostart';
import {startDesktopJumpListBridge} from '@app/features/platform/utils/DesktopJumpListBridge';
import {startDesktopLocaleBridge} from '@app/features/platform/utils/DesktopLocaleBridge';
import {
	attachMiddleClickAutoscrollPasteGuard,
	getDesktopWindowBehaviorSettings,
} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {useStopFlashFrameOnFocus} from '@app/features/window/hooks/useStopFlashFrameOnFocus';
import {useEffect} from 'react';

export function useAppPlatformBridges(): void {
	useEffect(() => {
		if (!isDesktop()) return;
		void ensureAutostartDefaultEnabled();
		void getDesktopWindowBehaviorSettings();
		return attachMiddleClickAutoscrollPasteGuard(window);
	}, []);
	useEffect(() => {
		void startDeepLinkHandling();
	}, []);
	useEffect(() => {
		if (!isDesktop()) return;
		startDesktopLocaleBridge();
		const disposeJumpList = startDesktopJumpListBridge();
		ScreenReader.startDesktopBridge();
		return disposeJumpList;
	}, []);
	useStopFlashFrameOnFocus();
	useEffect(() => {
		const detach = attachExternalLinkInterceptor();
		return () => detach?.();
	}, []);
}
