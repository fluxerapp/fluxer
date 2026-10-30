// SPDX-License-Identifier: AGPL-3.0-or-later

import {Modals} from '@app/features/app/components/dialogs/Modals';
import styles from '@app/features/app/components/layout/GlobalOverlays.module.css';
import {MEDIA_VIEWER_PORTAL_ROOT_ID} from '@app/features/messaging/components/modals/MediaViewerPortal';
import {createNamedLoadableComponent} from '@app/features/platform/components/loadable/LoadableComponent';
import {useAppStorageDegradedNotice} from '@app/features/platform/hooks/useAppStorageDegradedNotice';
import * as QuickSwitcherCommands from '@app/features/search/commands/QuickSwitcherCommands';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import {ContextMenu} from '@app/features/ui/action_menu/ContextMenu';
import {Popouts} from '@app/features/ui/popover/PopoverPopouts';
import LayerManager from '@app/features/ui/state/LayerManager';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Toasts} from '@app/features/ui/toast/Toasts';
import {PiPOverlay} from '@app/features/voice/components/PiPOverlay';
import {VoicePopoutHost} from '@app/features/voice/components/popout/VoicePopoutHost';
import {handleContextMenu} from '@app/lib/overlay/OverlayContextMenu';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useState} from 'react';

const QuickSwitcherBottomSheet = createNamedLoadableComponent<Record<string, unknown>>({
	displayName: 'QuickSwitcherBottomSheet',
	load: async () =>
		(await import('@app/features/search/components/bottomsheets/QuickSwitcherBottomSheet')).QuickSwitcherBottomSheet,
});
const GlobalOverlays: React.FC = observer(() => {
	const isMobile = MobileLayout.isMobileLayout();
	const quickSwitcherOpen = QuickSwitcher.isOpen;
	const [quickSwitcherBottomSheetLoaded, setQuickSwitcherBottomSheetLoaded] = useState(false);
	useAppStorageDegradedNotice();
	useEffect(() => {
		LayerManager.init();
		document.addEventListener('contextmenu', handleContextMenu, false);
		return () => {
			document.removeEventListener('contextmenu', handleContextMenu, false);
		};
	}, []);
	useEffect(() => {
		if (isMobile && quickSwitcherOpen) {
			setQuickSwitcherBottomSheetLoaded(true);
		}
	}, [isMobile, quickSwitcherOpen]);
	return (
		<>
			<Modals data-flx="app.global-overlays.modals" />
			<Popouts ownerDocument={document} data-flx="app.global-overlays.popouts" />
			<div
				id={MEDIA_VIEWER_PORTAL_ROOT_ID}
				className={styles.mediaViewerPortal}
				data-overlay-pass-through="true"
				data-media-viewer-portal-root="true"
				data-flx="app.global-overlays.media-viewer-portal"
			/>
			<ContextMenu ownerDocument={document} data-flx="app.global-overlays.context-menu" />
			<Toasts data-flx="app.global-overlays.toasts" />
			<PiPOverlay data-flx="app.global-overlays.pi-p-overlay" />
			<VoicePopoutHost data-flx="app.global-overlays.voice-popout-host" />
			{isMobile && quickSwitcherBottomSheetLoaded && (
				<QuickSwitcherBottomSheet
					isOpen={quickSwitcherOpen}
					onClose={QuickSwitcherCommands.hide}
					data-flx="app.global-overlays.quick-switcher-bottom-sheet"
				/>
			)}
		</>
	);
});

export default GlobalOverlays;
