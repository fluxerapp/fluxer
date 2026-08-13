// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/app/App.module.css';
import {router} from '@app/app/Router';
import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import {NekoSprite} from '@app/features/accessibility/components/NekoSprite';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import ScreenReader from '@app/features/accessibility/state/ScreenReader';
import {DndContext} from '@app/features/app/components/layout/DndContext';
import GlobalOverlays from '@app/features/app/components/layout/GlobalOverlays';
import {NativeTitlebar} from '@app/features/app/components/layout/NativeTitlebar';
import {useDesktopAllowTransparency} from '@app/features/app/hooks/useDesktopAllowTransparency';
import {useDesktopElectronBridges} from '@app/features/app/hooks/useDesktopElectronBridges';
import {useDocumentClassToggle} from '@app/features/app/hooks/useDocumentClassToggle';
import {useInertBackground} from '@app/features/app/hooks/useInertBackground';
import {useIsRootDocumentFullscreen} from '@app/features/app/hooks/useIsRootDocumentFullscreen';
import {useKeybindManager} from '@app/features/app/hooks/useKeybindManager';
import {useNativePlatform} from '@app/features/app/hooks/useNativePlatform';
import {usePlatformClasses} from '@app/features/app/hooks/usePlatformClasses';
import {useServiceWorkerBadge} from '@app/features/app/hooks/useServiceWorkerBadge';
import {useTabKeyFocusGuard} from '@app/features/app/hooks/useTabKeyFocusGuard';
import Initialization from '@app/features/app/state/Initialization';
import {type LayoutVariant, LayoutVariantProvider} from '@app/features/app/state/LayoutVariantContext';
import RuntimeCrash from '@app/features/app/state/RuntimeCrash';
import Authentication from '@app/features/auth/state/Authentication';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {showMyselfTypingHelper} from '@app/features/devtools/utils/ShowMyselfTypingHelper';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import {startDeepLinkHandling} from '@app/features/navigation/utils/DeepLinkUtils';
import {Outlet, RouterProvider} from '@app/features/platform/components/router/RouterReact';
import {ensureAutostartDefaultEnabled} from '@app/features/platform/utils/Autostart';
import {startDesktopJumpListBridge} from '@app/features/platform/utils/DesktopJumpListBridge';
import {startDesktopLocaleBridge} from '@app/features/platform/utils/DesktopLocaleBridge';
import {PremiumCheckoutReturnWatcher} from '@app/features/premium/components/PremiumCheckoutReturnWatcher';
import {QUICK_SWITCHER_PORTAL_ID} from '@app/features/search/components/quick_switcher/QuickSwitcherConstants';
import {useCustomThemeStyle} from '@app/features/theme/hooks/useCustomThemeStyle';
import {useThemeCssVariables} from '@app/features/theme/hooks/useThemeCssVariables';
import Theme from '@app/features/theme/state/Theme';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import {useThemeStudioBroadcast} from '@app/features/theme_studio/state/ThemeStudioBroadcast';
import ThemeStudioState from '@app/features/theme_studio/state/ThemeStudioState';
import {SVGMasks} from '@app/features/ui/components/SVGMasks';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {useTextInputContextMenu} from '@app/features/ui/hooks/useTextInputContextMenu';
import {getActivePortalHost, setActivePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Modal from '@app/features/ui/state/Modal';
import Popout from '@app/features/ui/state/Popout';
import {
	getDesktopWindowBehaviorSettings,
	setDesktopWindowBehaviorSettings,
} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {attachExternalLinkInterceptor, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {
	FIRST_CLICK_PASSTHROUGH_WHEN_UNFOCUSED_CLASS,
	UNFOCUSED_FULLY_INTERACTIVE_CLASS,
} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import UserSettings from '@app/features/user/state/UserSettings';
import {IncomingCallManager} from '@app/features/voice/components/IncomingCallManager';
import {VoiceLiveKitRoot} from '@app/features/voice/components/VoiceLiveKitRoot';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {useElectronScreenSharePicker} from '@app/features/voice/hooks/useElectronScreenSharePicker';
import {startScreenSharePiPController} from '@app/features/voice/state/ScreenSharePiPController';
import {startMediaDeviceStartupPreload} from '@app/features/voice/utils/MediaDeviceStartupPreload';
import {useNativeTitleBar} from '@app/features/window/hooks/useNativeTitleBar';
import {useStopFlashFrameOnFocus} from '@app/features/window/hooks/useStopFlashFrameOnFocus';
import {useWindowEventListeners} from '@app/features/window/hooks/useWindowEventListeners';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {I18nProvider} from '@lingui/react';
import {useLingui} from '@lingui/react/macro';
import {IconContext} from '@phosphor-icons/react';
import {reaction} from 'mobx';
import {observer} from 'mobx-react-lite';
import React, {type ReactNode, useCallback, useEffect, useMemo, useRef, useState} from 'react';

const SKIP_TO_CONTENT_DESCRIPTOR = msg({
	message: 'Skip to content',
	comment: 'Accessible skip-link label for keyboard users.',
});

interface AppWrapperProps {
	children: ReactNode;
}

export const AppWrapper = observer(({children}: AppWrapperProps) => {
	const {i18n} = useLingui();
	const reducedMotion = Accessibility.useReducedMotion;
	const stayInteractiveWhenUnfocused = Accessibility.stayInteractiveWhenUnfocused;
	const firstClickPassThroughWhenUnfocused = Accessibility.firstClickPassThroughWhenUnfocused;
	const {platform, isNative} = useNativePlatform();
	const useSystemTitleBar = useNativeTitleBar();
	const messageDisplayCompact = UserSettings.getMessageDisplayCompact();
	const isRootDocumentFullscreen = useIsRootDocumentFullscreen();
	const [layoutVariant, setLayoutVariant] = useState<LayoutVariant>('app');
	const layoutVariantContextValue = useMemo(
		() => ({variant: layoutVariant, setVariant: setLayoutVariant}),
		[layoutVariant],
	);
	const popouts = Popout.getPopouts();
	const topPopout = popouts.length ? popouts[popouts.length - 1] : null;
	const topPopoutRequiresBackdrop = Boolean(topPopout && !topPopout.disableBackdrop);
	const hasBlockingModal = Modal.hasModalOpen();
	const room = MediaEngine.room;
	const ringsContainerRef = useRef<HTMLDivElement>(null);
	const overlayScopeRef = useRef<HTMLDivElement>(null);
	useElectronScreenSharePicker();
	useTextInputContextMenu();
	useThemeStudioBroadcast(
		useCallback((message) => {
			switch (message.type) {
				case 'customThemeCss':
					if ((Accessibility.customThemeCss ?? null) !== message.value) {
						AccessibilityCommands.update({customThemeCss: message.value});
					}
					return;
				case 'themeLibrary':
					void ThemeLibrary.reload();
					return;
				case 'studio:closed-popout':
					ThemeStudioState.clearPoppedOut();
					return;
				default:
					return;
			}
		}, []),
	);
	const handleSkipLinkFocus = useTabKeyFocusGuard();
	const isSplashScreenActive =
		Authentication.isAuthenticated &&
		!DeveloperOptions.bypassSplashScreen &&
		(GatewayConnection.isConnectionInterrupted || !Initialization.canNavigateToProtectedRoutes);
	useInertBackground(ringsContainerRef, hasBlockingModal || topPopoutRequiresBackdrop);
	useInertBackground(overlayScopeRef, isSplashScreenActive);
	useEffect(() => {
		showMyselfTypingHelper.start();
		return () => showMyselfTypingHelper.stop();
	}, []);
	useEffect(
		() =>
			reaction(
				() => GatewayConnection.sessionId,
				(sessionId) => MemberSidebar.synchronizeGatewaySession(sessionId),
				{fireImmediately: true},
			),
		[],
	);
	useEffect(() => {
		const clearForeignPortalHost = (): void => {
			const activePortalHost = getActivePortalHost();
			if (!activePortalHost) return;
			if (activePortalHost.ownerDocument === document) return;
			setActivePortalHost(null);
		};
		document.addEventListener('pointerdown', clearForeignPortalHost, true);
		document.addEventListener('focusin', clearForeignPortalHost, true);
		document.addEventListener('keydown', clearForeignPortalHost, true);
		return () => {
			document.removeEventListener('pointerdown', clearForeignPortalHost, true);
			document.removeEventListener('focusin', clearForeignPortalHost, true);
			document.removeEventListener('keydown', clearForeignPortalHost, true);
		};
	}, []);
	useEffect(() => startScreenSharePiPController(), []);
	useEffect(() => startMediaDeviceStartupPreload(), []);
	useServiceWorkerBadge();
	useKeybindManager(i18n);
	useDesktopElectronBridges();
	useDocumentClassToggle('reduced-motion', reducedMotion);
	useDocumentClassToggle('mobile-layout', MobileLayout.platformMobileDetected || MobileLayout.enabled);
	useDocumentClassToggle(UNFOCUSED_FULLY_INTERACTIVE_CLASS, stayInteractiveWhenUnfocused);
	useDocumentClassToggle(FIRST_CLICK_PASSTHROUGH_WHEN_UNFOCUSED_CLASS, firstClickPassThroughWhenUnfocused);
	useEffect(() => {
		if (!isNative) return;
		void setDesktopWindowBehaviorSettings({firstClickPassThroughWhenUnfocused});
	}, [isNative, firstClickPassThroughWhenUnfocused]);
	useDesktopAllowTransparency(isNative);
	useWindowEventListeners({preventDocumentScroll: !isNative});
	usePlatformClasses(platform, isNative);
	useThemeCssVariables({
		effectiveTheme: Theme.effectiveTheme,
		saturationFactor: Accessibility.saturationFactor,
		alwaysUnderlineLinks: Accessibility.alwaysUnderlineLinks,
		dimStrikethroughText: Accessibility.dimStrikethroughText,
		enableTextSelection: Accessibility.textSelectionEnabled,
		fontSize: Accessibility.fontSize,
		messageGutter: Accessibility.messageGutter,
		messageGroupSpacing: Accessibility.getMessageGroupSpacingValue(messageDisplayCompact),
		hdrDisplayMode: Accessibility.hdrDisplayMode,
	});
	useCustomThemeStyle({
		enabledThemeCss: ThemeLibrary.activeThemeCss,
		customThemeCss: Accessibility.customThemeCss,
		themeLibraryAssets: ThemeLibrary.assets,
		themeLibraryLocalFiles: ThemeLibrary.localFiles,
		themeLibraryRevision: ThemeLibrary.revision,
	});
	return (
		<LayoutVariantProvider value={layoutVariantContextValue} data-flx="app.app.app-wrapper.layout-variant-provider">
			<SVGMasks data-flx="app.app.app-wrapper.svg-masks" />
			<VoiceLiveKitRoot room={room} data-flx="app.app.app-wrapper.voice-live-kit-root">
				<div ref={ringsContainerRef} className={styles.appContainer} data-flx="app.app.app-wrapper.app-container">
					<FocusRingScope containerRef={ringsContainerRef} data-flx="app.app.app-wrapper.focus-ring-scope">
						<a
							href="#main-content"
							className={styles.skipLink}
							onFocus={handleSkipLinkFocus}
							data-flx="app.app.app-wrapper.skip-link"
						>
							{i18n._(SKIP_TO_CONTENT_DESCRIPTOR)}
						</a>
						{isNative && !useSystemTitleBar && !isRootDocumentFullscreen && (
							<NativeTitlebar platform={platform} data-flx="app.app.app-wrapper.native-titlebar" />
						)}
						{children}
					</FocusRingScope>
				</div>
				<div ref={overlayScopeRef} className={styles.overlayScope} data-flx="app.app.app-wrapper.overlay-scope">
					<NekoSprite data-flx="app.app.app-wrapper.neko-sprite" />
					<div
						id={QUICK_SWITCHER_PORTAL_ID}
						className={styles.quickSwitcherPortal}
						data-overlay-pass-through="true"
						aria-hidden="true"
						data-flx="app.app.app-wrapper.quick-switcher-portal"
					/>
					<GlobalOverlays data-flx="app.app.app-wrapper.global-overlays" />
					<IncomingCallManager data-flx="app.app.app-wrapper.incoming-call-manager" />
				</div>
			</VoiceLiveKitRoot>
		</LayoutVariantProvider>
	);
});
export const App = observer((): React.ReactElement => {
	const fatalError = RuntimeCrash.fatalError;
	if (fatalError) {
		throw fatalError;
	}
	useEffect(() => {
		if (!isDesktop()) return;
		void ensureAutostartDefaultEnabled();
		void getDesktopWindowBehaviorSettings();
	}, []);
	useEffect(() => {
		void startDeepLinkHandling();
	}, []);
	useEffect(() => {
		if (!isDesktop()) return;
		startDesktopLocaleBridge();
		startDesktopJumpListBridge();
		ScreenReader.startDesktopBridge();
	}, []);
	useStopFlashFrameOnFocus();
	useEffect(() => {
		const detach = attachExternalLinkInterceptor();
		return () => detach?.();
	}, []);
	return (
		<I18nProvider i18n={i18n}>
			<IconContext.Provider value={{color: 'currentColor', weight: 'fill'}}>
				<PremiumCheckoutReturnWatcher data-flx="app.app.premium-checkout-return-watcher" />
				<DndContext data-flx="app.app.dnd-context">
					<RouterProvider router={router}>
						<AppWrapper data-flx="app.app.app-wrapper">
							<Outlet />
						</AppWrapper>
					</RouterProvider>
				</DndContext>
			</IconContext.Provider>
		</I18nProvider>
	);
});
