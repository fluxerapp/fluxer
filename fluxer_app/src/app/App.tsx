// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/app/App.module.css';
import {router} from '@app/app/Router';
import {NekoSprite} from '@app/features/accessibility/components/NekoSprite';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import ScreenReader from '@app/features/accessibility/state/ScreenReader';
import AccountEngine from '@app/features/account_engine/state/AccountEngine';
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
import {type LayoutVariant, LayoutVariantProvider} from '@app/features/app/state/LayoutVariantContext';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import RuntimeCrash from '@app/features/app/state/RuntimeCrash';
import {getServedDocumentBranding} from '@app/features/app/state/ServedDocumentBranding';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {showMyselfTypingHelper} from '@app/features/devtools/utils/ShowMyselfTypingHelper';
import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
import {AppI18nProvider} from '@app/features/i18n/components/AppI18nProvider';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import {attachExternalLinkInterceptor} from '@app/features/messaging/utils/ExternalLinkUtils';
import {startDeepLinkHandling} from '@app/features/navigation/utils/DeepLinkUtils';
import {Outlet, RouterProvider} from '@app/features/platform/components/router/RouterReact';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ensureAutostartDefaultEnabled} from '@app/features/platform/utils/Autostart';
import {startDesktopJumpListBridge} from '@app/features/platform/utils/DesktopJumpListBridge';
import {startDesktopLocaleBridge} from '@app/features/platform/utils/DesktopLocaleBridge';
import {PremiumCheckoutReturnWatcher} from '@app/features/premium/components/PremiumCheckoutReturnWatcher';
import {QUICK_SWITCHER_PORTAL_ID} from '@app/features/search/components/quick_switcher/QuickSwitcherConstants';
import {useCustomThemeStyle, useInstanceCustomCss} from '@app/features/theme/hooks/useCustomThemeStyle';
import {useRemScaleTracking} from '@app/features/theme/hooks/useRemScaleTracking';
import {useThemeCssVariables} from '@app/features/theme/hooks/useThemeCssVariables';
import Theme from '@app/features/theme/state/Theme';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import ThemeProfile from '@app/features/theme/state/ThemeProfile';
import WindowsFont from '@app/features/theme/state/WindowsFont';
import * as ThemeSettingsCommands from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import {QuickCssEditorHost} from '@app/features/theme_settings/components/QuickCssEditorHost';
import ThemeSettingsState from '@app/features/theme_settings/state/ThemeSettingsState';
import {SVGMasks} from '@app/features/ui/components/SVGMasks';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {useTextInputContextMenu} from '@app/features/ui/hooks/useTextInputContextMenu';
import {getActivePortalHost, setActivePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Modal from '@app/features/ui/state/Modal';
import Popout from '@app/features/ui/state/Popout';
import {
	attachMiddleClickAutoscrollPasteGuard,
	getDesktopWindowBehaviorSettings,
} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {UNFOCUSED_FULLY_INTERACTIVE_CLASS} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import UserSettings from '@app/features/user/state/UserSettings';
import {IncomingCallManager} from '@app/features/voice/components/IncomingCallManager';
import {VoiceLiveKitRoot} from '@app/features/voice/components/VoiceLiveKitRoot';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {useElectronScreenSharePicker} from '@app/features/voice/hooks/useElectronScreenSharePicker';
import {startScreenSharePiPController} from '@app/features/voice/state/ScreenSharePiPController';
import {useNativeTitleBar} from '@app/features/window/hooks/useNativeTitleBar';
import {useStopFlashFrameOnFocus} from '@app/features/window/hooks/useStopFlashFrameOnFocus';
import {useWindowEventListeners} from '@app/features/window/hooks/useWindowEventListeners';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {IconContext} from '@phosphor-icons/react';
import {reaction} from 'mobx';
import {observer} from 'mobx-react-lite';
import React, {type ReactNode, useEffect, useMemo, useRef, useState} from 'react';

const SKIP_TO_CONTENT_DESCRIPTOR = msg({
	message: 'Skip to content',
	comment: 'Accessible skip-link label for keyboard users.',
});

function focusMainContent(event: React.MouseEvent<HTMLButtonElement>): void {
	event.currentTarget.ownerDocument.getElementById('main-content')?.focus();
}

const logger = new Logger('App');

interface AppWrapperProps {
	children: ReactNode;
}

interface InstanceBrandStyle {
	readonly themeColor: string | null;
	readonly customCss: string | null;
}

const NO_INSTANCE_BRAND_STYLE: InstanceBrandStyle = Object.freeze({themeColor: null, customCss: null});

function getInstanceBrandStyle(): InstanceBrandStyle {
	const snapshot = RuntimeConfig.getSnapshotOrNull();
	if (snapshot === null) {
		return getServedDocumentBranding();
	}
	if (!(DeveloperOptions.selfHostedModeOverride || snapshot.features.self_hosted)) {
		return NO_INSTANCE_BRAND_STYLE;
	}
	return {themeColor: snapshot.appPublic.branding.theme_color, customCss: snapshot.appPublic.branding.custom_css};
}

export const AppWrapper = observer(({children}: AppWrapperProps) => {
	const {i18n} = useLingui();
	const reducedMotion = Accessibility.useReducedMotion;
	const stayInteractiveWhenUnfocused = Accessibility.stayInteractiveWhenUnfocused;
	const {platform, isNative} = useNativePlatform();
	const useSystemTitleBar = useNativeTitleBar();
	const messageDisplayCompact = UserSettings.getMessageDisplayCompact();
	const isRootDocumentFullscreen = useIsRootDocumentFullscreen();
	const [layoutVariant, setLayoutVariant] = useState<LayoutVariant>('app');
	const layoutVariantContextValue = useMemo(
		() => ({variant: layoutVariant, setVariant: setLayoutVariant}),
		[layoutVariant],
	);
	const topPopoutRequiresBackdrop = Popout.requiresBackdrop();
	const hasBlockingModal = Modal.hasModalOpen();
	const room = MediaEngine.room;
	const ringsContainerRef = useRef<HTMLDivElement>(null);
	const overlayScopeRef = useRef<HTMLDivElement>(null);
	useElectronScreenSharePicker();
	useTextInputContextMenu();
	const handleSkipLinkFocus = useTabKeyFocusGuard();
	useInertBackground(ringsContainerRef, hasBlockingModal || topPopoutRequiresBackdrop);
	useEffect(() => {
		showMyselfTypingHelper.start();
		return () => showMyselfTypingHelper.stop();
	}, []);
	useEffect(() => {
		let active = true;
		AccountEngine.start();
		void SessionManager.initialize()
			.then(() => {
				if (active) {
					GatewaySessions.startRestoredSession();
				}
			})
			.catch((error: unknown) => {
				logger.error('Failed to initialize the restored gateway session', error);
			});
		return () => {
			active = false;
			GatewaySessions.dispose();
			AccountEngine.stop();
		};
	}, []);
	useEffect(
		() =>
			reaction(
				() => GatewaySessions.foregroundConnection.sessionId,
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
	useServiceWorkerBadge();
	useKeybindManager(i18n);
	useDesktopElectronBridges();
	useDocumentClassToggle('reduced-motion', reducedMotion);
	useDocumentClassToggle('mobile-layout', MobileLayout.platformMobileDetected || MobileLayout.enabled);
	useDocumentClassToggle(UNFOCUSED_FULLY_INTERACTIVE_CLASS, stayInteractiveWhenUnfocused);
	useDocumentClassToggle('windows-fluxer-sans', WindowsFont.useFluxerSans);
	useDesktopAllowTransparency(isNative);
	useWindowEventListeners();
	useRemScaleTracking();
	usePlatformClasses(platform, isNative);
	const isAuthLayout = layoutVariant === 'auth';
	const quickCssEditorAllowed = !isAuthLayout && !MobileLayout.enabled;
	useEffect(() => {
		if (!quickCssEditorAllowed) ThemeSettingsCommands.closeQuickCssEditor();
	}, [quickCssEditorAllowed]);
	const instanceBrandStyle = getInstanceBrandStyle();
	useThemeCssVariables({
		effectiveTheme: isAuthLayout ? ThemeTypes.DARK : Theme.effectiveTheme,
		saturationFactor: Accessibility.saturationFactor,
		alwaysUnderlineLinks: Accessibility.alwaysUnderlineLinks,
		dimStrikethroughText: Accessibility.dimStrikethroughText,
		enableTextSelection: Accessibility.textSelectionEnabled,
		fontSize: Accessibility.fontSize,
		messageGutter: Accessibility.messageGutter,
		messageGroupSpacing: Accessibility.getMessageGroupSpacingValue(messageDisplayCompact),
		hdrDisplayMode: Accessibility.hdrDisplayMode,
		instanceThemeColor: instanceBrandStyle.themeColor,
	});
	useInstanceCustomCss(instanceBrandStyle.customCss);
	const userCssApplied = !isAuthLayout && !ThemeSettingsState.customCssOff;
	useCustomThemeStyle({
		enabledThemeCss: userCssApplied ? ThemeProfile.activeThemeCss : null,
		customThemeCss: userCssApplied ? ThemeProfile.customThemeCss : null,
		themeLibraryAssets: ThemeLibrary.assets,
		themeLibraryLocalFiles: ThemeLibrary.localFiles,
		themeLibraryRevision: ThemeLibrary.revision,
		onMissingReferences: ThemeSettingsState.setMissingReferences,
		onBlockedUrl: ThemeSettingsState.recordBlockedUrl,
	});
	return (
		<LayoutVariantProvider value={layoutVariantContextValue} data-flx="app.app.app-wrapper.layout-variant-provider">
			<SVGMasks data-flx="app.app.app-wrapper.svg-masks" />
			<VoiceLiveKitRoot room={room} data-flx="app.app.app-wrapper.voice-live-kit-root">
				<div ref={ringsContainerRef} className={styles.appContainer} data-flx="app.app.app-wrapper.app-container">
					<FocusRingScope containerRef={ringsContainerRef} data-flx="app.app.app-wrapper.focus-ring-scope">
						<button
							type="button"
							className={styles.skipLink}
							onFocus={handleSkipLinkFocus}
							onClick={focusMainContent}
							data-flx="app.app.app-wrapper.skip-link"
						>
							{i18n._(SKIP_TO_CONTENT_DESCRIPTOR)}
						</button>
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
					{quickCssEditorAllowed ? <QuickCssEditorHost data-flx="app.app.app-wrapper.quick-css-editor-host" /> : null}
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
	return (
		<AppI18nProvider i18n={i18n}>
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
		</AppI18nProvider>
	);
});
