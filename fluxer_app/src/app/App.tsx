// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/app/App.module.css';
import {router} from '@app/app/Router';
import {SkipToContentLink} from '@app/app/SkipToContentLink';
import {NekoSprite} from '@app/features/accessibility/components/NekoSprite';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import AccountEngine from '@app/features/account_engine/state/AccountEngine';
import {DndContext} from '@app/features/app/components/layout/DndContext';
import GlobalOverlays from '@app/features/app/components/layout/GlobalOverlays';
import {NativeTitlebar} from '@app/features/app/components/layout/NativeTitlebar';
import {useAppPlatformBridges} from '@app/features/app/hooks/useAppPlatformBridges';
import {useDesktopAllowTransparency} from '@app/features/app/hooks/useDesktopAllowTransparency';
import {useDesktopElectronBridges} from '@app/features/app/hooks/useDesktopElectronBridges';
import {useDocumentClassToggle} from '@app/features/app/hooks/useDocumentClassToggle';
import {useInertBackground} from '@app/features/app/hooks/useInertBackground';
import {useIsRootDocumentFullscreen} from '@app/features/app/hooks/useIsRootDocumentFullscreen';
import {useKeybindManager} from '@app/features/app/hooks/useKeybindManager';
import {useNativePlatform} from '@app/features/app/hooks/useNativePlatform';
import {usePlatformClasses} from '@app/features/app/hooks/usePlatformClasses';
import {useServiceWorkerBadge} from '@app/features/app/hooks/useServiceWorkerBadge';
import {getInstanceBrandStyle, getInstanceCssOrigins} from '@app/features/app/state/InstanceBrandStyle';
import {type LayoutVariant, LayoutVariantProvider} from '@app/features/app/state/LayoutVariantContext';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import RuntimeCrash from '@app/features/app/state/RuntimeCrash';
import {showMyselfTypingHelper} from '@app/features/devtools/utils/ShowMyselfTypingHelper';
import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
import {AppI18nProvider} from '@app/features/i18n/components/AppI18nProvider';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import {createNamedLoadableComponent} from '@app/features/platform/components/loadable/LoadableComponent';
import {Outlet, RouterProvider} from '@app/features/platform/components/router/RouterReact';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
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
import ThemeSettingsState from '@app/features/theme_settings/state/ThemeSettingsState';
import {canTrustSites} from '@app/features/theme_settings/state/TrustedSites';
import {SVGMasks} from '@app/features/ui/components/SVGMasks';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {useTextInputContextMenu} from '@app/features/ui/hooks/useTextInputContextMenu';
import {getActivePortalHost, setActivePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Modal from '@app/features/ui/state/Modal';
import Popout from '@app/features/ui/state/Popout';
import {UNFOCUSED_FULLY_INTERACTIVE_CLASS} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import UserSettings from '@app/features/user/state/UserSettings';
import {IncomingCallManager} from '@app/features/voice/components/IncomingCallManager';
import {VoiceLiveKitRoot} from '@app/features/voice/components/VoiceLiveKitRoot';
import VoiceRuntime from '@app/features/voice/engine/VoiceRuntime';
import {useNativeTitleBar} from '@app/features/window/hooks/useNativeTitleBar';
import {useWindowEventListeners} from '@app/features/window/hooks/useWindowEventListeners';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import {i18n} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {IconContext} from '@phosphor-icons/react';
import {reaction} from 'mobx';
import {observer} from 'mobx-react-lite';
import React, {type ReactNode, useEffect, useMemo, useRef, useState} from 'react';

const NO_TRUSTED_SITES: ReadonlyArray<string> = [];
const QuickCssEditorHost = createNamedLoadableComponent<Record<string, never>>({
	displayName: 'QuickCssEditorHost',
	load: async () => (await import('@app/features/theme_settings/components/QuickCssEditorHost')).QuickCssEditorHost,
});

const logger = new Logger('App');

interface AppWrapperProps {
	children: ReactNode;
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
	const voiceRuntime = VoiceRuntime.module;
	const ringsContainerRef = useRef<HTMLDivElement>(null);
	useTextInputContextMenu();
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
	const quickCssEditorOpen = quickCssEditorAllowed && ThemeSettingsState.editorOpen;
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
	useInstanceCustomCss(instanceBrandStyle.customCss, getInstanceCssOrigins());
	const userCssApplied = !ThemeSettingsState.customCssOff;
	useCustomThemeStyle({
		enabledThemeCss: userCssApplied ? ThemeProfile.activeThemeCss : null,
		customThemeCss: userCssApplied ? ThemeProfile.customThemeCss : null,
		themeLibraryAssets: ThemeLibrary.assets,
		themeLibraryLocalFiles: ThemeLibrary.localFiles,
		themeLibraryRevision: ThemeLibrary.revision,
		mediaEndpoint: RuntimeConfig.getSnapshotOrNull()?.mediaEndpoint ?? '',
		trustedSites: canTrustSites() ? ThemeSettingsState.trustedSites : NO_TRUSTED_SITES,
		onMissingReferences: ThemeSettingsState.setMissingReferences,
		onBlockedUrl: ThemeSettingsState.recordBlockedUrl,
		onCssRejected: ThemeSettingsState.setCssRejected,
	});
	return (
		<LayoutVariantProvider value={layoutVariantContextValue} data-flx="app.app.app-wrapper.layout-variant-provider">
			<SVGMasks data-flx="app.app.app-wrapper.svg-masks" />
			<VoiceLiveKitRoot data-flx="app.app.app-wrapper.voice-live-kit-root">
				<div ref={ringsContainerRef} className={styles.appContainer} data-flx="app.app.app-wrapper.app-container">
					<FocusRingScope containerRef={ringsContainerRef} data-flx="app.app.app-wrapper.focus-ring-scope">
						<SkipToContentLink data-flx="app.app.app-wrapper.skip-to-content-link" />
						{isNative && !useSystemTitleBar && !isRootDocumentFullscreen && (
							<NativeTitlebar platform={platform} data-flx="app.app.app-wrapper.native-titlebar" />
						)}
						{children}
					</FocusRingScope>
				</div>
				<div className={styles.overlayScope} data-flx="app.app.app-wrapper.overlay-scope">
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
					{voiceRuntime ? (
						<voiceRuntime.VoiceRuntimeEffects data-flx="app.app.app-wrapper.voice-runtime-voice-runtime-effects" />
					) : null}
					{quickCssEditorOpen ? <QuickCssEditorHost data-flx="app.app.app-wrapper.quick-css-editor-host" /> : null}
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
	useAppPlatformBridges();
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
