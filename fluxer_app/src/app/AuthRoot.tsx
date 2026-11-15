// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/app/App.module.css';
import {authRouter, disposeAuthRouter} from '@app/app/AuthRouter';
import {SkipToContentLink} from '@app/app/SkipToContentLink';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ModalStack} from '@app/features/app/components/dialogs/ModalStack';
import modalStyles from '@app/features/app/components/dialogs/Modals.module.css';
import {NativeTitlebar} from '@app/features/app/components/layout/NativeTitlebar';
import {useAppPlatformBridges} from '@app/features/app/hooks/useAppPlatformBridges';
import {useDesktopAllowTransparency} from '@app/features/app/hooks/useDesktopAllowTransparency';
import {useDesktopElectronBridges} from '@app/features/app/hooks/useDesktopElectronBridges';
import {useDocumentClassToggle} from '@app/features/app/hooks/useDocumentClassToggle';
import {useInertBackground} from '@app/features/app/hooks/useInertBackground';
import {useIsRootDocumentFullscreen} from '@app/features/app/hooks/useIsRootDocumentFullscreen';
import {useNativePlatform} from '@app/features/app/hooks/useNativePlatform';
import {usePlatformClasses} from '@app/features/app/hooks/usePlatformClasses';
import {getInstanceBrandStyle, getInstanceCssOrigins} from '@app/features/app/state/InstanceBrandStyle';
import {type LayoutVariant, LayoutVariantProvider} from '@app/features/app/state/LayoutVariantContext';
import RuntimeCrash from '@app/features/app/state/RuntimeCrash';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthRouteLoadError} from '@app/features/auth/flow/AuthRouteLoadError';
import Accounts from '@app/features/auth/state/Accounts';
import {AppI18nProvider} from '@app/features/i18n/components/AppI18nProvider';
import {
	createNamedLoadableComponent,
	type LoadableErrorProps,
} from '@app/features/platform/components/loadable/LoadableComponent';
import {Outlet, RouterProvider} from '@app/features/platform/components/router/RouterReact';
import {useAppStorageDegradedNotice} from '@app/features/platform/hooks/useAppStorageDegradedNotice';
import {prefetchBootApp} from '@app/features/platform/utils/BootAppPrefetch';
import {useInstanceCustomCss} from '@app/features/theme/hooks/useCustomThemeStyle';
import {useRemScaleTracking} from '@app/features/theme/hooks/useRemScaleTracking';
import {useThemeCssVariables} from '@app/features/theme/hooks/useThemeCssVariables';
import Theme from '@app/features/theme/state/Theme';
import WindowsFont from '@app/features/theme/state/WindowsFont';
import {ContextMenu} from '@app/features/ui/action_menu/ContextMenu';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {useTextInputContextMenu} from '@app/features/ui/hooks/useTextInputContextMenu';
import {Popouts} from '@app/features/ui/popover/PopoverPopouts';
import LayerManager from '@app/features/ui/state/LayerManager';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Modal from '@app/features/ui/state/Modal';
import Popout from '@app/features/ui/state/Popout';
import {Toasts} from '@app/features/ui/toast/Toasts';
import {UNFOCUSED_FULLY_INTERACTIVE_CLASS} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import UserSettings from '@app/features/user/state/UserSettings';
import {useNativeTitleBar} from '@app/features/window/hooks/useNativeTitleBar';
import {useWindowEventListeners} from '@app/features/window/hooks/useWindowEventListeners';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import {i18n} from '@lingui/core';
import {IconContext} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {type ComponentType, type ReactNode, useEffect, useMemo, useRef, useState} from 'react';

interface AuthRootProps {
	loadApp: () => Promise<ComponentType>;
}

const AuthOverlays = observer((): React.ReactElement => {
	useAppStorageDegradedNotice();
	useEffect(() => {
		LayerManager.init();
	}, []);
	return (
		<>
			<div
				className={modalStyles.modals}
				data-overlay-pass-through="true"
				data-flx="app.auth-root.auth-overlays.modals"
			>
				<ModalStack data-flx="app.auth-root.auth-overlays.modal-stack" />
			</div>
			<Popouts ownerDocument={document} data-flx="app.auth-root.auth-overlays.popouts" />
			<ContextMenu ownerDocument={document} data-flx="app.auth-root.auth-overlays.context-menu" />
			<Toasts data-flx="app.auth-root.auth-overlays.toasts" />
		</>
	);
});

const AuthWrapper = observer(({children}: {children: ReactNode}): React.ReactElement => {
	const {platform, isNative} = useNativePlatform();
	const useSystemTitleBar = useNativeTitleBar();
	const isRootDocumentFullscreen = useIsRootDocumentFullscreen();
	const [layoutVariant, setLayoutVariant] = useState<LayoutVariant>('app');
	const layoutVariantContextValue = useMemo(
		() => ({variant: layoutVariant, setVariant: setLayoutVariant}),
		[layoutVariant],
	);
	const ringsContainerRef = useRef<HTMLDivElement>(null);
	useTextInputContextMenu();
	useDesktopElectronBridges();
	useWindowEventListeners();
	useInertBackground(ringsContainerRef, Modal.hasModalOpen() || Popout.requiresBackdrop());
	useDocumentClassToggle('reduced-motion', Accessibility.useReducedMotion);
	useDocumentClassToggle('mobile-layout', MobileLayout.platformMobileDetected || MobileLayout.enabled);
	useDocumentClassToggle(UNFOCUSED_FULLY_INTERACTIVE_CLASS, Accessibility.stayInteractiveWhenUnfocused);
	useDocumentClassToggle('windows-fluxer-sans', WindowsFont.useFluxerSans);
	useDesktopAllowTransparency(isNative);
	useRemScaleTracking();
	usePlatformClasses(platform, isNative);
	const instanceBrandStyle = getInstanceBrandStyle();
	useThemeCssVariables({
		effectiveTheme: layoutVariant === 'auth' ? ThemeTypes.DARK : Theme.effectiveTheme,
		saturationFactor: Accessibility.saturationFactor,
		alwaysUnderlineLinks: Accessibility.alwaysUnderlineLinks,
		dimStrikethroughText: Accessibility.dimStrikethroughText,
		enableTextSelection: Accessibility.textSelectionEnabled,
		fontSize: Accessibility.fontSize,
		messageGutter: Accessibility.messageGutter,
		messageGroupSpacing: Accessibility.getMessageGroupSpacingValue(UserSettings.getMessageDisplayCompact()),
		hdrDisplayMode: Accessibility.hdrDisplayMode,
		instanceThemeColor: instanceBrandStyle.themeColor,
	});
	useInstanceCustomCss(instanceBrandStyle.customCss, getInstanceCssOrigins());
	return (
		<LayoutVariantProvider
			value={layoutVariantContextValue}
			data-flx="app.auth-root.auth-wrapper.layout-variant-provider"
		>
			<div ref={ringsContainerRef} className={styles.appContainer} data-flx="app.auth-root.auth-wrapper.app-container">
				<FocusRingScope containerRef={ringsContainerRef} data-flx="app.auth-root.auth-wrapper.focus-ring-scope">
					<SkipToContentLink data-flx="app.auth-root.auth-wrapper.skip-to-content-link" />
					{isNative && !useSystemTitleBar && !isRootDocumentFullscreen && (
						<NativeTitlebar platform={platform} data-flx="app.auth-root.auth-wrapper.native-titlebar" />
					)}
					{children}
				</FocusRingScope>
			</div>
			<div className={styles.overlayScope} data-flx="app.auth-root.auth-wrapper.overlay-scope">
				<AuthOverlays data-flx="app.auth-root.auth-wrapper.auth-overlays" />
			</div>
		</LayoutVariantProvider>
	);
});

const SignedOutApp = observer((): React.ReactElement => {
	const fatalError = RuntimeCrash.fatalError;
	if (fatalError) {
		throw fatalError;
	}
	useAppPlatformBridges();
	return (
		<AppI18nProvider i18n={i18n}>
			<IconContext.Provider value={{color: 'currentColor', weight: 'fill'}}>
				<RouterProvider router={authRouter}>
					<AuthWrapper data-flx="app.auth-root.signed-out-app.auth-wrapper">
						<Outlet />
					</AuthWrapper>
				</RouterProvider>
			</IconContext.Provider>
		</AppI18nProvider>
	);
});

function SignedInLoading(): React.ReactElement {
	return (
		<AppI18nProvider i18n={i18n}>
			<div className={styles.appContainer} data-flx="app.auth-root.signed-in-loading.app-container">
				<AuthLoadingState data-flx="app.auth-root.signed-in-loading.auth-loading-state" />
			</div>
		</AppI18nProvider>
	);
}

function SignedInLoadError(props: LoadableErrorProps): React.ReactElement {
	return (
		<AppI18nProvider i18n={i18n}>
			<div className={styles.appContainer} data-flx="app.auth-root.signed-in-load-error.app-container">
				<AuthRouteLoadError {...props} data-flx="app.auth-root.signed-in-load-error.auth-route-load-error" />
			</div>
		</AppI18nProvider>
	);
}

export const AuthRoot = observer(({loadApp}: AuthRootProps): React.ReactElement => {
	const [SignedInApp] = useState(() =>
		createNamedLoadableComponent<Record<string, never>>({
			displayName: 'SignedInApp',
			LoadingComponent: SignedInLoading,
			ErrorComponent: SignedInLoadError,
			load: async () => {
				const app = await loadApp();
				disposeAuthRouter();
				return app;
			},
		}),
	);
	useEffect(() => {
		let active = true;
		let handle: number | null = null;
		void authRouter.preload(`${window.location.pathname}${window.location.search}`).then(() => {
			if (active && typeof window.requestIdleCallback === 'function') {
				handle = window.requestIdleCallback(prefetchBootApp);
			}
		});
		return () => {
			active = false;
			if (handle !== null) {
				window.cancelIdleCallback(handle);
			}
		};
	}, []);
	if (Accounts.currentAccount !== null) {
		return <SignedInApp data-flx="app.auth-root.signed-in-app" />;
	}
	return <SignedOutApp data-flx="app.auth-root.signed-out-app" />;
});
