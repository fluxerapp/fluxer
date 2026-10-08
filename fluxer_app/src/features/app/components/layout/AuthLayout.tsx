// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n, {initI18n, isI18nInitialized} from '@app/app/I18n';
import styles from '@app/features/app/components/layout/AuthLayout.module.css';
import {CorruptedInstallationNagbar} from '@app/features/app/components/layout/app_layout/nagbars/CorruptedInstallationNagbar';
import {NativeDragRegion} from '@app/features/app/components/layout/NativeDragRegion';
import {NativeTitlebar} from '@app/features/app/components/layout/NativeTitlebar';
import {AuthShellHintKind, useAuthShellHintCapture} from '@app/features/app/components/skeleton/AuthShellHint';
import {useNativePlatform} from '@app/features/app/hooks/useNativePlatform';
import {useSetLayoutVariant} from '@app/features/app/state/LayoutVariantContext';
import {AuthBackground} from '@app/features/auth/flow/AuthBackground';
import {AuthCardContainer} from '@app/features/auth/flow/AuthCardContainer';
import {AuthRuntimeTargetBoundary} from '@app/features/auth/flow/AuthRuntimeTargetBoundary';
import {AuthSurfaceMotionKind, AuthSurfaceTransition} from '@app/features/auth/flow/AuthSurfaceTransition';
import {useSplashImageLoader} from '@app/features/auth/hooks/useAuthBackground';
import {
	type AuthCardVariant,
	AuthLayoutContentMode,
	AuthLayoutContext,
	type AuthLayoutContextType,
} from '@app/features/auth/state/AuthLayoutContext';
import {AuthRegisterDraftContext, useAuthRegisterDraft} from '@app/features/auth/state/AuthRegisterDraftContext';
import {AppI18nProvider} from '@app/features/i18n/components/AppI18nProvider';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {hasUnavailableElectronNativeContext} from '@app/features/ui/utils/NativeUtils';
import {useNativeTitleBar} from '@app/features/window/hooks/useNativeTitleBar';
import Window from '@app/features/window/state/Window';
import {flxElementClassName} from '@app/lib/react';
import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import {GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import {
	type Dispatch,
	type ReactNode,
	type SetStateAction,
	useCallback,
	useEffect,
	useLayoutEffect,
	useMemo,
	useRef,
	useState,
} from 'react';

interface ResolveAuthLayoutContentModeRequest {
	current: AuthLayoutContentMode | null;
	value: SetStateAction<AuthLayoutContentMode>;
}

function authSurfaceMotionKind(contentMode: AuthLayoutContentMode): AuthSurfaceMotionKind {
	if (contentMode === AuthLayoutContentMode.FULL) {
		return AuthSurfaceMotionKind.FULLSCREEN;
	}
	return AuthSurfaceMotionKind.CARD;
}

function resolveAuthLayoutContentMode({current, value}: ResolveAuthLayoutContentModeRequest): AuthLayoutContentMode {
	const currentMode = current ?? AuthLayoutContentMode.CARD;
	if (typeof value === 'function') {
		return value(currentMode);
	}
	return value;
}

const AuthLayoutContent = observer(function AuthLayoutContent({children}: {children?: ReactNode}) {
	const {width: viewportWidth, height: viewportHeight} = Window.windowSize;
	const [splashUrl, setSplashUrl] = useState<string | null>(null);
	const [cardVariant, setCardVariant] = useState<AuthCardVariant>('default');
	const [contentMode, setContentMode] = useState<AuthLayoutContentMode | null>(null);
	const [splashAlignment, setSplashAlignment] = useState<GuildSplashCardAlignmentValue>(
		GuildSplashCardAlignment.CENTER,
	);
	const {isNative, platform} = useNativePlatform();
	const useSystemTitleBar = useNativeTitleBar();
	const splashUrlRef = useRef<string | null>(null);
	const scrollerRef = useRef<ScrollerHandle>(null);
	const mainRef = useRef<HTMLElement>(null);
	const location = useLocation();
	const {dimensions: splashDimensions} = useSplashImageLoader(splashUrl);
	const handleSetSplashUrl = useCallback(
		(url: string | null) => {
			if (splashUrlRef.current === url) return;
			splashUrlRef.current = url;
			setSplashUrl(url);
			if (!url) {
				setSplashAlignment(GuildSplashCardAlignment.CENTER);
			}
		},
		[setSplashAlignment],
	);
	useEffect(() => {
		document.documentElement.classList.add('auth-page');
		return () => {
			document.documentElement.classList.remove('auth-page');
		};
	}, []);
	useEffect(() => {
		scrollerRef.current?.jumpToStartEdge();
	}, [location.pathname]);
	useLayoutEffect(() => {
		setContentMode((current) => current ?? AuthLayoutContentMode.CARD);
	}, []);
	const splashScale = useMemo(() => {
		if (!splashDimensions) return null;
		const {width, height} = splashDimensions;
		if (width <= 0 || height <= 0) return null;
		const heightScale = viewportHeight / height;
		const widthScale = viewportWidth / width;
		return Math.max(heightScale, widthScale);
	}, [splashDimensions, viewportHeight, viewportWidth]);
	const handleSetContentMode = useCallback<Dispatch<SetStateAction<AuthLayoutContentMode>>>((value) => {
		setContentMode((current) => resolveAuthLayoutContentMode({current, value}));
	}, []);
	const authLayoutContextValue = useMemo<AuthLayoutContextType>(
		() => ({
			setSplashUrl: handleSetSplashUrl,
			setCardVariant,
			setContentMode: handleSetContentMode,
			setSplashCardAlignment: setSplashAlignment,
		}),
		[handleSetContentMode, handleSetSplashUrl],
	);
	const authRegisterDraftContextValue = useAuthRegisterDraft();
	const isMobileExperience = isMobileExperienceEnabled();
	const showCorruptedInstallationNagbar = hasUnavailableElectronNativeContext();
	const isProbing = contentMode == null;
	const resolvedMode = contentMode ?? AuthLayoutContentMode.CARD;
	const surfaceKey = `${resolvedMode}:${location.pathname}`;
	const isFullscreen = resolvedMode === AuthLayoutContentMode.FULL;
	const resolveAuthShellHintTarget = useCallback((): HTMLElement | null => {
		const main = mainRef.current;
		if (main == null || isMobileExperience || isFullscreen) {
			return main;
		}
		return main.querySelector<HTMLElement>('flx-auth-card-surface');
	}, [isFullscreen, isMobileExperience]);
	useAuthShellHintCapture({
		enabled: !isProbing,
		pathname: location.pathname,
		mobile: isMobileExperience,
		kind: isFullscreen ? AuthShellHintKind.FULL : AuthShellHintKind.CARD,
		resolveTarget: resolveAuthShellHintTarget,
	});
	if (isMobileExperience) {
		return (
			<AuthRegisterDraftContext.Provider value={authRegisterDraftContextValue}>
				<AuthLayoutContext.Provider value={authLayoutContextValue}>
					<NativeDragRegion
						className={styles.topDragRegion}
						data-flx="app.auth-layout.auth-layout-content.top-drag-region"
					/>
					{showCorruptedInstallationNagbar && (
						<flx-app-auth-layout-nagbar-host
							className={flxElementClassName(styles.nagbarHost)}
							data-flx="app.auth-layout.auth-layout-content.nagbar-host"
						>
							<CorruptedInstallationNagbar
								isMobile={true}
								data-flx="app.auth-layout.auth-layout-content.corrupted-installation-nagbar"
							/>
						</flx-app-auth-layout-nagbar-host>
					)}
					<flx-app-auth-layout-scroller
						className={flxElementClassName(styles.scrollerWrapper)}
						data-flx="app.auth-layout.auth-layout-content.scroller-wrapper"
					>
						<Scroller
							ref={scrollerRef}
							className={styles.mobileContainer}
							fade={false}
							key="auth-layout-mobile-scroller"
							data-flx="app.auth-layout.auth-layout-content.mobile-container"
						>
							<flx-app-auth-layout-mobile-main
								ref={mainRef}
								id="main-content"
								className={flxElementClassName(styles.mobileContent)}
								tabIndex={-1}
								data-flx="app.auth-layout.auth-layout-content.main-content"
							>
								<AuthSurfaceTransition
									surfaceKey={surfaceKey}
									motionKind={authSurfaceMotionKind(resolvedMode)}
									suppressed={isProbing}
									anchorsContentOnInteraction={false}
									className={clsx(
										isProbing && styles.presentationProbe,
										!isProbing && isFullscreen && styles.mobileFullscreenContent,
									)}
									data-flx="app.auth-layout.auth-layout-content.auth-surface-transition"
								>
									{children}
								</AuthSurfaceTransition>
							</flx-app-auth-layout-mobile-main>
						</Scroller>
					</flx-app-auth-layout-scroller>
				</AuthLayoutContext.Provider>
			</AuthRegisterDraftContext.Provider>
		);
	}
	return (
		<AuthRegisterDraftContext.Provider value={authRegisterDraftContextValue}>
			<AuthLayoutContext.Provider value={authLayoutContextValue}>
				<NativeDragRegion
					className={styles.topDragRegion}
					data-flx="app.auth-layout.auth-layout-content.top-drag-region--2"
				/>
				{showCorruptedInstallationNagbar && (
					<flx-app-auth-layout-nagbar-host
						className={flxElementClassName(styles.nagbarHost)}
						data-flx="app.auth-layout.auth-layout-content.nagbar-host--2"
					>
						<CorruptedInstallationNagbar
							isMobile={false}
							data-flx="app.auth-layout.auth-layout-content.corrupted-installation-nagbar--2"
						/>
					</flx-app-auth-layout-nagbar-host>
				)}
				<flx-app-auth-layout-scroller
					className={flxElementClassName(styles.scrollerWrapper)}
					data-flx="app.auth-layout.auth-layout-content.scroller-wrapper--2"
				>
					<Scroller
						ref={scrollerRef}
						className={styles.container}
						key="auth-layout-scroller"
						data-flx="app.auth-layout.auth-layout-content.container"
					>
						{isNative && !useSystemTitleBar && (
							<NativeTitlebar platform={platform} data-flx="app.auth-layout.auth-layout-content.native-titlebar" />
						)}
						<flx-app-auth-layout-backdrop
							className={flxElementClassName(styles.characterBackground)}
							data-flx="app.auth-layout.auth-layout-content.character-background"
						>
							<AuthBackground
								splashUrl={splashUrl}
								splashDimensions={splashDimensions}
								splashScale={splashScale}
								splashAlignment={splashAlignment}
								useFullCover={false}
								data-flx="app.auth-layout.auth-layout-content.auth-background"
							/>
							<flx-app-auth-layout-split
								className={flxElementClassName(
									styles.leftSplit,
									splashAlignment === GuildSplashCardAlignment.LEFT && styles.alignLeft,
									splashAlignment === GuildSplashCardAlignment.RIGHT && styles.alignRight,
								)}
								data-flx="app.auth-layout.auth-layout-content.left-split"
							>
								<flx-app-auth-layout-split-column
									className={flxElementClassName(styles.leftSplitWrapper)}
									data-flx="app.auth-layout.auth-layout-content.left-split-wrapper"
								>
									<flx-app-auth-layout-main
										ref={mainRef}
										id="main-content"
										className={flxElementClassName(styles.leftSplitAnimated)}
										tabIndex={-1}
										data-flx="app.auth-layout.auth-layout-content.main-content--2"
									>
										<AuthSurfaceTransition
											surfaceKey={surfaceKey}
											motionKind={authSurfaceMotionKind(resolvedMode)}
											suppressed={isProbing}
											anchorsContentOnInteraction={!isFullscreen}
											className={clsx(
												isProbing && styles.presentationProbe,
												!isProbing && isFullscreen && styles.fullscreenContent,
												!isProbing && !isFullscreen && styles.cardMotionWrap,
											)}
											data-flx="app.auth-layout.auth-layout-content.auth-surface-transition--2"
										>
											<AuthCardContainer
												variant={cardVariant}
												presentation={resolvedMode}
												isInert={false}
												className={clsx(!isFullscreen && styles.cardContainerInMotion)}
												data-flx="app.auth-layout.auth-layout-content.auth-card-container"
											>
												{children}
											</AuthCardContainer>
										</AuthSurfaceTransition>
									</flx-app-auth-layout-main>
								</flx-app-auth-layout-split-column>
							</flx-app-auth-layout-split>
						</flx-app-auth-layout-backdrop>
					</Scroller>
				</flx-app-auth-layout-scroller>
			</AuthLayoutContext.Provider>
		</AuthRegisterDraftContext.Provider>
	);
});
export const AuthLayout = observer(function AuthLayout({children}: {children?: ReactNode}) {
	const [i18nReady, setI18nReady] = useState(isI18nInitialized);
	const setLayoutVariant = useSetLayoutVariant();
	useEffect(() => {
		setLayoutVariant('auth');
		return () => {
			setLayoutVariant('app');
		};
	}, [setLayoutVariant]);
	useEffect(() => {
		if (i18nReady) {
			return;
		}
		initI18n().then(() => {
			setI18nReady(true);
		});
	}, [i18nReady]);
	if (!i18nReady) {
		return null;
	}
	return (
		<AppI18nProvider i18n={i18n}>
			<AuthLayoutContent data-flx="app.auth-layout.auth-layout-content">
				<AuthRuntimeTargetBoundary data-flx="app.auth-layout.auth-runtime-target-boundary">
					{children}
				</AuthRuntimeTargetBoundary>
			</AuthLayoutContent>
		</AppI18nProvider>
	);
});
