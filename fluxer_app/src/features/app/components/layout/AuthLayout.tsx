// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n, {initI18n, isI18nInitialized} from '@app/app/I18n';
import styles from '@app/features/app/components/layout/AuthLayout.module.css';
import {AuthShell, resolveLayoutAuthBrand, useAuthLayoutVariant} from '@app/features/app/components/layout/AuthShell';
import {AuthShellHintKind, useAuthShellHintCapture} from '@app/features/app/components/skeleton/AuthShellHint';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';
import {AuthBrandLayout, AuthBrandPanel, useAuthBrandLogo} from '@app/features/auth/components/AuthBrandPanel';
import {AccountRestrictionGate} from '@app/features/auth/flow/AccountRestrictionScreen';
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
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
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

const SPLIT_CARD_MEDIA_QUERY = '(min-width: 55rem)';

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

function isWideAuthViewport(): boolean {
	return typeof window.matchMedia === 'function' && window.matchMedia(SPLIT_CARD_MEDIA_QUERY).matches;
}

const AuthLayoutContent = observer(function AuthLayoutContent({children}: {children?: ReactNode}) {
	const [splashUrl, setSplashUrl] = useState<string | null>(null);
	const [brandSnapshot, setBrandSnapshot] = useState<RuntimeConfigSnapshot | null>(() =>
		RuntimeConfig.getSnapshotOrNull(),
	);
	const [cardVariant, setCardVariant] = useState<AuthCardVariant>('default');
	const [contentMode, setContentMode] = useState<AuthLayoutContentMode | null>(null);
	const [splashAlignment, setSplashAlignment] = useState<GuildSplashCardAlignmentValue>(
		GuildSplashCardAlignment.CENTER,
	);
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
		scrollerRef.current?.jumpToStartEdge();
	}, [location.pathname]);
	useLayoutEffect(() => {
		setContentMode((current) => current ?? AuthLayoutContentMode.CARD);
	}, []);
	const handleSetContentMode = useCallback<Dispatch<SetStateAction<AuthLayoutContentMode>>>((value) => {
		setContentMode((current) => resolveAuthLayoutContentMode({current, value}));
	}, []);
	const authLayoutContextValue = useMemo<AuthLayoutContextType>(
		() => ({
			setSplashUrl: handleSetSplashUrl,
			setBrandSnapshot,
			setCardVariant,
			setContentMode: handleSetContentMode,
			setSplashCardAlignment: setSplashAlignment,
		}),
		[handleSetContentMode, handleSetSplashUrl],
	);
	const authRegisterDraftContextValue = useAuthRegisterDraft();
	const isMobileExperience = isMobileExperienceEnabled();
	const isProbing = contentMode == null;
	const resolvedMode = contentMode ?? AuthLayoutContentMode.CARD;
	const surfaceKey = `${resolvedMode}:${location.pathname}`;
	const isFullscreen = resolvedMode === AuthLayoutContentMode.FULL;
	const brand = useMemo(() => resolveLayoutAuthBrand(brandSnapshot), [brandSnapshot]);
	const {logo: brandLogo, onLogoError: handleBrandLogoError} = useAuthBrandLogo(brand);
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
		split: !isMobileExperience && !isFullscreen && brandLogo != null && isWideAuthViewport(),
		resolveTarget: resolveAuthShellHintTarget,
	});
	return (
		<AuthRegisterDraftContext.Provider value={authRegisterDraftContextValue}>
			<AuthLayoutContext.Provider value={authLayoutContextValue}>
				<AuthShell
					mobile={isMobileExperience}
					backdropUrl={brand?.backdropUrl ?? null}
					splashUrl={splashUrl}
					splashDimensions={isMobileExperience ? null : splashDimensions}
					splashAlignment={splashAlignment}
					scrollerRef={scrollerRef}
					mainRef={mainRef}
					data-flx="app.auth-layout.auth-layout-content.auth-shell"
				>
					{isMobileExperience ? (
						<>
							{brand == null || isFullscreen ? null : (
								<AuthBrandPanel
									brand={brand}
									logo={brandLogo}
									onLogoError={handleBrandLogoError}
									layout={AuthBrandLayout.MOBILE}
									data-flx="app.auth-layout.auth-layout-content.mobile-brand"
								/>
							)}
							<AuthSurfaceTransition
								surfaceKey={surfaceKey}
								motionKind={authSurfaceMotionKind(resolvedMode)}
								suppressed={isProbing}
								className={clsx(
									isProbing && styles.presentationProbe,
									!isProbing && isFullscreen && styles.mobileFullscreenContent,
								)}
								data-flx="app.auth-layout.auth-layout-content.auth-surface-transition"
							>
								{children}
							</AuthSurfaceTransition>
						</>
					) : (
						<AuthSurfaceTransition
							surfaceKey={surfaceKey}
							motionKind={authSurfaceMotionKind(resolvedMode)}
							suppressed={isProbing}
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
								isSplit={brandLogo != null}
								brandPanel={
									brand == null ? null : (
										<AuthBrandPanel
											brand={brand}
											logo={brandLogo}
											onLogoError={handleBrandLogoError}
											layout={AuthBrandLayout.CARD}
											data-flx="app.auth-layout.auth-layout-content.card-brand"
										/>
									)
								}
								data-flx="app.auth-layout.auth-layout-content.auth-card-container"
							>
								{children}
							</AuthCardContainer>
						</AuthSurfaceTransition>
					)}
				</AuthShell>
			</AuthLayoutContext.Provider>
		</AuthRegisterDraftContext.Provider>
	);
});
export const AuthLayout = observer(function AuthLayout({children}: {children?: ReactNode}) {
	const [i18nReady, setI18nReady] = useState(isI18nInitialized);
	useAuthLayoutVariant();
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
					<AccountRestrictionGate data-flx="app.auth-layout.account-restriction-gate">
						{children}
					</AccountRestrictionGate>
				</AuthRuntimeTargetBoundary>
			</AuthLayoutContent>
		</AppI18nProvider>
	);
});
