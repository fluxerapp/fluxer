// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n, {initI18n} from '@app/app/I18n';
import styles from '@app/features/app/components/layout/AuthLayout.module.css';
import {CorruptedInstallationNagbar} from '@app/features/app/components/layout/app_layout/nagbars/CorruptedInstallationNagbar';
import {NativeDragRegion} from '@app/features/app/components/layout/NativeDragRegion';
import {NativeTitlebar} from '@app/features/app/components/layout/NativeTitlebar';
import {useNativePlatform} from '@app/features/app/hooks/useNativePlatform';
import {useSetLayoutVariant} from '@app/features/app/state/LayoutVariantContext';
import {AuthBackground} from '@app/features/auth/flow/AuthBackground';
import {AuthCardContainer} from '@app/features/auth/flow/AuthCardContainer';
import {useAuthBackground} from '@app/features/auth/hooks/useAuthBackground';
import {type AuthCardVariant, AuthLayoutContext} from '@app/features/auth/state/AuthLayoutContext';
import {AuthRegisterDraftContext, type AuthRegisterFormDraft} from '@app/features/auth/state/AuthRegisterDraftContext';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {FluxerWordmark} from '@app/features/ui/components/icons/FluxerWordmark';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {hasUnavailableElectronNativeContext} from '@app/features/ui/utils/NativeUtils';
import {useNativeTitleBar} from '@app/features/window/hooks/useNativeTitleBar';
import foodPatternUrl from '@app/media/images/i-like-food.svg';
import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import {GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import {I18nProvider} from '@lingui/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useCallback, useEffect, useMemo, useRef, useState} from 'react';

const AuthLayoutContent = observer(function AuthLayoutContent({children}: {children?: ReactNode}) {
	const [viewportHeight, setViewportHeight] = useState(() => window.innerHeight);
	const [viewportWidth, setViewportWidth] = useState(() => window.innerWidth);
	const [splashUrl, setSplashUrl] = useState<string | null>(null);
	const [showLogoSide, setShowLogoSide] = useState(true);
	const [cardVariant, setCardVariant] = useState<AuthCardVariant>('default');
	const [splashAlignment, setSplashAlignment] = useState<GuildSplashCardAlignmentValue>(
		GuildSplashCardAlignment.CENTER,
	);
	const {isNative, platform} = useNativePlatform();
	const useSystemTitleBar = useNativeTitleBar();
	const splashUrlRef = useRef<string | null>(null);
	const registerFormDraftsRef = useRef<Map<string, AuthRegisterFormDraft>>(new Map());
	const scrollerRef = useRef<ScrollerHandle>(null);
	const location = useLocation();
	const {patternReady, splashLoaded, splashDimensions} = useAuthBackground(splashUrl, foodPatternUrl);
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
		const handleResize = () => {
			setViewportWidth(window.innerWidth);
			setViewportHeight(window.innerHeight);
		};
		handleResize();
		window.addEventListener('resize', handleResize);
		return () => window.removeEventListener('resize', handleResize);
	}, []);
	useEffect(() => {
		document.documentElement.classList.add('auth-page');
		return () => {
			document.documentElement.classList.remove('auth-page');
		};
	}, []);
	useEffect(() => {
		scrollerRef.current?.scrollToTop();
	}, [location.pathname]);
	const splashScale = useMemo(() => {
		if (!splashDimensions) return null;
		const {width, height} = splashDimensions;
		if (width <= 0 || height <= 0) return null;
		const heightScale = viewportHeight / height;
		const widthScale = viewportWidth / width;
		return Math.max(heightScale, widthScale);
	}, [splashDimensions, viewportHeight, viewportWidth]);
	const getRegisterFormDraft = useCallback((draftKey: string): AuthRegisterFormDraft | undefined => {
		const draft = registerFormDraftsRef.current.get(draftKey);
		if (!draft) {
			return undefined;
		}
		return {
			...draft,
			formValues: {...draft.formValues},
		};
	}, []);
	const setRegisterFormDraft = useCallback((draftKey: string, draft: AuthRegisterFormDraft) => {
		registerFormDraftsRef.current.set(draftKey, {
			...draft,
			formValues: {...draft.formValues},
		});
	}, []);
	const clearRegisterFormDraft = useCallback((draftKey: string) => {
		registerFormDraftsRef.current.delete(draftKey);
	}, []);
	const authLayoutContextValue = useMemo(
		() => ({
			setSplashUrl: handleSetSplashUrl,
			setShowLogoSide,
			setCardVariant,
			setSplashCardAlignment: setSplashAlignment,
		}),
		[handleSetSplashUrl],
	);
	const authRegisterDraftContextValue = useMemo(
		() => ({
			getRegisterFormDraft,
			setRegisterFormDraft,
			clearRegisterFormDraft,
		}),
		[clearRegisterFormDraft, getRegisterFormDraft, setRegisterFormDraft],
	);
	const isMobileExperience = isMobileExperienceEnabled();
	const showCorruptedInstallationNagbar = hasUnavailableElectronNativeContext();
	if (isMobileExperience) {
		return (
			<AuthRegisterDraftContext.Provider value={authRegisterDraftContextValue}>
				<AuthLayoutContext.Provider value={authLayoutContextValue}>
					<NativeDragRegion
						className={styles.topDragRegion}
						data-flx="app.auth-layout.auth-layout-content.top-drag-region"
					/>
					{showCorruptedInstallationNagbar && (
						<div className={styles.nagbarHost} data-flx="app.auth-layout.auth-layout-content.nagbar-host">
							<CorruptedInstallationNagbar
								isMobile={true}
								data-flx="app.auth-layout.auth-layout-content.corrupted-installation-nagbar"
							/>
						</div>
					)}
					<div className={styles.scrollerWrapper} data-flx="app.auth-layout.auth-layout-content.scroller-wrapper">
						<Scroller
							ref={scrollerRef}
							className={styles.mobileContainer}
							fade={false}
							key="auth-layout-mobile-scroller"
							data-flx="app.auth-layout.auth-layout-content.mobile-container"
						>
							<div
								id="main-content"
								className={styles.mobileContent}
								tabIndex={-1}
								data-flx="app.auth-layout.auth-layout-content.main-content"
							>
								<div
									className={styles.mobileLogoContainer}
									data-flx="app.auth-layout.auth-layout-content.mobile-logo-container"
								>
									<FluxerWordmark
										variant="monochrome"
										className={styles.mobileWordmark}
										data-flx="app.auth-layout.auth-layout-content.mobile-wordmark"
									/>
								</div>
								{children}
							</div>
						</Scroller>
					</div>
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
					<div className={styles.nagbarHost} data-flx="app.auth-layout.auth-layout-content.nagbar-host--2">
						<CorruptedInstallationNagbar
							isMobile={false}
							data-flx="app.auth-layout.auth-layout-content.corrupted-installation-nagbar--2"
						/>
					</div>
				)}
				<div className={styles.scrollerWrapper} data-flx="app.auth-layout.auth-layout-content.scroller-wrapper--2">
					<Scroller
						ref={scrollerRef}
						className={styles.container}
						key="auth-layout-scroller"
						data-flx="app.auth-layout.auth-layout-content.container"
					>
						{isNative && !useSystemTitleBar && (
							<NativeTitlebar platform={platform} data-flx="app.auth-layout.auth-layout-content.native-titlebar" />
						)}
						<div
							className={styles.characterBackground}
							data-flx="app.auth-layout.auth-layout-content.character-background"
						>
							<AuthBackground
								splashUrl={splashUrl}
								splashLoaded={splashLoaded}
								splashDimensions={splashDimensions}
								splashScale={splashScale}
								patternReady={patternReady}
								patternImageUrl={foodPatternUrl}
								splashAlignment={splashAlignment}
								useFullCover={false}
								data-flx="app.auth-layout.auth-layout-content.auth-background"
							/>
							<div
								className={clsx(
									styles.leftSplit,
									splashAlignment === GuildSplashCardAlignment.LEFT && styles.alignLeft,
									splashAlignment === GuildSplashCardAlignment.RIGHT && styles.alignRight,
								)}
								data-flx="app.auth-layout.auth-layout-content.left-split"
							>
								<div
									className={styles.leftSplitWrapper}
									data-flx="app.auth-layout.auth-layout-content.left-split-wrapper"
								>
									<div
										id="main-content"
										className={styles.leftSplitAnimated}
										tabIndex={-1}
										data-flx="app.auth-layout.auth-layout-content.main-content--2"
									>
										<AuthCardContainer
											showLogoSide={showLogoSide}
											variant={cardVariant}
											isInert={false}
											data-flx="app.auth-layout.auth-layout-content.auth-card-container"
										>
											{children}
										</AuthCardContainer>
									</div>
								</div>
							</div>
						</div>
					</Scroller>
				</div>
			</AuthLayoutContext.Provider>
		</AuthRegisterDraftContext.Provider>
	);
});
export const AuthLayout = observer(function AuthLayout({children}: {children?: ReactNode}) {
	const [isI18nInitialized, setIsI18nInitialized] = useState(false);
	const setLayoutVariant = useSetLayoutVariant();
	useEffect(() => {
		setLayoutVariant('auth');
		return () => {
			setLayoutVariant('app');
		};
	}, [setLayoutVariant]);
	useEffect(() => {
		initI18n().then(() => {
			setIsI18nInitialized(true);
		});
	}, []);
	if (!isI18nInitialized) {
		return null;
	}
	return (
		<I18nProvider i18n={i18n}>
			<AuthLayoutContent data-flx="app.auth-layout.auth-layout-content">{children}</AuthLayoutContent>
		</I18nProvider>
	);
});
