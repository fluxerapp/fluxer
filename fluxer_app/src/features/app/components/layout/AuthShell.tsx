// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/AuthLayout.module.css';
import {CorruptedInstallationNagbar} from '@app/features/app/components/layout/app_layout/nagbars/CorruptedInstallationNagbar';
import {NativeDragRegion} from '@app/features/app/components/layout/NativeDragRegion';
import {NativeTitlebar} from '@app/features/app/components/layout/NativeTitlebar';
import {useNativePlatform} from '@app/features/app/hooks/useNativePlatform';
import {runtimeConfigSnapshotsAreSameInstance} from '@app/features/app/state/InstanceSnapshotStore';
import {useSetLayoutVariant} from '@app/features/app/state/LayoutVariantContext';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';
import {AuthBackdropScrim, AuthBackground} from '@app/features/auth/flow/AuthBackground';
import {type AuthBrand, resolveAuthBrand} from '@app/features/auth/InstanceBranding';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {hasUnavailableElectronNativeContext, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {useNativeTitleBar} from '@app/features/window/hooks/useNativeTitleBar';
import {flxElementClassName} from '@app/lib/react';
import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import {GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import type {ReactNode, Ref} from 'react';
import {useEffect, useLayoutEffect} from 'react';

export function resolveLayoutAuthBrand(snapshot: RuntimeConfigSnapshot | null): AuthBrand | null {
	if (snapshot == null) {
		return null;
	}
	if (!isDesktop()) {
		return resolveAuthBrand(snapshot, {isDocumentInstance: true, documentHost: window.location.host});
	}
	return resolveAuthBrand(snapshot, {
		isDocumentInstance: runtimeConfigSnapshotsAreSameInstance(snapshot, RuntimeConfig.getSnapshotOrNull()),
		documentHost: null,
	});
}

export function useAuthLayoutVariant(): void {
	const setLayoutVariant = useSetLayoutVariant();
	useLayoutEffect(() => {
		setLayoutVariant('auth');
		return () => {
			setLayoutVariant('app');
		};
	}, [setLayoutVariant]);
}

interface AuthShellProps {
	mobile: boolean;
	backdropUrl: string | null;
	splashUrl?: string | null;
	splashDimensions?: {width: number; height: number} | null;
	splashAlignment?: GuildSplashCardAlignmentValue;
	scrollerRef?: Ref<ScrollerHandle>;
	mainRef?: Ref<HTMLElement>;
	children?: ReactNode;
}

export function AuthShell({
	mobile,
	backdropUrl,
	splashUrl = null,
	splashDimensions = null,
	splashAlignment = GuildSplashCardAlignment.CENTER,
	scrollerRef,
	mainRef,
	children,
}: AuthShellProps) {
	const {isNative, platform} = useNativePlatform();
	const useSystemTitleBar = useNativeTitleBar();
	const showCorruptedInstallationNagbar = hasUnavailableElectronNativeContext();
	useEffect(() => {
		document.documentElement.classList.add('auth-page');
		return () => {
			document.documentElement.classList.remove('auth-page');
		};
	}, []);
	const chrome = (
		<>
			<NativeDragRegion className={styles.topDragRegion} data-flx="app.auth-shell.top-drag-region" />
			{showCorruptedInstallationNagbar && (
				<flx-app-auth-layout-nagbar-host
					className={flxElementClassName(styles.nagbarHost)}
					data-flx="app.auth-shell.nagbar-host"
				>
					<CorruptedInstallationNagbar isMobile={mobile} data-flx="app.auth-shell.corrupted-installation-nagbar" />
				</flx-app-auth-layout-nagbar-host>
			)}
		</>
	);
	if (mobile) {
		return (
			<>
				{chrome}
				<flx-app-auth-layout-scroller
					className={flxElementClassName(styles.scrollerWrapper)}
					data-flx="app.auth-shell.scroller-wrapper"
				>
					<Scroller
						ref={scrollerRef}
						className={styles.mobileContainer}
						fade={false}
						key="auth-layout-mobile-scroller"
						data-flx="app.auth-shell.mobile-container"
					>
						<AuthBackground
							className={styles.splashBackdrop}
							splashUrl={splashUrl}
							backdropUrl={backdropUrl}
							backdropScrim={AuthBackdropScrim.MOBILE}
							data-flx="app.auth-shell.mobile-auth-background"
						/>
						<flx-app-auth-layout-mobile-main
							ref={mainRef}
							id="main-content"
							className={flxElementClassName(styles.mobileContent)}
							tabIndex={-1}
							data-flx="app.auth-shell.mobile-main"
						>
							{children}
						</flx-app-auth-layout-mobile-main>
					</Scroller>
				</flx-app-auth-layout-scroller>
			</>
		);
	}
	return (
		<>
			{chrome}
			<flx-app-auth-layout-scroller
				className={flxElementClassName(styles.scrollerWrapper)}
				data-flx="app.auth-shell.scroller-wrapper--2"
			>
				<Scroller
					ref={scrollerRef}
					className={styles.container}
					key="auth-layout-scroller"
					data-flx="app.auth-shell.container"
				>
					{isNative && !useSystemTitleBar && (
						<NativeTitlebar platform={platform} data-flx="app.auth-shell.native-titlebar" />
					)}
					<flx-app-auth-layout-backdrop
						className={flxElementClassName(styles.characterBackground)}
						data-flx="app.auth-shell.character-background"
					>
						<AuthBackground
							className={styles.splashBackdrop}
							splashUrl={splashUrl}
							splashDimensions={splashDimensions}
							splashAlignment={splashAlignment}
							backdropUrl={backdropUrl}
							data-flx="app.auth-shell.auth-background"
						/>
						<flx-app-auth-layout-split
							className={flxElementClassName(
								styles.leftSplit,
								splashAlignment === GuildSplashCardAlignment.LEFT && styles.alignLeft,
								splashAlignment === GuildSplashCardAlignment.RIGHT && styles.alignRight,
							)}
							data-flx="app.auth-shell.left-split"
						>
							<flx-app-auth-layout-split-column
								className={flxElementClassName(styles.leftSplitWrapper)}
								data-flx="app.auth-shell.left-split-wrapper"
							>
								<flx-app-auth-layout-main
									ref={mainRef}
									id="main-content"
									className={flxElementClassName(styles.leftSplitAnimated)}
									tabIndex={-1}
									data-flx="app.auth-shell.main-content"
								>
									{children}
								</flx-app-auth-layout-main>
							</flx-app-auth-layout-split-column>
						</flx-app-auth-layout-split>
					</flx-app-auth-layout-backdrop>
				</Scroller>
			</flx-app-auth-layout-scroller>
		</>
	);
}
