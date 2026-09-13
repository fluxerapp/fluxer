// SPDX-License-Identifier: AGPL-3.0-or-later

import {ModalStack} from '@app/features/app/components/dialogs/ModalStack';
import nativeTitlebarStyles from '@app/features/app/components/layout/NativeTitlebar.module.css';
import {NativeWindowControls} from '@app/features/app/components/layout/NativeWindowControls';
import {useNativePlatform} from '@app/features/app/hooks/useNativePlatform';
import {usePlatformClasses} from '@app/features/app/hooks/usePlatformClasses';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Theme from '@app/features/theme/state/Theme';
import {THEME_STUDIO_DESCRIPTOR, ThemeStudio} from '@app/features/theme_studio/ThemeStudio';
import styles from '@app/features/theme_studio/ThemeStudio.module.css';
import {
	getThemeStudioBaseTheme,
	getThemeStudioFallbackDefaultVariables,
	pinThemeStudioDefaultVariables,
	readThemeStudioComputedDefaultVariables,
} from '@app/features/theme_studio/utils/ThemeStudioPinnedVariables';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Toasts} from '@app/features/ui/toast/Toasts';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {useNativeTitleBar} from '@app/features/window/hooks/useNativeTitleBar';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {IconContext, PushPinIcon, PushPinSlashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const logger = new Logger('ThemeStudioStandaloneApp');
const THEME_STUDIO_POPOUT_KEY = 'fluxer_theme_studio';
const STAY_ON_TOP_DESCRIPTOR = msg({
	message: 'Stay on top',
	comment: 'Button label in the Theme Studio popout titlebar that pins the window above other windows.',
});
const REMOVE_FROM_TOP_DESCRIPTOR = msg({
	message: 'Remove from top',
	comment: 'Button label in the Theme Studio popout titlebar that unpins the window from staying above others.',
});

export const ThemeStudioStandaloneApp: React.FC = observer(() => {
	const {i18n} = useLingui();
	const standaloneRootRef = useRef<HTMLDivElement | null>(null);
	const effectiveTheme = Theme.effectiveTheme;
	const baseTheme = getThemeStudioBaseTheme(effectiveTheme);
	const fallbackDefaultVariables = useMemo(() => getThemeStudioFallbackDefaultVariables(baseTheme), [baseTheme]);
	const [standaloneDefaultVariables, setStandaloneDefaultVariables] =
		useState<Readonly<Record<string, string>>>(fallbackDefaultVariables);
	const {platform, isNative, isMacOS} = useNativePlatform();
	const useSystemTitleBar = useNativeTitleBar();
	const [isAlwaysOnTop, setIsAlwaysOnTop] = useState(true);
	useFluxerDocumentTitle(i18n._(THEME_STUDIO_DESCRIPTOR));
	usePlatformClasses(platform, isNative);
	useEffect(() => {
		const html = document.documentElement;
		html.classList.add(`theme-${effectiveTheme}`);
		return () => {
			html.classList.remove(`theme-${effectiveTheme}`);
		};
	}, [effectiveTheme]);
	useEffect(() => {
		setStandaloneDefaultVariables(readThemeStudioComputedDefaultVariables(fallbackDefaultVariables));
	}, [fallbackDefaultVariables, effectiveTheme]);
	useEffect(
		() => pinThemeStudioDefaultVariables(standaloneRootRef.current, standaloneDefaultVariables),
		[standaloneDefaultVariables],
	);
	const handleToggleAlwaysOnTop = useCallback(() => {
		const nextAlwaysOnTop = !isAlwaysOnTop;
		void getElectronAPI()
			?.popoutSetAlwaysOnTop?.(THEME_STUDIO_POPOUT_KEY, nextAlwaysOnTop)
			.then((changed) => {
				if (changed) {
					setIsAlwaysOnTop(nextAlwaysOnTop);
				}
			})
			.catch((error) => {
				logger.warn('Failed to toggle Theme Studio always-on-top', error);
			});
	}, [isAlwaysOnTop]);
	const pinLabel = isAlwaysOnTop ? i18n._(REMOVE_FROM_TOP_DESCRIPTOR) : i18n._(STAY_ON_TOP_DESCRIPTOR);
	const PinIcon = isAlwaysOnTop ? PushPinSlashIcon : PushPinIcon;
	return (
		<IconContext.Provider value={{color: 'currentColor', weight: 'fill'}}>
			<div
				ref={standaloneRootRef}
				className={styles.standaloneAppRoot}
				style={standaloneDefaultVariables as React.CSSProperties}
				data-flx="theme-studio.theme-studio-standalone-app.standalone-app-root"
			>
				<ThemeStudio
					baseTheme={baseTheme}
					defaultVariables={standaloneDefaultVariables}
					windowControls={
						isNative ? (
							<div
								className={styles.standaloneWindowControls}
								data-flx="theme-studio.theme-studio-standalone-app.window-controls"
							>
								<FocusRing offset={-2} data-flx="theme-studio.theme-studio-standalone-app.focus-ring.pin">
									<button
										type="button"
										tabIndex={-1}
										className={clsx(nativeTitlebarStyles.controlButton, isAlwaysOnTop && styles.windowControlActive)}
										onClick={handleToggleAlwaysOnTop}
										aria-pressed={isAlwaysOnTop}
										aria-label={pinLabel}
										title={pinLabel}
										data-flx="theme-studio.theme-studio-standalone-app.control-button.toggle-always-on-top"
									>
										<PinIcon weight="bold" data-flx="theme-studio.theme-studio-standalone-app.pin-icon" />
									</button>
								</FocusRing>
								{!isMacOS && !useSystemTitleBar && (
									<NativeWindowControls data-flx="theme-studio.theme-studio-standalone-app.native-window-controls" />
								)}
							</div>
						) : null
					}
					data-flx="theme-studio.theme-studio-standalone-app.theme-studio"
				/>
				<ModalStack data-flx="theme-studio.theme-studio-standalone-app.modal-stack" />
				<Toasts data-flx="theme-studio.theme-studio-standalone-app.toasts" />
			</div>
		</IconContext.Provider>
	);
});
