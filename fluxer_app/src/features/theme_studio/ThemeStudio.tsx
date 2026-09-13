// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Theme from '@app/features/theme/state/Theme';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import {AssetsSection} from '@app/features/theme_studio/sections/AssetsSection';
import {LibrarySection} from '@app/features/theme_studio/sections/LibrarySection';
import {QuickCssSection} from '@app/features/theme_studio/sections/QuickCssSection';
import {SettingsSection} from '@app/features/theme_studio/sections/ThemeStudioSettingsSection';
import {TokensSection} from '@app/features/theme_studio/sections/TokensSection';
import {
	broadcastThemeStudioMessage,
	useThemeStudioBroadcast,
} from '@app/features/theme_studio/state/ThemeStudioBroadcast';
import ThemeStudioState from '@app/features/theme_studio/state/ThemeStudioState';
import styles from '@app/features/theme_studio/ThemeStudio.module.css';
import {StudioSidebar, StudioSidebarFooterText, StudioSidebarItem} from '@app/features/theme_studio/ui/StudioSidebar';
import {StudioToolbar} from '@app/features/theme_studio/ui/StudioToolbar';
import {
	getThemeStudioBaseTheme,
	getThemeStudioFallbackDefaultVariables,
	pinThemeStudioDefaultVariables,
	type ThemeStudioBaseTheme,
} from '@app/features/theme_studio/utils/ThemeStudioPinnedVariables';
import {extractThemeVariableOverrides} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	FileCssIcon,
	GearIcon,
	ImageSquareIcon,
	PaintBrushBroadIcon,
	SlidersHorizontalIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {ReactNode} from 'react';
import {useCallback, useEffect, useMemo, useRef} from 'react';

const LIBRARY_DESCRIPTOR = msg({
	message: 'Library',
	comment: 'Short label in the theme studio. Keep it concise.',
});
const TOKENS_DESCRIPTOR = msg({
	message: 'Tokens',
	comment: 'Short label in the theme studio. Keep it concise. Keep the tone plain and specific.',
});
const QUICK_CSS_DESCRIPTOR = msg({
	message: 'Quick CSS',
	comment: 'Short label in the theme studio. Keep it concise.',
});
const ASSETS_DESCRIPTOR = msg({
	message: 'Assets',
	comment: 'Short label in the theme studio. Keep it concise.',
});
export const THEME_STUDIO_DESCRIPTOR = msg({
	message: 'Theme studio',
	comment: 'Short label in the theme studio. Keep it concise.',
});
const THEME_STUDIO_NAVIGATION_DESCRIPTOR = msg({
	message: 'Theme studio navigation',
	comment: 'Short label in the theme studio. Keep it concise.',
});
const DARK_BASE_DESCRIPTOR = msg({
	message: 'Dark base',
	comment: 'Short label in the theme studio. Keep it concise.',
});
const LIGHT_BASE_DESCRIPTOR = msg({
	message: 'Light base',
	comment: 'Short label in the theme studio. Keep it concise.',
});
const DARK_LEGACY_BASE_DESCRIPTOR = msg({
	message: 'Dark legacy base',
	comment: 'Short label in the theme studio. Keep it concise.',
});
const COAL_BASE_DESCRIPTOR = msg({
	message: 'Coal base',
	comment: 'Short label in the theme studio. Keep it concise.',
});
interface ThemeStudioProps {
	baseTheme?: ThemeStudioBaseTheme;
	defaultVariables?: Readonly<Record<string, string>>;
	windowControls?: ReactNode;
}

const ThemeStudioInner: React.FC<ThemeStudioProps> = observer(
	({baseTheme: providedBaseTheme, defaultVariables, windowControls}) => {
		const {i18n} = useLingui();
		const studioRootRef = useRef<HTMLDivElement | null>(null);
		useEffect(() => {
			void ThemeLibrary.init();
		}, []);
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
					case 'studio:opened-popout':
						ThemeStudioState.markPoppedOut(ThemeStudioState.popupRef);
						return;
					case 'studio:closed-popout':
						ThemeStudioState.clearPoppedOut();
						return;
					case 'studio:close-popout':
						window.close();
						return;
					case 'studio:focus-popout':
						window.focus();
						return;
					case 'themePreference':
						if (message.snapshot) {
							Theme.applyPreferenceSnapshot(message.snapshot);
						}
						return;
					default:
						return;
				}
			}, []),
		);
		useEffect(() => {
			const handler = () => {
				broadcastThemeStudioMessage({type: 'studio:closed-popout'});
			};
			window.addEventListener('beforeunload', handler);
			return () => window.removeEventListener('beforeunload', handler);
		}, []);
		const overrideCount = useMemo(
			() => Object.keys(extractThemeVariableOverrides(Accessibility.customThemeCss ?? '')).length,
			[Accessibility.customThemeCss],
		);
		const enabledThemeCount = ThemeLibrary.enabledThemeIds.length;
		const effectiveTheme = Theme.effectiveTheme;
		const baseTheme = providedBaseTheme ?? getThemeStudioBaseTheme(effectiveTheme);
		const studioDefaultVariables = defaultVariables ?? getThemeStudioFallbackDefaultVariables(baseTheme);
		const studioDefaultVariableStyle = studioDefaultVariables as React.CSSProperties;
		useEffect(
			() => pinThemeStudioDefaultVariables(studioRootRef.current, studioDefaultVariables),
			[studioDefaultVariables],
		);
		const renderSection = (): ReactNode => {
			switch (ThemeStudioState.section) {
				case 'library':
					return (
						<LibrarySection baseTheme={baseTheme} data-flx="theme-studio.theme-studio.render-section.library-section" />
					);
				case 'tokens':
					return (
						<TokensSection
							defaultVariableValues={studioDefaultVariables}
							data-flx="theme-studio.theme-studio.render-section.tokens-section"
						/>
					);
				case 'quickCss':
					return (
						<QuickCssSection
							baseTheme={baseTheme}
							data-flx="theme-studio.theme-studio.render-section.quick-css-section"
						/>
					);
				case 'assets':
					return <AssetsSection data-flx="theme-studio.theme-studio.render-section.assets-section" />;
				case 'settings':
					return <SettingsSection data-flx="theme-studio.theme-studio.render-section.settings-section" />;
				default:
					return (
						<TokensSection
							defaultVariableValues={studioDefaultVariables}
							data-flx="theme-studio.theme-studio.render-section.tokens-section--2"
						/>
					);
			}
		};
		const baseThemeLabel = useMemo(() => {
			switch (effectiveTheme) {
				case ThemeTypes.LIGHT:
					return i18n._(LIGHT_BASE_DESCRIPTOR);
				case ThemeTypes.DARK_LEGACY:
					return i18n._(DARK_LEGACY_BASE_DESCRIPTOR);
				case ThemeTypes.COAL:
					return i18n._(COAL_BASE_DESCRIPTOR);
				default:
					return i18n._(DARK_BASE_DESCRIPTOR);
			}
		}, [effectiveTheme, i18n.locale]);
		const sidebar = (
			<StudioSidebar
				ariaLabel={i18n._(THEME_STUDIO_NAVIGATION_DESCRIPTOR)}
				footer={
					<StudioSidebarFooterText data-flx="theme-studio.theme-studio.theme-studio-inner.studio-sidebar-footer-text">
						{baseThemeLabel}
					</StudioSidebarFooterText>
				}
				data-flx="theme-studio.theme-studio.theme-studio-inner.studio-sidebar"
			>
				<StudioSidebarItem
					icon={
						<PaintBrushBroadIcon
							size={16}
							weight="duotone"
							data-flx="theme-studio.theme-studio.theme-studio-inner.paint-brush-broad-icon"
						/>
					}
					label={i18n._(LIBRARY_DESCRIPTOR)}
					badge={enabledThemeCount > 0 ? enabledThemeCount : undefined}
					active={ThemeStudioState.section === 'library'}
					onClick={() => ThemeStudioState.setSection('library')}
					data-flx="theme-studio.theme-studio.theme-studio-inner.studio-sidebar-item.set-section"
				/>
				<StudioSidebarItem
					icon={
						<SlidersHorizontalIcon
							size={16}
							weight="duotone"
							data-flx="theme-studio.theme-studio.theme-studio-inner.sliders-horizontal-icon"
						/>
					}
					label={i18n._(TOKENS_DESCRIPTOR)}
					badge={overrideCount > 0 ? overrideCount : undefined}
					active={ThemeStudioState.section === 'tokens'}
					onClick={() => ThemeStudioState.setSection('tokens')}
					data-flx="theme-studio.theme-studio.theme-studio-inner.studio-sidebar-item.set-section--2"
				/>
				<StudioSidebarItem
					icon={
						<FileCssIcon
							size={16}
							weight="duotone"
							data-flx="theme-studio.theme-studio.theme-studio-inner.file-css-icon"
						/>
					}
					label={i18n._(QUICK_CSS_DESCRIPTOR)}
					active={ThemeStudioState.section === 'quickCss'}
					onClick={() => ThemeStudioState.setSection('quickCss')}
					data-flx="theme-studio.theme-studio.theme-studio-inner.studio-sidebar-item.set-section--3"
				/>
				<StudioSidebarItem
					icon={
						<ImageSquareIcon
							size={16}
							weight="duotone"
							data-flx="theme-studio.theme-studio.theme-studio-inner.image-square-icon"
						/>
					}
					label={i18n._(ASSETS_DESCRIPTOR)}
					badge={ThemeLibrary.assets.length + ThemeLibrary.localFiles.length || undefined}
					active={ThemeStudioState.section === 'assets'}
					onClick={() => ThemeStudioState.setSection('assets')}
					data-flx="theme-studio.theme-studio.theme-studio-inner.studio-sidebar-item.set-section--4"
				/>
				<StudioSidebarItem
					icon={
						<GearIcon size={16} weight="duotone" data-flx="theme-studio.theme-studio.theme-studio-inner.gear-icon" />
					}
					label={i18n._(SETTINGS_DESCRIPTOR)}
					active={ThemeStudioState.section === 'settings'}
					onClick={() => ThemeStudioState.setSection('settings')}
					data-flx="theme-studio.theme-studio.theme-studio-inner.studio-sidebar-item.set-section--5"
				/>
			</StudioSidebar>
		);
		const toolbar = (
			<StudioToolbar
				draggable
				trailing={windowControls}
				trailingEdgeToEdge={windowControls != null}
				data-flx="theme-studio.theme-studio.theme-studio-inner.studio-toolbar"
			/>
		);
		return (
			<div
				ref={studioRootRef}
				className={clsx(styles.studioRoot, styles.standaloneRoot)}
				data-base-theme={baseTheme}
				style={studioDefaultVariableStyle}
				role="main"
				aria-label={i18n._(THEME_STUDIO_DESCRIPTOR)}
				data-flx="theme-studio.theme-studio.theme-studio-inner.studio-root"
			>
				{toolbar}
				<div className={styles.bodyRow} data-flx="theme-studio.theme-studio.theme-studio-inner.body-row">
					{sidebar}
					<div className={styles.workspace} data-flx="theme-studio.theme-studio.theme-studio-inner.workspace">
						{renderSection()}
					</div>
				</div>
			</div>
		);
	},
);
export const ThemeStudio: React.FC<ThemeStudioProps> = (props) => (
	<ThemeStudioInner data-flx="theme-studio.theme-studio.theme-studio-inner" {...props} />
);

ThemeStudio.displayName = 'ThemeStudio';
