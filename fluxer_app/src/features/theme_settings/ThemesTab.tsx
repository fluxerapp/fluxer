// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import AccountThemeMode from '@app/features/theme/state/AccountThemeMode';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import {AssetsSection} from '@app/features/theme_settings/components/AssetsSection';
import {BaseThemeSection} from '@app/features/theme_settings/components/BaseThemeSection';
import {ColorsAndFontsSection} from '@app/features/theme_settings/components/ColorsAndFontsSection';
import {DesktopWindowSection} from '@app/features/theme_settings/components/DesktopWindowSection';
import {QuickCssSection} from '@app/features/theme_settings/components/QuickCssSection';
import {ShareAndBackUpSection} from '@app/features/theme_settings/components/ShareAndBackUpSection';
import {ThisAccountSection} from '@app/features/theme_settings/components/ThisAccountSection';
import {YourThemesSection} from '@app/features/theme_settings/components/YourThemesSection';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import inlineStyles from '@app/features/user/components/modals/tabs/TabInline.module.css';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

const ThemesSections: React.FC = observer(() => {
	useEffect(() => {
		void ThemeLibrary.init();
	}, []);
	return (
		<>
			{AccountThemeMode.canUseDesktopTheme ? (
				<ThisAccountSection data-flx="theme-settings.themes-tab.themes-sections.this-account-section" />
			) : null}
			<BaseThemeSection data-flx="theme-settings.themes-tab.themes-sections.base-theme-section" />
			<YourThemesSection data-flx="theme-settings.themes-tab.themes-sections.your-themes-section" />
			<ColorsAndFontsSection data-flx="theme-settings.themes-tab.themes-sections.colors-and-fonts-section" />
			<AssetsSection data-flx="theme-settings.themes-tab.themes-sections.assets-section" />
			{isDesktop() ? (
				<DesktopWindowSection data-flx="theme-settings.themes-tab.themes-sections.desktop-window-section" />
			) : null}
			<ShareAndBackUpSection data-flx="theme-settings.themes-tab.themes-sections.share-and-back-up-section" />
			<QuickCssSection data-flx="theme-settings.themes-tab.themes-sections.quick-css-section" />
		</>
	);
});

export const ThemesTab: React.FC = observer(() => (
	<SettingsTabContainer data-flx="theme-settings.themes-tab.settings-tab-container">
		<SettingsTabContent data-flx="theme-settings.themes-tab.settings-tab-content">
			<ThemesSections data-flx="theme-settings.themes-tab.themes-sections" />
		</SettingsTabContent>
	</SettingsTabContainer>
));

export const ThemesInlineContent: React.FC = observer(() => (
	<div className={inlineStyles.container} data-flx="theme-settings.themes-tab.themes-inline-content.div">
		<ThemesSections data-flx="theme-settings.themes-tab.themes-inline-content.themes-sections" />
	</div>
));
