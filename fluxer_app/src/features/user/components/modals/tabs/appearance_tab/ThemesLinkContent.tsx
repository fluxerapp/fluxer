// SPDX-License-Identifier: AGPL-3.0-or-later

import {openThemesSettings} from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import styles from '@app/features/user/components/modals/tabs/appearance_tab/ThemesLinkContent.module.css';
import {Trans} from '@lingui/react/macro';
import type React from 'react';

export const ThemesLinkContent: React.FC = () => (
	<p className={styles.text} data-flx="user.appearance-tab.themes-link-content.text">
		<Trans comment='Shown in Settings > Look and feel. "Themes" is a link to the Themes settings tab.'>
			You can pick a dark or light theme and change colors and quick CSS in{' '}
			<FocusRing offset={-2} data-flx="user.appearance-tab.themes-link-content.focus-ring">
				<button
					type="button"
					className={styles.link}
					onClick={openThemesSettings}
					data-flx="user.appearance-tab.themes-link-content.link.open-themes-tab.button"
				>
					"Themes"
				</button>
			</FocusRing>
			.
		</Trans>
	</p>
);
