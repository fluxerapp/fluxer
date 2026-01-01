/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {SettingsSection} from '~/components/modals/shared/SettingsSection';
import styles from './Inline.module.css';
import {InterfaceTabContent} from './InterfaceTab';
import {MessagesTabContent} from './MessagesTab';
import {ThemeTabContent} from './ThemeTab';

export const AppearanceInlineTab: React.FC = observer(() => {
	const {t} = useLingui();
	return (
		<div className={styles.container}>
			<SettingsSection id="appearance-theme" title={t`Theme`}>
				<ThemeTabContent />
			</SettingsSection>
			<SettingsSection id="appearance-messages" title={t`Messages`}>
				<MessagesTabContent />
			</SettingsSection>
			<SettingsSection id="appearance-interface" title={t`Interface`}>
				<InterfaceTabContent />
			</SettingsSection>
		</div>
	);
});
