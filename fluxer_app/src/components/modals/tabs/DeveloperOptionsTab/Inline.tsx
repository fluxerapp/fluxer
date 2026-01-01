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
import ConnectionStore from '~/stores/ConnectionStore';
import NagbarStore from '~/stores/NagbarStore';
import UserStore from '~/stores/UserStore';
import {AccountPremiumTabContent} from './AccountPremiumTab';
import {GeneralTabContent} from './GeneralTab';
import styles from './Inline.module.css';
import {MockingTabContent} from './MockingTab';
import {NagbarsTabContent} from './NagbarsTab';
import {ToolsTabContent} from './ToolsTab';

export const DeveloperOptionsInlineTab: React.FC = observer(() => {
	const {t} = useLingui();
	const socket = ConnectionStore.socket;
	const nagbarState = NagbarStore;
	const user = UserStore.currentUser;

	if (!(user && socket)) return null;

	return (
		<div className={styles.container}>
			<SettingsSection id="dev-general" title={t`General`}>
				<GeneralTabContent />
			</SettingsSection>
			<SettingsSection id="dev-account-premium" title={t`Account & Premium`}>
				<AccountPremiumTabContent user={user} />
			</SettingsSection>
			<SettingsSection id="dev-mocking" title={t`Mocking`}>
				<MockingTabContent />
			</SettingsSection>
			<SettingsSection id="dev-nagbars" title={t`Nagbars`}>
				<NagbarsTabContent nagbarState={nagbarState} />
			</SettingsSection>
			<SettingsSection id="dev-tools" title={t`Tools`}>
				<ToolsTabContent socket={socket} />
			</SettingsSection>
		</div>
	);
});
