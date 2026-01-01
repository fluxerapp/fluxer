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

import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {SettingsSection} from '~/components/modals/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '~/components/modals/shared/SettingsTabLayout';
import ConnectionStore from '~/stores/ConnectionStore';
import NagbarStore from '~/stores/NagbarStore';
import UserStore from '~/stores/UserStore';
import {AccountPremiumTabContent} from './AccountPremiumTab';
import {GeneralTabContent} from './GeneralTab';
import {MockingTabContent} from './MockingTab';
import {NagbarsTabContent} from './NagbarsTab';
import {ToolsTabContent} from './ToolsTab';
import {TypographyTabContent} from './TypographyTab';

const DeveloperOptionsTab: React.FC = observer(() => {
	const socket = ConnectionStore.socket;
	const nagbarState = NagbarStore;
	const user = UserStore.currentUser;

	if (!(user && socket)) return null;

	return (
		<SettingsTabContainer>
			<SettingsTabContent>
				<SettingsSection id="general" title={<Trans>General</Trans>}>
					<GeneralTabContent />
				</SettingsSection>

				<SettingsSection id="account_premium" title={<Trans>Account & Premium</Trans>}>
					<AccountPremiumTabContent user={user} />
				</SettingsSection>

				<SettingsSection id="mocking" title={<Trans>Mocking</Trans>}>
					<MockingTabContent />
				</SettingsSection>

				<SettingsSection id="nagbars" title={<Trans>Nagbars</Trans>}>
					<NagbarsTabContent nagbarState={nagbarState} />
				</SettingsSection>

				<SettingsSection id="tools" title={<Trans>Tools</Trans>}>
					<ToolsTabContent socket={socket} />
				</SettingsSection>

				<SettingsSection id="typography" title={<Trans>Typography</Trans>}>
					<TypographyTabContent />
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default DeveloperOptionsTab;
