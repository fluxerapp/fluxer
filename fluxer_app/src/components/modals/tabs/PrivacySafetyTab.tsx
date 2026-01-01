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
import {SettingsTabContainer, SettingsTabContent} from '~/components/modals/shared/SettingsTabLayout';
import {CommunicationTabContent} from './PrivacySafetyTab/CommunicationTab';
import {ConnectionsTabContent} from './PrivacySafetyTab/ConnectionsTab';
import {DataDeletionTabContent} from './PrivacySafetyTab/DataDeletionTab';
import {DataExportTabContent} from './PrivacySafetyTab/DataExportTab';

const PrivacySafetyTab: React.FC = observer(() => {
	const {t} = useLingui();
	return (
		<SettingsTabContainer>
			<SettingsTabContent>
				<SettingsSection
					id="connections"
					title={t`Connections`}
					description={t`Control who can send you friend requests and direct messages`}
				>
					<ConnectionsTabContent />
				</SettingsSection>

				<SettingsSection
					id="communication"
					title={t`Communication`}
					description={t`Control who can call you and add you to group chats`}
				>
					<CommunicationTabContent />
				</SettingsSection>

				<SettingsSection
					id="data-export"
					title={t`Data Export`}
					description={t`Download a complete package of your account data, including all messages and attachment URLs`}
					isAdvanced
					defaultExpanded={false}
				>
					<DataExportTabContent />
				</SettingsSection>

				<SettingsSection
					id="data-deletion"
					title={t`Data Deletion`}
					description={t`Permanently delete all messages you have sent across the platform`}
					isAdvanced
					defaultExpanded={false}
				>
					<DataDeletionTabContent />
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default PrivacySafetyTab;
