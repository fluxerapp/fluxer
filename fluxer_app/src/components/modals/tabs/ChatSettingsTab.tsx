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
import {DisplayTabContent} from './ChatSettingsTab/DisplayTab';
import {InputTabContent} from './ChatSettingsTab/InputTab';
import {InteractionTabContent} from './ChatSettingsTab/InteractionTab';
import {MediaTabContent} from './ChatSettingsTab/MediaTab';

const ChatSettingsTab: React.FC = observer(() => {
	const {t} = useLingui();
	return (
		<SettingsTabContainer>
			<SettingsTabContent>
				<SettingsSection
					id="display"
					title={t`Display`}
					description={t`Control how messages, media, and other content are displayed.`}
				>
					<DisplayTabContent />
				</SettingsSection>

				<SettingsSection id="media" title={t`Media`} description={t`Customize media size preferences and buttons.`}>
					<MediaTabContent />
				</SettingsSection>

				<SettingsSection id="input" title={t`Input`} description={t`Customize message input settings.`}>
					<InputTabContent />
				</SettingsSection>

				<SettingsSection
					id="interaction"
					title={t`Interaction`}
					description={t`Configure message interaction settings.`}
					isAdvanced
					defaultExpanded={false}
				>
					<InteractionTabContent />
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default ChatSettingsTab;
