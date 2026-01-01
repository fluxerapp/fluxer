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
import UserStore from '~/stores/UserStore';
import VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import {VideoTab} from './VideoTab';
import {VoiceTab} from './VoiceTab';

const VoiceVideoTab: React.FC = observer(() => {
	const {t} = useLingui();
	const user = UserStore.currentUser;
	const voiceSettings = VoiceSettingsStore;
	const hasPremium = user?.isPremium() ?? false;

	return (
		<SettingsTabContainer>
			<SettingsTabContent>
				<SettingsSection
					id="audio"
					title={t`Audio`}
					description={t`Configure your microphone, speakers, and input mode`}
				>
					<VoiceTab voiceSettings={voiceSettings} hasPremium={hasPremium} />
				</SettingsSection>

				<SettingsSection id="video" title={t`Video`} description={t`Configure your camera and video settings`}>
					<VideoTab voiceSettings={voiceSettings} hasPremium={hasPremium} />
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default VoiceVideoTab;
