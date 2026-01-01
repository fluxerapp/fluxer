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
import React from 'react';
import {SettingsSection} from '~/components/modals/shared/SettingsSection';
import UserStore from '~/stores/UserStore';
import VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import {VideoTab} from '../VideoTab';
import {VoiceTab} from '../VoiceTab';
import styles from './Inline.module.css';

export const VoiceVideoInlineTab: React.FC = observer(() => {
	const {t} = useLingui();
	const user = UserStore.currentUser;
	const voiceSettings = VoiceSettingsStore;
	const hasPremium = React.useMemo(() => user?.isPremium() ?? false, [user]);

	return (
		<div className={styles.container}>
			<SettingsSection id="voice-audio" title={t`Audio`}>
				<VoiceTab voiceSettings={voiceSettings} hasPremium={hasPremium} autoRequestPermission={false} />
			</SettingsSection>
			<SettingsSection id="voice-video" title={t`Video`}>
				<VideoTab voiceSettings={voiceSettings} hasPremium={hasPremium} autoRequestPermission={false} />
			</SettingsSection>
		</div>
	);
});
