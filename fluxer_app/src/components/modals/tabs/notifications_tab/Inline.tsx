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

import * as UserSettingsActionCreators from '@app/actions/UserSettingsActionCreators';
import {SettingsSection} from '@app/components/modals/shared/SettingsSection';
import styles from '@app/components/modals/tabs/notifications_tab/Inline.module.css';
import {Notifications} from '@app/components/modals/tabs/notifications_tab/Notifications';
import {PushSettings} from '@app/components/modals/tabs/notifications_tab/PushSettings';
import {Sounds} from '@app/components/modals/tabs/notifications_tab/Sounds';
import {TextToSpeech} from '@app/components/modals/tabs/notifications_tab/TextToSpeech';
import {useSoundSettings} from '@app/components/modals/tabs/notifications_tab/useSoundSettings';
import NotificationStore from '@app/stores/NotificationStore';
import SoundStore from '@app/stores/SoundStore';
import UserSettingsStore from '@app/stores/UserSettingsStore';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export const NotificationsInlineContent: React.FC = observer(() => {
	const {t} = useLingui();
	const browserNotificationsEnabled = NotificationStore.browserNotificationsEnabled;
	const unreadMessageBadgeEnabled = NotificationStore.unreadMessageBadgeEnabled;
	const soundSettings = SoundStore.settings;
	const {afkTimeout} = UserSettingsStore;

	const {
		soundTypeLabels,
		customSounds,
		handleToggleAllSounds,
		handleToggleSound,
		handleEnableAllSounds,
		handleDisableAllSounds,
		handlePreviewSound,
		handleUploadClick,
		handleCustomSoundDelete,
	} = useSoundSettings();

	const handleAfkTimeoutChange = async (value: number) => {
		try {
			await UserSettingsActionCreators.update({afkTimeout: value * 60});
		} catch {}
	};

	return (
		<div className={styles.container}>
			<SettingsSection id="notifications" title={t`Notifications`}>
				<Notifications
					browserNotificationsEnabled={browserNotificationsEnabled}
					unreadMessageBadgeEnabled={unreadMessageBadgeEnabled}
				/>
			</SettingsSection>
			<SettingsSection id="sounds" title={t`Sounds`}>
				<Sounds
					soundSettings={soundSettings}
					soundTypeLabels={soundTypeLabels}
					customSounds={customSounds}
					onToggleAllSounds={handleToggleAllSounds}
					onToggleSound={handleToggleSound}
					onEnableAllSounds={handleEnableAllSounds}
					onDisableAllSounds={handleDisableAllSounds}
					onPreviewSound={handlePreviewSound}
					onUploadClick={handleUploadClick}
					onCustomSoundDelete={handleCustomSoundDelete}
				/>
			</SettingsSection>
			<SettingsSection
				id="text-to-speech"
				title={t`Text-to-speech`}
				description={t`Control speech commands and narration for incoming content.`}
			>
				<TextToSpeech />
			</SettingsSection>
			<SettingsSection id="push" title={t`Push Settings`}>
				<PushSettings afkTimeout={afkTimeout} onAfkTimeoutChange={handleAfkTimeoutChange} />
			</SettingsSection>
		</div>
	);
});
