// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {GENERAL_DESCRIPTOR, SOUNDS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Notification from '@app/features/ui/state/Notification';
import Sound from '@app/features/ui/state/Sound';
import {MentionPreferenceTabContent} from '@app/features/user/components/modals/tabs/notifications_tab/MentionPreferenceTab';
import {Notifications} from '@app/features/user/components/modals/tabs/notifications_tab/Notifications';
import {Sounds} from '@app/features/user/components/modals/tabs/notifications_tab/NotificationsTabSounds';
import {TextToSpeech} from '@app/features/user/components/modals/tabs/notifications_tab/TextToSpeech';
import {useSoundSettings} from '@app/features/user/components/modals/tabs/notifications_tab/useSoundSettings';
import inlineStyles from '@app/features/user/components/modals/tabs/TabInline.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const MENTION_PREFERENCE_SECTION_TITLE_DESCRIPTOR = msg({
	message: 'Mention preference',
	comment: 'Notifications settings: section heading for the default reply-mention behavior.',
});
const TEXT_TO_SPEECH_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Text-to-speech notifications',
	comment: 'Notifications settings: section heading for text-to-speech notification behavior.',
});
const CONTROL_SPEECH_COMMANDS_AND_NARRATION_FOR_INCOMING_CONTENT_DESCRIPTOR = msg({
	message: 'Control speech commands and narration for incoming content.',
	comment: 'Description text in the inline.',
});
const INLINE_MENTION_PREFERENCE_DESCRIPTOR = msg({
	message: 'Mention preference',
	comment: 'Short label in the inline. Keep it concise.',
});
const INLINE_TEXT_TO_SPEECH_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Text-to-speech notifications',
	comment: 'Short label in the inline. Keep it concise.',
});
const NotificationsSections = observer(({inline}: {inline: boolean}) => {
	const {i18n} = useLingui();
	const browserNotificationsEnabled = Notification.browserNotificationsEnabled;
	const unreadMessageBadgeEnabled = Notification.unreadMessageBadgeEnabled;
	const soundSettings = Sound.settings;
	const {
		soundTypeLabels,
		customSounds,
		handleToggleAllSounds,
		handleToggleSound,
		handlePreviewSound,
		handleUploadClick,
		handleCustomSoundDelete,
		handleMasterVolumeChange,
		handleSoundOverrideChange,
		handleSoundOverrideReset,
		handleAllOverridesReset,
	} = useSoundSettings();
	const flx = inline ? 'user.notifications-tab.inline.notifications-inline-content' : 'user.notifications-tab';
	return (
		<>
			<SettingsSection id="notifications" title={i18n._(GENERAL_DESCRIPTOR)} data-flx={`${flx}.notifications`}>
				<Notifications
					browserNotificationsEnabled={browserNotificationsEnabled}
					unreadMessageBadgeEnabled={unreadMessageBadgeEnabled}
					data-flx={`${flx}.notifications--2`}
				/>
			</SettingsSection>
			<SettingsSection
				id="mention-preference"
				title={i18n._(inline ? INLINE_MENTION_PREFERENCE_DESCRIPTOR : MENTION_PREFERENCE_SECTION_TITLE_DESCRIPTOR)}
				data-flx={`${flx}.mention-preference`}
			>
				<MentionPreferenceTabContent data-flx={`${flx}.mention-preference-tab-content`} />
			</SettingsSection>
			<SettingsSection id="sounds" title={i18n._(SOUNDS_DESCRIPTOR)} data-flx={`${flx}.sounds`}>
				<Sounds
					soundSettings={soundSettings}
					soundTypeLabels={soundTypeLabels}
					customSounds={customSounds}
					isSoundEnabled={Sound.isSoundTypeEnabled}
					onToggleAllSounds={handleToggleAllSounds}
					onToggleSound={handleToggleSound}
					onPreviewSound={handlePreviewSound}
					onUploadClick={handleUploadClick}
					onCustomSoundDelete={handleCustomSoundDelete}
					onMasterVolumeChange={handleMasterVolumeChange}
					onSoundOverrideChange={handleSoundOverrideChange}
					onSoundOverrideReset={handleSoundOverrideReset}
					onAllOverridesReset={handleAllOverridesReset}
					data-flx={`${flx}.sounds--2`}
				/>
			</SettingsSection>
			<SettingsSection
				id="text-to-speech"
				title={i18n._(
					inline ? INLINE_TEXT_TO_SPEECH_NOTIFICATIONS_DESCRIPTOR : TEXT_TO_SPEECH_NOTIFICATIONS_DESCRIPTOR,
				)}
				description={inline ? i18n._(CONTROL_SPEECH_COMMANDS_AND_NARRATION_FOR_INCOMING_CONTENT_DESCRIPTOR) : undefined}
				data-flx={`${flx}.text-to-speech`}
			>
				<TextToSpeech data-flx={`${flx}.text-to-speech--2`} />
			</SettingsSection>
		</>
	);
});

const NotificationsTab: React.FC = () => (
	<SettingsTabContainer data-flx="user.notifications-tab.settings-tab-container">
		<SettingsTabContent data-flx="user.notifications-tab.settings-tab-content">
			<NotificationsSections inline={false} data-flx="user.notifications-tab.notifications-sections" />
		</SettingsTabContent>
	</SettingsTabContainer>
);

export const NotificationsInlineContent: React.FC = () => (
	<div
		className={inlineStyles.container}
		data-flx="user.notifications-tab.inline.notifications-inline-content.container"
	>
		<NotificationsSections inline={true} data-flx="user.notifications-tab.inline.notifications-sections" />
	</div>
);

export default NotificationsTab;
