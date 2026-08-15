// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import Notification, {TTSNotificationMode} from '@app/features/ui/state/Notification';
import styles from '@app/features/user/components/modals/tabs/notifications_tab/TextToSpeech.module.css';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {InfoIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const EVERY_CHANNEL_DESCRIPTOR = msg({
	message: 'Every channel',
	comment: 'Short label in the text to speech. Keep it concise.',
});
const LET_EVERY_INCOMING_MESSAGE_BE_SPOKEN_REGARDLESS_OF_DESCRIPTOR = msg({
	message: 'Let every incoming message be spoken, regardless of which channel is open.',
	comment: 'Description text in the text to speech.',
});
const ACTIVE_CHANNEL_ONLY_DESCRIPTOR = msg({
	message: 'Active channel only',
	comment: 'Short label in the text to speech. Keep it concise.',
});
const NARRATE_MESSAGES_ONLY_IN_WHICHEVER_CHANNEL_YOU_RE_DESCRIPTOR = msg({
	message: "Narrates only the channel you're viewing. Narration follows you between channels.",
	comment: 'Label in the text to speech.',
});
const NEVER_AUTOMATICALLY_DESCRIPTOR = msg({
	message: 'Never automatically',
	comment: 'Short label in the text to speech. Keep it concise.',
});
const REMAIN_SILENT_UNLESS_SOMEONE_RUNS_TTS_MANUALLY_DESCRIPTOR = msg({
	message: 'Remain silent unless someone runs /tts manually.',
	comment: 'Description text in the text to speech.',
});
const ENABLE_TTS_SPEECH_PLAYBACK_DESCRIPTOR = msg({
	message: 'Enable /tts speech playback',
	comment: 'Button or menu action label in the text to speech. Keep it concise.',
});
const LET_TTS_READ_YOUR_MESSAGE_ALOUD_DISABLING_THE_DESCRIPTOR = msg({
	message: 'Let /tts read your message aloud. Disabling the setting keeps those commands as regular text.',
	comment: 'Description text in the text to speech.',
});
const AUTOMATIC_MESSAGE_NARRATION_DESCRIPTOR = msg({
	message: 'Automatic message narration',
	comment: 'Short label in the text to speech. Keep it concise.',
});
const CONVERTS_INCOMING_CONTENT_TO_SPEECH_REGARDLESS_OF_WHETHER_DESCRIPTOR = msg({
	message: 'Converts incoming content to speech, regardless of whether it came from /tts.',
	comment: 'Description text in the text to speech.',
});
const SPEAK_ALL_MESSAGES_OUT_LOUD_DESCRIPTOR = msg({
	message: 'Speak all messages out loud',
	comment: 'Label in the text to speech.',
});
export const TextToSpeech = observer(() => {
	const {i18n} = useLingui();
	const handleToggleTtsCommand = useCallback((value: boolean) => {
		AccessibilityCommands.update({enableTTSCommand: value});
	}, []);
	const handleAccessibilityLinkClick = useCallback(() => {
		ComponentDispatch.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'accessibility', section: 'tts'});
	}, []);
	const ttsNotificationMode = Notification.getTTSNotificationMode();
	const ttsNotificationOptions: Array<RadioOption<TTSNotificationMode>> = [
		{
			value: TTSNotificationMode.FOR_ALL_CHANNELS,
			name: i18n._(EVERY_CHANNEL_DESCRIPTOR),
			desc: i18n._(LET_EVERY_INCOMING_MESSAGE_BE_SPOKEN_REGARDLESS_OF_DESCRIPTOR),
		},
		{
			value: TTSNotificationMode.FOR_CURRENT_CHANNEL,
			name: i18n._(ACTIVE_CHANNEL_ONLY_DESCRIPTOR),
			desc: i18n._(NARRATE_MESSAGES_ONLY_IN_WHICHEVER_CHANNEL_YOU_RE_DESCRIPTOR),
		},
		{
			value: TTSNotificationMode.NEVER,
			name: i18n._(NEVER_AUTOMATICALLY_DESCRIPTOR),
			desc: i18n._(REMAIN_SILENT_UNLESS_SOMEONE_RUNS_TTS_MANUALLY_DESCRIPTOR),
		},
	];
	const handleTtsNotificationChange = useCallback((value: TTSNotificationMode) => {
		Notification.setTTSNotificationMode(value);
	}, []);
	return (
		<div className={styles.container} data-flx="user.notifications-tab.text-to-speech.container">
			<Switch
				label={i18n._(ENABLE_TTS_SPEECH_PLAYBACK_DESCRIPTOR)}
				description={i18n._(LET_TTS_READ_YOUR_MESSAGE_ALOUD_DISABLING_THE_DESCRIPTOR)}
				value={Accessibility.enableTTSCommand}
				onChange={handleToggleTtsCommand}
				data-flx="user.notifications-tab.text-to-speech.switch.toggle-tts-command"
			/>
			<div className={styles.helperCallout} data-flx="user.notifications-tab.text-to-speech.helper-callout">
				<InfoIcon
					size={remFromPx(16)}
					weight="fill"
					className={styles.helperIcon}
					data-flx="user.notifications-tab.text-to-speech.helper-icon"
				/>
				<p className={styles.helperText} data-flx="user.notifications-tab.text-to-speech.helper-text">
					<Trans>
						Adjust playback speed in{' '}
						<button
							type="button"
							className={styles.linkButton}
							onClick={handleAccessibilityLinkClick}
							data-flx="user.notifications-tab.text-to-speech.link-button.accessibility-link-click"
						>
							Accessibility
						</button>
						.
					</Trans>
				</p>
			</div>
			<div className={styles.narrationSection} data-flx="user.notifications-tab.text-to-speech.narration-section">
				<div className={styles.narrationHeader} data-flx="user.notifications-tab.text-to-speech.narration-header">
					<h3 className={styles.narrationTitle} data-flx="user.notifications-tab.text-to-speech.narration-title">
						{i18n._(AUTOMATIC_MESSAGE_NARRATION_DESCRIPTOR)}
					</h3>
					<p
						className={styles.narrationDescription}
						data-flx="user.notifications-tab.text-to-speech.narration-description"
					>
						{i18n._(CONVERTS_INCOMING_CONTENT_TO_SPEECH_REGARDLESS_OF_WHETHER_DESCRIPTOR)}
					</p>
				</div>
				<RadioGroup
					options={ttsNotificationOptions}
					value={ttsNotificationMode}
					onChange={handleTtsNotificationChange}
					aria-label={i18n._(SPEAK_ALL_MESSAGES_OUT_LOUD_DESCRIPTOR)}
					data-flx="user.notifications-tab.text-to-speech.radio-group.tts-notification-change"
				/>
			</div>
		</div>
	);
});
