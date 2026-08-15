// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {LINK_PREVIEW_EXAMPLE_URL} from '@app/features/app/config/I18nDisplayConstants';
import {Message} from '@app/features/channel/components/ChannelMessage';
import {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {SCREEN_READER_DESCRIPTOR, TEXT_TO_SPEECH_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Message as MessageModel} from '@app/features/messaging/models/MessagingMessage';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import styles from '@app/features/user/components/modals/tabs/AccessibilityTab.module.css';
import {AnimationTabContent} from '@app/features/user/components/modals/tabs/accessibility_tab/AnimationTab';
import {KeyboardTabContent} from '@app/features/user/components/modals/tabs/accessibility_tab/KeyboardTab';
import {MotionTabContent} from '@app/features/user/components/modals/tabs/accessibility_tab/MotionTab';
import {VisualTabContent} from '@app/features/user/components/modals/tabs/accessibility_tab/VisualTab';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import Users from '@app/features/user/state/Users';
import TtsUtils from '@app/features/voice/utils/VoiceTtsUtils';
import {MessagePreviewContext, MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PauseIcon, PlayIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const THIS_SHOWS_HOW_LINKS_APPEAR_DESCRIPTOR = msg({
	message: 'This shows how links appear: {linkPreviewExampleUrl}',
	comment: 'Label in the accessibility tab. Preserve {linkPreviewExampleUrl}; it is inserted by code.',
});
const PREVIEW_BUTTON_DESCRIPTOR = msg({
	message: 'Preview button',
	comment: 'Short label in the accessibility tab. Keep it concise.',
});
const ANNOUNCE_NEW_MESSAGES_DESCRIPTOR = msg({
	message: 'Announce new messages',
	comment: 'Short label in the accessibility tab. Keep it concise.',
});
const LET_SCREEN_READERS_ANNOUNCE_NEW_MESSAGES_AS_THEY_DESCRIPTOR = msg({
	message:
		'Let screen readers announce new messages as they arrive in the open channel. Notification sounds are unaffected.',
	comment: 'Label in the accessibility tab.',
});
const DOC_I_M_FROM_THE_FUTURE_I_CAME_DESCRIPTOR = msg({
	message:
		"Doc, I'm from the future. I came here in a time machine that you invented. Now, I need your help to get back to the year 1985.",
	comment: 'Label in the accessibility tab.',
});
const SPEECH_PLAYBACK_SPEED_DESCRIPTOR = msg({
	message: 'Speech playback speed',
	comment: 'Short label in the accessibility tab. Keep it concise.',
});
const TTS_SPEED_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Choose a speed for spoken text.',
	comment: 'Description text in the accessibility tab.',
});
const SILENCE_SAMPLE_DESCRIPTOR = msg({
	message: 'Silence sample',
	comment: 'Short label in the accessibility tab. Keep it concise.',
});
const PLAY_SAMPLE_DESCRIPTOR = msg({
	message: 'Play sample',
	comment: 'Short label in the accessibility tab. Keep it concise.',
});
const SPEECH_SYNTHESIS_IS_UNAVAILABLE_IN_YOUR_BROWSER_DESCRIPTOR = msg({
	message: 'Speech synthesis is unavailable in your browser.',
	comment: 'Description text in the accessibility tab.',
});
const SPEECH_PLAYBACK_FAILED_TRY_AGAIN_OR_CHECK_THAT_DESCRIPTOR = msg({
	message: 'Speech playback failed. Try again, or check that audio output is working.',
	comment: 'Error message in the accessibility tab.',
});
const HEAR_THE_SAMPLE_LINE_SPOKEN_WITH_YOUR_CHOSEN_DESCRIPTOR = msg({
	message: 'Hear the sample line spoken with your chosen speed.',
	comment: 'Description text in the accessibility tab.',
});
const VISUAL_DESCRIPTOR = msg({
	message: 'Visual',
	comment: 'Short label in the accessibility tab. Keep it concise.',
});
const KEYBOARD_DESCRIPTOR = msg({
	message: 'Keyboard',
	comment: 'Short label in the accessibility tab. Keep it concise.',
});
const ANIMATION_DESCRIPTOR = msg({
	message: 'Animation',
	comment: 'Short label in the accessibility tab. Keep it concise.',
});
const MOTION_DESCRIPTOR = msg({
	message: 'Motion',
	comment: 'Short label in the accessibility tab. Keep it concise.',
});
const TTS_RATE_OPTIONS = [0.1, 0.5, 1.0, 1.5, 2.0] as const;

const getNearestTtsRate = (value: number): number => {
	let nearest: number = TTS_RATE_OPTIONS[0];
	let nearestDistance = Number.POSITIVE_INFINITY;
	for (const option of TTS_RATE_OPTIONS) {
		const distance = Math.abs(option - value);
		if (distance < nearestDistance) {
			nearest = option;
			nearestDistance = distance;
		}
	}
	return nearest;
};

const resolveTtsRateInput = (
	inputValue: string,
	options: ReadonlyArray<ComboboxOption<number>>,
): number | undefined => {
	const numericMatch = inputValue.trim().match(/([0-9]+(?:\.[0-9]+)?)/);
	if (!numericMatch) return undefined;
	const parsedValue = Number(numericMatch[1]);
	if (!Number.isFinite(parsedValue)) return undefined;
	return options.reduce((nearest, option) =>
		Math.abs(option.value - parsedValue) < Math.abs(nearest.value - parsedValue) ? option : nearest,
	).value;
};
export const AccessibilityTabPreview = observer(() => {
	const {i18n} = useLingui();
	const alwaysUnderlineLinks = Accessibility.alwaysUnderlineLinks;
	const fakeData = useMemo(() => {
		const tabOpenedAt = new Date();
		const currentUser = Users.getCurrentUser();
		const author = currentUser?.toJSON() || {
			id: '1000000000000000050',
			username: 'PreviewUser',
			discriminator: '0000',
			global_name: 'Preview User',
			avatar: null,
			avatar_color: null,
			bot: false,
			system: false,
			flags: 0,
		};
		const fakeChannel = new Channel({
			id: '1000000000000000051',
			type: 0,
			name: 'accessibility-preview',
			position: 0,
			parent_id: null,
			topic: null,
			url: null,
			nsfw: false,
			last_message_id: null,
			last_pin_timestamp: null,
			bitrate: null,
			user_limit: null,
			permission_overwrites: [],
		});
		const fakeMessage = new MessageModel(
			{
				id: '1000000000000000052',
				channel_id: '1000000000000000051',
				author,
				type: MessageTypes.DEFAULT,
				flags: 0,
				pinned: false,
				mention_everyone: false,
				content: i18n._(THIS_SHOWS_HOW_LINKS_APPEAR_DESCRIPTOR, {linkPreviewExampleUrl: LINK_PREVIEW_EXAMPLE_URL}),
				timestamp: tabOpenedAt.toISOString(),
				state: MessageStates.SENT,
			},
			{skipUserCache: true},
		);
		return {fakeChannel, fakeMessage};
	}, [i18n.locale]);
	useEffect(() => {
		Channels.handleChannelCreate({channel: fakeData.fakeChannel.toJSON()});
		return () => {
			Channels.handleChannelDelete({channel: fakeData.fakeChannel.toJSON()});
		};
	}, [fakeData.fakeChannel]);
	return (
		<div className={styles.previewWrapper} data-flx="user.accessibility-tab.accessibility-tab-preview.preview-wrapper">
			<div
				className={styles.previewContainer}
				data-flx="user.accessibility-tab.accessibility-tab-preview.preview-container"
			>
				<div
					className={styles.previewActionsRow}
					data-flx="user.accessibility-tab.accessibility-tab-preview.preview-actions-row"
				>
					<Button small={true} onClick={() => {}} data-flx="user.accessibility-tab.accessibility-tab-preview.button">
						{i18n._(PREVIEW_BUTTON_DESCRIPTOR)}
					</Button>
					<div
						className={styles.previewAvatarsRow}
						data-flx="user.accessibility-tab.accessibility-tab-preview.preview-avatars-row"
					>
						<MockAvatar
							size={32}
							status={StatusTypes.ONLINE}
							data-flx="user.accessibility-tab.accessibility-tab-preview.mock-avatar"
						/>
						<MockAvatar
							size={32}
							status={StatusTypes.DND}
							data-flx="user.accessibility-tab.accessibility-tab-preview.mock-avatar--2"
						/>
						<MockAvatar
							size={32}
							status={StatusTypes.IDLE}
							data-flx="user.accessibility-tab.accessibility-tab-preview.mock-avatar--3"
						/>
					</div>
				</div>
				<div
					className={styles.previewMessageContainer}
					data-flx="user.accessibility-tab.accessibility-tab-preview.preview-message-container"
				>
					<Message
						channel={fakeData.fakeChannel}
						message={fakeData.fakeMessage}
						previewContext={MessagePreviewContext.SETTINGS}
						previewOverrides={{
							usernameColor: '#e91e63',
							...(alwaysUnderlineLinks
								? {
										linkStyle: 'always-underline',
									}
								: {}),
						}}
						data-flx="user.accessibility-tab.accessibility-tab-preview.message"
					/>
				</div>
			</div>
		</div>
	);
});
export const AccessibilityScreenReaderTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			label={i18n._(ANNOUNCE_NEW_MESSAGES_DESCRIPTOR)}
			description={i18n._(LET_SCREEN_READERS_ANNOUNCE_NEW_MESSAGES_AS_THEY_DESCRIPTOR)}
			value={Accessibility.screenReaderAnnounceNewMessages}
			onChange={(value) => AccessibilityCommands.update({screenReaderAnnounceNewMessages: value})}
			data-flx="user.accessibility-tab.accessibility-screen-reader-tab-content.switch.update"
		/>
	);
});
export const AccessibilityTtsTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const ttsRate = Accessibility.ttsRate;
	const selectedTtsRate = getNearestTtsRate(ttsRate);
	const ttsRateOptions: ReadonlyArray<ComboboxOption<number>> = useMemo(
		() => TTS_RATE_OPTIONS.map((value) => ({value, label: `x${value.toFixed(1)}`})),
		[],
	);
	const [isSpeaking, setIsSpeaking] = useState(false);
	const previewMessage = i18n._(DOC_I_M_FROM_THE_FUTURE_I_CAME_DESCRIPTOR);
	const synthesisSupported = TtsUtils.isSupported();
	const [playbackError, setPlaybackError] = useState(false);
	const previewDisabled = !synthesisSupported || playbackError;
	const handlePreviewToggle = useCallback(() => {
		if (isSpeaking) {
			TtsUtils.stop();
			setIsSpeaking(false);
			return;
		}
		if (previewDisabled) return;
		setPlaybackError(false);
		TtsUtils.speak(previewMessage, {
			rate: ttsRate,
			onEnd: () => setIsSpeaking(false),
			onError: () => {
				setPlaybackError(true);
				setIsSpeaking(false);
			},
		});
		setIsSpeaking(true);
	}, [isSpeaking, previewDisabled, previewMessage, ttsRate]);
	const handleRateChange = useCallback(
		(value: number) => {
			AccessibilityCommands.update({ttsRate: value});
			if (isSpeaking) {
				TtsUtils.stop();
				setIsSpeaking(false);
			}
		},
		[isSpeaking],
	);
	useEffect(() => {
		if (ttsRate !== selectedTtsRate) handleRateChange(selectedTtsRate);
	}, [handleRateChange, selectedTtsRate, ttsRate]);
	useEffect(() => {
		return () => {
			TtsUtils.stop();
		};
	}, []);
	return (
		<div className={styles.ttsSection} data-flx="user.accessibility-tab.accessibility-tts-tab-content.tts-section">
			<CompactComboboxRow<number>
				label={i18n._(SPEECH_PLAYBACK_SPEED_DESCRIPTOR)}
				description={i18n._(TTS_SPEED_DESCRIPTION_DESCRIPTOR)}
				value={selectedTtsRate}
				options={ttsRateOptions}
				onChange={handleRateChange}
				autoSelectValueFromInput={resolveTtsRateInput}
				controlWidth="small"
				dataFlx="user.accessibility-tab.accessibility-tts-tab-content.select.tts-rate"
				data-flx="user.accessibility-tab.accessibility-tts-tab-content.compact-combobox-row.rate-change"
			/>
			<div
				className={styles.ttsPreviewRow}
				data-flx="user.accessibility-tab.accessibility-tts-tab-content.tts-preview-row"
			>
				<Button
					className={styles.ttsPreviewButton}
					leftIcon={
						isSpeaking ? (
							<PauseIcon
								size={remFromPx(16)}
								weight="fill"
								data-flx="user.accessibility-tab.accessibility-tts-tab-content.pause-icon"
							/>
						) : (
							<PlayIcon
								size={remFromPx(16)}
								weight="fill"
								data-flx="user.accessibility-tab.accessibility-tts-tab-content.play-icon"
							/>
						)
					}
					onClick={handlePreviewToggle}
					disabled={!isSpeaking && previewDisabled}
					small={true}
					data-flx="user.accessibility-tab.accessibility-tts-tab-content.tts-preview-button.preview-toggle"
				>
					{isSpeaking ? i18n._(SILENCE_SAMPLE_DESCRIPTOR) : i18n._(PLAY_SAMPLE_DESCRIPTOR)}
				</Button>
				<p
					className={styles.ttsPreviewDescription}
					data-flx="user.accessibility-tab.accessibility-tts-tab-content.tts-preview-description"
				>
					{!synthesisSupported
						? i18n._(SPEECH_SYNTHESIS_IS_UNAVAILABLE_IN_YOUR_BROWSER_DESCRIPTOR)
						: playbackError
							? i18n._(SPEECH_PLAYBACK_FAILED_TRY_AGAIN_OR_CHECK_THAT_DESCRIPTOR)
							: i18n._(HEAR_THE_SAMPLE_LINE_SPOKEN_WITH_YOUR_CHOSEN_DESCRIPTOR)}
				</p>
			</div>
		</div>
	);
});
export const AccessibilityTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<SettingsTabContainer data-flx="user.accessibility-tab.settings-tab-container">
			{!MobileLayout.enabled && <AccessibilityTabPreview data-flx="user.accessibility-tab.accessibility-tab-preview" />}
			<SettingsTabContent data-flx="user.accessibility-tab.settings-tab-content">
				<SettingsSection id="visual" title={i18n._(VISUAL_DESCRIPTOR)} data-flx="user.accessibility-tab.visual">
					<VisualTabContent data-flx="user.accessibility-tab.visual-tab-content" />
				</SettingsSection>
				<SettingsSection
					id="screen-reader"
					title={i18n._(SCREEN_READER_DESCRIPTOR)}
					data-flx="user.accessibility-tab.screen-reader"
				>
					<AccessibilityScreenReaderTabContent data-flx="user.accessibility-tab.accessibility-screen-reader-tab-content" />
				</SettingsSection>
				<SettingsSection id="tts" title={i18n._(TEXT_TO_SPEECH_DESCRIPTOR)} data-flx="user.accessibility-tab.tts">
					<AccessibilityTtsTabContent data-flx="user.accessibility-tab.accessibility-tts-tab-content" />
				</SettingsSection>
				<SettingsSection id="keyboard" title={i18n._(KEYBOARD_DESCRIPTOR)} data-flx="user.accessibility-tab.keyboard">
					<KeyboardTabContent data-flx="user.accessibility-tab.keyboard-tab-content" />
				</SettingsSection>
				<SettingsSection
					id="animation"
					title={i18n._(ANIMATION_DESCRIPTOR)}
					data-flx="user.accessibility-tab.animation"
				>
					<AnimationTabContent data-flx="user.accessibility-tab.animation-tab-content" />
				</SettingsSection>
				<SettingsSection id="motion" title={i18n._(MOTION_DESCRIPTOR)} data-flx="user.accessibility-tab.motion">
					<MotionTabContent data-flx="user.accessibility-tab.motion-tab-content" />
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});
