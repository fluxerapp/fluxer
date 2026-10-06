// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {
	getDefaultMessageGroupSpacing,
	getMessageGroupSpacingPatch,
} from '@app/features/accessibility/state/MessageGroupSpacing';
import {Message} from '@app/features/channel/components/ChannelMessage';
import {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import {Message as MessageModel} from '@app/features/messaging/models/MessagingMessage';
import {isNewMessageGroup} from '@app/features/messaging/utils/MessageGroupingUtils';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {canResetSliderValue, SliderResetIconButton} from '@app/features/ui/components/slider/SliderResetIconButton';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import appearanceTabStyles from '@app/features/user/components/modals/tabs/AppearanceTab.module.css';
import styles from '@app/features/user/components/modals/tabs/appearance_tab/MessagesTab.module.css';
import {FontSizeTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/ScalingTab';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {MessagePreviewContext, MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo} from 'react';

const THIS_IS_HOW_MESSAGES_APPEAR_DESCRIPTOR = msg({
	message: 'This is how messages appear',
	comment: 'Label in the messages tab.',
});
const WITH_DIFFERENT_DISPLAY_MODES_AVAILABLE_DESCRIPTOR = msg({
	message: 'With different display modes available',
	comment: 'Label in the messages tab.',
});
const CUSTOMIZE_THE_SPACING_AND_SIZE_DESCRIPTOR = msg({
	message: 'Customize the spacing and size',
	comment: 'Label in the messages tab.',
});
const WAITING_FOR_YOU_TO_DESCRIPTOR = msg({
	message: 'Waiting for you to...',
	comment: 'Label in the messages tab.',
});
const TURN_DENSE_MODE_ON_NICE_DESCRIPTOR = msg({
	message: '... turn dense mode on. Nice!',
	comment: 'Description text in the messages tab.',
});
const COMFY_DESCRIPTOR = msg({
	message: 'Comfy',
	comment: 'Short label in the messages tab. Keep it concise.',
});
const SPACIOUS_LAYOUT_WITH_CLEAR_VISUAL_SEPARATION_BETWEEN_MESSAGES_DESCRIPTOR = msg({
	message: 'Spacious layout with clear visual separation between messages.',
	comment: 'Description text in the messages tab.',
});
const DENSE_DESCRIPTOR = msg({
	message: 'Dense',
	comment: 'Short label in the messages tab. Keep it concise.',
});
const MAXIMIZES_VISIBLE_MESSAGES_WITH_MINIMAL_SPACING_DESCRIPTOR = msg({
	message: 'Maximizes visible messages with minimal spacing.',
	comment: 'Description text in the messages tab.',
});
const MESSAGE_DISPLAY_MODE_DESCRIPTOR = msg({
	message: 'Message display mode',
	comment: 'Short label in the messages tab. Keep it concise.',
});
const HIDE_USER_AVATARS_DESCRIPTOR = msg({
	message: 'Hide user avatars',
	comment: 'Short label in the messages tab. Keep it concise.',
});
const SPACE_BETWEEN_MESSAGE_GROUPS_DESCRIPTOR = msg({
	message: 'Space between message groups',
	comment: 'Label in the messages tab.',
});
const PIXELS_DESCRIPTOR = msg({
	message: '{messageGroupSpacing}px',
	comment: 'Message group spacing option label.',
});
const RESET_SPACING_DESCRIPTOR = msg({
	message: 'Reset spacing',
	comment: 'Button below the space between message groups setting. Restores message group spacing to the default.',
});
const MESSAGE_GROUP_SPACING_OPTIONS = [0, 4, 8, 16, 24] as const;

const getNearestMessageGroupSpacing = (value: number): number => {
	let nearest: number = MESSAGE_GROUP_SPACING_OPTIONS[0];
	let nearestDistance = Number.POSITIVE_INFINITY;
	for (const option of MESSAGE_GROUP_SPACING_OPTIONS) {
		const distance = Math.abs(option - value);
		if (distance < nearestDistance) {
			nearest = option;
			nearestDistance = distance;
		}
	}
	return nearest;
};

const resolveMessageGroupSpacingInput = (
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
const MessagesPreview: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {messageDisplayCompact} = UserSettings;
	const currentUser = Users.getCurrentUser();
	const author = currentUser?.toJSON() || {
		id: '1000000000000000030',
		username: 'PreviewUser',
		discriminator: '0000',
		global_name: 'Preview User',
		avatar: null,
		avatar_color: null,
		bot: false,
		system: false,
		flags: 0,
	};
	const fakeChannel = useMemo(
		() =>
			new Channel({
				id: '1000000000000000031',
				type: 0,
				name: 'fake-channel',
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
			}),
		[],
	);
	useEffect(() => {
		Channels.handleChannelCreate({channel: fakeChannel.toJSON()});
		return () => {
			Channels.handleChannelDelete({channel: fakeChannel.toJSON()});
		};
	}, [fakeChannel]);
	const baseTime = new Date();
	const messageContents = [
		{content: i18n._(THIS_IS_HOW_MESSAGES_APPEAR_DESCRIPTOR), offsetMinutes: 0},
		{content: i18n._(WITH_DIFFERENT_DISPLAY_MODES_AVAILABLE_DESCRIPTOR), offsetMinutes: 1},
		{content: i18n._(CUSTOMIZE_THE_SPACING_AND_SIZE_DESCRIPTOR), offsetMinutes: 2},
		{content: i18n._(WAITING_FOR_YOU_TO_DESCRIPTOR), offsetMinutes: 10},
		{content: i18n._(TURN_DENSE_MODE_ON_NICE_DESCRIPTOR), offsetMinutes: 11},
	];
	const fakeMessages = messageContents.map(({content, offsetMinutes}, index) => {
		const timestamp = new Date(baseTime.getTime() + offsetMinutes * 60 * 1000);
		return new MessageModel(
			{
				id: `100000000000000004${index}`,
				channel_id: '1000000000000000031',
				author,
				type: MessageTypes.DEFAULT,
				flags: 0,
				pinned: false,
				mention_everyone: false,
				content,
				timestamp: timestamp.toISOString(),
				state: MessageStates.SENT,
			},
			{skipUserCache: true},
		);
	});
	return (
		<div
			className={appearanceTabStyles.previewWrapper}
			data-flx="user.appearance-tab.messages-tab.messages-preview.div"
		>
			<div
				className={clsx(
					appearanceTabStyles.previewContainer,
					messageDisplayCompact
						? appearanceTabStyles.previewContainerCompact
						: appearanceTabStyles.previewContainerCozy,
				)}
				data-flx="user.appearance-tab.messages-tab.messages-preview.div--2"
			>
				<div
					className={appearanceTabStyles.previewMessagesContainer}
					key="appearance-messages-preview-scroller"
					data-flx="user.appearance-tab.messages-tab.messages-preview.div--3"
				>
					{fakeMessages.map((message, index) => {
						const prevMessage = index > 0 ? fakeMessages[index - 1] : undefined;
						const isNewGroup = isNewMessageGroup(fakeChannel, prevMessage, message);
						const shouldGroup = !messageDisplayCompact && !isNewGroup;
						return (
							<Message
								key={message.id}
								channel={fakeChannel}
								message={message}
								prevMessage={prevMessage}
								previewContext={MessagePreviewContext.SETTINGS}
								shouldGroup={shouldGroup}
								data-flx="user.appearance-tab.messages-tab.messages-preview.message"
							/>
						);
					})}
				</div>
				<div
					className={appearanceTabStyles.previewOverlay}
					data-flx="user.appearance-tab.messages-tab.messages-preview.div--4"
				/>
			</div>
		</div>
	);
});
export const AppearanceTabPreview = MessagesPreview;
export const MessagesTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {messageDisplayCompact} = UserSettings;
	const messageGroupSpacing = Accessibility.getMessageGroupSpacingValue(messageDisplayCompact);
	const selectedMessageGroupSpacing = getNearestMessageGroupSpacing(messageGroupSpacing);
	const defaultMessageGroupSpacing = getDefaultMessageGroupSpacing(messageDisplayCompact);
	const canResetMessageGroupSpacing = canResetSliderValue(messageGroupSpacing, defaultMessageGroupSpacing);
	const showUserAvatarsInCompactMode = Accessibility.showUserAvatarsInCompactMode;
	const mobileLayout = MobileLayout;
	const messageGroupSpacingOptions: ReadonlyArray<ComboboxOption<number>> = useMemo(
		() =>
			MESSAGE_GROUP_SPACING_OPTIONS.map((value) => ({
				value,
				label: i18n._(PIXELS_DESCRIPTOR, {messageGroupSpacing: getCachedNumberFormat(i18n.locale).format(value)}),
			})),
		[i18n.locale],
	);
	useEffect(() => {
		if (messageGroupSpacing === selectedMessageGroupSpacing) return;
		AccessibilityCommands.update(getMessageGroupSpacingPatch(messageDisplayCompact, selectedMessageGroupSpacing));
	}, [messageDisplayCompact, messageGroupSpacing, selectedMessageGroupSpacing]);
	const messageDisplayOptions: ReadonlyArray<RadioOption<boolean>> = [
		{
			value: false,
			name: i18n._(COMFY_DESCRIPTOR),
			desc: i18n._(SPACIOUS_LAYOUT_WITH_CLEAR_VISUAL_SEPARATION_BETWEEN_MESSAGES_DESCRIPTOR),
		},
		{
			value: true,
			name: i18n._(DENSE_DESCRIPTOR),
			desc: i18n._(MAXIMIZES_VISIBLE_MESSAGES_WITH_MINIMAL_SPACING_DESCRIPTOR),
		},
	];
	if (mobileLayout.enabled) {
		return <Trans>Message display settings are only available on desktop.</Trans>;
	}
	return (
		<>
			<div id="chat-font-scaling" data-flx="user.appearance-tab.messages-tab.messages-tab-content.font-size-anchor">
				<FontSizeTabContent data-flx="user.appearance-tab.messages-tab.messages-tab-content.font-size-tab-content" />
			</div>
			<CompactComboboxRow<number>
				label={i18n._(SPACE_BETWEEN_MESSAGE_GROUPS_DESCRIPTOR)}
				action={
					canResetMessageGroupSpacing ? (
						<SliderResetIconButton
							canReset={true}
							onReset={() =>
								AccessibilityCommands.update(
									getMessageGroupSpacingPatch(messageDisplayCompact, defaultMessageGroupSpacing),
								)
							}
							ariaLabel={i18n._(RESET_SPACING_DESCRIPTOR)}
							dataFlx="user.appearance-tab.messages-tab.messages-tab-content.reset-button.spacing"
							data-flx="user.appearance-tab.messages-tab.messages-tab-content.slider-reset-icon-button"
						/>
					) : null
				}
				value={selectedMessageGroupSpacing}
				options={messageGroupSpacingOptions}
				onChange={(value) => AccessibilityCommands.update(getMessageGroupSpacingPatch(messageDisplayCompact, value))}
				autoSelectValueFromInput={resolveMessageGroupSpacingInput}
				controlWidth="small"
				menuMinWidth={128}
				aria-label={i18n._(SPACE_BETWEEN_MESSAGE_GROUPS_DESCRIPTOR)}
				dataFlx="user.appearance-tab.messages-tab.messages-tab-content.select.spacing"
				data-flx="user.appearance-tab.messages-tab.messages-tab-content.compact-select-row.update"
			/>
			<RadioGroup
				options={messageDisplayOptions}
				value={messageDisplayCompact}
				onChange={(value) => {
					UserSettingsCommands.update({messageDisplayCompact: value});
				}}
				aria-label={i18n._(MESSAGE_DISPLAY_MODE_DESCRIPTOR)}
				data-flx="user.appearance-tab.messages-tab.messages-tab-content.radio-group.update"
			/>
			{messageDisplayCompact ? (
				<div
					className={styles.switchWrapper}
					data-flx="user.appearance-tab.messages-tab.messages-tab-content.switch-wrapper"
				>
					<Switch
						label={i18n._(HIDE_USER_AVATARS_DESCRIPTOR)}
						value={!showUserAvatarsInCompactMode}
						onChange={(value) => AccessibilityCommands.update({showUserAvatarsInCompactMode: !value})}
						data-flx="user.appearance-tab.messages-tab.messages-tab-content.switch.update"
					/>
				</div>
			) : null}
		</>
	);
});
