// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility, {ChannelTypingIndicatorMode} from '@app/features/accessibility/state/Accessibility';
import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {ChannelItemCore} from '@app/features/app/components/layout/ChannelItem';
import channelItemSurfaceStyles from '@app/features/app/components/layout/ChannelItemSurface.module.css';
import {Typing} from '@app/features/channel/components/ChannelTyping';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/user/components/modals/tabs/appearance_tab/InterfaceTab.module.css';
import {getDefaultAvatarURLForIndex} from '@app/features/user/utils/AvatarUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const TYPING_INDICATOR_AVATARS_DESCRIPTOR = msg({
	message: 'Typing indicator + avatars',
	comment: 'Label in the interface tab.',
});
const SHOW_TYPING_INDICATOR_WITH_USER_AVATARS_IN_THE_DESCRIPTOR = msg({
	message: 'Show typing indicator with user avatars in the channel list',
	comment: 'Label in the interface tab.',
});
const TYPING_INDICATOR_ONLY_DESCRIPTOR = msg({
	message: 'Typing indicator only',
	comment: 'Short label in the interface tab. Keep it concise.',
});
const SHOW_JUST_THE_TYPING_INDICATOR_WITHOUT_AVATARS_DESCRIPTOR = msg({
	message: 'Show just the typing indicator without avatars',
	comment: 'Label in the interface tab.',
});
const HIDDEN_DESCRIPTOR = msg({
	message: 'Hidden',
	comment: 'Short label in the interface tab. Keep it concise.',
});
const DON_T_SHOW_TYPING_INDICATORS_IN_THE_CHANNEL_DESCRIPTOR = msg({
	message: "Don't show typing indicators in the channel list",
	comment: 'Label in the interface tab.',
});
const CHANNEL_LIST_TYPING_INDICATORS_DESCRIPTOR = msg({
	message: 'Channel list typing indicators',
	comment: 'Label in the interface tab.',
});
const CHANNEL_LIST_TYPING_INDICATOR_MODE_DESCRIPTOR = msg({
	message: 'Channel list typing indicator mode',
	comment: 'Label in the interface tab.',
});
const ChannelListPreview = observer(({mode}: {mode: ChannelTypingIndicatorMode}) => {
	const typingIndicator =
		mode !== ChannelTypingIndicatorMode.HIDDEN ? (
			<Tooltip
				text={() => (
					<span
						className={styles.tooltipContent}
						data-flx="user.appearance-tab.interface-tab.channel-list-preview.tooltip-content"
					>
						<Trans>
							<strong data-flx="user.appearance-tab.interface-tab.channel-list-preview.strong">Kenji</strong>,{' '}
							<strong data-flx="user.appearance-tab.interface-tab.channel-list-preview.strong--2">Amara</strong> and{' '}
							<strong data-flx="user.appearance-tab.interface-tab.channel-list-preview.strong--3">Mateo</strong> are
							typing...
						</Trans>
					</span>
				)}
				data-flx="user.appearance-tab.interface-tab.channel-list-preview.tooltip"
			>
				<div
					className={styles.typingContainer}
					data-flx="user.appearance-tab.interface-tab.channel-list-preview.typing-container"
				>
					<Typing
						className={styles.typingAnimationWrapper}
						size={20}
						color="var(--surface-interactive-selected-color)"
						data-flx="user.appearance-tab.interface-tab.channel-list-preview.typing-animation-wrapper"
					/>
					{mode === ChannelTypingIndicatorMode.AVATARS && (
						<AvatarStack
							size={12}
							maxVisible={5}
							className={styles.typingAvatars}
							data-flx="user.appearance-tab.interface-tab.channel-list-preview.typing-avatars"
						>
							{[1, 2, 3].map((index) => (
								<MockAvatar
									key={index}
									size={12}
									userTag={`User ${index}`}
									avatarUrl={getDefaultAvatarURLForIndex(index)}
									data-flx="user.appearance-tab.interface-tab.channel-list-preview.mock-avatar"
								/>
							))}
						</AvatarStack>
					)}
				</div>
			</Tooltip>
		) : undefined;
	return (
		<div
			className={styles.previewContainer}
			data-flx="user.appearance-tab.interface-tab.channel-list-preview.preview-container"
		>
			<div
				className={styles.previewContent}
				data-flx="user.appearance-tab.interface-tab.channel-list-preview.preview-content"
			>
				<ChannelItemCore
					channel={{name: 'general', type: 0}}
					forceHover
					typingIndicator={typingIndicator}
					className={clsx('cursor-default', channelItemSurfaceStyles.channelItemSurface)}
					data-flx="user.appearance-tab.interface-tab.channel-list-preview.cursor-default"
				/>
			</div>
		</div>
	);
});
export const InterfaceTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const channelTypingIndicatorOptions: ReadonlyArray<RadioOption<ChannelTypingIndicatorMode>> = [
		{
			value: ChannelTypingIndicatorMode.AVATARS,
			name: i18n._(TYPING_INDICATOR_AVATARS_DESCRIPTOR),
			desc: i18n._(SHOW_TYPING_INDICATOR_WITH_USER_AVATARS_IN_THE_DESCRIPTOR),
		},
		{
			value: ChannelTypingIndicatorMode.INDICATOR_ONLY,
			name: i18n._(TYPING_INDICATOR_ONLY_DESCRIPTOR),
			desc: i18n._(SHOW_JUST_THE_TYPING_INDICATOR_WITHOUT_AVATARS_DESCRIPTOR),
		},
		{
			value: ChannelTypingIndicatorMode.HIDDEN,
			name: i18n._(HIDDEN_DESCRIPTOR),
			desc: i18n._(DON_T_SHOW_TYPING_INDICATORS_IN_THE_CHANNEL_DESCRIPTOR),
		},
	];
	return (
		<SettingsTabSection
			title={i18n._(CHANNEL_LIST_TYPING_INDICATORS_DESCRIPTOR)}
			data-flx="user.appearance-tab.interface-tab.interface-tab-content.settings-tab-section"
		>
			<ChannelListPreview
				mode={Accessibility.channelTypingIndicatorMode}
				data-flx="user.appearance-tab.interface-tab.interface-tab-content.channel-list-preview"
			/>
			<RadioGroup
				options={channelTypingIndicatorOptions}
				value={Accessibility.channelTypingIndicatorMode}
				onChange={(value) => AccessibilityCommands.update({channelTypingIndicatorMode: value})}
				aria-label={i18n._(CHANNEL_LIST_TYPING_INDICATOR_MODE_DESCRIPTOR)}
				data-flx="user.appearance-tab.interface-tab.interface-tab-content.radio-group.update"
			/>
		</SettingsTabSection>
	);
});
