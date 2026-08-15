// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {EVERYONE_MENTION, HERE_MENTION} from '@app/features/app/config/I18nDisplayConstants';
import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';
import {MUTE_CHANNEL_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import styles from '@app/features/guild/components/modals/GuildNotificationSettingsModal.module.css';
import Guilds from '@app/features/guild/state/Guilds';
import {
	COMMUNITY_NOTIFICATION_SETTINGS_DESCRIPTOR,
	NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR,
	NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR,
	NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR,
	NOTIFICATION_SETTINGS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import AdvancedSettings from '@app/features/user/state/AdvancedSettings';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {MentionReplyPreferences} from '@fluxer/constants/src/UserConstants';
import type {ChannelId} from '@fluxer/schema/src/branded/WireIds';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {FolderIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DEFAULT_UNREAD_BADGES_OPTION_DESCRIPTOR = msg({
	message: 'Default',
	comment:
		'Unread badges option in the community notification settings modal. Means inherit from the community notification level. Short standalone label.',
});
const INHERIT_FROM_ACCOUNT_OPTION_DESCRIPTOR = msg({
	message: 'Inherit from account',
	comment:
		'Reply mention preference option in the community notification settings modal. Means use the account-wide reply mention preference instead of overriding it for this community.',
});
const PREFER_MENTION_OPTION_DESCRIPTOR = msg({
	message: 'Prefer @mention',
	comment:
		'Reply mention preference option in the community notification settings modal. When the user is replied to, they want the @ mention enabled. The @ is literal.',
});
const PREFER_NO_MENTION_OPTION_DESCRIPTOR = msg({
	message: 'Prefer no @mention',
	comment:
		'Reply mention preference option in the community notification settings modal. When the user is replied to, they prefer no @ mention. The @ is literal.',
});
const MUTE_GUILD_SWITCH_LABEL_DESCRIPTOR = msg({
	message: 'Mute {guildName}',
	comment:
		'Switch label in the community notification settings modal that mutes the entire community. {guildName} is the community name.',
});
const MUTE_GUILD_SWITCH_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Muting a community hides its unread badge and prevents notifications from appearing',
	comment:
		'Helper text under the Mute community switch in the notification settings modal. Explains the effect on the community sidebar badge and notifications.',
});
const COMMUNITY_NOTIFICATION_LEVEL_ARIA_DESCRIPTOR = msg({
	message: 'Community notification level',
	comment: 'Accessible label for the community notification level radio group in the notification settings modal.',
});
const UNREAD_BADGES_SECTION_DESCRIPTOR = msg({
	message: 'Unread badges',
	comment: 'Section heading in the community notification settings modal for the unread badges options.',
});
const UNREAD_BADGES_SECTION_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Choose when this community shows an unread badge. "Default" preserves the normal unread indicator behavior and respects mute.',
	comment:
		'Helper text under the unread badges section in the community notification settings modal. The quoted "Default" should match the matching option label.',
});
const COMMUNITY_UNREAD_BADGES_LEVEL_ARIA_DESCRIPTOR = msg({
	message: 'Community unread badges level',
	comment: 'Accessible label for the unread badges level radio group in the community notification settings modal.',
});
const SUPPRESS_EVERYONE_AND_HERE_SWITCH_DESCRIPTOR = msg({
	message: 'Suppress {everyoneMention} and {hereMention}',
	comment:
		'Switch label in the community notification settings modal. Suppresses @everyone and @here notifications in this community. The two placeholders render the literal @everyone and @here tokens and must not be translated.',
});
const SUPPRESS_ALL_ROLE_MENTIONS_SWITCH_DESCRIPTOR = msg({
	message: 'Suppress all role mentions',
	comment:
		'Switch label in the community notification settings modal. Suppresses notifications for any role mention in this community.',
});
const REPLY_MENTION_PREFERENCE_SECTION_DESCRIPTOR = msg({
	message: 'Reply mention preference',
	comment:
		'Section heading in the community notification settings modal for the per-community reply mention preference.',
});
const REPLY_MENTION_PREFERENCE_SECTION_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Override how replies to your messages handle the @ mention in this community. "Inherit from account" uses your account-wide setting.',
	comment:
		'Helper text under the reply mention preference section in the community notification settings modal. The quoted "Inherit from account" should match the matching option label.',
});
const REPLY_MENTION_PREFERENCE_ARIA_DESCRIPTOR = msg({
	message: 'Reply mention preference for this community',
	comment:
		'Accessible label for the reply mention preference radio group in the community notification settings modal.',
});
const MOBILE_PUSH_NOTIFICATIONS_SWITCH_DESCRIPTOR = msg({
	message: 'Mobile push notifications',
	comment:
		'Switch label in the community notification settings modal. Controls whether mobile push notifications are delivered for this community.',
});
const NOTIFICATION_OVERRIDES_SECTION_DESCRIPTOR = msg({
	message: 'Notification overrides',
	comment:
		'Section heading in the community notification settings modal for per-channel and per-category notification overrides.',
});
const SELECT_CHANNEL_OR_CATEGORY_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Select a channel or category',
	comment:
		'Placeholder in the notification overrides picker. The user picks a channel or category to add a per-target notification override.',
});
const CHANNEL_OR_CATEGORY_COLUMN_HEADER_DESCRIPTOR = msg({
	message: 'Channel or category',
	comment: 'Column header in the notification overrides table identifying the target channel or category.',
});
const ALL_COLUMN_HEADER_DESCRIPTOR = msg({
	message: 'All',
	comment:
		'Column header in the notification overrides table. Short label for the All messages option. Standalone word; refers to "all messages".',
});
const MENTIONS_COLUMN_HEADER_DESCRIPTOR = msg({
	message: 'Mentions',
	comment:
		'Column header in the notification overrides table. Short label for the Only mentions option. Standalone word.',
});
const MUTE_COLUMN_HEADER_DESCRIPTOR = msg({
	message: 'Mute',
	comment:
		'Column header in the notification overrides table. Short label for the per-channel mute toggle. Standalone verb used as a column label.',
});
const NO_CATEGORY_FALLBACK_DESCRIPTOR = msg({
	message: 'No category',
	comment:
		'Subtext under a channel name in the notification overrides list when the channel has no parent category. Short standalone label.',
});
const REMOVE_OVERRIDE_BUTTON_ARIA_DESCRIPTOR = msg({
	message: 'Remove override',
	comment:
		'Accessible label for the X button on a notification override row. Removes that channel or category override.',
});

interface ChannelOption {
	value: string;
	label: string;
	icon: React.ReactNode;
	categoryName?: string;
	isCategory: boolean;
}

export const GuildNotificationSettingsModal = observer(({guildId}: {guildId: string}) => {
	const {i18n} = useLingui();
	const muteChannelLabel = i18n._(MUTE_CHANNEL_DESCRIPTOR);
	const guild = Guilds.getGuild(guildId);
	const settings = UserGuildSettings.getSettings(guildId);
	if (!guild || !settings) return null;
	const channels = Channels.getGuildChannels(guildId);
	const categories = channels.filter((c) => c.type === ChannelTypes.GUILD_CATEGORY);
	const channelOptions: Array<ChannelOption> = [
		...categories.map((cat) => ({
			value: cat.id,
			label: cat.name || '',
			icon: (
				<FolderIcon
					size={16}
					className={styles.iconTertiary}
					data-flx="guild.guild-notification-settings-modal.icon-tertiary"
				/>
			),
			isCategory: true,
		})),
		...channels
			.filter((c) => c.type !== ChannelTypes.GUILD_CATEGORY)
			.map((ch) => {
				const category = ch.parentId ? categories.find((c) => c.id === ch.parentId) : null;
				return {
					value: ch.id,
					label: ch.name || '',
					icon: ChannelUtils.getIcon(ch, {size: 16, className: styles.iconTertiary}),
					categoryName: category?.name ?? undefined,
					isCategory: false,
				};
			}),
	];
	const selectOptions = channelOptions.map((option) => ({
		value: option.value,
		label: option.label,
		isDisabled: false,
	}));
	const notificationOptions: Array<RadioOption<number>> = [
		{
			value: MessageNotifications.ALL_MESSAGES,
			name: i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR),
		},
		{
			value: MessageNotifications.ONLY_MENTIONS,
			name: i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR),
		},
		{
			value: MessageNotifications.NO_MESSAGES,
			name: i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR),
		},
	];
	const unreadBadgesOptions: Array<RadioOption<number>> = [
		{
			value: MessageNotifications.INHERIT,
			name: i18n._(DEFAULT_UNREAD_BADGES_OPTION_DESCRIPTOR),
		},
		{
			value: MessageNotifications.ALL_MESSAGES,
			name: i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR),
		},
		{
			value: MessageNotifications.ONLY_MENTIONS,
			name: i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR),
		},
		{
			value: MessageNotifications.NO_MESSAGES,
			name: i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR),
		},
	];
	const guildNotificationLevel = UserGuildSettings.getGuildMessageNotifications(guildId);
	const guildUnreadBadgesValue = settings.unread_badges == null ? MessageNotifications.INHERIT : settings.unread_badges;
	const showUnreadBadgeCustomization = AdvancedSettings.unreadBadgeCustomizationEnabled;
	const currentMember = GuildMembers.getMember(guildId, Authentication.currentUserId);
	const memberMentionFlags = currentMember?.mentionFlags ?? MentionReplyPreferences.NO_PREFERENCE;
	const mentionPreferenceOptions: Array<RadioOption<number>> = [
		{
			value: MentionReplyPreferences.NO_PREFERENCE,
			name: i18n._(INHERIT_FROM_ACCOUNT_OPTION_DESCRIPTOR),
		},
		{
			value: MentionReplyPreferences.PREFER_MENTION,
			name: i18n._(PREFER_MENTION_OPTION_DESCRIPTOR),
		},
		{
			value: MentionReplyPreferences.PREFER_NO_MENTION,
			name: i18n._(PREFER_NO_MENTION_OPTION_DESCRIPTOR),
		},
	];
	const handleMentionPreferenceChange = (value: number) => {
		void GuildMemberCommands.updateProfile(guildId, {mention_flags: value});
	};
	const handleAddOverride = (value: string | null) => {
		if (!value) return;
		const existingOverride = settings.channel_overrides?.[value as ChannelId];
		if (existingOverride) {
			return;
		}
		UserGuildSettingsCommands.updateChannelOverride(guildId, value, {
			message_notifications: MessageNotifications.INHERIT,
			muted: false,
		});
	};
	const handleRemoveOverride = (channelId: string) => {
		UserGuildSettingsCommands.updateChannelOverride(guildId, channelId, null);
	};
	const handleOverrideNotificationChange = (channelId: string, level: number) => {
		UserGuildSettingsCommands.updateChannelOverride(guildId, channelId, {
			message_notifications: level,
		});
	};
	const handleOverrideMuteChange = (channelId: string, muted: boolean) => {
		UserGuildSettingsCommands.updateChannelOverride(guildId, channelId, {
			muted,
		});
	};
	const overrideChannels = settings.channel_overrides
		? Object.entries(settings.channel_overrides)
				.map(([channelId, override]) => {
					const channel = Channels.getChannel(channelId);
					const category = channel?.parentId ? Channels.getChannel(channel.parentId) : null;
					const isCategory = channel?.type === ChannelTypes.GUILD_CATEGORY;
					return {
						channelId,
						override,
						channel,
						category,
						isCategory,
					};
				})
				.sort((a, b) => {
					if (!a.channel && !b.channel) return 0;
					if (!a.channel) return 1;
					if (!b.channel) return -1;
					const posA = a.channel.position ?? 0;
					const posB = b.channel.position ?? 0;
					if (posA !== posB) {
						return posA - posB;
					}
					return a.channelId.localeCompare(b.channelId);
				})
		: [];
	return (
		<Modal.Root size="medium" data-flx="guild.guild-notification-settings-modal.modal-root">
			<Modal.Header
				title={i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR)}
				data-flx="guild.guild-notification-settings-modal.modal-header"
			/>
			<Modal.Content data-flx="guild.guild-notification-settings-modal.modal-content">
				<div className={styles.container} data-flx="guild.guild-notification-settings-modal.container">
					<div className={styles.section} data-flx="guild.guild-notification-settings-modal.section">
						<Switch
							label={i18n._(MUTE_GUILD_SWITCH_LABEL_DESCRIPTOR, {guildName: guild.name})}
							description={i18n._(MUTE_GUILD_SWITCH_DESCRIPTION_DESCRIPTOR)}
							value={settings.muted}
							onChange={(value) => UserGuildSettingsCommands.updateGuildSettings(guildId, {muted: value})}
							data-flx="guild.guild-notification-settings-modal.switch.update-guild-settings"
						/>
					</div>
					<div
						className={styles.notificationSection}
						data-flx="guild.guild-notification-settings-modal.notification-section"
					>
						<h3 className={styles.sectionTitle} data-flx="guild.guild-notification-settings-modal.section-title">
							{i18n._(COMMUNITY_NOTIFICATION_SETTINGS_DESCRIPTOR)}
						</h3>
						<RadioGroup
							options={notificationOptions}
							value={guildNotificationLevel}
							onChange={(value) =>
								UserGuildSettingsCommands.updateGuildSettings(guildId, {message_notifications: value})
							}
							aria-label={i18n._(COMMUNITY_NOTIFICATION_LEVEL_ARIA_DESCRIPTOR)}
							data-flx="guild.guild-notification-settings-modal.radio-group.update-guild-settings"
						/>
					</div>
					{showUnreadBadgeCustomization && (
						<div
							className={styles.notificationSection}
							data-flx="guild.guild-notification-settings-modal.notification-section--2"
						>
							<h3 className={styles.sectionTitle} data-flx="guild.guild-notification-settings-modal.section-title--2">
								{i18n._(UNREAD_BADGES_SECTION_DESCRIPTOR)}
							</h3>
							<p
								className={styles.sectionDescription}
								data-flx="guild.guild-notification-settings-modal.section-description"
							>
								{i18n._(UNREAD_BADGES_SECTION_DESCRIPTION_DESCRIPTOR)}
							</p>
							<RadioGroup
								options={unreadBadgesOptions}
								value={guildUnreadBadgesValue}
								onChange={(value) =>
									UserGuildSettingsCommands.updateUnreadBadgesLevel(
										guildId,
										value === MessageNotifications.INHERIT ? null : value,
									)
								}
								aria-label={i18n._(COMMUNITY_UNREAD_BADGES_LEVEL_ARIA_DESCRIPTOR)}
								data-flx="guild.guild-notification-settings-modal.radio-group.update-unread-badges-level"
							/>
						</div>
					)}
					<div className={styles.suppressSection} data-flx="guild.guild-notification-settings-modal.suppress-section">
						<Switch
							label={i18n._(SUPPRESS_EVERYONE_AND_HERE_SWITCH_DESCRIPTOR, {
								everyoneMention: EVERYONE_MENTION,
								hereMention: HERE_MENTION,
							})}
							value={settings.suppress_everyone}
							onChange={(value) => UserGuildSettingsCommands.updateGuildSettings(guildId, {suppress_everyone: value})}
							data-flx="guild.guild-notification-settings-modal.switch.update-guild-settings--2"
						/>
						<Switch
							label={i18n._(SUPPRESS_ALL_ROLE_MENTIONS_SWITCH_DESCRIPTOR)}
							value={settings.suppress_roles}
							onChange={(value) => UserGuildSettingsCommands.updateGuildSettings(guildId, {suppress_roles: value})}
							data-flx="guild.guild-notification-settings-modal.switch.update-guild-settings--3"
						/>
					</div>
					<div
						className={styles.notificationSection}
						data-flx="guild.guild-notification-settings-modal.notification-section--3"
					>
						<h3 className={styles.sectionTitle} data-flx="guild.guild-notification-settings-modal.section-title--3">
							{i18n._(REPLY_MENTION_PREFERENCE_SECTION_DESCRIPTOR)}
						</h3>
						<p
							className={styles.sectionDescription}
							data-flx="guild.guild-notification-settings-modal.section-description--2"
						>
							{i18n._(REPLY_MENTION_PREFERENCE_SECTION_DESCRIPTION_DESCRIPTOR)}
						</p>
						<RadioGroup
							options={mentionPreferenceOptions}
							value={memberMentionFlags}
							onChange={handleMentionPreferenceChange}
							aria-label={i18n._(REPLY_MENTION_PREFERENCE_ARIA_DESCRIPTOR)}
							data-flx="guild.guild-notification-settings-modal.radio-group.mention-preference-change"
						/>
					</div>
					<div
						className={styles.mobilePushSection}
						data-flx="guild.guild-notification-settings-modal.mobile-push-section"
					>
						<Switch
							label={i18n._(MOBILE_PUSH_NOTIFICATIONS_SWITCH_DESCRIPTOR)}
							value={settings.mobile_push}
							onChange={(value) => UserGuildSettingsCommands.updateGuildSettings(guildId, {mobile_push: value})}
							data-flx="guild.guild-notification-settings-modal.switch.update-guild-settings--4"
						/>
					</div>
					<div className={styles.overridesSection} data-flx="guild.guild-notification-settings-modal.overrides-section">
						<h3 className={styles.sectionTitle} data-flx="guild.guild-notification-settings-modal.section-title--4">
							{i18n._(NOTIFICATION_OVERRIDES_SECTION_DESCRIPTOR)}
						</h3>
						<Combobox<string | null>
							value={null}
							options={selectOptions}
							onChange={handleAddOverride}
							placeholder={i18n._(SELECT_CHANNEL_OR_CATEGORY_PLACEHOLDER_DESCRIPTOR)}
							data-flx="guild.guild-notification-settings-modal.select.add-override"
						/>
						{overrideChannels.length > 0 && (
							<div
								className={styles.overridesSection}
								data-flx="guild.guild-notification-settings-modal.overrides-section--2"
							>
								<div
									className={styles.overridesHeader}
									data-flx="guild.guild-notification-settings-modal.overrides-header"
								>
									<div
										className={styles.overridesHeaderCellLeft}
										data-flx="guild.guild-notification-settings-modal.overrides-header-cell-left"
									>
										{i18n._(CHANNEL_OR_CATEGORY_COLUMN_HEADER_DESCRIPTOR)}
									</div>
									<div
										className={styles.overridesHeaderCell}
										data-flx="guild.guild-notification-settings-modal.overrides-header-cell"
									>
										{i18n._(ALL_COLUMN_HEADER_DESCRIPTOR)}
									</div>
									<div
										className={styles.overridesHeaderCell}
										data-flx="guild.guild-notification-settings-modal.overrides-header-cell--2"
									>
										{i18n._(MENTIONS_COLUMN_HEADER_DESCRIPTOR)}
									</div>
									<div
										className={styles.overridesHeaderCell}
										data-flx="guild.guild-notification-settings-modal.overrides-header-cell--3"
									>
										{i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR)}
									</div>
									<div
										className={styles.overridesHeaderCellMute}
										data-flx="guild.guild-notification-settings-modal.overrides-header-cell-mute"
									>
										{i18n._(MUTE_COLUMN_HEADER_DESCRIPTOR)}
									</div>
								</div>
								{overrideChannels.map(({channelId, override, channel, category, isCategory}) => {
									if (!channel) return null;
									const notifLevel = override.message_notifications ?? MessageNotifications.INHERIT;
									const isAll = notifLevel === MessageNotifications.ALL_MESSAGES;
									const isMentions = notifLevel === MessageNotifications.ONLY_MENTIONS;
									const isNothing = notifLevel === MessageNotifications.NO_MESSAGES;
									const isInherit = notifLevel === MessageNotifications.INHERIT;
									const resolvedLevel = isInherit ? guildNotificationLevel : notifLevel;
									return (
										<div
											key={channelId}
											className={styles.overrideItem}
											data-flx="guild.guild-notification-settings-modal.override-item"
										>
											<div
												className={styles.overrideHeader}
												data-flx="guild.guild-notification-settings-modal.override-header"
											>
												<div
													className={styles.channelInfo}
													data-flx="guild.guild-notification-settings-modal.channel-info"
												>
													{isCategory ? (
														<FolderIcon
															size={remFromPx(20)}
															className={styles.channelIcon}
															data-flx="guild.guild-notification-settings-modal.channel-icon"
														/>
													) : (
														ChannelUtils.getIcon(channel, {
															size: 20,
															className: styles.channelIcon,
														})
													)}
													<div
														className={styles.channelDetails}
														data-flx="guild.guild-notification-settings-modal.channel-details"
													>
														<span
															className={styles.channelName}
															data-flx="guild.guild-notification-settings-modal.channel-name"
														>
															{channel.name ?? ''}
														</span>
														{!isCategory && (
															<span
																className={styles.categoryName}
																data-flx="guild.guild-notification-settings-modal.category-name"
															>
																{category ? (category.name ?? '') : i18n._(NO_CATEGORY_FALLBACK_DESCRIPTOR)}
															</span>
														)}
													</div>
												</div>
												<button
													type="button"
													onClick={() => handleRemoveOverride(channelId)}
													className={styles.removeButton}
													aria-label={i18n._(REMOVE_OVERRIDE_BUTTON_ARIA_DESCRIPTOR)}
													data-flx="guild.guild-notification-settings-modal.remove-button.remove-override"
												>
													<XIcon
														size={remFromPx(14)}
														weight="bold"
														data-flx="guild.guild-notification-settings-modal.x-icon"
													/>
												</button>
											</div>
											<div
												className={styles.mobileOverrideOptions}
												data-flx="guild.guild-notification-settings-modal.mobile-override-options"
											>
												<Switch
													label={i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR)}
													value={isAll || (isInherit && resolvedLevel === MessageNotifications.ALL_MESSAGES)}
													onChange={() =>
														handleOverrideNotificationChange(channelId, MessageNotifications.ALL_MESSAGES)
													}
													compact
													data-flx="guild.guild-notification-settings-modal.switch.override-notification-change"
												/>
												<Switch
													label={i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR)}
													value={isMentions || (isInherit && resolvedLevel === MessageNotifications.ONLY_MENTIONS)}
													onChange={() =>
														handleOverrideNotificationChange(channelId, MessageNotifications.ONLY_MENTIONS)
													}
													compact
													data-flx="guild.guild-notification-settings-modal.switch.override-notification-change--2"
												/>
												<Switch
													label={i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR)}
													value={isNothing || (isInherit && resolvedLevel === MessageNotifications.NO_MESSAGES)}
													onChange={() => handleOverrideNotificationChange(channelId, MessageNotifications.NO_MESSAGES)}
													compact
													data-flx="guild.guild-notification-settings-modal.switch.override-notification-change--3"
												/>
												<Switch
													label={muteChannelLabel}
													value={override.muted}
													onChange={(checked) => handleOverrideMuteChange(channelId, checked)}
													compact
													data-flx="guild.guild-notification-settings-modal.switch.override-mute-change"
												/>
											</div>
											<div
												className={styles.desktopNotificationOptions}
												data-flx="guild.guild-notification-settings-modal.desktop-notification-options"
											>
												<div
													className={styles.checkboxCell}
													data-flx="guild.guild-notification-settings-modal.checkbox-cell"
												>
													<Checkbox
														checked={isAll || (isInherit && resolvedLevel === MessageNotifications.ALL_MESSAGES)}
														onChange={() =>
															handleOverrideNotificationChange(channelId, MessageNotifications.ALL_MESSAGES)
														}
														aria-label={i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR)}
														data-flx="guild.guild-notification-settings-modal.checkbox.override-notification-change"
													/>
												</div>
												<div
													className={styles.checkboxCell}
													data-flx="guild.guild-notification-settings-modal.checkbox-cell--2"
												>
													<Checkbox
														checked={isMentions || (isInherit && resolvedLevel === MessageNotifications.ONLY_MENTIONS)}
														onChange={() =>
															handleOverrideNotificationChange(channelId, MessageNotifications.ONLY_MENTIONS)
														}
														aria-label={i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR)}
														data-flx="guild.guild-notification-settings-modal.checkbox.override-notification-change--2"
													/>
												</div>
												<div
													className={styles.checkboxCell}
													data-flx="guild.guild-notification-settings-modal.checkbox-cell--3"
												>
													<Checkbox
														checked={isNothing || (isInherit && resolvedLevel === MessageNotifications.NO_MESSAGES)}
														onChange={() =>
															handleOverrideNotificationChange(channelId, MessageNotifications.NO_MESSAGES)
														}
														aria-label={i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR)}
														data-flx="guild.guild-notification-settings-modal.checkbox.override-notification-change--3"
													/>
												</div>
												<div
													className={styles.checkboxCell}
													data-flx="guild.guild-notification-settings-modal.checkbox-cell--4"
												>
													<Checkbox
														checked={override.muted}
														onChange={(checked) => handleOverrideMuteChange(channelId, checked)}
														aria-label={muteChannelLabel}
														data-flx="guild.guild-notification-settings-modal.checkbox.override-mute-change"
													/>
												</div>
											</div>
										</div>
									);
								})}
							</div>
						)}
					</div>
				</div>
			</Modal.Content>
		</Modal.Root>
	);
});
