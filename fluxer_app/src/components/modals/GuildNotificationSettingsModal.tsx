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
import {FolderIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {ChannelTypes, MessageNotifications} from '~/Constants';
import {Select} from '~/components/form/Select';
import {Switch} from '~/components/form/Switch';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import {RadioGroup, type RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import styles from './GuildNotificationSettingsModal.module.css';

interface ChannelOption {
	value: string;
	label: string;
	icon: React.ReactNode;
	categoryName?: string;
	isCategory: boolean;
}

export const GuildNotificationSettingsModal = observer(({guildId}: {guildId: string}) => {
	const {t} = useLingui();
	const guild = GuildStore.getGuild(guildId);
	const settings = UserGuildSettingsStore.getSettings(guildId);

	if (!guild || !settings) return null;

	const channels = ChannelStore.getGuildChannels(guildId);
	const categories = channels.filter((c) => c.type === ChannelTypes.GUILD_CATEGORY);

	const channelOptions: Array<ChannelOption> = [
		...categories.map((cat) => ({
			value: cat.id,
			label: cat.name || '',
			icon: <FolderIcon size={16} className={styles.iconTertiary} />,
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
			name: t`All Messages`,
		},
		{
			value: MessageNotifications.ONLY_MENTIONS,
			name: t`Only @mentions`,
		},
		{
			value: MessageNotifications.NO_MESSAGES,
			name: t`Nothing`,
		},
	];

	const handleAddOverride = (value: string | null) => {
		if (!value) return;

		const existingOverride = settings.channel_overrides?.[value];
		if (existingOverride) {
			return;
		}

		UserGuildSettingsActionCreators.updateChannelOverride(guildId, value, {
			message_notifications: MessageNotifications.INHERIT,
			muted: false,
		});
	};

	const handleRemoveOverride = (channelId: string) => {
		UserGuildSettingsActionCreators.updateChannelOverride(guildId, channelId, null);
	};

	const handleOverrideNotificationChange = (channelId: string, level: number) => {
		UserGuildSettingsActionCreators.updateChannelOverride(guildId, channelId, {
			message_notifications: level,
		});
	};

	const handleOverrideMuteChange = (channelId: string, muted: boolean) => {
		UserGuildSettingsActionCreators.updateChannelOverride(guildId, channelId, {
			muted,
		});
	};

	const overrideChannels = settings.channel_overrides
		? Object.entries(settings.channel_overrides)
				.map(([channelId, override]) => {
					const channel = ChannelStore.getChannel(channelId);
					const category = channel?.parentId ? ChannelStore.getChannel(channel.parentId) : null;
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
		<Modal.Root size="medium">
			<Modal.Header title={t`Notification Settings`} />
			<Modal.Content>
				<div className={styles.container}>
					<div className={styles.section}>
						<Switch
							label={t`Mute ${guild.name}`}
							description={t`Muting a community prevents unread indicators and notifications from appearing unless you are mentioned`}
							value={settings.muted}
							onChange={(value) => UserGuildSettingsActionCreators.updateGuildSettings(guildId, {muted: value})}
						/>
					</div>

					<div className={styles.notificationSection}>
						<h3 className={styles.sectionTitle}>{t`Community Notification Settings`}</h3>
						<RadioGroup
							options={notificationOptions}
							value={settings.message_notifications}
							onChange={(value) =>
								UserGuildSettingsActionCreators.updateGuildSettings(guildId, {message_notifications: value})
							}
							aria-label={t`Community notification level`}
						/>
					</div>

					<div className={styles.suppressSection}>
						<Switch
							label={t`Suppress @everyone and @here`}
							value={settings.suppress_everyone}
							onChange={(value) =>
								UserGuildSettingsActionCreators.updateGuildSettings(guildId, {suppress_everyone: value})
							}
						/>
						<Switch
							label={t`Suppress all role @mentions`}
							value={settings.suppress_roles}
							onChange={(value) =>
								UserGuildSettingsActionCreators.updateGuildSettings(guildId, {suppress_roles: value})
							}
						/>
					</div>

					<div className={styles.mobilePushSection}>
						<Switch
							label={t`Mobile Push Notifications`}
							value={settings.mobile_push}
							onChange={(value) => UserGuildSettingsActionCreators.updateGuildSettings(guildId, {mobile_push: value})}
						/>
					</div>

					<div className={styles.overridesSection}>
						<h3 className={styles.sectionTitle}>{t`Notification Overrides`}</h3>
						<Select<string | null>
							value={null}
							options={selectOptions}
							onChange={handleAddOverride}
							placeholder={t`Select a channel or category`}
						/>

						{overrideChannels.length > 0 && (
							<div className={styles.overridesSection}>
								<div className={styles.overridesHeader}>
									<div className={styles.overridesHeaderCellLeft}>{t`Channel or Category`}</div>
									<div className={styles.overridesHeaderCell}>{t`All`}</div>
									<div className={styles.overridesHeaderCell}>{t`Mentions`}</div>
									<div className={styles.overridesHeaderCell}>{t`Nothing`}</div>
									<div className={styles.overridesHeaderCellMute}>{t`Mute`}</div>
								</div>
								{overrideChannels.map(({channelId, override, channel, category, isCategory}) => {
									if (!channel) return null;

									const notifLevel = override.message_notifications ?? MessageNotifications.INHERIT;
									const isAll = notifLevel === MessageNotifications.ALL_MESSAGES;
									const isMentions = notifLevel === MessageNotifications.ONLY_MENTIONS;
									const isNothing = notifLevel === MessageNotifications.NO_MESSAGES;
									const isInherit = notifLevel === MessageNotifications.INHERIT;

									const resolvedLevel = isInherit ? settings.message_notifications : notifLevel;

									return (
										<div key={channelId} className={styles.overrideItem}>
											<div className={styles.overrideHeader}>
												<div className={styles.channelInfo}>
													{isCategory ? (
														<FolderIcon size={20} className={styles.channelIcon} />
													) : (
														ChannelUtils.getIcon(channel, {
															size: 20,
															className: styles.channelIcon,
														})
													)}
													<div className={styles.channelDetails}>
														<span className={styles.channelName}>{channel.name ?? ''}</span>
														{!isCategory && (
															<span className={styles.categoryName}>
																{category ? (category.name ?? '') : t`No Category`}
															</span>
														)}
													</div>
												</div>
												<button
													type="button"
													onClick={() => handleRemoveOverride(channelId)}
													className={styles.removeButton}
													aria-label={t`Remove override`}
												>
													<XIcon size={14} weight="bold" />
												</button>
											</div>

											<div className={styles.mobileOverrideOptions}>
												<Switch
													label={t`All Messages`}
													value={isAll || (isInherit && resolvedLevel === MessageNotifications.ALL_MESSAGES)}
													onChange={() =>
														handleOverrideNotificationChange(channelId, MessageNotifications.ALL_MESSAGES)
													}
													compact
												/>
												<Switch
													label={t`Only @mentions`}
													value={isMentions || (isInherit && resolvedLevel === MessageNotifications.ONLY_MENTIONS)}
													onChange={() =>
														handleOverrideNotificationChange(channelId, MessageNotifications.ONLY_MENTIONS)
													}
													compact
												/>
												<Switch
													label={t`Nothing`}
													value={isNothing || (isInherit && resolvedLevel === MessageNotifications.NO_MESSAGES)}
													onChange={() => handleOverrideNotificationChange(channelId, MessageNotifications.NO_MESSAGES)}
													compact
												/>
												<Switch
													label={t`Mute Channel`}
													value={override.muted}
													onChange={(checked) => handleOverrideMuteChange(channelId, checked)}
													compact
												/>
											</div>

											<div className={styles.desktopNotificationOptions}>
												<div className={styles.checkboxCell}>
													<Checkbox
														checked={isAll || (isInherit && resolvedLevel === MessageNotifications.ALL_MESSAGES)}
														onChange={() =>
															handleOverrideNotificationChange(channelId, MessageNotifications.ALL_MESSAGES)
														}
														aria-label={t`All Messages`}
													/>
												</div>
												<div className={styles.checkboxCell}>
													<Checkbox
														checked={isMentions || (isInherit && resolvedLevel === MessageNotifications.ONLY_MENTIONS)}
														onChange={() =>
															handleOverrideNotificationChange(channelId, MessageNotifications.ONLY_MENTIONS)
														}
														aria-label={t`Only @mentions`}
													/>
												</div>
												<div className={styles.checkboxCell}>
													<Checkbox
														checked={isNothing || (isInherit && resolvedLevel === MessageNotifications.NO_MESSAGES)}
														onChange={() =>
															handleOverrideNotificationChange(channelId, MessageNotifications.NO_MESSAGES)
														}
														aria-label={t`Nothing`}
													/>
												</div>
												<div className={styles.checkboxCell}>
													<Checkbox
														checked={override.muted}
														onChange={(checked) => handleOverrideMuteChange(channelId, checked)}
														aria-label={t`Mute channel`}
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
			<Modal.Footer>
				<Button onClick={() => ModalActionCreators.pop()}>{t`Done`}</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
