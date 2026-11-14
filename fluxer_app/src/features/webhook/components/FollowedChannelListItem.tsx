// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {getFollowerWebhookTargetChannels} from '@app/features/channel/utils/ChannelFollowUtils';
import {UNKNOWN_CHANNEL_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as WebhookCommands from '@app/features/webhook/commands/WebhookCommands';
import followedStyles from '@app/features/webhook/components/FollowedChannelListItem.module.css';
import styles from '@app/features/webhook/components/WebhookListItem.module.css';
import type {Webhook} from '@app/features/webhook/models/Webhook';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CaretDownIcon, LinkBreakIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const FOLLOWED_CHANNEL_DESCRIPTOR = msg({
	message: 'Followed channel {currentName}',
	comment:
		'Accessible label of the expand button for one followed announcement channel in the webhook settings. currentName is the name its messages are posted under.',
});
const NAME_DESCRIPTOR = msg({
	message: 'Name',
	comment: 'Form label for the name that messages from a followed announcement channel are posted under.',
});
const POST_TO_DESCRIPTOR = msg({
	message: 'Post to',
	comment: 'Form label for the channel that receives messages from a followed announcement channel.',
});
const UNFOLLOW_TITLE_DESCRIPTOR = msg({
	message: 'Unfollow {name}?',
	comment:
		'Title of the confirmation dialog before unfollowing an announcement channel. name is the name its messages are posted under.',
});
const UNFOLLOW_BODY_DESCRIPTOR = msg({
	message:
		'Updates from this channel will stop arriving here. Messages already delivered stay. You can follow it again from the announcement channel.',
	comment: 'Body of the confirmation dialog before unfollowing an announcement channel.',
});
const UNFOLLOW_DESCRIPTOR = msg({
	message: 'Unfollow',
	comment: 'Danger button label that stops copying messages from a followed announcement channel into this channel.',
});
const UNFOLLOW_FAILED_DESCRIPTOR = msg({
	message: "Couldn't unfollow this channel",
	comment: 'Error shown when unfollowing an announcement channel fails.',
});

interface FollowedChannelListItemProps {
	webhook: Webhook;
	onUpdate: (webhookId: string, updates: {name?: string; channelId?: string}) => void;
	isExpanded: boolean;
	onExpandedChange: (expanded: boolean) => void;
	formVersion?: number;
}

const logger = new Logger('FollowedChannelListItem');

export const FollowedChannelListItem: React.FC<FollowedChannelListItemProps> = observer(
	({webhook, onUpdate, isExpanded, onExpandedChange, formVersion}) => {
		const {i18n} = useLingui();
		const [selectedChannelId, setSelectedChannelId] = useState(webhook.channelId);
		const [currentName, setCurrentName] = useState(webhook.name);
		useEffect(() => {
			if (formVersion == null) return;
			setCurrentName(webhook.name);
			setSelectedChannelId(webhook.channelId);
		}, [formVersion, webhook.name, webhook.channelId]);
		const sourceGuild = webhook.sourceGuild;
		const sourceChannel = webhook.sourceChannel;
		const availableChannels = getFollowerWebhookTargetChannels(
			webhook.guildId,
			sourceChannel?.id ?? null,
			webhook.channelId,
		).map((channel) => ({id: channel.id, label: channel.name ?? i18n._(UNKNOWN_CHANNEL_DESCRIPTOR)}));
		const avatarUrl = useMemo(
			() => AvatarUtils.getWebhookAvatarURL({id: webhook.id, avatar: webhook.avatar}, false),
			[webhook.id, webhook.avatar],
		);
		const handleChannelChange = useCallback(
			(newChannelId: string) => {
				setSelectedChannelId(newChannelId);
				onUpdate(webhook.id, {channelId: newChannelId});
			},
			[onUpdate, webhook.id],
		);
		const handleUnfollow = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(UNFOLLOW_TITLE_DESCRIPTOR, {name: webhook.name})}
						description={i18n._(UNFOLLOW_BODY_DESCRIPTOR)}
						primaryText={i18n._(UNFOLLOW_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={async () => {
							try {
								await WebhookCommands.deleteWebhook(webhook.id);
							} catch (error) {
								logger.error('Failed to unfollow channel', error);
								showGenericErrorModal({
									title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
									message: () => i18n._(UNFOLLOW_FAILED_DESCRIPTOR),
									dataFlx: 'webhook.followed-channel-list-item.unfollow-error-modal',
								});
							}
						}}
						data-flx="webhook.followed-channel-list-item.unfollow-confirm-modal"
					/>
				)),
			);
		}, [i18n, webhook.id, webhook.name]);
		const sourceGuildName = sourceGuild?.name ?? '';
		const sourceChannelName = sourceChannel?.name ?? '';
		return (
			<div className={styles.container} data-flx="webhook.followed-channel-list-item.container">
				<FocusRing offset={-2} data-flx="webhook.followed-channel-list-item.focus-ring">
					<button
						type="button"
						className={styles.headerButton}
						onClick={() => onExpandedChange(!isExpanded)}
						aria-expanded={isExpanded}
						aria-label={i18n._(FOLLOWED_CHANNEL_DESCRIPTOR, {currentName})}
						data-flx="webhook.followed-channel-list-item.header-button.toggle-expanded"
					>
						<div className={styles.left} data-flx="webhook.followed-channel-list-item.left">
							<div
								className={styles.avatarLarge}
								style={{backgroundImage: `url(${avatarUrl})`}}
								aria-hidden
								data-flx="webhook.followed-channel-list-item.avatar-large"
							/>
							<div className={styles.textBlock} data-flx="webhook.followed-channel-list-item.text-block">
								<div className={styles.titleRow} data-flx="webhook.followed-channel-list-item.title-row">
									<span className={styles.name} data-flx="webhook.followed-channel-list-item.name">
										{currentName}
									</span>
								</div>
								<div className={styles.metaRow} data-flx="webhook.followed-channel-list-item.meta-row">
									{sourceGuild ? (
										<span className={followedStyles.subline} data-flx="webhook.followed-channel-list-item.source">
											{sourceChannel ? (
												<Trans comment="Subline of a followed announcement channel. sourceGuildName is the community it comes from and sourceChannelName the announcement channel, without the leading #.">
													From {sourceGuildName} #{sourceChannelName}
												</Trans>
											) : (
												<Trans comment="Subline of a followed announcement channel. sourceGuildName is the community it comes from.">
													From {sourceGuildName}
												</Trans>
											)}
										</span>
									) : (
										<span
											className={clsx(followedStyles.subline, followedStyles.paused)}
											data-flx="webhook.followed-channel-list-item.paused"
										>
											<Trans comment="Subline of a followed announcement channel whose updates are paused because the person who followed it lost access to it.">
												Paused. The person who followed this channel can no longer see it. Follow it again to resume.
											</Trans>
										</span>
									)}
								</div>
							</div>
						</div>
						<CaretDownIcon
							className={clsx(styles.chevron, isExpanded && styles.chevronExpanded)}
							weight="bold"
							data-flx="webhook.followed-channel-list-item.chevron"
						/>
					</button>
				</FocusRing>
				{isExpanded && (
					<div className={styles.details} data-flx="webhook.followed-channel-list-item.details">
						<div className={styles.fieldsRow} data-flx="webhook.followed-channel-list-item.fields-row">
							<div className={styles.fieldGrow} data-flx="webhook.followed-channel-list-item.field-name">
								<Input
									id={`followed-channel-name-${webhook.id}`}
									label={i18n._(NAME_DESCRIPTOR)}
									value={currentName}
									onChange={(event) => {
										const newName = event.target.value;
										setCurrentName(newName);
										onUpdate(webhook.id, {name: newName});
									}}
									data-flx="webhook.followed-channel-list-item.input.name"
								/>
							</div>
							{availableChannels.length > 0 && (
								<div className={styles.fieldGrow} data-flx="webhook.followed-channel-list-item.field-channel">
									<Combobox
										label={i18n._(POST_TO_DESCRIPTOR)}
										value={selectedChannelId}
										options={availableChannels.map((option) => ({value: option.id, label: option.label}))}
										onChange={handleChannelChange}
										data-flx="webhook.followed-channel-list-item.select.channel"
									/>
								</div>
							)}
						</div>
						<div className={styles.actions} data-flx="webhook.followed-channel-list-item.actions">
							<Button
								variant="danger"
								small={true}
								onClick={handleUnfollow}
								leftIcon={
									<LinkBreakIcon
										className={styles.iconSmall}
										weight="bold"
										data-flx="webhook.followed-channel-list-item.unfollow-icon"
									/>
								}
								data-flx="webhook.followed-channel-list-item.button.unfollow"
							>
								{i18n._(UNFOLLOW_DESCRIPTOR)}
							</Button>
						</div>
					</div>
				)}
			</div>
		);
	},
);
