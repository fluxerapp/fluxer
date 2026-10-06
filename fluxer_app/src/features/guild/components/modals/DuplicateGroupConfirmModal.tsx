// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import styles from '@app/features/guild/components/modals/DuplicateGroupConfirmModal.module.css';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {formatShortRelativeTime} from '@app/features/ui/utils/ShortRelativeTimeLabels';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

const NO_ACTIVITY_YET_DESCRIPTOR = msg({
	message: 'No activity yet',
	comment: 'Empty-state text in the duplicate group confirm modal.',
});
const CONFIRM_NEW_GROUP_DESCRIPTOR = msg({
	message: 'Confirm new group',
	comment: 'Short label in the duplicate group confirm modal. Keep it concise.',
});
const CREATE_NEW_GROUP_DESCRIPTOR = msg({
	message: 'Create new group',
	comment: 'Button or menu action label in the duplicate group confirm modal. Keep it concise.',
});

interface DuplicateGroupConfirmModalProps {
	channels: Array<Channel>;
	onConfirm: () => Promise<void> | void;
}

export const DuplicateGroupConfirmModal = observer(({channels, onConfirm}: DuplicateGroupConfirmModalProps) => {
	const {i18n} = useLingui();
	const handleChannelClick = useCallback((channelId: string) => {
		ModalCommands.pop();
		NavigationCommands.selectChannel(undefined, channelId);
		focusChannelTextareaAfterNavigation(channelId);
	}, []);
	const description = useMemo(() => {
		return (
			<>
				<p className={styles.description} data-flx="guild.duplicate-group-confirm-modal.description.description">
					<Trans>
						You already have a group with these users. Do you really want to create a new one? That's fine too!
					</Trans>
				</p>
				{channels.length > 0 && (
					<div className={styles.channelList} data-flx="guild.duplicate-group-confirm-modal.description.channel-list">
						{channels.map((channel) => {
							const lastActivitySnowflake = channel.lastMessageId ?? channel.id;
							const lastActiveText = formatShortRelativeTime(
								i18n,
								SnowflakeUtils.extractTimestamp(lastActivitySnowflake),
							);
							const lastActiveLabel = lastActiveText || i18n._(NO_ACTIVITY_YET_DESCRIPTOR);
							return (
								<FocusRing
									key={channel.id}
									offset={-2}
									data-flx="guild.duplicate-group-confirm-modal.description.focus-ring"
								>
									<button
										type="button"
										className={styles.channelItem}
										onClick={() => handleChannelClick(channel.id)}
										data-flx="guild.duplicate-group-confirm-modal.description.channel-item.channel-click.button"
									>
										<div
											className={styles.avatarWrapper}
											data-flx="guild.duplicate-group-confirm-modal.description.avatar-wrapper"
										>
											<GroupDMAvatar
												channel={channel}
												size={40}
												data-flx="guild.duplicate-group-confirm-modal.description.group-dm-avatar"
											/>
										</div>
										<div
											className={styles.channelDetails}
											data-flx="guild.duplicate-group-confirm-modal.description.channel-details"
										>
											<span
												className={styles.channelName}
												data-flx="guild.duplicate-group-confirm-modal.description.channel-name"
											>
												{ChannelUtils.getDMDisplayName(channel)}
											</span>
											<span
												className={styles.lastActive}
												data-flx="guild.duplicate-group-confirm-modal.description.last-active"
											>
												{lastActiveLabel}
											</span>
										</div>
									</button>
								</FocusRing>
							);
						})}
					</div>
				)}
			</>
		);
	}, [channels, handleChannelClick, i18n.locale]);
	return (
		<ConfirmModal
			title={i18n._(CONFIRM_NEW_GROUP_DESCRIPTOR)}
			description={description}
			primaryText={i18n._(CREATE_NEW_GROUP_DESCRIPTOR)}
			primaryVariant="primary"
			secondaryText={i18n._(CANCEL_DESCRIPTOR)}
			size="small"
			onPrimary={onConfirm}
			disableAutoDismiss={true}
			data-flx="guild.duplicate-group-confirm-modal.confirm-modal"
		/>
	);
});
