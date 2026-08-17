// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/shared/RecipientList.module.css';
import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {
	DIRECT_MESSAGE_DESCRIPTOR,
	SEARCH_FRIENDS_DESCRIPTOR,
	SENT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Relationships from '@app/features/relationship/state/Relationships';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Scroller} from '@app/features/ui/components/Scroller';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useRef, useState} from 'react';

const GROUP_DM_DESCRIPTOR = msg({
	message: 'Group DM',
	comment: 'Short label in the settings dialog recipient list.',
});

export interface RecipientItem {
	id: string;
	user: User;
	type: 'dm' | 'group_dm' | 'friend';
	channelId?: string;
	channelName?: string;
}

export const useRecipientItems = () => {
	const {i18n} = useLingui();
	const relationships = Relationships.getRelationships();
	const dmChannels = Channels.dmChannels;
	const usersSnapshot = Users.usersList;
	const initialOrderRef = useRef<Array<string> | null>(null);
	return useMemo(() => {
		const recipients: Array<RecipientItem> = [];
		const friends = relationships.filter((r) => r.type === RelationshipTypes.FRIEND);
		const friendIds = new Set(friends.map((f) => f.id));
		dmChannels.forEach((channel) => {
			if (channel.type === ChannelTypes.DM && channel.recipientIds.length > 0) {
				const recipientId = channel.recipientIds[0];
				const user = Users.getUser(recipientId);
				if (user && friendIds.has(recipientId)) {
					recipients.push({
						id: recipientId,
						user,
						type: 'dm',
						channelId: channel.id,
						channelName: channel.name,
					});
					friendIds.delete(recipientId);
				}
			}
		});
		dmChannels.forEach((channel) => {
			if (channel.type === ChannelTypes.GROUP_DM) {
				const recipientId = channel.recipientIds[0];
				const user = Users.getUser(recipientId);
				if (user) {
					recipients.push({
						id: channel.id,
						user,
						type: 'group_dm',
						channelId: channel.id,
						channelName: ChannelUtils.getDMDisplayName(channel),
					});
				}
			}
		});
		friendIds.forEach((userId) => {
			const user = Users.getUser(userId);
			if (user) {
				recipients.push({
					id: userId,
					user,
					type: 'friend',
				});
			}
		});
		if (initialOrderRef.current === null) {
			initialOrderRef.current = recipients.map((r) => r.id);
			return recipients;
		}
		const orderMap = new Map(initialOrderRef.current.map((id, index) => [id, index]));
		const existingIds = new Set(initialOrderRef.current);
		const sorted = recipients.sort((a, b) => {
			const aInOrder = existingIds.has(a.id);
			const bInOrder = existingIds.has(b.id);
			if (aInOrder && bInOrder) {
				return (orderMap.get(a.id) ?? 0) - (orderMap.get(b.id) ?? 0);
			}
			if (aInOrder) return -1;
			if (bInOrder) return 1;
			return 0;
		});
		return sorted;
	}, [relationships, dmChannels, usersSnapshot, i18n.locale]);
};

interface RecipientListProps {
	recipients: Array<RecipientItem>;
	sendingTo: Set<string>;
	sentTo: Map<string, boolean>;
	onSend: (item: RecipientItem) => void;
	defaultButtonLabel: React.ReactNode;
	sentButtonLabel?: React.ReactNode;
	buttonClassName?: string;
	buttonDisabled?: (item: RecipientItem, isSent: boolean) => boolean;
	searchPlaceholder?: string;
	noResultsMessage?: React.ReactNode;
	scrollerKey?: string;
	searchQuery?: string;
	onSearchQueryChange?: (value: string) => void;
	showSearchInput?: boolean;
}

export const RecipientList = observer((props: RecipientListProps) => {
	const {i18n} = useLingui();
	const [internalSearchQuery, setInternalSearchQuery] = useState('');
	const searchQuery = props.searchQuery ?? internalSearchQuery;
	const filteredRecipients = useMemo(() => {
		if (!searchQuery.trim()) {
			return props.recipients;
		}
		const query = searchQuery.toLowerCase();
		return props.recipients.filter((item) => {
			const username = item.user.username.toLowerCase();
			const displayName = NicknameUtils.getNickname(item.user, null).toLowerCase();
			const channelName = (item.channelName || '').toLowerCase();
			return username.includes(query) || displayName.includes(query) || channelName.includes(query);
		});
	}, [searchQuery, props.recipients]);
	const handleSearchChange = (value: string) => {
		if (props.onSearchQueryChange) {
			props.onSearchQueryChange(value);
		} else {
			setInternalSearchQuery(value);
		}
	};
	return (
		<div className={styles.content} data-flx="app.recipient-list.content">
			{(props.showSearchInput ?? true) && (
				<Input
					value={searchQuery}
					onChange={(e) => handleSearchChange(e.target.value)}
					placeholder={props.searchPlaceholder ?? i18n._(SEARCH_FRIENDS_DESCRIPTOR)}
					leftIcon={
						<MagnifyingGlassIcon
							size={20}
							weight="bold"
							className={styles.searchIcon}
							data-flx="app.recipient-list.search-icon"
						/>
					}
					className={styles.searchInput}
					data-flx="app.recipient-list.search-input.search-change"
				/>
			)}
			<div className={styles.listContainer} data-flx="app.recipient-list.list-container">
				<Scroller
					className={styles.scroller}
					key={props.scrollerKey ?? 'recipient-list-scroller'}
					fade={false}
					data-flx="app.recipient-list.scroller"
				>
					{filteredRecipients.length === 0 ? (
						<div className={styles.noResults} data-flx="app.recipient-list.no-results">
							{props.noResultsMessage ?? <Trans>No friends match that.</Trans>}
						</div>
					) : (
						<div className={styles.friendList} data-flx="app.recipient-list.friend-list">
							{filteredRecipients.map((item) => {
								const userId = item.type === 'group_dm' ? item.id : item.user.id;
								const isSending = props.sendingTo.has(userId);
								const isSent = props.sentTo.has(userId);
								const displayName =
									item.type === 'group_dm' ? item.channelName : NicknameUtils.getNickname(item.user, null);
								const secondaryText = (() => {
									if (item.type === 'dm') return i18n._(DIRECT_MESSAGE_DESCRIPTOR);
									if (item.type === 'group_dm') return i18n._(GROUP_DM_DESCRIPTOR);
									return item.user.username;
								})();
								const groupChannel =
									item.type === 'group_dm' && item.channelId ? Channels.getChannel(item.channelId) : null;
								return (
									<div key={userId} className={styles.friendItem} data-flx="app.recipient-list.friend-item">
										<div className={styles.friendItemLeft} data-flx="app.recipient-list.friend-item-left">
											{groupChannel ? (
												<GroupDMAvatar channel={groupChannel} size={32} data-flx="app.recipient-list.group-dm-avatar" />
											) : (
												<StatusAwareAvatar
													user={item.user}
													size={32}
													data-flx="app.recipient-list.status-aware-avatar"
												/>
											)}
											<div className={styles.friendInfo} data-flx="app.recipient-list.friend-info">
												<span className={styles.friendName} data-flx="app.recipient-list.friend-name">
													{displayName}
												</span>
												<span className={styles.friendSecondary} data-flx="app.recipient-list.friend-secondary">
													{secondaryText}
												</span>
											</div>
										</div>
										<Button
											small
											variant="secondary"
											onClick={() => props.onSend(item)}
											disabled={props.buttonDisabled ? props.buttonDisabled(item, isSent) : isSent}
											submitting={isSending}
											className={props.buttonClassName ?? styles.actionButton}
											data-flx="app.recipient-list.action-button.send"
										>
											{isSent ? (props.sentButtonLabel ?? i18n._(SENT_DESCRIPTOR)) : props.defaultButtonLabel}
										</Button>
									</div>
								);
							})}
						</div>
					)}
				</Scroller>
			</div>
		</div>
	);
});
