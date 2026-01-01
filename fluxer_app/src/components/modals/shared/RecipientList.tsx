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

import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {ChannelTypes, RelationshipTypes} from '~/Constants';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {Input} from '~/components/form/Input';
import {Button} from '~/components/uikit/Button/Button';
import {Scroller} from '~/components/uikit/Scroller';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import type {UserRecord} from '~/records/UserRecord';
import ChannelStore from '~/stores/ChannelStore';
import RelationshipStore from '~/stores/RelationshipStore';
import UserStore from '~/stores/UserStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import styles from './RecipientList.module.css';

export interface RecipientItem {
	id: string;
	user: UserRecord;
	type: 'dm' | 'group_dm' | 'friend';
	channelId?: string;
	channelName?: string;
}

export const useRecipientItems = () => {
	const relationships = RelationshipStore.getRelationships();
	const dmChannels = ChannelStore.dmChannels;
	const usersSnapshot = UserStore.usersList;

	const initialOrderRef = React.useRef<Array<string> | null>(null);

	return React.useMemo(() => {
		const recipients: Array<RecipientItem> = [];
		const friends = relationships.filter((r) => r.type === RelationshipTypes.FRIEND);
		const friendIds = new Set(friends.map((f) => f.id));

		dmChannels.forEach((channel) => {
			if (channel.type === ChannelTypes.DM && channel.recipientIds.length > 0) {
				const recipientId = channel.recipientIds[0];
				const user = UserStore.getUser(recipientId);
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
				const user = UserStore.getUser(recipientId);
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
			const user = UserStore.getUser(userId);
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
	}, [relationships, dmChannels, usersSnapshot]);
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
	const {t} = useLingui();
	const [internalSearchQuery, setInternalSearchQuery] = React.useState('');
	const searchQuery = props.searchQuery ?? internalSearchQuery;

	const filteredRecipients = React.useMemo(() => {
		if (!searchQuery.trim()) {
			return props.recipients;
		}

		const query = searchQuery.toLowerCase();
		return props.recipients.filter((item) => {
			const username = item.user.username.toLowerCase();
			const displayName = NicknameUtils.getNickname(item.user).toLowerCase();
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
		<div className={styles.content}>
			{(props.showSearchInput ?? true) && (
				<Input
					value={searchQuery}
					onChange={(e) => handleSearchChange(e.target.value)}
					placeholder={props.searchPlaceholder ?? t`Search friends`}
					leftIcon={<MagnifyingGlassIcon size={20} weight="bold" className={styles.searchIcon} />}
					className={styles.searchInput}
				/>
			)}

			<div className={styles.listContainer}>
				<Scroller
					className={styles.scroller}
					key={props.scrollerKey ?? 'recipient-list-scroller'}
					fade={false}
					reserveScrollbarTrack={false}
				>
					{filteredRecipients.length === 0 ? (
						<div className={styles.noResults}>{props.noResultsMessage ?? <Trans>No friends found</Trans>}</div>
					) : (
						<div className={styles.friendList}>
							{filteredRecipients.map((item) => {
								const userId = item.type === 'group_dm' ? item.id : item.user.id;
								const isSending = props.sendingTo.has(userId);
								const isSent = props.sentTo.has(userId);
								const displayName = item.type === 'group_dm' ? item.channelName : NicknameUtils.getNickname(item.user);
								const secondaryText =
									item.type === 'dm' ? t`Direct Message` : item.type === 'group_dm' ? t`Group DM` : item.user.username;

								return (
									<div key={userId} className={styles.friendItem}>
										<div className={styles.friendItemLeft}>
											{item.type === 'group_dm' && item.channelId ? (
												<GroupDMAvatar channel={ChannelStore.getChannel(item.channelId)!} size={32} />
											) : (
												<StatusAwareAvatar user={item.user} size={32} />
											)}
											<div className={styles.friendInfo}>
												<span className={styles.friendName}>{displayName}</span>
												<span className={styles.friendSecondary}>{secondaryText}</span>
											</div>
										</div>
										<Button
											small
											variant="secondary"
											onClick={() => props.onSend(item)}
											disabled={props.buttonDisabled ? props.buttonDisabled(item, isSent) : isSent}
											submitting={isSending}
											className={props.buttonClassName ?? styles.actionButton}
										>
											{isSent ? (props.sentButtonLabel ?? t`Sent`) : props.defaultButtonLabel}
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
