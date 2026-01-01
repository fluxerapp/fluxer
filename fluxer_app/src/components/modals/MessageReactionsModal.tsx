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
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ReactionActionCreators from '~/actions/ReactionActionCreators';
import {Permissions} from '~/Constants';
import reactionStyles from '~/components/channel/MessageReactions.module.css';
import styles from '~/components/modals/MessageReactionsModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Avatar} from '~/components/uikit/Avatar';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Scroller} from '~/components/uikit/Scroller';
import {Spinner} from '~/components/uikit/Spinner';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useHover} from '~/hooks/useHover';
import type {MessageReaction} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';
import MessageReactionsStore from '~/stores/MessageReactionsStore';
import MessageStore from '~/stores/MessageStore';
import PermissionStore from '~/stores/PermissionStore';
import {emojiEquals, getEmojiName, getEmojiNameWithColons, getReactionKey, useEmojiURL} from '~/utils/ReactionUtils';

const MessageReactionItem = observer(
	({
		reaction,
		selectedReaction,
		setSelectedReaction,
	}: {
		reaction: MessageReaction;
		selectedReaction: MessageReaction;
		setSelectedReaction: (reaction: MessageReaction) => void;
	}) => {
		const {t} = useLingui();
		const [hoverRef, isHovering] = useHover();
		const emojiName = getEmojiName(reaction.emoji);
		const emojiUrl = useEmojiURL({emoji: reaction.emoji, isHovering});
		const isUnicodeEmoji = reaction.emoji.id == null;
		const isSelected = emojiEquals(selectedReaction.emoji, reaction.emoji);

		return (
			<Tooltip text={getEmojiNameWithColons(reaction.emoji)} position="left">
				<div className={styles.reactionFilterButtonContainer}>
					<FocusRing offset={-2}>
						<button
							type="button"
							aria-label={
								reaction.count === 1
									? t`${emojiName}, ${reaction.count} reaction`
									: t`${emojiName}, ${reaction.count} reactions`
							}
							onClick={() => setSelectedReaction(reaction)}
							ref={hoverRef}
							className={clsx(
								reactionStyles.reactionButton,
								styles.reactionFilterButton,
								isSelected ? styles.reactionFilterButtonSelected : styles.reactionFilterButtonIdle,
							)}
						>
							<div className={reactionStyles.reactionInner}>
								{emojiUrl ? (
									<img
										className={clsx('emoji', reactionStyles.emoji)}
										src={emojiUrl}
										alt={emojiName}
										draggable={false}
									/>
								) : isUnicodeEmoji ? (
									<span className={clsx('emoji', reactionStyles.emoji)}>{reaction.emoji.name}</span>
								) : null}
								<div className={reactionStyles.countWrapper}>{reaction.count}</div>
							</div>
						</button>
					</FocusRing>
				</div>
			</Tooltip>
		);
	},
);

export const MessageReactionsModal = observer(
	({channelId, messageId, openToReaction}: {channelId: string; messageId: string; openToReaction: MessageReaction}) => {
		const {t, i18n} = useLingui();
		const [selectedReaction, setSelectedReaction] = React.useState(openToReaction);
		const message = MessageStore.getMessage(channelId, messageId);
		const channel = ChannelStore.getChannel(channelId);
		const guildId = channel?.guildId;
		const canManageMessages = PermissionStore.can(Permissions.MANAGE_MESSAGES, {
			guildId,
			channelId,
		});

		const reactors = MessageReactionsStore.getReactions(messageId, selectedReaction.emoji);
		const fetchStatus = MessageReactionsStore.getFetchStatus(messageId, selectedReaction.emoji);
		const isLoading = fetchStatus === 'pending';
		const reactorScrollerKey = React.useMemo(
			() =>
				message
					? `message-reactions-reactor-scroller-${getReactionKey(message.id, selectedReaction.emoji)}`
					: 'message-reactions-reactor-scroller',
			[message?.id, selectedReaction.emoji],
		);

		React.useEffect(() => {
			if (!message || fetchStatus === 'pending') {
				return;
			}

			if (!message?.reactions) return;

			const reactionOnMessage = message.reactions.find((reaction) =>
				emojiEquals(reaction.emoji, selectedReaction.emoji),
			);

			if (!reactionOnMessage || reactionOnMessage.count === 0) {
				return;
			}

			const desired = Math.min(100, reactionOnMessage.count);
			if (fetchStatus === 'idle' || reactors.length < desired) {
				ReactionActionCreators.getReactions(channelId, messageId, selectedReaction.emoji, 100);
			}
		}, [channelId, messageId, selectedReaction.emoji, fetchStatus, reactors.length, message?.reactions]);

		React.useEffect(() => {
			if (!message) {
				ModalActionCreators.pop();
				return;
			}

			const reactions = message.reactions;
			if (!reactions || reactions.length === 0) {
				ModalActionCreators.pop();
				return;
			}

			const selectedReactionExists = reactions.some((reaction) => emojiEquals(reaction.emoji, selectedReaction.emoji));
			if (!selectedReactionExists) {
				setSelectedReaction(reactions[0]);
			}
		}, [message, selectedReaction.emoji]);

		if (!message) {
			return null;
		}

		return (
			<Modal.Root size="medium" className={styles.modalRoot} onClose={() => ModalActionCreators.pop()}>
				<Modal.Header title={<Trans>Reactions</Trans>} />
				<Modal.Content className={styles.modalContent} padding="none">
					<div className={styles.modalLayout}>
						<div className={styles.sidebar}>
							<div className={styles.reactionFiltersPane}>
								<Scroller
									className={clsx(styles.scrollerPadding, styles.sidebarScroller)}
									key="message-reactions-filter-scroller"
									reserveScrollbarTrack={false}
								>
									{message.reactions.length > 0 &&
										message.reactions.map((reaction) => (
											<MessageReactionItem
												key={getReactionKey(message.id, reaction.emoji)}
												reaction={reaction}
												selectedReaction={selectedReaction}
												setSelectedReaction={setSelectedReaction}
											/>
										))}
									{message.reactions.length === 0 && (
										<div className={styles.noReactionsContainer}>
											<div className={styles.noReactionsText}>{t`No reactions`}</div>
										</div>
									)}
								</Scroller>
							</div>
						</div>
						<div className={styles.reactionListContainer}>
							<div className={styles.reactionListPanel}>
								<Scroller
									className={clsx(styles.scrollerColumn, styles.reactorScroller)}
									key={reactorScrollerKey}
									reserveScrollbarTrack={false}
								>
									{reactors.map((reactor, index) => (
										<div
											key={reactor.id}
											className={clsx(styles.reactorItem, index > 0 && styles.reactorItemBorder)}
											data-user-id={reactor.id}
											data-channel-id={channelId}
										>
											<Avatar user={reactor} size={24} guildId={guildId} />
											<div className={styles.reactorInfo}>
												<span className={styles.reactorName}>{reactor.displayName}</span>
												<span className={styles.reactorTag}>{reactor.tag}</span>
											</div>
											{canManageMessages && (
												<FocusRing offset={-2}>
													<button
														type="button"
														onClick={() =>
															ReactionActionCreators.removeReaction(
																i18n,
																channelId,
																messageId,
																selectedReaction.emoji,
																reactor.id,
															)
														}
														className={styles.removeReactionButton}
													>
														<XIcon weight="regular" className={styles.removeReactionIcon} />
													</button>
												</FocusRing>
											)}
										</div>
									))}
									{isLoading && reactors.length === 0 && (
										<div className={styles.loadingContainer}>
											<Spinner size="medium" />
											<span className={styles.srOnly}>Loading reactions</span>
										</div>
									)}
								</Scroller>
							</div>
						</div>
					</div>
				</Modal.Content>
			</Modal.Root>
		);
	},
);
