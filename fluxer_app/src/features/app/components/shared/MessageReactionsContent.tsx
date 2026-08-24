// SPDX-License-Identifier: AGPL-3.0-or-later

import {LongPressable} from '@app/features/app/components/LongPressable';
import styles from '@app/features/app/components/shared/MessageReactionsContent.module.css';
import {useHover} from '@app/features/app/hooks/useHover';
import reactionStyles from '@app/features/channel/components/MessageReactions.module.css';
import {ReactionImage} from '@app/features/messaging/components/ReactionImage';
import {
	emojiEquals,
	getEmojiName,
	getEmojiNameWithColons,
	getReactionKey,
	useEmojiURL,
} from '@app/features/messaging/utils/ReactionUtils';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Scroller} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {MessageReaction} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type MouseEvent, type UIEvent, useCallback} from 'react';

const MESSAGE_DESCRIPTOR = msg({
	message: '{emojiName}, {reactionCountText}',
	comment:
		'Short label in the shared app message reactions content. Preserve placeholders {emojiName}, {reactionCountText}; they are inserted by code.',
});
const REMOVE_REACTION_FROM_DESCRIPTOR = msg({
	message: 'Remove reaction from {displayName}',
	comment:
		'Short label in the shared app message reactions content. Preserve {displayName}; it is inserted by code. Keep the tone plain and specific.',
});

interface ReactionFilterButtonProps {
	reaction: MessageReaction;
	isSelected: boolean;
	onSelect: () => void;
	onContextMenu?: (event: MouseEvent<HTMLButtonElement>) => void;
	showTooltip: boolean;
}

const ReactionFilterButton = observer(
	({reaction, isSelected, onSelect, onContextMenu, showTooltip}: ReactionFilterButtonProps) => {
		const {i18n} = useLingui();
		const [hoverRef, isHovering] = useHover();
		const emojiHasAnimatedVariant = reaction.emoji.animated === true;
		const emojiName = getEmojiName(reaction.emoji);
		const emojiUrl = useEmojiURL({emoji: reaction.emoji, isHovering});
		const reactionCountText = plural(
			{count: reaction.count},
			{
				one: '# reaction',
				other: '# reactions',
			},
		);
		const ariaLabel = i18n._(MESSAGE_DESCRIPTOR, {emojiName, reactionCountText});
		const button = (
			<FocusRing offset={-2} data-flx="app.message-reactions-content.reaction-filter-button.focus-ring">
				<button
					type="button"
					aria-label={ariaLabel}
					aria-pressed={isSelected}
					onClick={onSelect}
					onContextMenu={onContextMenu}
					ref={emojiHasAnimatedVariant ? hoverRef : undefined}
					className={clsx(
						reactionStyles.reactionButton,
						styles.filterButton,
						isSelected ? styles.filterButtonSelected : styles.filterButtonIdle,
					)}
					data-flx="app.message-reactions-content.reaction-filter-button.filter-button.select"
				>
					<div
						className={reactionStyles.reactionInner}
						data-flx="app.message-reactions-content.reaction-filter-button.div"
					>
						{emojiUrl ? (
							<ReactionImage
								className={clsx('emoji', reactionStyles.emoji)}
								src={emojiUrl}
								alt={emojiName}
								aria-hidden={true}
								draggable={false}
								data-flx="app.message-reactions-content.reaction-filter-button.emoji"
							/>
						) : null}
						<div
							className={reactionStyles.countWrapper}
							data-flx="app.message-reactions-content.reaction-filter-button.div--2"
						>
							{reaction.count}
						</div>
					</div>
				</button>
			</FocusRing>
		);
		if (!showTooltip) {
			return button;
		}
		return (
			<Tooltip
				text={getEmojiNameWithColons(reaction.emoji)}
				position="left"
				data-flx="app.message-reactions-content.reaction-filter-button.tooltip"
			>
				{button}
			</Tooltip>
		);
	},
);

interface MessageReactionsFiltersProps {
	messageId: string;
	reactions: ReadonlyArray<MessageReaction>;
	selectedReaction: MessageReaction;
	onSelectReaction: (reaction: MessageReaction) => void;
	canManageMessages: boolean;
	variant: 'modal' | 'sheet';
	showScrollerTrack?: boolean;
	onReactionLongPress?: (reaction: MessageReaction) => void;
	onReactionContextMenu?: (reaction: MessageReaction, event: MouseEvent<HTMLButtonElement>) => void;
}

export const MessageReactionsFilters = observer(
	({
		messageId,
		reactions,
		selectedReaction,
		onSelectReaction,
		variant,
		showScrollerTrack = true,
		onReactionLongPress,
		onReactionContextMenu,
	}: MessageReactionsFiltersProps) => {
		if (reactions.length === 0) {
			return null;
		}
		const isHorizontal = variant === 'sheet';
		const listClassName = clsx(
			styles.filtersList,
			isHorizontal ? styles.filtersListHorizontal : styles.filtersListVertical,
		);
		const itemClassName = clsx(
			styles.filterItem,
			isHorizontal ? styles.filterItemHorizontal : styles.filterItemVertical,
		);
		return (
			<Scroller
				key="message-reactions-filter-scroller"
				orientation={isHorizontal ? 'horizontal' : 'vertical'}
				className={styles.filtersScroller}
				showTrack={showScrollerTrack}
				data-flx="app.message-reactions-content.message-reactions-filters.filters-scroller"
			>
				<div className={listClassName} data-flx="app.message-reactions-content.message-reactions-filters.div">
					{reactions.map((reaction) => {
						const isSelected = emojiEquals(reaction.emoji, selectedReaction.emoji);
						const handleContextMenu = onReactionContextMenu
							? (event: MouseEvent<HTMLButtonElement>) => onReactionContextMenu(reaction, event)
							: undefined;
						const button = (
							<ReactionFilterButton
								reaction={reaction}
								isSelected={isSelected}
								onSelect={() => onSelectReaction(reaction)}
								onContextMenu={handleContextMenu}
								showTooltip={variant === 'modal'}
								data-flx="app.message-reactions-content.message-reactions-filters.reaction-filter-button.select-reaction"
							/>
						);
						if (onReactionLongPress) {
							return (
								<LongPressable
									key={getReactionKey(messageId, reaction.emoji)}
									className={itemClassName}
									onLongPress={() => onReactionLongPress(reaction)}
									data-flx="app.message-reactions-content.message-reactions-filters.long-pressable"
								>
									{button}
								</LongPressable>
							);
						}
						return (
							<div
								key={getReactionKey(messageId, reaction.emoji)}
								className={itemClassName}
								data-flx="app.message-reactions-content.message-reactions-filters.div--2"
							>
								{button}
							</div>
						);
					})}
				</div>
			</Scroller>
		);
	},
);

interface ReactorListItemProps {
	channelId: string;
	reactor: User;
	canManageMessages: boolean;
	currentUserId: string | null;
	guildId?: string;
	avatarSize: number;
	isFirst: boolean;
	onRemoveReactor?: (reactor: User) => void;
	onReactorLongPress?: (reactor: User) => void;
}

const ReactorListItem = observer(
	({
		channelId,
		reactor,
		canManageMessages,
		currentUserId,
		guildId,
		avatarSize,
		isFirst,
		onRemoveReactor,
		onReactorLongPress,
	}: ReactorListItemProps) => {
		const {i18n} = useLingui();
		const handleRemove = useCallback(() => {
			onRemoveReactor?.(reactor);
		}, [onRemoveReactor, reactor]);
		const isOwnReaction = currentUserId != null && reactor.id === currentUserId;
		const showRemoveButton = Boolean(onRemoveReactor) && (canManageMessages || isOwnReaction);
		const itemClassName = clsx(styles.reactorItem, !isFirst && styles.reactorItemBorder);
		const displayName = NicknameUtils.getNickname(reactor, guildId, channelId);
		const content = (
			<>
				<Avatar
					user={reactor}
					size={avatarSize}
					guildId={guildId}
					data-flx="app.message-reactions-content.reactor-list-item.avatar"
				/>
				<div className={styles.reactorInfo} data-flx="app.message-reactions-content.reactor-list-item.reactor-info">
					<span className={styles.reactorName} data-flx="app.message-reactions-content.reactor-list-item.reactor-name">
						{displayName}
					</span>
					<span className={styles.reactorTag} data-flx="app.message-reactions-content.reactor-list-item.reactor-tag">
						{reactor.tag}
					</span>
				</div>
				{showRemoveButton && (
					<FocusRing offset={-2} data-flx="app.message-reactions-content.reactor-list-item.focus-ring">
						<button
							type="button"
							onClick={handleRemove}
							className={styles.removeReactionButton}
							aria-label={i18n._(REMOVE_REACTION_FROM_DESCRIPTOR, {displayName})}
							data-flx="app.message-reactions-content.reactor-list-item.remove-reaction-button"
						>
							<XIcon
								weight="bold"
								className={styles.removeReactionIcon}
								data-flx="app.message-reactions-content.reactor-list-item.remove-reaction-icon"
							/>
						</button>
					</FocusRing>
				)}
			</>
		);
		if (onReactorLongPress) {
			return (
				<LongPressable
					className={itemClassName}
					onLongPress={() => onReactorLongPress(reactor)}
					data-user-id={reactor.id}
					data-channel-id={channelId}
					data-flx="app.message-reactions-content.reactor-list-item.long-pressable"
				>
					{content}
				</LongPressable>
			);
		}
		return (
			<div
				className={itemClassName}
				data-user-id={reactor.id}
				data-channel-id={channelId}
				data-flx="app.message-reactions-content.reactor-list-item.div"
			>
				{content}
			</div>
		);
	},
);

interface MessageReactionsReactorsListProps {
	channelId: string;
	reactors: ReadonlyArray<User>;
	isLoading: boolean;
	hasMore?: boolean;
	onLoadMore?: () => void;
	canManageMessages: boolean;
	currentUserId: string | null;
	guildId?: string;
	avatarSize?: number;
	scrollerKey: string;
	loadingLabel?: string;
	emptyLabel?: string;
	showLoadingLabel?: boolean;
	showScrollerTrack?: boolean;
	onRemoveReactor?: (reactor: User) => void;
	onReactorLongPress?: (reactor: User) => void;
}

export const MessageReactionsReactorsList = observer(
	({
		channelId,
		reactors,
		isLoading,
		hasMore = false,
		onLoadMore,
		canManageMessages,
		currentUserId,
		guildId,
		avatarSize = 24,
		scrollerKey,
		loadingLabel,
		emptyLabel,
		showLoadingLabel = false,
		showScrollerTrack = true,
		onRemoveReactor,
		onReactorLongPress,
	}: MessageReactionsReactorsListProps) => {
		const loadingLabelClassName = showLoadingLabel ? styles.loadingLabel : styles.srOnly;
		const handleScroll = useCallback(
			(event: UIEvent<HTMLDivElement>) => {
				if (!onLoadMore || !hasMore || isLoading) {
					return;
				}
				const target = event.currentTarget;
				const scrollPercentage = (target.scrollTop + target.offsetHeight) / target.scrollHeight;
				if (scrollPercentage > 0.8) {
					onLoadMore();
				}
			},
			[hasMore, isLoading, onLoadMore],
		);
		return (
			<div
				className={styles.reactionListPanel}
				data-flx="app.message-reactions-content.message-reactions-reactors-list.reaction-list-panel"
			>
				<Scroller
					className={styles.reactorScroller}
					key={scrollerKey}
					showTrack={showScrollerTrack}
					onScroll={onLoadMore ? handleScroll : undefined}
					data-flx="app.message-reactions-content.message-reactions-reactors-list.reactor-scroller"
				>
					{reactors.map((reactor, index) => (
						<ReactorListItem
							key={reactor.id}
							channelId={channelId}
							reactor={reactor}
							canManageMessages={canManageMessages}
							currentUserId={currentUserId}
							guildId={guildId}
							avatarSize={avatarSize}
							isFirst={index === 0}
							onRemoveReactor={onRemoveReactor}
							onReactorLongPress={onReactorLongPress}
							data-flx="app.message-reactions-content.message-reactions-reactors-list.reactor-list-item"
						/>
					))}
					{isLoading && reactors.length === 0 && (
						<div
							className={styles.loadingContainer}
							data-flx="app.message-reactions-content.message-reactions-reactors-list.loading-container"
						>
							<Spinner size="medium" data-flx="app.message-reactions-content.message-reactions-reactors-list.spinner" />
							{loadingLabel && (
								<span
									className={loadingLabelClassName}
									data-flx="app.message-reactions-content.message-reactions-reactors-list.span"
								>
									{loadingLabel}
								</span>
							)}
						</div>
					)}
					{isLoading && reactors.length > 0 && (
						<div
							className={styles.loadingContainer}
							data-flx="app.message-reactions-content.message-reactions-reactors-list.loading-container--2"
						>
							<Spinner
								size="small"
								data-flx="app.message-reactions-content.message-reactions-reactors-list.spinner--2"
							/>
						</div>
					)}
					{!isLoading && reactors.length === 0 && emptyLabel && (
						<div
							className={styles.emptyState}
							data-flx="app.message-reactions-content.message-reactions-reactors-list.empty-state"
						>
							<span
								className={styles.emptyStateText}
								data-flx="app.message-reactions-content.message-reactions-reactors-list.empty-state-text"
							>
								{emptyLabel}
							</span>
						</div>
					)}
				</Scroller>
			</div>
		);
	},
);
