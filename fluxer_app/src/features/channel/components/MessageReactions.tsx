// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {LongPressable} from '@app/features/app/components/LongPressable';
import {useHover} from '@app/features/app/hooks/useHover';
import {
	createMessageActionHandlers,
	isClientSystemMessage,
	useMessagePermissions,
} from '@app/features/channel/components/MessageActionUtils';
import styles from '@app/features/channel/components/MessageReactions.module.css';
import {EmojiInfoBottomSheet} from '@app/features/emoji/components/bottomsheets/EmojiInfoBottomSheet';
import {EmojiPickerPopout} from '@app/features/emoji/components/popouts/EmojiPickerPopout';
import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionPickerSheet} from '@app/features/expressions/components/modals/ExpressionPickerSheet';
import {ADD_REACTION_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ReactionCommands from '@app/features/messaging/commands/ReactionCommands';
import {ReactionTooltip} from '@app/features/messaging/components/popouts/ReactionTooltip';
import {ReactionImage} from '@app/features/messaging/components/ReactionImage';
import {useMatureMedia} from '@app/features/messaging/hooks/useMatureMedia';
import {useMessageReactions as useMessageReactionsSnapshot} from '@app/features/messaging/hooks/useMessageReactionStore';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {getEmojiName, getReactionKey, useEmojiURL} from '@app/features/messaging/utils/ReactionUtils';
import matureStyles from '@app/features/theme/styles/MatureBlur.module.css';
import {EmojiContextMenuItems} from '@app/features/ui/action_menu/items/EmojiContextMenuItems';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import type {MessageReaction} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {SmileyIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

const PRESS_TO_REMOVE_REACTION_DESCRIPTOR = msg({
	message: 'press to remove reaction',
	comment: 'Label in the channel and chat message reactions. Keep the tone plain and specific.',
});
const PRESS_TO_ADD_REACTION_DESCRIPTOR = msg({
	message: 'press to add reaction',
	comment: 'Label in the channel and chat message reactions.',
});
const MESSAGE_DESCRIPTOR = msg({
	message: '{emojiName}: {reactionCountText}',
	comment:
		'Short label in the channel and chat message reactions. Keep it concise. Preserve {emojiName}, {reactionCountText}; they are inserted by code.',
});
const MESSAGE_2_DESCRIPTOR = msg({
	message: '{emojiName}: {reactionCountText}, {actionText}',
	comment:
		'Short label in the channel and chat message reactions. Keep it concise. Preserve {emojiName}, {reactionCountText}, {actionText}; they are inserted by code.',
});

interface EmojiInfoData {
	id?: string;
	name: string;
	animated?: boolean;
}

const MessageReactionItem = observer(
	({
		message,
		reaction,
		isPreview = false,
		disableInteraction = false,
	}: {
		message: Message;
		reaction: MessageReaction;
		isPreview?: boolean;
		disableInteraction?: boolean;
	}) => {
		const {i18n} = useLingui();
		const [hoverRef, isHovering] = useHover();
		const [prevCount, setPrevCount] = useState(reaction.count);
		const [animationSyncKey, setAnimationSyncKey] = useState(0);
		const [emojiInfoOpen, setEmojiInfoOpen] = useState(false);
		const [selectedEmoji, setSelectedEmoji] = useState<EmojiInfoData | null>(null);
		const [tooltipHovering, setTooltipHovering] = useState(false);
		const isMobile = MobileLayout.isMobileLayout();
		const handleTooltipAnimationSync = useCallback(() => {
			setAnimationSyncKey((prev) => prev + 1);
		}, []);
		useEffect(() => {
			if (prevCount !== reaction.count) {
				setPrevCount(reaction.count);
			}
		}, [reaction.count, prevCount]);
		const isDisabled = isPreview || disableInteraction;
		const handleClick = () => {
			if (isDisabled) {
				return;
			}
			if (reaction.me) {
				ReactionCommands.removeReaction(i18n, message.channelId, message.id, reaction.emoji);
			} else {
				ReactionCommands.addReaction(i18n, message.channelId, message.id, reaction.emoji);
			}
		};
		const handleLongPress = () => {
			if (isDisabled) {
				return;
			}
			setSelectedEmoji({
				id: reaction.emoji.id ?? undefined,
				name: reaction.emoji.name,
				animated: reaction.emoji.animated,
			});
			setEmojiInfoOpen(true);
		};
		const handleCloseEmojiInfo = useCallback(() => {
			setEmojiInfoOpen(false);
			setSelectedEmoji(null);
		}, []);
		const emojiRecord = reaction.emoji.id ? Emoji.getEmojiById(reaction.emoji.id) : null;
		const handleContextMenu = (e: React.MouseEvent) => {
			if (isDisabled) return;
			if (!reaction.emoji.id) return;
			e.preventDefault();
			e.stopPropagation();
			const emojiForMenu: FlatEmoji = emojiRecord ?? {
				id: reaction.emoji.id,
				name: reaction.emoji.name,
				uniqueName: reaction.emoji.name,
				allNamesString: `:${reaction.emoji.name}:`,
				animated: Boolean(reaction.emoji.animated),
			};
			ContextMenuCommands.openFromEvent(e, ({onClose}) => (
				<EmojiContextMenuItems
					emoji={emojiForMenu}
					onClose={onClose}
					data-flx="channel.message-reactions.handle-context-menu.emoji-context-menu-items"
				/>
			));
		};
		const emojiName = getEmojiName(reaction.emoji);
		const emojiUrl = useEmojiURL({emoji: reaction.emoji, isHovering: isHovering || tooltipHovering});
		const isMature = !!emojiRecord?.nsfw;
		const {shouldBlur: reactionShouldBlur, shouldBlock: reactionShouldBlock} = useMatureMedia(
			isMature,
			message.channelId,
		);
		const variants = {
			up: {y: -20, opacity: 0},
			down: {y: 20, opacity: 0},
			center: {y: 0, opacity: 1},
		};
		const reactionCountText = plural(
			{count: reaction.count},
			{
				one: '# reaction',
				other: '# reactions',
			},
		);
		const actionText = reaction.me
			? i18n._(PRESS_TO_REMOVE_REACTION_DESCRIPTOR)
			: i18n._(PRESS_TO_ADD_REACTION_DESCRIPTOR);
		const ariaLabel = isDisabled
			? i18n._(MESSAGE_DESCRIPTOR, {emojiName, reactionCountText})
			: i18n._(MESSAGE_2_DESCRIPTOR, {emojiName, reactionCountText, actionText});
		const buttonContent = (
			<FocusRing offset={-2} data-flx="channel.message-reactions.message-reaction-item.focus-ring">
				<button
					type="button"
					className={clsx(styles.reactionButton, isDisabled && styles.reactionButtonDisabled)}
					aria-label={ariaLabel}
					aria-pressed={isDisabled ? undefined : reaction.me}
					aria-disabled={isDisabled || undefined}
					disabled={disableInteraction}
					tabIndex={isDisabled ? -1 : undefined}
					onClick={handleClick}
					onContextMenu={handleContextMenu}
					data-flx="channel.message-reactions.message-reaction-item.reaction-button.click"
				>
					<div
						className={styles.reactionInner}
						data-flx="channel.message-reactions.message-reaction-item.reaction-inner"
					>
						{emojiUrl ? (
							reactionShouldBlock ? null : (
								<ReactionImage
									src={emojiUrl}
									alt={emojiName}
									draggable={false}
									className={clsx('emoji', styles.emoji, reactionShouldBlur && matureStyles.matureBlurred)}
									data-flx="channel.message-reactions.message-reaction-item.emoji"
								/>
							)
						) : null}
						<div
							className={styles.countWrapper}
							data-flx="channel.message-reactions.message-reaction-item.count-wrapper"
						>
							<AnimatePresence
								initial={false}
								data-flx="channel.message-reactions.message-reaction-item.animate-presence"
							>
								<motion.div
									key={reaction.count}
									initial={reaction.count > prevCount ? 'up' : 'down'}
									animate="center"
									exit={reaction.count > prevCount ? 'down' : 'up'}
									variants={variants}
									transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2}}
									data-flx="channel.message-reactions.message-reaction-item.div"
								>
									{reaction.count}
								</motion.div>
							</AnimatePresence>
						</div>
					</div>
				</button>
			</FocusRing>
		);
		const containerClassName = clsx(
			styles.reactionContainer,
			reaction.me && styles.reactionMe,
			isDisabled && styles.reactionContainerDisabled,
		);
		if (isDisabled) {
			return (
				<div className={containerClassName} data-flx="channel.message-reactions.message-reaction-item.div--2">
					{buttonContent}
				</div>
			);
		}
		if (isMobile) {
			return (
				<LongPressable
					className={containerClassName}
					onLongPress={handleLongPress}
					data-flx="channel.message-reactions.message-reaction-item.long-pressable"
				>
					{buttonContent}
					<EmojiInfoBottomSheet
						isOpen={emojiInfoOpen}
						onClose={handleCloseEmojiInfo}
						emoji={selectedEmoji}
						data-flx="channel.message-reactions.message-reaction-item.emoji-info-bottom-sheet"
					/>
				</LongPressable>
			);
		}
		return (
			<ReactionTooltip
				message={message}
				reaction={reaction}
				hoveredEmojiUrl={emojiUrl}
				animationSyncKey={animationSyncKey}
				onRequestAnimationSync={handleTooltipAnimationSync}
				onTooltipHoverChange={setTooltipHovering}
				data-flx="channel.message-reactions.message-reaction-item.reaction-tooltip"
			>
				<div
					className={containerClassName}
					ref={hoverRef}
					data-flx="channel.message-reactions.message-reaction-item.div--3"
				>
					{buttonContent}
				</div>
			</ReactionTooltip>
		);
	},
);
export const MessageReactions = observer(
	({
		message,
		isPreview = false,
		onPopoutToggle,
	}: {
		message: Message;
		isPreview?: boolean;
		onPopoutToggle?: (isOpen: boolean) => void;
	}) => {
		const {i18n} = useLingui();
		const [emojiPickerOpen, setEmojiPickerOpen] = useState(false);
		const addReactionButtonRef = useRef<HTMLButtonElement>(null);
		const permissions = useMessagePermissions(message);
		const handlers = createMessageActionHandlers(message, {i18n, channel: permissions?.channel});
		const reactions = useMessageReactionsSnapshot(message.id);
		const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
		const isMobileLayout = MobileLayout.isMobileLayout();
		const disableReactionInteraction = isClientSystemMessage(message);
		const blurReactionTrigger = useCallback(() => {
			if (keyboardModeEnabled) {
				return;
			}
			requestAnimationFrame(() => addReactionButtonRef.current?.blur());
		}, [keyboardModeEnabled]);
		const handleEmojiPickerToggle = useCallback(
			(open: boolean) => {
				setEmojiPickerOpen(open);
				onPopoutToggle?.(open);
				if (!open) {
					blurReactionTrigger();
				}
			},
			[onPopoutToggle, blurReactionTrigger],
		);
		const handleEmojiPickerOpen = useCallback(() => handleEmojiPickerToggle(true), [handleEmojiPickerToggle]);
		const handleEmojiPickerClose = useCallback(() => handleEmojiPickerToggle(false), [handleEmojiPickerToggle]);
		useEffect(() => {
			return () => {
				if (emojiPickerOpen) {
					onPopoutToggle?.(false);
				}
			};
		}, [emojiPickerOpen, onPopoutToggle]);
		const hasReactions = reactions.length > 0;
		return (
			<div className={styles.reactionsGrid} data-flx="channel.message-reactions.reactions-grid">
				{reactions.map((reaction) => (
					<MessageReactionItem
						key={getReactionKey(message.id, reaction.emoji)}
						message={message}
						reaction={reaction}
						isPreview={isPreview}
						disableInteraction={disableReactionInteraction}
						data-flx="channel.message-reactions.message-reaction-item"
					/>
				))}
				{hasReactions &&
					permissions?.canAddReactions &&
					!disableReactionInteraction &&
					!isPreview &&
					(isMobileLayout ? (
						<>
							<FocusRing offset={-2} data-flx="channel.message-reactions.focus-ring">
								<button
									ref={addReactionButtonRef}
									type="button"
									className={clsx(styles.reactionPickerButton, emojiPickerOpen && styles.reactionPickerButtonActive)}
									aria-label={i18n._(ADD_REACTION_DESCRIPTOR)}
									aria-haspopup="dialog"
									aria-expanded={emojiPickerOpen}
									data-action="message-add-reaction-button"
									onClick={handleEmojiPickerOpen}
									data-flx="channel.message-reactions.add-reaction-button.emoji-picker-open"
								>
									<SmileyIcon size={20} weight="fill" data-flx="channel.message-reactions.smiley-icon" />
								</button>
							</FocusRing>
							<ExpressionPickerSheet
								isOpen={emojiPickerOpen}
								onClose={handleEmojiPickerClose}
								channelId={message.channelId}
								onEmojiSelect={handlers.handleEmojiSelect}
								visibleTabs={['emojis']}
								data-flx="channel.message-reactions.expression-picker-sheet"
							/>
						</>
					) : (
						<Popout
							render={({onClose}) => (
								<EmojiPickerPopout
									channelId={message.channelId}
									handleSelect={handlers.handleEmojiSelect}
									onClose={onClose}
									data-flx="channel.message-reactions.emoji-picker-popout"
								/>
							)}
							position="right-start"
							uniqueId={`emoji_picker-reactions-${message.id}`}
							shouldAutoUpdate={false}
							animationType="none"
							onOpen={handleEmojiPickerOpen}
							onClose={handleEmojiPickerClose}
							data-flx="channel.message-reactions.popout"
						>
							<FocusRing offset={-2} data-flx="channel.message-reactions.focus-ring--2">
								<button
									ref={addReactionButtonRef}
									type="button"
									className={clsx(styles.reactionPickerButton, emojiPickerOpen && styles.reactionPickerButtonActive)}
									aria-label={i18n._(ADD_REACTION_DESCRIPTOR)}
									aria-haspopup="dialog"
									aria-expanded={emojiPickerOpen}
									data-action="message-add-reaction-button"
									data-flx="channel.message-reactions.add-reaction-button"
								>
									<SmileyIcon size={20} weight="fill" data-flx="channel.message-reactions.smiley-icon--2" />
								</button>
							</FocusRing>
						</Popout>
					))}
			</div>
		);
	},
);
