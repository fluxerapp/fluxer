// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import styles from '@app/features/channel/components/MessageActionBar.module.css';
import {useShiftKey} from '@app/features/channel/components/MessageActionBarShiftKey';
import {
	createMessageActionHandlers,
	isClientSystemMessage,
	useMessagePermissions,
} from '@app/features/channel/components/MessageActionUtils';
import {REACT_WITH_EMOJI_DESCRIPTOR} from '@app/features/channel/components/QuickReactionsRow';
import type {Channel} from '@app/features/channel/models/Channel';
import {useQuickReactionEmojis} from '@app/features/channel/state/QuickReactionStore';
import {MessageDebugModal} from '@app/features/devtools/components/debug/MessageDebugModal';
import * as EmojiPickerCommands from '@app/features/emoji/commands/EmojiPickerCommands';
import {EmojiPickerPopout} from '@app/features/emoji/components/popouts/EmojiPickerPopout';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {buildCustomEmojiURL} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import {getEmojiDisplayData} from '@app/features/expressions/utils/SkinToneUtils';
import {
	ADD_REACTION_DESCRIPTOR,
	COPY_MESSAGE_ID_DESCRIPTOR,
	COPY_MESSAGE_LINK_DESCRIPTOR,
	DELETE_MESSAGE_DESCRIPTOR,
	EDIT_MESSAGE_DESCRIPTOR,
	MARK_AS_UNREAD_DESCRIPTOR,
	PIN_MESSAGE_DESCRIPTOR,
	REPLY_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
	UNPIN_MESSAGE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {getEmojiNameWithColons, toReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import {
	AddReactionIcon,
	CopyIdIcon,
	CopyLinkIcon,
	DebugMessageIcon,
	DeleteIcon,
	EditMessageIcon,
	ForwardIcon,
	MarkAsUnreadIcon,
	MoreIcon,
	PinIcon,
	ReplyIcon,
	RetryIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MessageContextMenu} from '@app/features/ui/action_menu/MessageContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import ContextMenu from '@app/features/ui/state/ContextMenu';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import UserSettings from '@app/features/user/state/UserSettings';
import {MessageStates} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const CLICK_TO_REACT_DESCRIPTOR = msg({
	message: 'Click to react',
	comment: 'Tooltip on the add-reaction button in the inline message hover action bar.',
});
const MESSAGE_DEBUG_DESCRIPTOR = msg({
	message: 'Message debug',
	comment: 'Title of the developer-mode message debug modal opened from the message action bar.',
});
const MORE_DESCRIPTOR = msg({
	message: 'More',
	comment: 'Tooltip on the overflow button in the inline message hover action bar. Opens the full action menu.',
});
const DEBUG_MESSAGE_DESCRIPTOR = msg({
	message: 'Debug message',
	comment: 'Developer-mode item in the message action bar overflow menu. Opens the message debug modal.',
});
const FORWARD_DESCRIPTOR = msg({
	message: 'Forward',
	comment: 'Tooltip on the forward button in the inline message hover action bar.',
});

interface MessageActionBarButtonProps {
	label: string;
	icon: React.ReactNode;
	onClick?: (event: React.MouseEvent | React.KeyboardEvent) => void;
	onPointerDownCapture?: (event: React.PointerEvent) => void;
	danger?: boolean;
	isActive?: boolean;
	hidden?: boolean;
	dataAction?: string;
}

const MessageActionBarButton = React.forwardRef<HTMLButtonElement, MessageActionBarButtonProps>(
	({label, icon, onClick, onPointerDownCapture, danger, isActive, hidden, dataAction}, ref) => {
		const handleClick = useCallback(
			(event: React.MouseEvent | React.KeyboardEvent) => {
				event.preventDefault();
				event.stopPropagation();
				onClick?.(event);
			},
			[onClick],
		);
		const handlePointerDownCapture = useCallback(
			(event: React.PointerEvent) => {
				onPointerDownCapture?.(event);
			},
			[onPointerDownCapture],
		);
		const buttonClassName = useMemo(
			() => clsx(styles.button, danger && styles.danger, isActive && styles.active),
			[danger, isActive],
		);
		return (
			<Tooltip text={label} data-flx="channel.message-action-bar.message-action-bar-button.tooltip">
				<FocusRing data-flx="channel.message-action-bar.message-action-bar-button.focus-ring">
					<button
						type="button"
						ref={ref}
						aria-label={label}
						hidden={hidden}
						onClick={handleClick}
						onPointerDownCapture={handlePointerDownCapture}
						className={buttonClassName}
						data-action={dataAction}
						data-flx="channel.message-action-bar.message-action-bar-button.button.click"
					>
						<div
							className={styles.actionBarIcon}
							data-flx="channel.message-action-bar.message-action-bar-button.action-bar-icon"
						>
							{icon}
						</div>
					</button>
				</FocusRing>
			</Tooltip>
		);
	},
);

MessageActionBarButton.displayName = 'MessageActionBarButton';

interface QuickReactionButtonProps {
	emoji: FlatEmoji;
	onReact: (emoji: FlatEmoji) => void;
	hidden?: boolean;
}

export const QuickReactionButton = observer(
	React.forwardRef<HTMLButtonElement, QuickReactionButtonProps>(({emoji, onReact, hidden}, ref) => {
		const {i18n} = useLingui();
		const isAnimatedEmoji = emoji.animated === true;
		const [isHovered, setIsHovered] = useState(false);
		const {url: displayUrl} = getEmojiDisplayData(emoji);
		const handleClick = useCallback(
			(event: React.MouseEvent | React.KeyboardEvent) => {
				event.preventDefault();
				event.stopPropagation();
				EmojiPickerCommands.trackEmojiUsage(emoji);
				onReact(emoji);
			},
			[emoji, onReact],
		);
		const beginEmojiHover = useMemo(() => (isAnimatedEmoji ? () => setIsHovered(true) : undefined), [isAnimatedEmoji]);
		const endEmojiHover = useMemo(() => (isAnimatedEmoji ? () => setIsHovered(false) : undefined), [isAnimatedEmoji]);
		const emojiNameWithColons = useMemo(() => getEmojiNameWithColons(toReactionEmoji(emoji)), [emoji]);
		const shouldShowAnimated = useShouldAnimate({kind: 'emoji', isAnimated: isAnimatedEmoji, isHovering: isHovered});
		const emojiSrc = useMemo(
			() => (emoji.id ? buildCustomEmojiURL({id: emoji.id, animated: shouldShowAnimated}) : (displayUrl ?? '')),
			[emoji.id, displayUrl, shouldShowAnimated],
		);
		const tooltipContent = useCallback(
			() => (
				<div className={styles.tooltipContent} data-flx="channel.message-action-bar.tooltip-content.tooltip-content">
					<span data-flx="channel.message-action-bar.tooltip-content.span">{emojiNameWithColons}</span>
					<span className={styles.tooltipHint} data-flx="channel.message-action-bar.tooltip-content.tooltip-hint">
						{i18n._(CLICK_TO_REACT_DESCRIPTOR)}
					</span>
				</div>
			),
			[emojiNameWithColons, i18n],
		);
		const ariaLabel = useMemo(
			() => i18n._(REACT_WITH_EMOJI_DESCRIPTOR, {emojiShortcode: emojiNameWithColons}),
			[emojiNameWithColons, i18n.locale],
		);
		return (
			<Tooltip text={tooltipContent} data-flx="channel.message-action-bar.tooltip">
				<FocusRing data-flx="channel.message-action-bar.focus-ring">
					<button
						type="button"
						ref={ref}
						aria-label={ariaLabel}
						hidden={hidden}
						onClick={handleClick}
						onMouseEnter={beginEmojiHover}
						onMouseLeave={endEmojiHover}
						onFocus={beginEmojiHover}
						onBlur={endEmojiHover}
						className={styles.button}
						data-flx="channel.message-action-bar.button.click"
					>
						<img
							src={emojiSrc}
							alt={emoji.name}
							aria-hidden={true}
							className={styles.emojiImage}
							data-flx="channel.message-action-bar.emoji-image"
						/>
					</button>
				</FocusRing>
			</Tooltip>
		);
	}),
);

QuickReactionButton.displayName = 'QuickReactionButton';

interface MessageActionBarCoreProps {
	message: Message;
	handleDelete: (bypassConfirm?: boolean) => void;
	permissions: {
		channel: Channel;
		canSendMessages: boolean;
		canAddReactions: boolean;
		canEditMessage: boolean;
		canDeleteMessage: boolean;
		canPinMessage: boolean;
		canForwardMessage: boolean;
		shouldRenderSuppressEmbeds: boolean;
	};
	developerMode: boolean;
	isActive: boolean;
	onPopoutToggle?: (isOpen: boolean) => void;
}

export const MessageActionBarCore: React.FC<MessageActionBarCoreProps> = observer(
	({message, handleDelete, permissions, developerMode, isActive, onPopoutToggle}) => {
		const {i18n} = useLingui();
		const [emojiPickerOpen, setEmojiPickerOpen] = useState(false);
		const moreOptionsButtonRef = useRef<HTMLButtonElement>(null);
		const emojiPickerButtonRef = useRef<HTMLButtonElement>(null);
		const actionBarRef = useRef<HTMLDivElement>(null);
		const keepOpenOnNextMoreMenuCloseRef = useRef(false);
		const emojiPickerOpenRef = useRef(false);
		const contextMenuOpen = useContextMenuHoverState(actionBarRef);
		const moreMenuOpen = useContextMenuHoverState(moreOptionsButtonRef);
		const showMessageActionBar = Accessibility.showMessageActionBar;
		const showQuickReactions = Accessibility.showMessageActionBarQuickReactions;
		const showShiftExpand = Accessibility.showMessageActionBarShiftExpand;
		const onlyMoreButton = Accessibility.showMessageActionBarOnlyMoreButton;
		const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
		const shouldListenForShift = showShiftExpand && showMessageActionBar && !onlyMoreButton && !keyboardModeEnabled;
		const shiftPressed = useShiftKey(shouldListenForShift);
		const showFullActions = shouldListenForShift && shiftPressed;
		const {canSendMessages, canAddReactions, canEditMessage, canDeleteMessage, canPinMessage, canForwardMessage} =
			permissions;
		const showsEditInTail = message.isUserMessage() && !message.messageSnapshots && canEditMessage;
		const supportsInteractiveActions = useMemo(() => !isClientSystemMessage(message), [message]);
		const handlers = useMemo(
			() => createMessageActionHandlers(message, {i18n, channel: permissions.channel}),
			[message, i18n.locale, permissions.channel],
		);
		const channel = permissions.channel;
		const quickReactionEmojis = useQuickReactionEmojis(
			channel,
			3,
			canAddReactions && showQuickReactions && message.state === MessageStates.SENT,
		);
		const blurEmojiPickerTrigger = useCallback(() => {
			if (keyboardModeEnabled) {
				return;
			}
			requestAnimationFrame(() => emojiPickerButtonRef.current?.blur());
		}, [keyboardModeEnabled]);
		useEffect(() => {
			if (isActive || keyboardModeEnabled) {
				return;
			}
			const container = actionBarRef.current;
			if (container == null) {
				return;
			}
			const focused = container.ownerDocument.activeElement;
			if (!(focused instanceof HTMLElement) || !container.contains(focused) || focused.matches(':focus-visible')) {
				return;
			}
			focused.blur();
		}, [isActive, keyboardModeEnabled]);
		const handleEmojiPickerToggle = useCallback(
			(open: boolean) => {
				emojiPickerOpenRef.current = open;
				setEmojiPickerOpen(open);
				onPopoutToggle?.(open);
				if (!open) {
					blurEmojiPickerTrigger();
				}
			},
			[onPopoutToggle, blurEmojiPickerTrigger],
		);
		const handleEmojiPickerOpen = useCallback(() => handleEmojiPickerToggle(true), [handleEmojiPickerToggle]);
		const handleEmojiPickerClose = useCallback(() => handleEmojiPickerToggle(false), [handleEmojiPickerToggle]);
		const handleMoreMenuOpenReactionPicker = useCallback(() => {
			keepOpenOnNextMoreMenuCloseRef.current = true;
		}, []);
		const handleMoreMenuClose = useCallback(
			(closeMenu: () => void) => {
				if (!keepOpenOnNextMoreMenuCloseRef.current || !emojiPickerOpenRef.current) {
					onPopoutToggle?.(false);
				}
				closeMenu();
			},
			[onPopoutToggle],
		);
		const handleMoreMenuClosed = useCallback(() => {
			const handedOffToReactionPicker = keepOpenOnNextMoreMenuCloseRef.current;
			keepOpenOnNextMoreMenuCloseRef.current = false;
			if (handedOffToReactionPicker && emojiPickerOpenRef.current) {
				return;
			}
			onPopoutToggle?.(false);
		}, [onPopoutToggle]);
		useEffect(() => {
			return () => {
				if (emojiPickerOpen) {
					onPopoutToggle?.(false);
				}
			};
		}, [emojiPickerOpen, onPopoutToggle]);
		const handleDebugClick = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<MessageDebugModal
						title={i18n._(MESSAGE_DEBUG_DESCRIPTOR)}
						message={message}
						data-flx="channel.message-action-bar.handle-debug-click.message-debug-modal"
					/>
				)),
			);
		}, [message, i18n]);
		useEffect(() => {
			const unsubscribe = ComponentBus.subscribe('EMOJI_PICKER_OPEN', (payload?: unknown) => {
				const data = (payload ?? {}) as {messageId?: string};
				if (data.messageId === message.id && emojiPickerButtonRef.current) {
					PopoutCommands.open({
						key: `emoji_picker-${message.id}`,
						position: 'left-start',
						render: ({onClose}) => (
							<EmojiPickerPopout
								channelId={message.channelId}
								handleSelect={handlers.handleEmojiSelect}
								onClose={onClose}
								data-flx="channel.message-action-bar.unsubscribe.emoji-picker-popout"
							/>
						),
						target: emojiPickerButtonRef.current,
						shouldAutoUpdate: false,
						animationType: 'none',
						onOpen: () => handleEmojiPickerToggle(true),
						onClose: () => handleEmojiPickerToggle(false),
					});
				}
			});
			return () => unsubscribe();
		}, [message.id, message.channelId, handlers.handleEmojiSelect, handleEmojiPickerToggle]);
		const handleMoreOptionsPointerDown = useCallback(
			(event: React.PointerEvent) => {
				const contextMenu = ContextMenu.contextMenu;
				const isOpen = !!contextMenu && contextMenu.target.target === moreOptionsButtonRef.current;
				if (isOpen) {
					event.stopPropagation();
					event.preventDefault();
					ContextMenuCommands.close();
					onPopoutToggle?.(false);
				}
			},
			[onPopoutToggle],
		);
		useEffect(() => {
			if (!showFullActions || !moreMenuOpen) {
				return;
			}
			const contextMenu = ContextMenu.contextMenu;
			if (contextMenu?.target.target === moreOptionsButtonRef.current) {
				ContextMenuCommands.close();
			}
		}, [moreMenuOpen, showFullActions]);
		const openMoreOptionsMenu = useCallback(
			(event: React.MouseEvent | React.KeyboardEvent) => {
				if (!showMessageActionBar || showFullActions) {
					return;
				}
				const contextMenu = ContextMenu.contextMenu;
				const isOpen = !!contextMenu && contextMenu.target.target === event.currentTarget;
				if (isOpen) {
					return;
				}
				onPopoutToggle?.(true);
				ContextMenuCommands.openFromElementLeftStart(
					event,
					(props) => (
						<MessageContextMenu
							message={message}
							sourceChannel={channel}
							onClose={() => {
								handleMoreMenuClose(props.onClose);
							}}
							onOpenReactionPicker={handleMoreMenuOpenReactionPicker}
							onDelete={handleDelete}
							data-flx="channel.message-action-bar.open-more-options-menu.message-context-menu"
						/>
					),
					{
						config: {
							onClose: handleMoreMenuClosed,
						},
					},
				);
			},
			[
				channel,
				handleDelete,
				handleMoreMenuClose,
				handleMoreMenuClosed,
				handleMoreMenuOpenReactionPicker,
				message,
				showFullActions,
				showMessageActionBar,
			],
		);
		return (
			<div
				ref={actionBarRef}
				className={clsx(
					styles.actionBarContainer,
					messageStyles.buttons,
					(emojiPickerOpen || contextMenuOpen) && messageStyles.emojiPickerOpen,
					(emojiPickerOpen || contextMenuOpen || moreMenuOpen) && styles.actionBarPinned,
				)}
				data-flx="channel.message-action-bar.message-action-bar-core.action-bar-container"
			>
				<div className={styles.actionBar} data-flx="channel.message-action-bar.message-action-bar-core.action-bar">
					{message.state === MessageStates.SENT &&
						(onlyMoreButton ? (
							<MessageActionBarButton
								ref={moreOptionsButtonRef}
								icon={<MoreIcon size={20} data-flx="channel.message-action-bar.message-action-bar-core.more-icon" />}
								label={i18n._(MORE_DESCRIPTOR)}
								onPointerDownCapture={handleMoreOptionsPointerDown}
								onClick={openMoreOptionsMenu}
								isActive={moreMenuOpen}
								data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.open-more-options-menu"
							/>
						) : (
							<>
								{canAddReactions &&
									showQuickReactions &&
									quickReactionEmojis.map((emoji) => (
										<QuickReactionButton
											key={emoji.id ?? emoji.uniqueName}
											emoji={emoji}
											hidden={showFullActions}
											onReact={handlers.handleEmojiSelect}
											data-flx="channel.message-action-bar.message-action-bar-core.quick-reaction-button"
										/>
									))}
								{showFullActions && (
									<>
										{developerMode && (
											<MessageActionBarButton
												icon={
													<DebugMessageIcon
														size={20}
														data-flx="channel.message-action-bar.message-action-bar-core.debug-message-icon"
													/>
												}
												label={i18n._(DEBUG_MESSAGE_DESCRIPTOR)}
												onClick={handleDebugClick}
												data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.debug-click"
											/>
										)}
										<MessageActionBarButton
											icon={
												<CopyIdIcon
													size={20}
													data-flx="channel.message-action-bar.message-action-bar-core.copy-id-icon"
												/>
											}
											label={i18n._(COPY_MESSAGE_ID_DESCRIPTOR)}
											onClick={handlers.handleCopyMessageId}
											data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.copy-message-id"
										/>
										{supportsInteractiveActions && (
											<MessageActionBarButton
												icon={
													<CopyLinkIcon
														size={20}
														data-flx="channel.message-action-bar.message-action-bar-core.copy-link-icon"
													/>
												}
												label={i18n._(COPY_MESSAGE_LINK_DESCRIPTOR)}
												onClick={handlers.handleCopyMessageLink}
												data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.copy-message-link"
											/>
										)}
										<MessageActionBarButton
											icon={
												<MarkAsUnreadIcon
													size={20}
													data-flx="channel.message-action-bar.message-action-bar-core.mark-as-unread-icon"
												/>
											}
											label={i18n._(MARK_AS_UNREAD_DESCRIPTOR)}
											onClick={handlers.handleMarkAsUnread}
											data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.mark-as-unread"
										/>
										{message.isUserMessage() && canPinMessage && (
											<MessageActionBarButton
												icon={
													<PinIcon size={20} data-flx="channel.message-action-bar.message-action-bar-core.pin-icon" />
												}
												label={message.pinned ? i18n._(UNPIN_MESSAGE_DESCRIPTOR) : i18n._(PIN_MESSAGE_DESCRIPTOR)}
												onClick={handlers.handlePinMessage}
												data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.pin-message"
											/>
										)}
										{showsEditInTail && supportsInteractiveActions && canSendMessages && (
											<MessageActionBarButton
												icon={
													<ReplyIcon
														size={20}
														data-flx="channel.message-action-bar.message-action-bar-core.reply-icon--2"
													/>
												}
												label={i18n._(REPLY_DESCRIPTOR)}
												onClick={handlers.handleReply}
												data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.reply--2"
											/>
										)}
									</>
								)}
								{canAddReactions && (
									<Popout
										render={({onClose}) => (
											<EmojiPickerPopout
												channelId={message.channelId}
												handleSelect={handlers.handleEmojiSelect}
												onClose={onClose}
												data-flx="channel.message-action-bar.message-action-bar-core.emoji-picker-popout"
											/>
										)}
										position="left-start"
										uniqueId={`emoji_picker-actionbar-${message.id}`}
										shouldAutoUpdate={false}
										animationType="none"
										onOpen={handleEmojiPickerOpen}
										onClose={handleEmojiPickerClose}
										data-flx="channel.message-action-bar.message-action-bar-core.popout"
									>
										<MessageActionBarButton
											ref={emojiPickerButtonRef}
											icon={
												<AddReactionIcon
													size={20}
													data-flx="channel.message-action-bar.message-action-bar-core.add-reaction-icon"
												/>
											}
											label={i18n._(ADD_REACTION_DESCRIPTOR)}
											isActive={emojiPickerOpen}
											dataAction="message-add-reaction-button"
											data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button"
										/>
									</Popout>
								)}
								{showsEditInTail && (
									<MessageActionBarButton
										icon={
											<EditMessageIcon
												size={20}
												data-flx="channel.message-action-bar.message-action-bar-core.edit-message-icon"
											/>
										}
										label={i18n._(EDIT_MESSAGE_DESCRIPTOR)}
										onClick={handlers.handleEditMessage}
										data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.edit-message"
									/>
								)}
								{message.isUserMessage() && supportsInteractiveActions && canSendMessages && !showsEditInTail && (
									<MessageActionBarButton
										icon={
											<ReplyIcon size={20} data-flx="channel.message-action-bar.message-action-bar-core.reply-icon" />
										}
										label={i18n._(REPLY_DESCRIPTOR)}
										onClick={handlers.handleReply}
										data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.reply"
									/>
								)}
								{message.isUserMessage() && supportsInteractiveActions && canForwardMessage && (
									<MessageActionBarButton
										icon={
											<ForwardIcon
												size={20}
												data-flx="channel.message-action-bar.message-action-bar-core.forward-icon"
											/>
										}
										label={i18n._(FORWARD_DESCRIPTOR)}
										onClick={handlers.handleForward}
										data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.forward"
									/>
								)}
								{showFullActions && canDeleteMessage && (
									<MessageActionBarButton
										danger={true}
										icon={
											<DeleteIcon size={20} data-flx="channel.message-action-bar.message-action-bar-core.delete-icon" />
										}
										label={i18n._(DELETE_MESSAGE_DESCRIPTOR)}
										onClick={(event) => handleDelete(event.shiftKey)}
										data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.delete"
									/>
								)}
								<MessageActionBarButton
									ref={moreOptionsButtonRef}
									icon={
										<MoreIcon size={20} data-flx="channel.message-action-bar.message-action-bar-core.more-icon--2" />
									}
									label={i18n._(MORE_DESCRIPTOR)}
									hidden={showFullActions && canDeleteMessage}
									onPointerDownCapture={handleMoreOptionsPointerDown}
									onClick={openMoreOptionsMenu}
									isActive={moreMenuOpen}
									data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.open-more-options-menu--2"
								/>
							</>
						))}
					{message.state === MessageStates.FAILED && (
						<>
							<MessageActionBarButton
								icon={<RetryIcon size={20} data-flx="channel.message-action-bar.message-action-bar-core.retry-icon" />}
								label={i18n._(TRY_AGAIN_DESCRIPTOR)}
								onClick={handlers.handleRetryMessage}
								data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.retry-message"
							/>
							<MessageActionBarButton
								danger={true}
								icon={
									<DeleteIcon size={20} data-flx="channel.message-action-bar.message-action-bar-core.delete-icon--2" />
								}
								label={i18n._(DELETE_MESSAGE_DESCRIPTOR)}
								onClick={handlers.handleFailedMessageDelete}
								data-flx="channel.message-action-bar.message-action-bar-core.message-action-bar-button.failed-message-delete"
							/>
						</>
					)}
				</div>
			</div>
		);
	},
);
export const MessageActionBar = observer(
	({
		message,
		handleDelete,
		sourceChannel,
		isActive,
		onPopoutToggle,
	}: {
		message: Message;
		handleDelete: (bypassConfirm?: boolean) => void;
		sourceChannel?: Channel | null;
		isActive: boolean;
		onPopoutToggle?: (isOpen: boolean) => void;
	}) => {
		const developerMode = UserSettings.developerMode;
		const permissions = useMessagePermissions(message, sourceChannel);
		if (!permissions) {
			return null;
		}
		return (
			<MessageActionBarCore
				message={message}
				handleDelete={handleDelete}
				permissions={permissions}
				developerMode={developerMode}
				isActive={isActive}
				onPopoutToggle={onPopoutToggle}
				data-flx="channel.message-action-bar.message-action-bar-core"
			/>
		);
	},
);
