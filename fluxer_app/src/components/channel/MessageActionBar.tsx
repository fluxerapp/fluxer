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

import {ArrowsClockwiseIcon, DotsThreeIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {autorun} from 'mobx';
import {observer} from 'mobx-react-lite';
import React, {useSyncExternalStore} from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import {MessageStates} from '~/Constants';
import styles from '~/components/channel/MessageActionBar.module.css';
import {
	createMessageActionHandlers,
	isEmbedsSuppressed,
	triggerAddReaction,
	useMessagePermissions,
} from '~/components/channel/messageActionUtils';
import {MessageDebugModal} from '~/components/debug/MessageDebugModal';
import {EmojiPickerPopout} from '~/components/popouts/EmojiPickerPopout';
import {
	AddReactionIcon,
	BookmarkIcon,
	CopyIdIcon,
	CopyLinkIcon,
	CopyTextIcon,
	DebugIcon,
	DeleteIcon,
	EditIcon,
	ForwardIcon,
	MarkAsUnreadIcon,
	PinIcon,
	ReplyIcon,
	SuppressEmbedsIcon,
} from '~/components/uikit/ContextMenu/ContextMenuIcons';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Popout} from '~/components/uikit/Popout/Popout';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import type {MessageRecord} from '~/records/MessageRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import ContextMenuStore, {isContextMenuNodeTarget} from '~/stores/ContextMenuStore';
import EmojiPickerStore from '~/stores/EmojiPickerStore';
import EmojiStore, {type Emoji} from '~/stores/EmojiStore';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import SavedMessagesStore from '~/stores/SavedMessagesStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import messageStyles from '~/styles/Message.module.css';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {shouldUseNativeEmoji} from '~/utils/EmojiUtils';

const shiftKeyManager = (() => {
	let isShiftPressed = false;
	const listeners = new Set<() => void>();
	let notifyTimeout: NodeJS.Timeout | null = null;

	const scheduleNotify = () => {
		if (notifyTimeout) return;
		notifyTimeout = setTimeout(() => {
			notifyTimeout = null;
			listeners.forEach((listener) => listener());
		}, 0);
	};

	const setShiftPressed = (pressed: boolean) => {
		if (isShiftPressed !== pressed) {
			isShiftPressed = pressed;
			scheduleNotify();
		}
	};

	const handleKeyDown = (event: KeyboardEvent) => {
		if (event.key === 'Shift') {
			setShiftPressed(true);
		}
	};

	const handleKeyUp = (event: KeyboardEvent) => {
		if (event.key === 'Shift') {
			setShiftPressed(false);
		}
	};

	const handleWindowBlur = () => {
		setShiftPressed(false);
	};

	window.addEventListener('keydown', handleKeyDown);
	window.addEventListener('keyup', handleKeyUp);
	window.addEventListener('blur', handleWindowBlur);

	return {
		subscribe: (listener: () => void) => {
			listeners.add(listener);
			return () => listeners.delete(listener);
		},
		getSnapshot: () => isShiftPressed,
		getServerSnapshot: () => false,
	};
})();

const quickReactionManager = (() => {
	let cache: Array<Emoji> | null = null;
	const listeners = new Set<() => void>();

	const recompute = () => {
		const allEmojis = EmojiStore.getAllEmojis(null);
		cache = EmojiPickerStore.getQuickReactionEmojis(allEmojis, 3);
		listeners.forEach((listener) => listener());
	};

	autorun(() => {
		const usage = EmojiPickerStore.emojiUsage;
		for (const key in usage) {
			const entry = usage[key];
			entry?.count;
			entry?.lastUsed;
		}
		recompute();
	});

	return {
		subscribe(listener: () => void) {
			listeners.add(listener);
			return () => listeners.delete(listener);
		},
		getSnapshot(): Array<Emoji> {
			if (!cache) {
				recompute();
			}
			return cache ?? [];
		},
		getServerSnapshot(): Array<Emoji> {
			return [];
		},
	};
})();

const useShiftKey = (enabled: boolean) => {
	const subscribe = React.useCallback(
		(listener: () => void) => {
			if (!enabled) {
				return () => undefined;
			}
			return shiftKeyManager.subscribe(listener);
		},
		[enabled],
	);

	const getSnapshot = React.useCallback(() => {
		return enabled ? shiftKeyManager.getSnapshot() : false;
	}, [enabled]);

	return useSyncExternalStore(subscribe, getSnapshot, shiftKeyManager.getServerSnapshot);
};

interface MessageActionBarButtonProps {
	label: string;
	icon: React.ReactNode;
	onClick?: (event: React.MouseEvent | React.KeyboardEvent) => void;
	onPointerDownCapture?: (event: React.PointerEvent) => void;
	danger?: boolean;
	isActive?: boolean;
	dataAction?: string;
}

const MessageActionBarButton = React.forwardRef<HTMLButtonElement, MessageActionBarButtonProps>(
	({label, icon, onClick, onPointerDownCapture, danger, isActive, dataAction}, ref) => {
		const handleClick = (event: React.MouseEvent | React.KeyboardEvent) => {
			event.preventDefault();
			event.stopPropagation();
			onClick?.(event);
		};

		return (
			<Tooltip text={label}>
				<FocusRing>
					<button
						type="button"
						ref={ref}
						aria-label={label}
						onClick={handleClick}
						onPointerDownCapture={onPointerDownCapture}
						className={clsx(styles.button, danger && styles.danger, isActive && styles.active)}
						data-action={dataAction}
					>
						<div className={styles.actionBarIcon}>{icon}</div>
					</button>
				</FocusRing>
			</Tooltip>
		);
	},
);

MessageActionBarButton.displayName = 'MessageActionBarButton';

interface QuickReactionButtonProps {
	emoji: Emoji;
	onReact: (emoji: Emoji) => void;
}

const QuickReactionButton = React.forwardRef<HTMLButtonElement, QuickReactionButtonProps>(({emoji, onReact}, ref) => {
	const {t} = useLingui();
	const [isHovered, setIsHovered] = React.useState(false);

	const handleClick = (event: React.MouseEvent | React.KeyboardEvent) => {
		event.preventDefault();
		event.stopPropagation();
		onReact(emoji);
	};

	const emojiNameWithColons = `:${emoji.name}:`;
	const isUnicodeEmoji = !emoji.guildId && !emoji.id;
	const useNativeRendering = shouldUseNativeEmoji && isUnicodeEmoji;

	const shouldShowAnimated = emoji.animated && isHovered;
	const emojiSrc =
		emoji.animated && emoji.id && !useNativeRendering
			? AvatarUtils.getEmojiURL({id: emoji.id, animated: shouldShowAnimated})
			: (emoji.url ?? '');

	return (
		<Tooltip
			text={() => (
				<div className={styles.tooltipContent}>
					<span>{emojiNameWithColons}</span>
					<span className={styles.tooltipHint}>{t`Click to react`}</span>
				</div>
			)}
		>
			<FocusRing>
				<button
					type="button"
					ref={ref}
					aria-label={`React with ${emojiNameWithColons}`}
					onClick={handleClick}
					onMouseEnter={() => setIsHovered(true)}
					onMouseLeave={() => setIsHovered(false)}
					className={styles.button}
				>
					{useNativeRendering ? (
						<span className={styles.emojiImage}>{emoji.surrogates}</span>
					) : (
						<img src={emojiSrc} alt={emoji.name} className={styles.emojiImage} />
					)}
				</button>
			</FocusRing>
		</Tooltip>
	);
});

QuickReactionButton.displayName = 'QuickReactionButton';

interface MessageActionBarCoreProps {
	message: MessageRecord;
	handleDelete: (bypassConfirm?: boolean) => void;
	permissions: {
		canSendMessages: boolean;
		canAddReactions: boolean;
		canEditMessage: boolean;
		canDeleteMessage: boolean;
		canPinMessage: boolean;
		shouldRenderSuppressEmbeds: boolean;
	};
	isSaved: boolean;
	developerMode: boolean;
	isHovering?: boolean;
	onPopoutToggle?: (isOpen: boolean) => void;
}

const MessageActionBarCore: React.FC<MessageActionBarCoreProps> = observer(
	({message, handleDelete, permissions, isSaved, developerMode, isHovering = false, onPopoutToggle}) => {
		const {t} = useLingui();
		const [emojiPickerOpen, setEmojiPickerOpen] = React.useState(false);
		const [contextMenuOpen, setContextMenuOpen] = React.useState(false);
		const [moreMenuOpen, setMoreMenuOpen] = React.useState(false);
		const moreOptionsButtonRef = React.useRef<HTMLButtonElement>(null);
		const emojiPickerButtonRef = React.useRef<HTMLButtonElement>(null);
		const actionBarRef = React.useRef<HTMLDivElement>(null);

		const showMessageActionBar = AccessibilityStore.showMessageActionBar;
		const showQuickReactions = AccessibilityStore.showMessageActionBarQuickReactions;
		const showShiftExpand = AccessibilityStore.showMessageActionBarShiftExpand;
		const onlyMoreButton = AccessibilityStore.showMessageActionBarOnlyMoreButton;
		const keyboardModeEnabled = KeyboardModeStore.keyboardModeEnabled;

		const isActionBarActive = isHovering || contextMenuOpen || emojiPickerOpen || moreMenuOpen;
		const shouldListenForShift =
			showShiftExpand && showMessageActionBar && !onlyMoreButton && isActionBarActive && !keyboardModeEnabled;
		const shiftPressed = useShiftKey(shouldListenForShift);
		const showFullActions = showShiftExpand && shiftPressed;

		const {
			canSendMessages,
			canAddReactions,
			canEditMessage,
			canDeleteMessage,
			canPinMessage,
			shouldRenderSuppressEmbeds,
		} = permissions;

		const handlers = createMessageActionHandlers(message);

		const quickReactionEmojis = useSyncExternalStore(
			quickReactionManager.subscribe,
			quickReactionManager.getSnapshot,
			quickReactionManager.getServerSnapshot,
		);

		const blurEmojiPickerTrigger = React.useCallback(() => {
			if (keyboardModeEnabled) {
				return;
			}
			requestAnimationFrame(() => emojiPickerButtonRef.current?.blur());
		}, [keyboardModeEnabled]);

		const handleEmojiPickerToggle = React.useCallback(
			(open: boolean) => {
				setEmojiPickerOpen(open);
				onPopoutToggle?.(open);
				if (!open) {
					blurEmojiPickerTrigger();
				}
			},
			[onPopoutToggle, blurEmojiPickerTrigger],
		);

		const handleEmojiPickerOpen = React.useCallback(() => handleEmojiPickerToggle(true), [handleEmojiPickerToggle]);
		const handleEmojiPickerClose = React.useCallback(() => handleEmojiPickerToggle(false), [handleEmojiPickerToggle]);

		React.useEffect(() => {
			return () => {
				if (emojiPickerOpen) {
					onPopoutToggle?.(false);
				}
			};
		}, [emojiPickerOpen, onPopoutToggle]);

		const handleDebugClick = React.useCallback(() => {
			ModalActionCreators.push(modal(() => <MessageDebugModal title={t`Message Debug`} message={message} />));
		}, [message]);

		React.useEffect(() => {
			const disposer = autorun(() => {
				const contextMenu = ContextMenuStore.contextMenu;
				const contextMenuTarget = contextMenu?.target?.target ?? null;
				const actionBarElement = actionBarRef.current;
				const isOpen =
					Boolean(contextMenu) &&
					isContextMenuNodeTarget(contextMenuTarget) &&
					Boolean(actionBarElement?.contains(contextMenuTarget));
				setContextMenuOpen(!!isOpen);

				const isMoreOpen =
					!!contextMenu && !!moreOptionsButtonRef.current && contextMenu.target.target === moreOptionsButtonRef.current;

				setMoreMenuOpen(isMoreOpen);
			});

			return () => disposer();
		}, []);

		React.useEffect(() => {
			const unsubscribe = ComponentDispatch.subscribe('EMOJI_PICKER_OPEN', (payload?: unknown) => {
				const data = (payload ?? {}) as {messageId?: string};
				if (data.messageId === message.id && emojiPickerButtonRef.current) {
					PopoutActionCreators.open({
						key: `emoji-picker-${message.id}`,
						position: 'left-start',
						render: ({onClose}) => (
							<EmojiPickerPopout
								channelId={message.channelId}
								handleSelect={handlers.handleEmojiSelect}
								onClose={onClose}
							/>
						),
						target: emojiPickerButtonRef.current,
						animationType: 'none',
						onOpen: () => handleEmojiPickerToggle(true),
						onClose: () => handleEmojiPickerToggle(false),
					});
				}
			});

			return () => unsubscribe();
		}, [message.id, message.channelId, handlers.handleEmojiSelect, handleEmojiPickerToggle]);

		const handleMoreOptionsPointerDown = React.useCallback((event: React.PointerEvent) => {
			const contextMenu = ContextMenuStore.contextMenu;
			const isOpen = !!contextMenu && contextMenu.target.target === moreOptionsButtonRef.current;

			if (isOpen) {
				event.stopPropagation();
				event.preventDefault();
				ContextMenuActionCreators.close();
			}
		}, []);

		const openMoreOptionsMenu = React.useCallback(
			(event: React.MouseEvent | React.KeyboardEvent) => {
				if (!showMessageActionBar) {
					return;
				}

				const contextMenu = ContextMenuStore.contextMenu;
				const isOpen = !!contextMenu && contextMenu.target.target === event.currentTarget;

				if (isOpen) {
					return;
				}

				if (!(event.nativeEvent instanceof MouseEvent)) {
					return;
				}

				ContextMenuActionCreators.openFromEvent(event as React.MouseEvent, (props) => (
					<>
						<MenuGroup>
							{canAddReactions && (
								<MenuItem
									icon={<AddReactionIcon />}
									onClick={() => {
										triggerAddReaction(message.id);
										props.onClose();
									}}
									shortcut="+"
								>
									{t`Add Reaction`}
								</MenuItem>
							)}

							{message.isUserMessage() && !message.messageSnapshots && canEditMessage && (
								<MenuItem
									icon={<EditIcon />}
									onClick={() => {
										handlers.handleEditMessage();
										props.onClose();
									}}
									shortcut="e"
								>
									{t`Edit Message`}
								</MenuItem>
							)}

							{message.isUserMessage() && canSendMessages && (
								<MenuItem
									icon={<ReplyIcon />}
									onClick={() => {
										handlers.handleReply();
										props.onClose();
									}}
									shortcut="r"
								>
									{t`Reply`}
								</MenuItem>
							)}

							{message.isUserMessage() && (
								<MenuItem
									icon={<ForwardIcon />}
									onClick={() => {
										handlers.handleForward();
										props.onClose();
									}}
									shortcut="f"
								>
									{t`Forward`}
								</MenuItem>
							)}
						</MenuGroup>

						{(message.isUserMessage() || shouldRenderSuppressEmbeds || message.content) && (
							<MenuGroup>
								{message.isUserMessage() && canPinMessage && (
									<MenuItem
										icon={<PinIcon />}
										onClick={() => {
											handlers.handlePinMessage();
											props.onClose();
										}}
										shortcut="p"
									>
										{message.pinned ? t`Unpin Message` : t`Pin Message`}
									</MenuItem>
								)}

								{message.isUserMessage() && (
									<MenuItem
										icon={<BookmarkIcon filled={isSaved} />}
										onClick={() => {
											handlers.handleSaveMessage(isSaved)();
											props.onClose();
										}}
										shortcut="b"
									>
										{isSaved ? t`Remove Bookmark` : t`Bookmark Message`}
									</MenuItem>
								)}

								<MenuItem
									icon={<MarkAsUnreadIcon />}
									onClick={() => {
										handlers.handleMarkAsUnread();
										props.onClose();
									}}
									shortcut="u"
								>
									{t`Mark as Unread`}
								</MenuItem>

								{shouldRenderSuppressEmbeds && (
									<MenuItem
										icon={<SuppressEmbedsIcon />}
										onClick={() => {
											handlers.handleToggleSuppressEmbeds();
											props.onClose();
										}}
										shortcut="s"
									>
										{isEmbedsSuppressed(message) ? t`Unsuppress Embeds` : t`Suppress Embeds`}
									</MenuItem>
								)}

								{message.content && (
									<MenuItem
										icon={<CopyTextIcon />}
										onClick={() => {
											handlers.handleCopyMessage();
											props.onClose();
										}}
										shortcut="c"
									>
										{t`Copy Text`}
									</MenuItem>
								)}
							</MenuGroup>
						)}

						<MenuGroup>
							<MenuItem
								icon={<CopyLinkIcon />}
								onClick={() => {
									handlers.handleCopyMessageLink();
									props.onClose();
								}}
								shortcut="l"
							>
								{t`Copy Message Link`}
							</MenuItem>

							<MenuItem
								icon={<CopyIdIcon />}
								onClick={() => {
									handlers.handleCopyMessageId();
									props.onClose();
								}}
							>
								{t`Copy Message ID`}
							</MenuItem>

							{developerMode && (
								<MenuItem
									icon={<DebugIcon />}
									onClick={() => {
										handleDebugClick();
										props.onClose();
									}}
								>
									{t`Debug Message`}
								</MenuItem>
							)}
						</MenuGroup>

						{canDeleteMessage && (
							<MenuGroup>
								<MenuItem
									icon={<DeleteIcon />}
									onClick={(event) => {
										const shiftKey = Boolean((event as {shiftKey?: boolean} | undefined)?.shiftKey);
										handleDelete(shiftKey);
										props.onClose();
									}}
									danger
									shortcut="d"
								>
									{t`Delete Message`}
								</MenuItem>
							</MenuGroup>
						)}
					</>
				));
			},
			[
				canAddReactions,
				canSendMessages,
				canEditMessage,
				canPinMessage,
				shouldRenderSuppressEmbeds,
				developerMode,
				canDeleteMessage,
				message,
				handlers,
				handleDelete,
				isSaved,
				handleDebugClick,
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
				)}
			>
				<div className={styles.actionBar}>
					{message.state === MessageStates.SENT &&
						(onlyMoreButton ? (
							<MessageActionBarButton
								ref={moreOptionsButtonRef}
								icon={<DotsThreeIcon size={20} weight="bold" />}
								label={t`More`}
								onPointerDownCapture={handleMoreOptionsPointerDown}
								onClick={openMoreOptionsMenu}
								isActive={moreMenuOpen}
							/>
						) : (
							<>
								{!showFullActions &&
									canAddReactions &&
									showQuickReactions &&
									quickReactionEmojis.map((emoji) => (
										<QuickReactionButton key={emoji.name} emoji={emoji} onReact={handlers.handleEmojiSelect} />
									))}

								{showFullActions && (
									<>
										{developerMode && (
											<MessageActionBarButton
												icon={<DebugIcon size={20} />}
												label={t`Debug Message`}
												onClick={handleDebugClick}
											/>
										)}

										<MessageActionBarButton
											icon={<CopyIdIcon size={20} />}
											label={t`Copy Message ID`}
											onClick={handlers.handleCopyMessageId}
										/>

										<MessageActionBarButton
											icon={<CopyLinkIcon size={20} />}
											label={t`Copy Message Link`}
											onClick={handlers.handleCopyMessageLink}
										/>

										{message.content && (
											<MessageActionBarButton
												icon={<CopyTextIcon size={20} />}
												label={t`Copy Text`}
												onClick={handlers.handleCopyMessage}
											/>
										)}

										{shouldRenderSuppressEmbeds && (
											<MessageActionBarButton
												icon={<SuppressEmbedsIcon size={20} />}
												label={isEmbedsSuppressed(message) ? t`Unsuppress Embeds` : t`Suppress Embeds`}
												onClick={handlers.handleToggleSuppressEmbeds}
											/>
										)}

										<MessageActionBarButton
											icon={<MarkAsUnreadIcon size={20} />}
											label={t`Mark as Unread`}
											onClick={handlers.handleMarkAsUnread}
										/>

										{message.isUserMessage() && (
											<MessageActionBarButton
												icon={<BookmarkIcon size={20} filled={isSaved} />}
												label={isSaved ? t`Remove Bookmark` : t`Bookmark Message`}
												onClick={handlers.handleSaveMessage(isSaved)}
											/>
										)}

										{message.isUserMessage() && canPinMessage && (
											<MessageActionBarButton
												icon={<PinIcon size={20} />}
												label={message.pinned ? t`Unpin Message` : t`Pin Message`}
												onClick={handlers.handlePinMessage}
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
											/>
										)}
										position="left-start"
										uniqueId={`emoji-picker-actionbar-${message.id}`}
										animationType="none"
										onOpen={handleEmojiPickerOpen}
										onClose={handleEmojiPickerClose}
									>
										<MessageActionBarButton
											ref={emojiPickerButtonRef}
											icon={<AddReactionIcon size={20} />}
											label={t`Add Reaction`}
											isActive={emojiPickerOpen}
											dataAction="message-add-reaction-button"
										/>
									</Popout>
								)}

								{message.isUserMessage() && !message.messageSnapshots && canEditMessage && (
									<MessageActionBarButton
										icon={<EditIcon size={20} />}
										label={t`Edit Message`}
										onClick={handlers.handleEditMessage}
									/>
								)}

								{message.isUserMessage() && canSendMessages && (
									<MessageActionBarButton
										icon={<ReplyIcon size={20} />}
										label={t`Reply`}
										onClick={handlers.handleReply}
									/>
								)}

								{message.isUserMessage() && (
									<MessageActionBarButton
										icon={<ForwardIcon size={20} />}
										label={t`Forward`}
										onClick={handlers.handleForward}
									/>
								)}

								{(!showFullActions || !canDeleteMessage) && (
									<MessageActionBarButton
										ref={moreOptionsButtonRef}
										icon={<DotsThreeIcon size={20} weight="bold" />}
										label={t`More`}
										onPointerDownCapture={handleMoreOptionsPointerDown}
										onClick={openMoreOptionsMenu}
										isActive={moreMenuOpen}
									/>
								)}

								{showFullActions && canDeleteMessage && (
									<MessageActionBarButton
										danger={true}
										icon={<DeleteIcon size={20} />}
										label={t`Delete Message`}
										onClick={(event) => handleDelete(event.shiftKey)}
									/>
								)}
							</>
						))}

					{message.state === MessageStates.FAILED && (
						<>
							<MessageActionBarButton
								icon={<ArrowsClockwiseIcon size={20} weight="fill" />}
								label={t`Retry`}
								onClick={handlers.handleRetryMessage}
							/>
							<MessageActionBarButton
								danger={true}
								icon={<DeleteIcon size={20} />}
								label={t`Delete Message`}
								onClick={handlers.handleFailedMessageDelete}
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
		isHovering = false,
		onPopoutToggle,
	}: {
		message: MessageRecord;
		handleDelete: (bypassConfirm?: boolean) => void;
		isHovering?: boolean;
		onPopoutToggle?: (isOpen: boolean) => void;
	}) => {
		const isSaved = SavedMessagesStore.isSaved(message.id);
		const developerMode = UserSettingsStore.developerMode;
		const permissions = useMessagePermissions(message);

		return (
			<MessageActionBarCore
				message={message}
				handleDelete={handleDelete}
				permissions={permissions}
				isSaved={isSaved}
				developerMode={developerMode}
				isHovering={isHovering}
				onPopoutToggle={onPopoutToggle}
			/>
		);
	},
);

export {MessageActionBarCore};
