// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {messageActionMenuItemIds, useMessageActionMenuData} from '@app/features/channel/components/MessageActionMenu';
import {getEffectiveContent, triggerAddReaction} from '@app/features/channel/components/MessageActionUtils';
import {
	getQuickReactionEmojiSrc,
	REACT_WITH_EMOJI_DESCRIPTOR,
	renderQuickReactionEmoji,
} from '@app/features/channel/components/QuickReactionsRow';
import type {Channel} from '@app/features/channel/models/Channel';
import EmojiPicker from '@app/features/emoji/state/EmojiPicker';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {useExpressionImagesPreload} from '@app/features/expressions/utils/ExpressionImageCache';
import {
	COPY_LINK_DESCRIPTOR,
	COPY_TEXT_DESCRIPTOR,
	OPEN_LINK_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ReactionCommands from '@app/features/messaging/commands/ReactionCommands';
import {MessageReactionsModal} from '@app/features/messaging/components/modals/MessageReactionsModal';
import {ReactionImage} from '@app/features/messaging/components/ReactionImage';
import {useMessageReactions as useMessageReactionsSnapshot} from '@app/features/messaging/hooks/useMessageReactionStore';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MessagesState from '@app/features/messaging/state/MessagingMessages';
import {buildMessagePlaintextCopyText} from '@app/features/messaging/utils/MessagePlaintextCopyUtils';
import {
	buildMessageSelectionCopyTextForRange,
	getMessageSelectionRoot,
} from '@app/features/messaging/utils/MessageSelectionCopyUtils';
import {getEmojiNameWithColons, toReactionEmoji, useEmojiURL} from '@app/features/messaging/utils/ReactionUtils';
import {useContextMenuClose} from '@app/features/ui/action_menu/ContextMenu';
import contextMenuStyles from '@app/features/ui/action_menu/ContextMenu.module.css';
import {CopyIdIcon, CopyLinkIcon, CopyTextIcon, OpenLinkIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import type {MediaMenuHandlers} from '@app/features/ui/action_menu/items/MediaMenuData';
import {TranslateMenuItems} from '@app/features/ui/action_menu/items/TranslateMenuItems';
import {WebSearchMenuItems} from '@app/features/ui/action_menu/items/WebSearchMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import type {MenuGroupType, MenuItemType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {ContextMenu as BaseContextMenu} from '@base-ui/react/context-menu';
import type {MessageReaction} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const OTHER_REACTION_DESCRIPTOR = msg({
	message: 'Other reaction…',
	comment: 'Message action that opens the full reaction picker.',
});
const REMOVE_REACTIONS_DESCRIPTOR = msg({
	message: 'Remove reactions',
	comment: 'Message action that removes a specific reaction from the selected message.',
});
const MORE_MEDIA_ACTIONS_DESCRIPTOR = msg({
	message: 'More media actions',
	comment: 'Submenu label that contains additional media context menu actions.',
});
const COPY_ATTACHMENT_ID_DESCRIPTOR = msg({
	message: 'Copy attachment ID',
	comment: 'Developer-mode action that copies the attachment ID to the clipboard.',
});
const COPY_DESCRIPTOR = msg({
	message: 'Copy',
	comment: 'Action label that copies the selected content to the clipboard.',
});
const PRESS_SHORTCUT_TO_REACT_DESCRIPTOR = msg({
	message: 'Press {shortcut} to react',
	comment:
		'Tooltip hint for a quick reaction in the message context menu. Preserve {shortcut}; it is a keyboard key such as 1.',
});
const QUICK_REACTION_SHORTCUTS = ['1', '2', '3', '4'] as const;

interface SelectionSnapshot {
	text: string;
	range: Range | null;
}

const getSelectionSnapshot = (normalizeText?: (range: Range, fallbackText: string) => string): SelectionSnapshot => {
	const selection = window.getSelection();
	if (!selection || selection.rangeCount === 0) {
		return {text: '', range: null};
	}
	const text = selection.toString().trim();
	if (!text) {
		return {text: '', range: null};
	}
	try {
		const range = selection.getRangeAt(0).cloneRange();
		return {text: normalizeText?.(range, text) ?? text, range};
	} catch {
		return {text, range: null};
	}
};

interface MessageContextMenuProps {
	message: Message;
	sourceChannel?: Channel | null;
	onClose: () => void;
	onDelete: (bypassConfirm?: boolean) => void;
	linkUrl?: string;
	excludeMediaActions?: boolean;
	onOpenReactionPicker?: () => void;
	mediaHandlers?: MediaMenuHandlers;
	mediaGroups?: Array<MenuGroupType>;
	attachmentId?: string;
	attachmentExtraContent?: React.ReactNode;
	inlineMediaItems?: React.ReactNode;
	inlineStickerOrEmojiItems?: React.ReactNode;
}

interface RemoveReactionsSubmenuProps {
	reactions: ReadonlyArray<MessageReaction>;
	channelId: string;
	messageId: string;
}

const RemoveReactionsSubmenuItem = observer(
	({reaction, channelId, messageId}: {reaction: MessageReaction; channelId: string; messageId: string}) => {
		const {i18n} = useLingui();
		const emojiUrl = useEmojiURL({emoji: reaction.emoji, size: 32});
		const label = getEmojiNameWithColons(reaction.emoji);
		if (reaction.count <= 0) {
			return null;
		}
		const handleSelect = () => {
			ReactionCommands.removeReactionEmoji(i18n, channelId, messageId, reaction.emoji);
		};
		const renderEmojiPreview = () => {
			if (emojiUrl) {
				return (
					<span
						className={contextMenuStyles.emojiSubmenuIcon}
						data-flx="ui.action-menu.message-context-menu.render-emoji-preview.span"
					>
						<ReactionImage
							className={contextMenuStyles.emojiSubmenuImg}
							src={emojiUrl}
							alt=""
							draggable={false}
							data-flx="ui.action-menu.message-context-menu.render-emoji-preview.img"
						/>
					</span>
				);
			}
			return null;
		};
		return (
			<MenuItem
				icon={renderEmojiPreview()}
				onClick={handleSelect}
				closeOnSelect={false}
				hint={`${reaction.count}`}
				data-flx="ui.action-menu.message-context-menu.remove-reactions-submenu-item.menu-item.select"
			>
				{label}
			</MenuItem>
		);
	},
);

RemoveReactionsSubmenuItem.displayName = 'RemoveReactionsSubmenuItem';

const RemoveReactionsSubmenu = observer(({reactions, channelId, messageId}: RemoveReactionsSubmenuProps) => {
	if (reactions.length === 0) return null;
	return (
		<MenuGroup data-flx="ui.action-menu.message-context-menu.remove-reactions-submenu.menu-group">
			{reactions.map((reaction) => (
				<RemoveReactionsSubmenuItem
					key={`${reaction.emoji.id ?? reaction.emoji.name}-${reaction.emoji.name}`}
					reaction={reaction}
					channelId={channelId}
					messageId={messageId}
					data-flx="ui.action-menu.message-context-menu.remove-reactions-submenu.remove-reactions-submenu-item"
				/>
			))}
		</MenuGroup>
	);
});

RemoveReactionsSubmenu.displayName = 'RemoveReactionsSubmenu';

const AddReactionSubmenuItem = observer(
	({emoji, onSelect}: {emoji: FlatEmoji; onSelect: (emoji: FlatEmoji) => void}) => {
		const reactionEmoji = useMemo(() => toReactionEmoji(emoji), [emoji]);
		const emojiUrl = useEmojiURL({emoji: reactionEmoji, size: 32});
		const label = getEmojiNameWithColons(reactionEmoji);
		const renderEmojiPreview = () => {
			if (emojiUrl) {
				return (
					<span
						className={contextMenuStyles.emojiSubmenuIcon}
						data-flx="ui.action-menu.message-context-menu.render-emoji-preview.span--4"
					>
						<ReactionImage
							className={contextMenuStyles.emojiSubmenuImg}
							src={emojiUrl}
							alt=""
							draggable={false}
							data-flx="ui.action-menu.message-context-menu.render-emoji-preview.img--2"
						/>
					</span>
				);
			}
			return null;
		};
		return (
			<MenuItem
				icon={renderEmojiPreview()}
				onClick={() => onSelect(emoji)}
				data-flx="ui.action-menu.message-context-menu.add-reaction-submenu-item.menu-item.select"
			>
				{label}
			</MenuItem>
		);
	},
);

AddReactionSubmenuItem.displayName = 'AddReactionSubmenuItem';

const QuickReactionContextMenuItem = observer(
	({emoji, shortcut, onReact}: {emoji: FlatEmoji; shortcut: string; onReact: (emoji: FlatEmoji) => void}) => {
		const {i18n} = useLingui();
		const shouldShowShortcut = Accessibility.showContextMenuShortcuts;
		const emojiShortcode = useMemo(() => getEmojiNameWithColons(toReactionEmoji(emoji)), [emoji]);
		const label = useMemo(() => i18n._(REACT_WITH_EMOJI_DESCRIPTOR, {emojiShortcode}), [emojiShortcode, i18n.locale]);
		const shortcutHint = useMemo(() => i18n._(PRESS_SHORTCUT_TO_REACT_DESCRIPTOR, {shortcut}), [shortcut, i18n.locale]);
		const tooltipContent = useCallback(
			() => (
				<div
					className={contextMenuStyles.quickReactionTooltipContent}
					data-flx="ui.action-menu.message-context-menu.tooltip-content.div"
				>
					<span data-flx="ui.action-menu.message-context-menu.tooltip-content.span">{emojiShortcode}</span>
					<span
						className={contextMenuStyles.quickReactionTooltipHint}
						data-flx="ui.action-menu.message-context-menu.tooltip-content.span--2"
					>
						{shortcutHint}
					</span>
				</div>
			),
			[emojiShortcode, shortcutHint],
		);
		return (
			<Tooltip
				text={tooltipContent}
				allowWhenContextMenuOpen={true}
				data-flx="ui.action-menu.message-context-menu.quick-reaction-context-menu-item.tooltip"
			>
				<BaseContextMenu.Item
					className={contextMenuStyles.quickReactionItem}
					onClick={() => onReact(emoji)}
					label={emojiShortcode}
					aria-label={label}
					aria-keyshortcuts={shortcut}
					data-menu-shortcut={shortcut}
					data-flx="ui.action-menu.message-context-menu.quick-reaction-context-menu-item.aria-menu-item"
				>
					{renderQuickReactionEmoji(emoji)}
					{shouldShowShortcut && (
						<span
							className={contextMenuStyles.quickReactionShortcut}
							aria-hidden="true"
							data-flx="ui.action-menu.message-context-menu.quick-reaction-context-menu-item.span"
						>
							{shortcut}
						</span>
					)}
				</BaseContextMenu.Item>
			</Tooltip>
		);
	},
);

QuickReactionContextMenuItem.displayName = 'QuickReactionContextMenuItem';

export const MessageContextMenu: React.FC<MessageContextMenuProps> = observer(
	({
		message,
		sourceChannel,
		onClose,
		onDelete,
		linkUrl,
		excludeMediaActions = false,
		onOpenReactionPicker,
		mediaHandlers,
		mediaGroups,
		attachmentId,
		attachmentExtraContent,
		inlineMediaItems,
		inlineStickerOrEmojiItems,
	}) => {
		const {i18n} = useLingui();
		const closeMenu = useContextMenuClose();
		const getMessagePlaintext = useCallback(
			(messageId: string): string | null => {
				const selectedMessage =
					message.id === messageId ? message : MessagesState.getMessage(message.channelId, messageId);
				if (!selectedMessage) {
					return null;
				}
				return buildMessagePlaintextCopyText(selectedMessage, i18n);
			},
			[i18n, message],
		);
		const normalizeSelectionText = useCallback(
			(range: Range, fallbackText: string): string => {
				const selectionRoot = getMessageSelectionRoot(range.commonAncestorContainer);
				if (!selectionRoot) {
					return fallbackText;
				}
				return (
					buildMessageSelectionCopyTextForRange({
						rootElement: selectionRoot,
						selectionRange: range,
						getMessagePlaintext,
					}) ?? fallbackText
				);
			},
			[getMessagePlaintext],
		);
		const [{text: initialSelectionText, range: initialSelectionRange}] = useState(() =>
			getSelectionSnapshot(normalizeSelectionText),
		);
		const [selectionText, setSelectionText] = useState(initialSelectionText);
		const savedSelectionRangeRef = useRef<Range | null>(initialSelectionRange);
		const restoringSelectionRef = useRef(false);
		const restoreSelection = useCallback(() => {
			const savedRange = savedSelectionRangeRef.current;
			if (!savedRange) return;
			const selection = window.getSelection();
			if (!selection) return;
			try {
				const rangeForSelection = savedRange.cloneRange();
				const rangeForStorage = rangeForSelection.cloneRange();
				restoringSelectionRef.current = true;
				selection.removeAllRanges();
				selection.addRange(rangeForSelection);
				savedSelectionRangeRef.current = rangeForStorage;
				const rawText = rangeForStorage.toString().trim();
				setSelectionText(rawText ? normalizeSelectionText(rangeForStorage, rawText) : '');
			} catch {
				savedSelectionRangeRef.current = null;
				return;
			} finally {
				window.requestAnimationFrame(() => {
					restoringSelectionRef.current = false;
				});
			}
		}, [normalizeSelectionText]);
		useLayoutEffect(() => {
			if (!savedSelectionRangeRef.current) return;
			restoreSelection();
		}, [restoreSelection]);
		useEffect(() => {
			const handleSelectionChange = () => {
				if (restoringSelectionRef.current) return;
				const {text, range} = getSelectionSnapshot(normalizeSelectionText);
				if (text) {
					savedSelectionRangeRef.current = range;
					setSelectionText(text);
					return;
				}
				if (savedSelectionRangeRef.current) {
					restoreSelection();
					return;
				}
				setSelectionText('');
			};
			document.addEventListener('selectionchange', handleSelectionChange);
			return () => document.removeEventListener('selectionchange', handleSelectionChange);
		}, [restoreSelection, normalizeSelectionText]);
		const copyShortcut = useMemo(() => {
			return /Mac|iPod|iPhone|iPad/.test(navigator.platform) ? '⌘C' : 'Ctrl+C';
		}, []);
		const handleCopySelection = useCallback(async () => {
			if (!selectionText) return;
			await TextCopyCommands.copy(i18n, selectionText, true);
			onClose();
		}, [selectionText, onClose, i18n]);
		const handleCopyLink = useCallback(async () => {
			if (!linkUrl) return;
			await TextCopyCommands.copy(i18n, linkUrl, true);
			onClose();
		}, [linkUrl, onClose, i18n]);
		const handleOpenLink = useCallback(() => {
			if (!linkUrl) return;
			void openExternalUrl(linkUrl);
			onClose();
		}, [linkUrl, onClose]);
		const reactions = useMessageReactionsSnapshot(message.id);
		const handleOpenReactionsModal = useCallback(() => {
			if (reactions.length === 0) return;
			ModalCommands.push(
				modal(() => (
					<MessageReactionsModal
						channelId={message.channelId}
						messageId={message.id}
						message={message}
						openToReaction={reactions[0]}
						data-flx="ui.action-menu.message-context-menu.handle-open-reactions-modal.message-reactions-modal"
					/>
				)),
			);
		}, [message.channelId, message.id, reactions]);
		const handleOpenEmojiPickerAction = useCallback(() => {
			onOpenReactionPicker?.();
			triggerAddReaction(message);
			onClose();
		}, [message, onClose, onOpenReactionPicker]);
		const {groups, handlers, quickReactionEmojis, submenuReactionEmojis, quickReactionRowVisible, isFailed} =
			useMessageActionMenuData(message, {
				onClose,
				onDelete: (bypassConfirm) => onDelete(bypassConfirm),
				sourceChannel,
				onOpenReactionsSheet: handleOpenReactionsModal,
				onOpenEmojiPicker: handleOpenEmojiPickerAction,
				quickReactionCount: 4,
				submenuReactionCount: 16,
			});
		const preloadedReactionImageUrls = useMemo(
			() =>
				Array.from(
					new Set([...quickReactionEmojis, ...submenuReactionEmojis].map((emoji) => getQuickReactionEmojiSrc(emoji))),
				),
			[quickReactionEmojis, submenuReactionEmojis],
		);
		useExpressionImagesPreload(preloadedReactionImageUrls);
		const handleQuickReact = useCallback(
			(emoji: FlatEmoji) => {
				EmojiPicker.trackEmoji(emoji);
				handlers.handleEmojiSelect(emoji);
				closeMenu();
			},
			[handlers, closeMenu],
		);
		const isSent = !isFailed && message.state !== 'SENDING';
		const itemById = useMemo(() => {
			const map = new Map<string, MenuItemType>();
			for (const group of groups) {
				for (const item of group.items as Array<MenuItemType>) {
					if (item.id && !map.has(item.id)) {
						map.set(item.id, item);
					}
				}
			}
			return map;
		}, [groups]);
		const renderDataMenuItem = useCallback(
			(menuItem: MenuItemType | undefined, key?: React.Key, labelOverride?: string) => {
				if (!menuItem) return null;
				return (
					<MenuItem
						key={key ?? menuItem.id ?? menuItem.label}
						icon={menuItem.icon}
						onClick={menuItem.onClick}
						danger={menuItem.danger}
						disabled={menuItem.disabled}
						hint={menuItem.hint}
						shortcut={menuItem.shortcut}
						closeOnSelect={menuItem.closeOnSelect}
						data-flx="ui.action-menu.message-context-menu.render-data-menu-item.menu-item.click"
					>
						{labelOverride ?? menuItem.label}
					</MenuItem>
				);
			},
			[],
		);
		if (isFailed) {
			const nonEmptyGroups = groups.filter((group) => group.items.length > 0);
			return (
				<>
					{nonEmptyGroups.map((group, groupIndex) => (
						<MenuGroup key={groupIndex} data-flx="ui.action-menu.message-context-menu.menu-group">
							{group.items.map((item, itemIndex) => {
								const menuItem = item as MenuItemType;
								return renderDataMenuItem(menuItem, menuItem.id ?? `${menuItem.label}-${itemIndex}`);
							})}
						</MenuGroup>
					))}
				</>
			);
		}
		if (!isSent) {
			return null;
		}
		const ids = messageActionMenuItemIds;
		const addReactionItem = itemById.get(ids.addReaction);
		const viewReactionsItem = itemById.get(ids.viewReactions);
		const removeAllReactionsItem = itemById.get(ids.removeAllReactions);
		const editItem = itemById.get(ids.edit);
		const replyItem = itemById.get(ids.reply);
		const forwardItem = itemById.get(ids.forward);
		const copyMessageItem = itemById.get(ids.copyMessage);
		const pinMessageItem = itemById.get(ids.pinMessage);
		const bookmarkMessageItem = itemById.get(ids.bookmarkMessage);
		const markUnreadItem = itemById.get(ids.markUnread);
		const copyMessageLinkItem = itemById.get(ids.copyMessageLink);
		const speakMessageItem = itemById.get(ids.speakMessage);
		const suppressEmbedsItem = itemById.get(ids.suppressEmbeds);
		const deleteItem = itemById.get(ids.deleteMessage);
		const reportMessageItem = itemById.get(ids.reportMessage);
		const copyMessageIdItem = itemById.get(ids.copyMessageId);
		const debugMessageItem = itemById.get(ids.debugMessage);
		const canShowRemoveReactionsSubmenu = Boolean(removeAllReactionsItem) && reactions.length > 0;
		const hasMessageContent = Boolean(copyMessageItem);
		const copyTextAvailable = !excludeMediaActions && hasMessageContent;
		const messageTextContent = getEffectiveContent(message).trim();
		const translateText = selectionText || messageTextContent;
		const renderCopyTextItem = () => {
			if (!copyTextAvailable || !copyMessageItem) return null;
			return renderDataMenuItem(copyMessageItem, 'copy-message', i18n._(COPY_TEXT_DESCRIPTOR));
		};
		const hasSelectionGroup = Boolean(selectionText);
		const renderReactionsHeader = () => {
			const showQuickReactions = quickReactionRowVisible && quickReactionEmojis.length > 0;
			const showAddReaction = Boolean(addReactionItem);
			const showViewReactions = Boolean(viewReactionsItem) && reactions.length > 0;
			if (!showQuickReactions && !showAddReaction && !showViewReactions) {
				return null;
			}
			return (
				<MenuGroup data-flx="ui.action-menu.message-context-menu.render-reactions-header.menu-group">
					{showQuickReactions && (
						<BaseContextMenu.Group
							className={contextMenuStyles.quickReactionsGroup}
							data-flx="ui.action-menu.message-context-menu.render-reactions-header.aria-menu-section"
						>
							{quickReactionEmojis.map((emoji, index) => (
								<QuickReactionContextMenuItem
									key={emoji.name}
									emoji={emoji}
									shortcut={QUICK_REACTION_SHORTCUTS[index] ?? ''}
									onReact={handleQuickReact}
									data-flx="ui.action-menu.message-context-menu.render-reactions-header.quick-reaction-context-menu-item"
								/>
							))}
						</BaseContextMenu.Group>
					)}
					{showAddReaction && addReactionItem && (
						<MenuItemSubmenu
							label={addReactionItem.label}
							onTriggerSelect={handleOpenEmojiPickerAction}
							render={() => (
								<>
									{submenuReactionEmojis.length > 0 && (
										<MenuGroup data-flx="ui.action-menu.message-context-menu.render-reactions-header.menu-group--2">
											{submenuReactionEmojis.map((emoji) => (
												<AddReactionSubmenuItem
													key={`${emoji.id ?? ''}:${emoji.name}`}
													emoji={emoji}
													onSelect={handleQuickReact}
													data-flx="ui.action-menu.message-context-menu.render-reactions-header.add-reaction-submenu-item.quick-react"
												/>
											))}
										</MenuGroup>
									)}
									<MenuGroup data-flx="ui.action-menu.message-context-menu.render-reactions-header.menu-group--3">
										<MenuItem
											onClick={handleOpenEmojiPickerAction}
											data-flx="ui.action-menu.message-context-menu.render-reactions-header.menu-item.open-emoji-picker"
										>
											{i18n._(OTHER_REACTION_DESCRIPTOR)}
										</MenuItem>
									</MenuGroup>
								</>
							)}
							data-flx="ui.action-menu.message-context-menu.render-reactions-header.menu-item-submenu"
						/>
					)}
					{showViewReactions && viewReactionsItem && renderDataMenuItem(viewReactionsItem, 'view-reactions')}
				</MenuGroup>
			);
		};
		const renderInteractionGroup = () => {
			if (!editItem && !replyItem && !forwardItem) return null;
			return (
				<MenuGroup data-flx="ui.action-menu.message-context-menu.render-interaction-group.menu-group">
					{editItem && renderDataMenuItem(editItem, 'edit')}
					{replyItem && renderDataMenuItem(replyItem, 'reply')}
					{forwardItem && renderDataMenuItem(forwardItem, 'forward')}
				</MenuGroup>
			);
		};
		const renderUtilityGroup = () => {
			const hasAny =
				copyTextAvailable ||
				pinMessageItem ||
				bookmarkMessageItem ||
				markUnreadItem ||
				copyMessageLinkItem ||
				speakMessageItem;
			if (!hasAny) return null;
			return (
				<MenuGroup data-flx="ui.action-menu.message-context-menu.render-utility-group.menu-group">
					{renderCopyTextItem()}
					{pinMessageItem && renderDataMenuItem(pinMessageItem, 'pin-message')}
					{bookmarkMessageItem && renderDataMenuItem(bookmarkMessageItem, 'bookmark-message')}
					{markUnreadItem && renderDataMenuItem(markUnreadItem, 'mark-unread')}
					{copyMessageLinkItem && renderDataMenuItem(copyMessageLinkItem, 'copy-message-link')}
					{speakMessageItem && renderDataMenuItem(speakMessageItem, 'speak-message')}
					{suppressEmbedsItem && renderDataMenuItem(suppressEmbedsItem, 'suppress-embeds')}
				</MenuGroup>
			);
		};
		const renderDangerGroup = () => {
			const hasAny = canShowRemoveReactionsSubmenu || removeAllReactionsItem || deleteItem || reportMessageItem;
			if (!hasAny) return null;
			return (
				<MenuGroup data-flx="ui.action-menu.message-context-menu.render-danger-group.menu-group">
					{canShowRemoveReactionsSubmenu && (
						<MenuItemSubmenu
							label={i18n._(REMOVE_REACTIONS_DESCRIPTOR)}
							danger
							render={() => (
								<RemoveReactionsSubmenu
									reactions={reactions}
									channelId={message.channelId}
									messageId={message.id}
									data-flx="ui.action-menu.message-context-menu.render-danger-group.remove-reactions-submenu"
								/>
							)}
							data-flx="ui.action-menu.message-context-menu.render-danger-group.menu-item-submenu"
						/>
					)}
					{removeAllReactionsItem && renderDataMenuItem(removeAllReactionsItem, 'remove-all-reactions')}
					{deleteItem && renderDataMenuItem(deleteItem, 'delete')}
					{reportMessageItem && renderDataMenuItem(reportMessageItem, 'report-message')}
				</MenuGroup>
			);
		};
		const renderIdGroup = () => {
			if (!copyMessageIdItem && !debugMessageItem) return null;
			return (
				<MenuGroup data-flx="ui.action-menu.message-context-menu.render-id-group.menu-group">
					{copyMessageIdItem && (
						<MenuItem
							key="copy-message-id"
							icon={copyMessageIdItem.icon}
							danger={copyMessageIdItem.danger}
							disabled={copyMessageIdItem.disabled}
							hint={copyMessageIdItem.hint}
							shortcut={copyMessageIdItem.shortcut}
							closeOnSelect={copyMessageIdItem.closeOnSelect}
							onClick={(event) => {
								if (event.shiftKey) {
									void TextCopyCommands.copy(i18n, `${message.channelId}-${message.id}`);
									onClose();
									return;
								}
								copyMessageIdItem.onClick?.(event);
							}}
							data-flx="ui.action-menu.message-context-menu.render-id-group.menu-item.click"
						>
							{copyMessageIdItem.label}
						</MenuItem>
					)}
					{debugMessageItem && renderDataMenuItem(debugMessageItem, 'debug-message')}
				</MenuGroup>
			);
		};
		const nonEmptyMediaGroups = (mediaGroups ?? []).filter((group) => group.items.length > 0);
		const shouldRenderAttachmentCopyId = Boolean(attachmentId && mediaHandlers?.canCopyAttachmentId);
		const renderMoreMediaActionsSubmenu = () => {
			if (nonEmptyMediaGroups.length === 0 && !shouldRenderAttachmentCopyId && !attachmentExtraContent) {
				return null;
			}
			return (
				<MenuItemSubmenu
					label={i18n._(MORE_MEDIA_ACTIONS_DESCRIPTOR)}
					render={() => (
						<>
							{nonEmptyMediaGroups.map((group, groupIndex) => (
								<MenuGroup
									key={`media-${groupIndex}`}
									data-flx="ui.action-menu.message-context-menu.render-more-media-actions-submenu.menu-group"
								>
									{group.items.map((item, itemIndex) =>
										renderDataMenuItem(
											item as MenuItemType,
											(item as MenuItemType).id ?? `media-${groupIndex}-${itemIndex}`,
										),
									)}
								</MenuGroup>
							))}
							{shouldRenderAttachmentCopyId && (
								<MenuGroup data-flx="ui.action-menu.message-context-menu.render-more-media-actions-submenu.menu-group--2">
									<MenuItem
										icon={
											<CopyIdIcon
												size={20}
												data-flx="ui.action-menu.message-context-menu.render-more-media-actions-submenu.copy-id-icon"
											/>
										}
										onClick={() => {
											void mediaHandlers?.handleCopyAttachmentId();
										}}
										data-flx="ui.action-menu.message-context-menu.render-more-media-actions-submenu.menu-item"
									>
										{i18n._(COPY_ATTACHMENT_ID_DESCRIPTOR)}
									</MenuItem>
								</MenuGroup>
							)}
							{attachmentExtraContent}
						</>
					)}
					data-flx="ui.action-menu.message-context-menu.render-more-media-actions-submenu.menu-item-submenu"
				/>
			);
		};
		const renderMediaInlineGroups = () => {
			if (!inlineMediaItems && !mediaHandlers) return null;
			const moreActions = renderMoreMediaActionsSubmenu();
			return (
				<>
					{inlineMediaItems && (
						<MenuGroup data-flx="ui.action-menu.message-context-menu.render-media-inline-groups.menu-group">
							{inlineMediaItems}
						</MenuGroup>
					)}
					{mediaHandlers && (
						<MenuGroup data-flx="ui.action-menu.message-context-menu.render-media-inline-groups.menu-group--2">
							<MenuItem
								onClick={() => {
									void mediaHandlers.handleCopyLink();
								}}
								data-flx="ui.action-menu.message-context-menu.render-media-inline-groups.menu-item"
							>
								{mediaHandlers.copyLinkLabel}
							</MenuItem>
							<MenuItem
								onClick={mediaHandlers.handleOpenLink}
								data-flx="ui.action-menu.message-context-menu.render-media-inline-groups.menu-item.open-link"
							>
								{mediaHandlers.openLinkLabel}
							</MenuItem>
							{moreActions}
						</MenuGroup>
					)}
				</>
			);
		};
		const renderEmbeddedLinkGroup = () => {
			if (excludeMediaActions || !linkUrl) return null;
			return (
				<MenuGroup data-flx="ui.action-menu.message-context-menu.render-embedded-link-group.menu-group">
					<MenuItem
						icon={
							<OpenLinkIcon
								size={20}
								data-flx="ui.action-menu.message-context-menu.render-embedded-link-group.open-link-icon"
							/>
						}
						onClick={handleOpenLink}
						data-flx="ui.action-menu.message-context-menu.render-embedded-link-group.menu-item.open-link"
					>
						{i18n._(OPEN_LINK_DESCRIPTOR)}
					</MenuItem>
					<MenuItem
						icon={
							<CopyLinkIcon
								size={20}
								data-flx="ui.action-menu.message-context-menu.render-embedded-link-group.copy-link-icon"
							/>
						}
						onClick={handleCopyLink}
						data-flx="ui.action-menu.message-context-menu.render-embedded-link-group.menu-item.copy-link"
					>
						{i18n._(COPY_LINK_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			);
		};
		return (
			<>
				{!hasSelectionGroup && renderReactionsHeader()}
				{hasSelectionGroup && (
					<MenuGroup data-flx="ui.action-menu.message-context-menu.menu-group--2">
						<MenuItem
							icon={<CopyTextIcon size={20} data-flx="ui.action-menu.message-context-menu.copy-text-icon" />}
							onClick={handleCopySelection}
							shortcut={copyShortcut}
							data-flx="ui.action-menu.message-context-menu.menu-item.copy-selection"
						>
							{i18n._(COPY_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				)}
				{hasSelectionGroup && (
					<WebSearchMenuItems
						selectionText={selectionText}
						onClose={onClose}
						wrapInGroup
						data-flx="ui.action-menu.message-context-menu.web-search-menu-items"
					/>
				)}
				{translateText && (
					<TranslateMenuItems
						selectionText={translateText}
						onClose={onClose}
						wrapInGroup
						data-flx="ui.action-menu.message-context-menu.translate-menu-items"
					/>
				)}
				{renderInteractionGroup()}
				{renderMediaInlineGroups()}
				{renderEmbeddedLinkGroup()}
				{renderUtilityGroup()}
				{renderDangerGroup()}
				{inlineStickerOrEmojiItems}
				{renderIdGroup()}
			</>
		);
	},
);

MessageContextMenu.displayName = 'MessageContextMenu';
