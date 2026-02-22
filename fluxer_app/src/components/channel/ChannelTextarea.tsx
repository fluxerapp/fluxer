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

import * as ContextMenuActionCreators from '@app/actions/ContextMenuActionCreators';
import * as DraftActionCreators from '@app/actions/DraftActionCreators';
import * as MessageActionCreators from '@app/actions/MessageActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as PopoutActionCreators from '@app/actions/PopoutActionCreators';
import * as ScheduledMessageActionCreators from '@app/actions/ScheduledMessageActionCreators';
import {TooManyAttachmentsModal} from '@app/components/alerts/TooManyAttachmentsModal';
import {Autocomplete} from '@app/components/channel/Autocomplete';
import {ChannelAttachmentArea} from '@app/components/channel/ChannelAttachmentArea';
import {ChannelStickersArea} from '@app/components/channel/ChannelStickersArea';
import {EditBar} from '@app/components/channel/EditBar';
import {
	getMentionDescription,
	getMentionTitle,
	MentionEveryonePopout,
} from '@app/components/channel/MentionEveryonePopout';
import {MessageCharacterCounter} from '@app/components/channel/MessageCharacterCounter';
import {ReplyBar} from '@app/components/channel/ReplyBar';
import {ScheduledMessageEditBar} from '@app/components/channel/ScheduledMessageEditBar';
import wrapperStyles from '@app/components/channel/textarea/InputWrapper.module.css';
import {MobileTextareaLayout} from '@app/components/channel/textarea/MobileTextareaLayout';
import {MobileTextareaPlusBottomSheet} from '@app/components/channel/textarea/MobileTextareaPlusBottomSheet';
import {TextareaButton} from '@app/components/channel/textarea/TextareaButton';
import {TextareaButtons} from '@app/components/channel/textarea/TextareaButtons';
import styles from '@app/components/channel/textarea/TextareaInput.module.css';
import {TextareaInputField} from '@app/components/channel/textarea/TextareaInputField';
import {TextareaPlusMenu} from '@app/components/channel/textarea/TextareaPlusMenu';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {ExpressionPickerSheet} from '@app/components/modals/ExpressionPickerSheet';
import {ScheduleMessageModal} from '@app/components/modals/ScheduleMessageModal';
import FocusRing from '@app/components/uikit/focus_ring/FocusRing';
import {openPopout} from '@app/components/uikit/popout/Popout';
import {Scroller, type ScrollerHandle} from '@app/components/uikit/Scroller';
import {useTextareaAttachments} from '@app/hooks/useCloudUpload';
import {useContextMenuHoverState} from '@app/hooks/useContextMenuHoverState';
import {
	doesEventMatchShortcut,
	MARKDOWN_FORMATTING_SHORTCUTS,
	useMarkdownKeybinds,
} from '@app/hooks/useMarkdownKeybinds';
import {type SendMessageFunction, useMessageSubmission} from '@app/hooks/useMessageSubmission';
import {useSlowmode} from '@app/hooks/useSlowmode';
import {useTextareaAutocomplete} from '@app/hooks/useTextareaAutocomplete';
import {useTextareaDraftAndTyping} from '@app/hooks/useTextareaDraftAndTyping';
import {useTextareaEditing} from '@app/hooks/useTextareaEditing';
import {useTextareaEmojiPicker} from '@app/hooks/useTextareaEmojiPicker';
import {useTextareaExpressionHandlers} from '@app/hooks/useTextareaExpressionHandlers';
import {useTextareaExpressionPicker} from '@app/hooks/useTextareaExpressionPicker';
import {useTextareaKeyboard} from '@app/hooks/useTextareaKeyboard';
import {useTextareaPaste} from '@app/hooks/useTextareaPaste';
import {useTextareaSegments} from '@app/hooks/useTextareaSegments';
import {type MentionConfirmationInfo, useTextareaSubmit} from '@app/hooks/useTextareaSubmit';
import {CloudUpload} from '@app/lib/CloudUpload';
import {ComponentDispatch} from '@app/lib/ComponentDispatch';
import {safeFocus} from '@app/lib/InputFocusManager';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import AccessibilityStore from '@app/stores/AccessibilityStore';
import ChannelStickerStore from '@app/stores/ChannelStickerStore';
import DeveloperOptionsStore from '@app/stores/DeveloperOptionsStore';
import DraftStore from '@app/stores/DraftStore';
import KeyboardModeStore from '@app/stores/KeyboardModeStore';
import MessageEditMobileStore from '@app/stores/MessageEditMobileStore';
import MessageEditStore from '@app/stores/MessageEditStore';
import MessageReplyStore from '@app/stores/MessageReplyStore';
import MessageStore from '@app/stores/MessageStore';
import MobileLayoutStore from '@app/stores/MobileLayoutStore';
import PermissionStore from '@app/stores/PermissionStore';
import ScheduledMessageEditorStore from '@app/stores/ScheduledMessageEditorStore';
import UserStore from '@app/stores/UserStore';
import * as ChannelUtils from '@app/utils/ChannelUtils';
import {openFilePicker} from '@app/utils/FilePickerUtils';
import * as FileUploadUtils from '@app/utils/FileUploadUtils';
import {Limits} from '@app/utils/limits/UserLimits';
import {normalizeMessageContent} from '@app/utils/MessageRequestUtils';
import * as MessageSubmitUtils from '@app/utils/MessageSubmitUtils';
import * as PlaceholderUtils from '@app/utils/PlaceholderUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	MAX_ATTACHMENTS_PER_MESSAGE,
	MAX_MESSAGE_LENGTH_NON_PREMIUM,
	MAX_MESSAGE_LENGTH_PREMIUM,
} from '@fluxer/constants/src/LimitConstants';
import {useLingui} from '@lingui/react/macro';
import {PlusCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

function readBorderBoxBlockSize(entry: ResizeObserverEntry): number {
	const borderBoxSize = (
		entry as ResizeObserverEntry & {borderBoxSize?: ReadonlyArray<{blockSize: number}> | {blockSize: number}}
	).borderBoxSize;

	if (Array.isArray(borderBoxSize) && borderBoxSize[0] && typeof borderBoxSize[0].blockSize === 'number') {
		return borderBoxSize[0].blockSize;
	}

	if (borderBoxSize && 'blockSize' in borderBoxSize && typeof borderBoxSize.blockSize === 'number') {
		return borderBoxSize.blockSize;
	}

	return (entry.target as HTMLElement).getBoundingClientRect().height;
}

const ChannelTextareaContent = observer(
	({
		channel,
		draft,
		disabled,
		canAttachFiles,
		canSendFavoriteMemeId,
	}: {
		channel: ChannelRecord;
		draft: string | null;
		disabled: boolean;
		canAttachFiles: boolean;
		canSendFavoriteMemeId: boolean;
	}) => {
		const {t, i18n} = useLingui();
		const [isFocused, setIsFocused] = useState(false);
		const [isInputAreaFocused, setIsInputAreaFocused] = useState(false);
		const [value, setValue] = useState('');
		const [showAllButtons, setShowAllButtons] = useState(true);
		const [pendingMentionConfirmation, setPendingMentionConfirmation] = useState<MentionConfirmationInfo | null>(null);
		const mentionPopoutKey = useMemo(() => `mention-everyone-${channel.id}`, [channel.id]);
		const mentionModalKey = useMemo(() => `mention-everyone-modal-${channel.id}`, [channel.id]);
		const [isScheduleModalOpen, setIsScheduleModalOpen] = useState(false);
		const [mobilePlusSheetOpen, setMobilePlusSheetOpen] = useState(false);

		const textareaRef = useRef<HTMLTextAreaElement>(null);
		const expressionPickerTriggerRef = useRef<HTMLButtonElement>(null);
		const invisibleExpressionPickerTriggerRef = useRef<HTMLDivElement>(null);
		const containerRef = useRef<HTMLDivElement>(null);
		const scrollerRef = useRef<ScrollerHandle>(null);
		const plusButtonRef = useRef<HTMLButtonElement | null>(null);
		useMarkdownKeybinds(isFocused);
		const plusContextMenuOpen = useContextMenuHoverState(plusButtonRef);

		const textareaHeightRef = useRef<number>(0);
		const handleTextareaHeightChange = useCallback((height: number) => {
			textareaHeightRef.current = height;
		}, []);

		const inputBoxHeightRef = useRef<number | null>(null);
		const pendingLayoutDeltaRef = useRef(0);
		const flushScheduledRef = useRef(false);

		useLayoutEffect(() => {
			const el = containerRef.current;
			if (!el || typeof ResizeObserver === 'undefined') return;

			inputBoxHeightRef.current = null;
			pendingLayoutDeltaRef.current = 0;
			flushScheduledRef.current = false;

			const flush = () => {
				flushScheduledRef.current = false;
				const delta = pendingLayoutDeltaRef.current;
				pendingLayoutDeltaRef.current = 0;
				if (!delta) return;

				ComponentDispatch.dispatch('LAYOUT_RESIZED', {
					channelId: channel.id,
					heightDelta: delta,
				});
			};

			const ro = new ResizeObserver((entries) => {
				const entry = entries[0];
				if (!entry) return;

				const nextHeight = Math.round(readBorderBoxBlockSize(entry));
				const prevHeight = inputBoxHeightRef.current;

				if (prevHeight == null) {
					inputBoxHeightRef.current = nextHeight;
					return;
				}

				const delta = nextHeight - prevHeight;
				if (!delta) return;

				inputBoxHeightRef.current = nextHeight;
				pendingLayoutDeltaRef.current += delta;

				if (!flushScheduledRef.current) {
					flushScheduledRef.current = true;
					queueMicrotask(flush);
				}
			});

			ro.observe(el);
			return () => ro.disconnect();
		}, [channel.id]);

		const showGifButton = AccessibilityStore.showGifButton;
		const showMemesButton = AccessibilityStore.showMemesButton;
		const showStickersButton = AccessibilityStore.showStickersButton;
		const showEmojiButton = AccessibilityStore.showEmojiButton;
		const showMessageSendButton = AccessibilityStore.showMessageSendButton;
		const editingMessageId = MessageEditStore.getEditingMessageId(channel.id);
		const editingMobileMessageId = MessageEditMobileStore.getEditingMobileMessageId(channel.id);
		const mobileLayout = MobileLayoutStore;
		const replyingMessage = MessageReplyStore.getReplyingMessage(channel.id);
		const referencedMessage = replyingMessage ? MessageStore.getMessage(channel.id, replyingMessage.messageId) : null;
		const editingMessage = editingMobileMessageId ? MessageStore.getMessage(channel.id, editingMobileMessageId) : null;
		const currentUser = UserStore.getCurrentUser();
		const maxMessageLength = currentUser?.maxMessageLength ?? MAX_MESSAGE_LENGTH_NON_PREMIUM;
		const premiumMaxLength = Limits.getPremiumValue('max_message_length', MAX_MESSAGE_LENGTH_PREMIUM);
		const maxAttachments = currentUser?.maxAttachmentsPerMessage ?? MAX_ATTACHMENTS_PER_MESSAGE;

		const uploadAttachments = useTextareaAttachments(channel.id);
		const {isSlowmodeActive} = useSlowmode(channel);
		const {segmentManagerRef, previousValueRef, displayToActual, insertSegment, handleTextChange, clearSegments} =
			useTextareaSegments();
		const {handleEmojiSelect} = useTextareaEmojiPicker({
			setValue,
			textareaRef,
			segmentManagerRef,
			previousValueRef,
			channelId: channel.id,
		});
		const scheduledMessageEditorState = ScheduledMessageEditorStore.getEditingState();
		const isEditingScheduledMessage = ScheduledMessageEditorStore.isEditingChannel(channel.id);
		const editingScheduledMessage = isEditingScheduledMessage ? scheduledMessageEditorState : null;
		const hasMessageSchedulingAccess = UserStore.getCurrentUser()?.isStaff() ?? false;

		const {sendMessage, sendOptimisticMessage} = useMessageSubmission({
			channel,
			referencedMessage: referencedMessage ?? null,
			replyingMessage,
			clearSegments,
		});

		const handleCancelScheduledEdit = useCallback(() => {
			ScheduledMessageEditorStore.stopEditing();
			DraftActionCreators.deleteDraft(channel.id);
			setValue('');
			clearSegments();
		}, [channel.id, clearSegments, setValue]);

		const handleSendMessage: SendMessageFunction = useCallback(
			(...args) => {
				setValue('');
				clearSegments();
				sendMessage(...args);
			},
			[sendMessage, clearSegments],
		);

		const handleMentionConfirmationNeeded = useCallback((info: MentionConfirmationInfo) => {
			setPendingMentionConfirmation(info);
		}, []);

		const handleMentionConfirm = useCallback(() => {
			if (pendingMentionConfirmation) {
				handleSendMessage(pendingMentionConfirmation.content, false, pendingMentionConfirmation.tts);
				setPendingMentionConfirmation(null);
			}
		}, [pendingMentionConfirmation, handleSendMessage]);

		const handleMentionCancel = useCallback(() => {
			setPendingMentionConfirmation(null);
			textareaRef.current?.focus();
		}, []);

		useEffect(() => {
			if (!pendingMentionConfirmation) {
				PopoutActionCreators.close(mentionPopoutKey);
				ModalActionCreators.popWithKey(mentionModalKey);
				return;
			}

			if (mobileLayout.enabled) {
				const index = pendingMentionConfirmation.mentionType;
				const title = getMentionTitle(index, pendingMentionConfirmation.roleName);
				const description = getMentionDescription(
					index,
					pendingMentionConfirmation.memberCount,
					pendingMentionConfirmation.roleName,
				);

				ModalActionCreators.pushWithKey(
					modal(() => (
						<ConfirmModal
							title={title}
							description={description}
							primaryText={t`Continue`}
							secondaryText={t`Cancel`}
							onPrimary={() => {
								handleMentionConfirm();
							}}
							onSecondary={() => {
								handleMentionCancel();
							}}
						/>
					)),
					mentionModalKey,
				);

				return () => {
					ModalActionCreators.popWithKey(mentionModalKey);
				};
			}

			const containerElement = containerRef.current;
			if (!containerElement) {
				return;
			}

			openPopout(
				containerElement,
				{
					render: ({onClose}) => (
						<MentionEveryonePopout
							mentionType={pendingMentionConfirmation.mentionType}
							memberCount={pendingMentionConfirmation.memberCount}
							roleName={pendingMentionConfirmation.roleName}
							onConfirm={() => {
								handleMentionConfirm();
								onClose();
							}}
							onCancel={() => {
								handleMentionCancel();
								onClose();
							}}
						/>
					),
					position: 'top-start',
					offsetMainAxis: 8,
					shouldAutoUpdate: true,
					returnFocusRef: textareaRef,
					onCloseRequest: () => {
						handleMentionCancel();
						return true;
					},
				},
				mentionPopoutKey,
			);

			return () => {
				PopoutActionCreators.close(mentionPopoutKey);
			};
		}, [
			pendingMentionConfirmation,
			mentionPopoutKey,
			mentionModalKey,
			handleMentionConfirm,
			handleMentionCancel,
			textareaRef,
			mobileLayout.enabled,
		]);

		const {
			autocompleteQuery,
			autocompleteOptions,
			autocompleteType,
			selectedIndex,
			isAutocompleteAttached,
			setSelectedIndex,
			onCursorMove,
			handleSelect,
		} = useTextareaAutocomplete({
			channel,
			value,
			setValue,
			textareaRef,
			segmentManagerRef,
			previousValueRef,
		});

		useEffect(() => {
			ComponentDispatch.safeDispatch('TEXTAREA_AUTOCOMPLETE_CHANGED', {
				channelId: channel.id,
				open: isAutocompleteAttached,
			});
		}, [channel.id, isAutocompleteAttached]);

		const trimmedMessageContent = displayToActual(value).trim();
		const hasScheduleContent = trimmedMessageContent.length > 0 || uploadAttachments.length > 0;
		const canScheduleMessage = hasMessageSchedulingAccess && !disabled && hasScheduleContent;

		const handlePasteExceedsLimit = useCallback(
			async (pastedText: string) => {
				const result = await FileUploadUtils.convertTextToFile(
					channel.id,
					pastedText,
					uploadAttachments.length,
					maxAttachments,
				);

				if (!result.success && result.error === 'too_many_attachments') {
					ModalActionCreators.push(modal(() => <TooManyAttachmentsModal />));
				}
			},
			[channel.id, uploadAttachments.length, maxAttachments],
		);

		useTextareaPaste({
			channel,
			textareaRef,
			segmentManagerRef,
			setValue,
			previousValueRef,
			maxMessageLength,
			onPasteExceedsLimit: canAttachFiles ? handlePasteExceedsLimit : undefined,
		});

		const handleOpenScheduleModal = useCallback(() => {
			if (!hasMessageSchedulingAccess) {
				return;
			}
			setIsScheduleModalOpen(true);
		}, [hasMessageSchedulingAccess]);

		const handleOpenMobilePlusSheet = useCallback(() => {
			setMobilePlusSheetOpen(true);
		}, []);

		const handleCloseMobilePlusSheet = useCallback(() => {
			setMobilePlusSheetOpen(false);
		}, []);

		const handleScheduleSubmit = useCallback(
			async (scheduledLocalAt: string, timezone: string) => {
				const actualContent = displayToActual(value).trim();
				if (!actualContent && uploadAttachments.length === 0) {
					return;
				}

				const normalized = normalizeMessageContent(actualContent, undefined);

				if (editingScheduledMessage) {
					await ScheduledMessageActionCreators.updateScheduledMessage(i18n, {
						channelId: channel.id,
						scheduledMessageId: editingScheduledMessage.scheduledMessageId,
						scheduledLocalAt,
						timezone,
						normalized,
						payload: editingScheduledMessage.payload,
						replyMentioning: replyingMessage?.mentioning,
					});
					ScheduledMessageEditorStore.stopEditing();
				} else {
					await ScheduledMessageActionCreators.scheduleMessage(i18n, {
						channelId: channel.id,
						content: actualContent,
						scheduledLocalAt,
						timezone,
						messageReference: MessageSubmitUtils.prepareMessageReference(channel.id, referencedMessage),
						replyMentioning: replyingMessage?.mentioning,
						favoriteMemeId: undefined,
						stickers: undefined,
						tts: false,
						hasAttachments: uploadAttachments.length > 0,
					});
				}

				setValue('');
				clearSegments();
				setIsScheduleModalOpen(false);
			},
			[
				channel.id,
				clearSegments,
				displayToActual,
				editingScheduledMessage,
				referencedMessage,
				replyingMessage?.mentioning,
				setIsScheduleModalOpen,
				setValue,
				uploadAttachments.length,
				value,
			],
		);

		const handleFileButtonClick = async () => {
			if (disabled || !canAttachFiles) {
				return;
			}

			const files = await openFilePicker({multiple: true});
			const result = await FileUploadUtils.handleFileUpload(
				channel.id,
				files,
				uploadAttachments.length,
				maxAttachments,
			);

			if (!result.success && result.error === 'too_many_attachments') {
				ModalActionCreators.push(modal(() => <TooManyAttachmentsModal />));
			}
		};

		const handleUploadMessageAsFile = useCallback(async () => {
			if (disabled || !canAttachFiles) {
				return;
			}

			const result = await FileUploadUtils.convertTextToFile(
				channel.id,
				value,
				uploadAttachments.length,
				maxAttachments,
			);

			if (!result.success) {
				if (result.error === 'too_many_attachments') {
					ModalActionCreators.push(modal(() => <TooManyAttachmentsModal />));
				}
				return;
			}

			setValue('');
			DraftActionCreators.deleteDraft(channel.id);
		}, [disabled, canAttachFiles, value, channel.id, uploadAttachments.length, maxAttachments]);

		useTextareaExpressionHandlers({
			setValue,
			textareaRef,
			canSendFavoriteMemeId,
			insertSegment,
			previousValueRef,
			sendOptimisticMessage,
		});

		const {expressionPickerOpen, setExpressionPickerOpen, handleExpressionPickerTabToggle, selectedTab} =
			useTextareaExpressionPicker({
				channelId: channel.id,
				onEmojiSelect: handleEmojiSelect,
				expressionPickerTriggerRef,
				invisibleExpressionPickerTriggerRef,
				textareaRef,
			});

		useTextareaEditing({
			channelId: channel.id,
			editingMessageId: editingMessageId ?? null,
			editingMessage: editingMessage ?? null,
			isMobileEditMode: mobileLayout.enabled,
			replyingMessage,
			value,
			setValue,
			textareaRef,
			previousValueRef,
		});

		const hasPendingSticker = ChannelStickerStore.getPendingSticker(channel.id) !== null;
		const hasAttachments = uploadAttachments.length > 0;
		const showAttachments = hasAttachments;
		const showStickers = hasPendingSticker;
		const isOverCharacterLimit = trimmedMessageContent.length > maxMessageLength;

		const {onSubmit} = useTextareaSubmit({
			channelId: channel.id,
			guildId: channel.guildId ?? null,
			editingMessage: editingMessage ?? null,
			isMobileEditMode: mobileLayout.enabled,
			uploadAttachmentsLength: uploadAttachments.length,
			hasPendingSticker,
			value,
			setValue,
			displayToActual,
			clearSegments,
			isSlowmodeActive,
			handleSendMessage,
			onMentionConfirmationNeeded: handleMentionConfirmationNeeded,
			i18n: i18n,
		});

		const handleEscapeKey = useCallback(
			(event: React.KeyboardEvent<HTMLTextAreaElement>) => {
				if (event.key !== 'Escape') return;

				if (hasAttachments || hasPendingSticker || replyingMessage) {
					event.preventDefault();

					if (hasAttachments) {
						CloudUpload.clearTextarea(channel.id);
					}

					if (hasPendingSticker) {
						ChannelStickerStore.removePendingSticker(channel.id);
					}

					if (replyingMessage) {
						MessageActionCreators.stopReply(channel.id);
					}

					return;
				}

				if (isInputAreaFocused && KeyboardModeStore.keyboardModeEnabled) {
					event.preventDefault();
					KeyboardModeStore.exitKeyboardMode();
					return;
				}

				if (AccessibilityStore.escapeExitsKeyboardMode) {
					KeyboardModeStore.exitKeyboardMode();
				}
			},
			[
				channel.id,
				hasAttachments,
				hasPendingSticker,
				replyingMessage,
				isInputAreaFocused,
				KeyboardModeStore.keyboardModeEnabled,
				AccessibilityStore.escapeExitsKeyboardMode,
			],
		);

		const handleFormattingShortcut = useCallback(
			(event: React.KeyboardEvent<HTMLTextAreaElement>) => {
				for (const {combo: shortcutCombo, wrapper} of MARKDOWN_FORMATTING_SHORTCUTS) {
					if (!doesEventMatchShortcut(event, shortcutCombo)) {
						continue;
					}

					const textarea = textareaRef.current;
					if (!textarea) {
						return;
					}

					const selectionStart = textarea.selectionStart ?? 0;
					const selectionEnd = textarea.selectionEnd ?? 0;
					if (selectionStart === selectionEnd) {
						return;
					}

					const selectedText = value.slice(selectionStart, selectionEnd);
					const wrapperLength = wrapper.length;
					const alreadyWrappedInside =
						selectedText.length >= wrapperLength * 2 &&
						selectedText.startsWith(wrapper) &&
						selectedText.endsWith(wrapper);
					const hasPrefixWrapper =
						wrapperLength > 0 &&
						selectionStart >= wrapperLength &&
						value.slice(selectionStart - wrapperLength, selectionStart) === wrapper;
					const hasSuffixWrapper =
						wrapperLength > 0 &&
						selectionEnd + wrapperLength <= value.length &&
						value.slice(selectionEnd, selectionEnd + wrapperLength) === wrapper;

					let newValue: string;
					let newSelectionStart: number;
					let newSelectionEnd: number;

					if (alreadyWrappedInside) {
						const unwrappedText = selectedText.slice(wrapperLength, selectedText.length - wrapperLength);
						newValue = value.slice(0, selectionStart) + unwrappedText + value.slice(selectionEnd);
						newSelectionStart = selectionStart;
						newSelectionEnd = selectionStart + unwrappedText.length;
					} else if (hasPrefixWrapper && hasSuffixWrapper) {
						newValue =
							value.slice(0, selectionStart - wrapperLength) + selectedText + value.slice(selectionEnd + wrapperLength);
						newSelectionStart = selectionStart - wrapperLength;
						newSelectionEnd = selectionEnd - wrapperLength;
					} else {
						const wrappedText = `${wrapper}${selectedText}${wrapper}`;
						newValue = value.slice(0, selectionStart) + wrappedText + value.slice(selectionEnd);
						newSelectionStart = selectionStart + wrapperLength;
						newSelectionEnd = selectionEnd + wrapperLength;
					}

					handleTextChange(newValue, previousValueRef.current);
					setValue(newValue);

					const updateSelection = () => {
						textarea.setSelectionRange(newSelectionStart, newSelectionEnd);
					};

					window.requestAnimationFrame(updateSelection);

					event.preventDefault();
					event.stopPropagation();
					return;
				}
			},
			[handleTextChange, previousValueRef, setValue, textareaRef, value],
		);

		const handleTextareaKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLTextAreaElement>) => {
				handleFormattingShortcut(event);
				handleEscapeKey(event);
			},
			[handleFormattingShortcut, handleEscapeKey],
		);

		const handleSubmit = useCallback(() => {
			if (isOverCharacterLimit || isEditingScheduledMessage) {
				return;
			}
			onSubmit();
		}, [isOverCharacterLimit, onSubmit, isEditingScheduledMessage]);

		useTextareaDraftAndTyping({
			channelId: channel.id,
			value,
			setValue,
			draft,
			previousValueRef,
			isAutocompleteAttached,
			enabled: !disabled,
		});

		const {handleArrowUp} = useTextareaKeyboard({
			channelId: channel.id,
			isFocused,
			textareaRef,
			value,
			setValue,
			handleTextChange,
			previousValueRef,
			clearSegments,
			replyingMessage,
			editingMessage: editingMessage || null,
			getLastEditableMessage: () => MessageStore.getLastEditableMessage(channel.id) || null,
			enabled: !disabled,
		});

		const placeholderText = disabled
			? t`You do not have permission to send messages in this channel.`
			: channel.guildId != null
				? PlaceholderUtils.getChannelPlaceholder(channel.name || t`channel`, t`Message #`, Number.MAX_SAFE_INTEGER)
				: PlaceholderUtils.getDMPlaceholder(
						ChannelUtils.getDMDisplayName(channel),
						channel.isDM() ? t`Message @` : t`Message `,
						Number.MAX_SAFE_INTEGER,
					);

		useEffect(() => {
			const unsubscribe = ComponentDispatch.subscribe('FOCUS_TEXTAREA', (payload?: unknown) => {
				const {channelId, enterKeyboardMode} = (payload ?? {}) as {channelId?: string; enterKeyboardMode?: boolean};
				if (channelId && channelId !== channel.id) return;
				if (disabled) return;
				const textarea = textareaRef.current;
				if (textarea) {
					if (enterKeyboardMode) {
						KeyboardModeStore.enterKeyboardMode(true);
					} else {
						KeyboardModeStore.exitKeyboardMode();
					}
					safeFocus(textarea, true);
				}
			});
			return unsubscribe;
		}, [channel.id]);

		useEffect(() => {
			if (!canAttachFiles || disabled) return;
			const unsubscribe = ComponentDispatch.subscribe('TEXTAREA_UPLOAD_FILE', (payload?: unknown) => {
				const {channelId} = (payload ?? {}) as {channelId?: string};
				if (channelId && channelId !== channel.id) return;
				handleFileButtonClick();
			});
			return unsubscribe;
		}, [channel.id, canAttachFiles, disabled]);

		useLayoutEffect(() => {
			if (!containerRef.current) return;

			let lastWidth = -1;

			const checkButtonVisibility = () => {
				if (!containerRef.current) return;
				const containerWidthLocal = containerRef.current.offsetWidth;

				if (containerWidthLocal === lastWidth) return;
				lastWidth = containerWidthLocal;

				const shouldShowAll = containerWidthLocal > 500;
				setShowAllButtons(shouldShowAll);
			};

			const resizeObserver = new ResizeObserver(checkButtonVisibility);
			resizeObserver.observe(containerRef.current);
			checkButtonVisibility();

			return () => {
				resizeObserver.disconnect();
			};
		}, [mobileLayout.enabled]);

		const handleCancelEdit = useCallback(() => {
			setValue('');
			clearSegments();
		}, [clearSegments]);

		const handlePlusMenuClick = useCallback(
			(event: React.MouseEvent) => {
				event.preventDefault();
				event.stopPropagation();

				ContextMenuActionCreators.openFromElementTopLeft(event, () => (
					<TextareaPlusMenu
						onUploadFile={handleFileButtonClick}
						onSchedule={handleOpenScheduleModal}
						canSchedule={canScheduleMessage}
						canAttachFiles={canAttachFiles}
						canSendMessages={!disabled}
						textareaValue={value}
						onUploadAsFile={handleUploadMessageAsFile}
					/>
				));
			},
			[
				canAttachFiles,
				canScheduleMessage,
				disabled,
				handleFileButtonClick,
				handleOpenScheduleModal,
				value,
				handleUploadMessageAsFile,
			],
		);

		const hasStackedSections = Boolean(
			referencedMessage ||
				(editingMessage && mobileLayout.enabled) ||
				uploadAttachments.length > 0 ||
				hasPendingSticker,
		);

		const topBarContent =
			editingMessage && mobileLayout.enabled ? (
				<EditBar channel={channel} onCancel={handleCancelEdit} />
			) : (
				referencedMessage && (
					<ReplyBar
						replyingMessageObject={referencedMessage}
						shouldReplyMention={replyingMessage?.mentioning ?? false}
						setShouldReplyMention={(mentioning) => MessageActionCreators.setReplyMentioning(channel.id, mentioning)}
						channel={channel}
					/>
				)
			);

		const renderSection = (content: React.ReactNode) => <div className={wrapperStyles.stackSection}>{content}</div>;

		return (
			<>
				{topBarContent && renderSection(<div className={wrapperStyles.topBarContainer}>{topBarContent}</div>)}

				{hasMessageSchedulingAccess &&
					editingScheduledMessage &&
					renderSection(
						<ScheduledMessageEditBar
							scheduledLocalAt={editingScheduledMessage.scheduledLocalAt}
							timezone={editingScheduledMessage.timezone}
							onCancel={handleCancelScheduledEdit}
						/>,
					)}

				<FocusRing
					focusTarget={textareaRef}
					ringTarget={containerRef}
					offset={0}
					enabled={!disabled && AccessibilityStore.showTextareaFocusRing}
					ringClassName={styles.textareaFocusRing}
				>
					<div
						ref={containerRef}
						className={clsx(
							wrapperStyles.box,
							wrapperStyles.wrapperSides,
							styles.textareaOuter,
							mobileLayout.enabled && styles.textareaOuterMobile,
							hasStackedSections ? wrapperStyles.roundedBottom : wrapperStyles.roundedAll,
							wrapperStyles.bottomSpacing,
							disabled && wrapperStyles.disabled,
							!mobileLayout.enabled && styles.textareaOuterMinHeight,
						)}
					>
						{showAttachments && renderSection(<ChannelAttachmentArea channelId={channel.id} />)}
						{showStickers &&
							renderSection(<ChannelStickersArea channelId={channel.id} hasAttachments={hasAttachments} />)}

						{mobileLayout.enabled
							? renderSection(
									<MobileTextareaLayout
										disabled={disabled}
										canAttachFiles={canAttachFiles}
										value={value}
										placeholderText={placeholderText}
										textareaRef={textareaRef}
										scrollerRef={scrollerRef}
										isFocused={isFocused}
										isAutocompleteAttached={isAutocompleteAttached}
										autocompleteOptions={autocompleteOptions}
										selectedIndex={selectedIndex}
										channelId={channel.id}
										isSlowmodeActive={isSlowmodeActive}
										isOverCharacterLimit={isOverCharacterLimit}
										hasContent={trimmedMessageContent.length > 0}
										hasAttachments={uploadAttachments.length > 0}
										hasPendingSticker={hasPendingSticker}
										isEditingScheduledMessage={isEditingScheduledMessage}
										onFocus={() => {
											setIsFocused(true);
											setIsInputAreaFocused(true);
										}}
										onBlur={() => {
											setIsFocused(false);
											setIsInputAreaFocused(false);
										}}
										onChange={(newValue) => {
											handleTextChange(newValue, previousValueRef.current);
											setValue(newValue);
										}}
										onHeightChange={handleTextareaHeightChange}
										onCursorMove={onCursorMove}
										onArrowUp={handleArrowUp}
										onSubmit={handleSubmit}
										onAutocompleteSelect={handleSelect}
										setSelectedIndex={setSelectedIndex}
										onKeyDown={handleTextareaKeyDown}
										onPlusClick={handleOpenMobilePlusSheet}
										onEmojiClick={() => handleExpressionPickerTabToggle('emojis')}
									/>,
								)
							: renderSection(
									<div className={clsx(styles.mainWrapperDense, disabled && wrapperStyles.disabled)}>
										<div className={clsx(styles.uploadButtonColumn, styles.sideButtonPadding)}>
											<TextareaButton
												icon={PlusCircleIcon}
												label={t`Open menu`}
												onClick={handlePlusMenuClick}
												forceHover={plusContextMenuOpen}
												ref={plusButtonRef}
											/>
										</div>

										<div className={styles.contentAreaDense}>
											<Scroller
												ref={scrollerRef}
												fade={true}
												className={styles.scroller}
												key="channel-textarea-scroller"
											>
												<div className={styles.flexColumn}>
													<TextareaInputField
														channelId={channel.id}
														disabled={disabled}
														isMobile={mobileLayout.enabled}
														value={value}
														placeholder={placeholderText}
														textareaRef={textareaRef}
														isFocused={isFocused}
														isAutocompleteAttached={isAutocompleteAttached}
														autocompleteOptions={autocompleteOptions}
														selectedIndex={selectedIndex}
														onFocus={() => {
															setIsFocused(true);
															setIsInputAreaFocused(true);
														}}
														onBlur={() => {
															setIsFocused(false);
															setIsInputAreaFocused(false);
														}}
														onChange={(newValue) => {
															handleTextChange(newValue, previousValueRef.current);
															setValue(newValue);
														}}
														onHeightChange={handleTextareaHeightChange}
														onCursorMove={onCursorMove}
														onArrowUp={handleArrowUp}
														onEnter={handleSubmit}
														onAutocompleteSelect={handleSelect}
														setSelectedIndex={setSelectedIndex}
														onKeyDown={handleTextareaKeyDown}
													/>
												</div>
											</Scroller>
										</div>

										<TextareaButtons
											disabled={disabled}
											showAllButtons={showAllButtons}
											showGifButton={showGifButton}
											showMemesButton={showMemesButton}
											showStickersButton={showStickersButton}
											showEmojiButton={showEmojiButton}
											showMessageSendButton={showMessageSendButton}
											showVoiceMessageButton={false}
											expressionPickerOpen={expressionPickerOpen}
											selectedTab={selectedTab}
											isMobile={mobileLayout.enabled}
											isSlowmodeActive={isSlowmodeActive}
											isOverLimit={isOverCharacterLimit}
											hasContent={trimmedMessageContent.length > 0}
											hasAttachments={uploadAttachments.length > 0}
											expressionPickerTriggerRef={expressionPickerTriggerRef}
											invisibleExpressionPickerTriggerRef={invisibleExpressionPickerTriggerRef}
											onExpressionPickerToggle={handleExpressionPickerTabToggle}
											onSubmit={handleSubmit}
											disableSendButton={isEditingScheduledMessage}
											channelId={channel.id}
										/>
										{isScheduleModalOpen && hasMessageSchedulingAccess && (
											<ScheduleMessageModal
												onClose={() => setIsScheduleModalOpen(false)}
												onSubmit={handleScheduleSubmit}
												initialScheduledLocalAt={editingScheduledMessage?.scheduledLocalAt}
												initialTimezone={editingScheduledMessage?.timezone}
												title={isEditingScheduledMessage ? t`Reschedule Message` : undefined}
												submitLabel={isEditingScheduledMessage ? t`Update` : undefined}
												helpText={
													isEditingScheduledMessage
														? t`This will modify the existing scheduled message rather than sending immediately.`
														: undefined
												}
											/>
										)}
									</div>,
								)}

						<MessageCharacterCounter
							currentLength={trimmedMessageContent.length}
							maxLength={maxMessageLength}
							canUpgrade={maxMessageLength < premiumMaxLength}
							premiumMaxLength={premiumMaxLength}
						/>

						{isAutocompleteAttached && (
							<Autocomplete
								type={autocompleteType}
								onSelect={handleSelect}
								selectedIndex={selectedIndex}
								options={autocompleteOptions}
								setSelectedIndex={setSelectedIndex}
								referenceElement={containerRef.current}
								query={autocompleteQuery}
								attached={true}
							/>
						)}
					</div>
				</FocusRing>

				{mobileLayout.enabled && (
					<>
						<ExpressionPickerSheet
							isOpen={expressionPickerOpen}
							onClose={() => setExpressionPickerOpen(false)}
							channelId={channel.id}
							onEmojiSelect={handleEmojiSelect}
						/>
						<MobileTextareaPlusBottomSheet
							isOpen={mobilePlusSheetOpen}
							onClose={handleCloseMobilePlusSheet}
							onUploadFile={handleFileButtonClick}
							textareaValue={value}
							onUploadAsFile={handleUploadMessageAsFile}
						/>
					</>
				)}
			</>
		);
	},
);

export const ChannelTextarea = observer(({channel}: {channel: ChannelRecord}) => {
	const draft = DraftStore.getDraft(channel.id);
	const forceNoSendMessages = DeveloperOptionsStore.forceNoSendMessages;
	const forceNoAttachFiles = DeveloperOptionsStore.forceNoAttachFiles;

	const disabled = channel.isPrivate()
		? forceNoSendMessages
		: forceNoSendMessages || !PermissionStore.can(Permissions.SEND_MESSAGES, channel);
	const canAttachFiles = channel.isPrivate()
		? !forceNoAttachFiles
		: !forceNoAttachFiles && PermissionStore.can(Permissions.ATTACH_FILES, channel);
	const canEmbedLinks = channel.isPrivate() ? true : PermissionStore.can(Permissions.EMBED_LINKS, channel);
	const canSendFavoriteMemeId = canAttachFiles && canEmbedLinks;

	return (
		<ChannelTextareaContent
			key={channel.id}
			channel={channel}
			disabled={disabled}
			canAttachFiles={canAttachFiles}
			canSendFavoriteMemeId={canSendFavoriteMemeId}
			draft={draft}
		/>
	);
});
