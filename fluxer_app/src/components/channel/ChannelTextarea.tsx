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

import {PlusCircleIcon} from '@phosphor-icons/react';

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';

import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as DraftActionCreators from '~/actions/DraftActionCreators';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import * as ScheduledMessageActionCreators from '~/actions/ScheduledMessageActionCreators';

import {MAX_MESSAGE_LENGTH_NON_PREMIUM, Permissions} from '~/Constants';

import {TooManyAttachmentsModal} from '~/components/alerts/TooManyAttachmentsModal';
import {Autocomplete} from '~/components/channel/Autocomplete';
import {ChannelAttachmentArea} from '~/components/channel/ChannelAttachmentArea';
import {ChannelStickersArea} from '~/components/channel/ChannelStickersArea';
import {EditBar} from '~/components/channel/EditBar';
import {
	getMentionDescription,
	getMentionTitle,
	MentionEveryonePopout,
} from '~/components/channel/MentionEveryonePopout';
import {MessageCharacterCounter} from '~/components/channel/MessageCharacterCounter';
import {ReplyBar} from '~/components/channel/ReplyBar';
import {ScheduledMessageEditBar} from '~/components/channel/ScheduledMessageEditBar';
import {MessageInputButtonsContextMenu} from '~/components/channel/textarea/MessageInputButtonsContextMenu';
import {TextareaButton} from '~/components/channel/textarea/TextareaButton';
import {TextareaButtons} from '~/components/channel/textarea/TextareaButtons';
import {TextareaInputField} from '~/components/channel/textarea/TextareaInputField';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {ExpressionPickerSheet} from '~/components/modals/ExpressionPickerSheet';
import {ScheduleMessageModal} from '~/components/modals/ScheduleMessageModal';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {openPopout} from '~/components/uikit/Popout/Popout';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';

import {useTextareaAttachments} from '~/hooks/useCloudUpload';
import {doesEventMatchShortcut, MARKDOWN_FORMATTING_SHORTCUTS, useMarkdownKeybinds} from '~/hooks/useMarkdownKeybinds';
import {useMessageSubmission} from '~/hooks/useMessageSubmission';
import {useSlowmode} from '~/hooks/useSlowmode';
import {useTextareaAutocomplete} from '~/hooks/useTextareaAutocomplete';
import {useTextareaDraftAndTyping} from '~/hooks/useTextareaDraftAndTyping';
import {useTextareaEditing} from '~/hooks/useTextareaEditing';
import {useTextareaEmojiPicker} from '~/hooks/useTextareaEmojiPicker';
import {useTextareaExpressionHandlers} from '~/hooks/useTextareaExpressionHandlers';
import {useTextareaExpressionPicker} from '~/hooks/useTextareaExpressionPicker';
import {useTextareaKeyboard} from '~/hooks/useTextareaKeyboard';
import {useTextareaPaste} from '~/hooks/useTextareaPaste';
import {useTextareaSegments} from '~/hooks/useTextareaSegments';
import {type MentionConfirmationInfo, useTextareaSubmit} from '~/hooks/useTextareaSubmit';

import {CloudUpload} from '~/lib/CloudUpload';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import {safeFocus} from '~/lib/InputFocusManager';

import type {ChannelRecord} from '~/records/ChannelRecord';

import AccessibilityStore from '~/stores/AccessibilityStore';
import ChannelStickerStore from '~/stores/ChannelStickerStore';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import DraftStore from '~/stores/DraftStore';
import FeatureFlagStore from '~/stores/FeatureFlagStore';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import MessageEditMobileStore from '~/stores/MessageEditMobileStore';
import MessageEditStore from '~/stores/MessageEditStore';
import MessageReplyStore from '~/stores/MessageReplyStore';
import MessageStore from '~/stores/MessageStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PermissionStore from '~/stores/PermissionStore';
import ScheduledMessageEditorStore from '~/stores/ScheduledMessageEditorStore';
import SelectedGuildStore from '~/stores/SelectedGuildStore';
import UserStore from '~/stores/UserStore';

import * as ChannelUtils from '~/utils/ChannelUtils';
import {openFilePicker} from '~/utils/FilePickerUtils';
import * as FileUploadUtils from '~/utils/FileUploadUtils';
import {normalizeMessageContent} from '~/utils/MessageRequestUtils';
import * as MessageSubmitUtils from '~/utils/MessageSubmitUtils';
import * as PlaceholderUtils from '~/utils/PlaceholderUtils';

import wrapperStyles from './textarea/InputWrapper.module.css';
import styles from './textarea/TextareaInput.module.css';

const ChannelTextareaContent = observer(
	({
		channel,
		draft,
		disabled,
		canAttachFiles,
	}: {
		channel: ChannelRecord;
		draft: string | null;
		disabled: boolean;
		canAttachFiles: boolean;
	}) => {
		const {t, i18n} = useLingui();
		const [isFocused, setIsFocused] = React.useState(false);
		const [isInputAreaFocused, setIsInputAreaFocused] = React.useState(false);
		const [value, setValue] = React.useState('');
		const [showAllButtons, setShowAllButtons] = React.useState(true);
		const [_textareaHeight, setTextareaHeight] = React.useState(0);
		const [containerWidth, setContainerWidth] = React.useState(0);
		const [pendingMentionConfirmation, setPendingMentionConfirmation] = React.useState<MentionConfirmationInfo | null>(
			null,
		);
		const mentionPopoutKey = React.useMemo(() => `mention-everyone-${channel.id}`, [channel.id]);
		const mentionModalKey = React.useMemo(() => `mention-everyone-modal-${channel.id}`, [channel.id]);
		const [isScheduleModalOpen, setIsScheduleModalOpen] = React.useState(false);

		const textareaRef = React.useRef<HTMLTextAreaElement>(null);
		const expressionPickerTriggerRef = React.useRef<HTMLButtonElement>(null);
		const invisibleExpressionPickerTriggerRef = React.useRef<HTMLDivElement>(null);
		const containerRef = React.useRef<HTMLDivElement>(null);
		const scrollerRef = React.useRef<ScrollerHandle>(null);
		useMarkdownKeybinds(isFocused);

		const showGiftButton = AccessibilityStore.showGiftButton;
		const showGifButton = AccessibilityStore.showGifButton;
		const showMemesButton = AccessibilityStore.showMemesButton;
		const showStickersButton = AccessibilityStore.showStickersButton;
		const showEmojiButton = AccessibilityStore.showEmojiButton;
		const showUploadButton = AccessibilityStore.showUploadButton;
		const showMessageSendButton = AccessibilityStore.showMessageSendButton;
		const editingMessageId = MessageEditStore.getEditingMessageId(channel.id);
		const editingMobileMessageId = MessageEditMobileStore.getEditingMobileMessageId(channel.id);
		const mobileLayout = MobileLayoutStore;
		const replyingMessage = MessageReplyStore.getReplyingMessage(channel.id);
		const referencedMessage = replyingMessage ? MessageStore.getMessage(channel.id, replyingMessage.messageId) : null;
		const editingMessage = editingMobileMessageId ? MessageStore.getMessage(channel.id, editingMobileMessageId) : null;
		const currentUser = UserStore.getCurrentUser();
		const maxMessageLength = currentUser?.maxMessageLength ?? MAX_MESSAGE_LENGTH_NON_PREMIUM;

		const uploadAttachments = useTextareaAttachments(channel.id);
		const {isSlowmodeActive} = useSlowmode(channel);
		const {segmentManagerRef, previousValueRef, displayToActual, insertSegment, handleTextChange, clearSegments} =
			useTextareaSegments();
		const {handleEmojiSelect} = useTextareaEmojiPicker({
			setValue,
			textareaRef,
			insertSegment,
			previousValueRef,
			channelId: channel.id,
		});
		const scheduledMessageEditorState = ScheduledMessageEditorStore.getEditingState();
		const isEditingScheduledMessage = ScheduledMessageEditorStore.isEditingChannel(channel.id);
		const editingScheduledMessage = isEditingScheduledMessage ? scheduledMessageEditorState : null;
		const selectedGuildId = SelectedGuildStore.selectedGuildId;
		const hasMessageSchedulingAccess = FeatureFlagStore.isMessageSchedulingEnabled(selectedGuildId ?? undefined);

		const {sendMessage, sendOptimisticMessage} = useMessageSubmission({
			channel,
			referencedMessage: referencedMessage ?? null,
			replyingMessage,
			clearSegments,
		});

		const handleCancelScheduledEdit = React.useCallback(() => {
			ScheduledMessageEditorStore.stopEditing();
			DraftActionCreators.deleteDraft(channel.id);
			setValue('');
			clearSegments();
		}, [channel.id, clearSegments, setValue]);

		const handleSendMessage = React.useCallback(
			(...args: Parameters<typeof sendMessage>) => {
				setValue('');
				clearSegments();
				sendMessage(...args);
			},
			[sendMessage, clearSegments],
		);

		const handleMentionConfirmationNeeded = React.useCallback((info: MentionConfirmationInfo) => {
			setPendingMentionConfirmation(info);
		}, []);

		const handleMentionConfirm = React.useCallback(() => {
			if (pendingMentionConfirmation) {
				handleSendMessage(pendingMentionConfirmation.content, false, pendingMentionConfirmation.tts);
				setPendingMentionConfirmation(null);
			}
		}, [pendingMentionConfirmation, handleSendMessage]);

		const handleMentionCancel = React.useCallback(() => {
			setPendingMentionConfirmation(null);
			textareaRef.current?.focus();
		}, []);

		React.useEffect(() => {
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

		React.useEffect(() => {
			ComponentDispatch.safeDispatch('TEXTAREA_AUTOCOMPLETE_CHANGED', {
				channelId: channel.id,
				open: isAutocompleteAttached,
			});
		}, [channel.id, isAutocompleteAttached]);

		const trimmedMessageContent = displayToActual(value).trim();
		const hasScheduleContent = trimmedMessageContent.length > 0 || uploadAttachments.length > 0;
		const canScheduleMessage = hasMessageSchedulingAccess && !disabled && hasScheduleContent;

		useTextareaPaste({
			channel,
			textareaRef,
			segmentManagerRef,
			setValue,
			previousValueRef,
		});

		const handleOpenScheduleModal = React.useCallback(() => {
			if (!hasMessageSchedulingAccess) {
				return;
			}
			setIsScheduleModalOpen(true);
		}, [hasMessageSchedulingAccess]);

		const handleScheduleSubmit = React.useCallback(
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
			const files = await openFilePicker({multiple: true});
			const result = await FileUploadUtils.handleFileUpload(channel.id, files, uploadAttachments.length);

			if (!result.success && result.error === 'too_many_attachments') {
				ModalActionCreators.push(modal(() => <TooManyAttachmentsModal />));
			}
		};

		useTextareaExpressionHandlers({
			setValue,
			textareaRef,
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
		const isComposing = !!value.trim() || hasAttachments || hasPendingSticker;
		const isOverCharacterLimit = value.length > maxMessageLength;
		const shouldShowMobileGiftButton = mobileLayout.enabled && showGiftButton && containerWidth > 540;

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

		const handleEscapeKey = React.useCallback(
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

		const handleFormattingShortcut = React.useCallback(
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

		const handleTextareaKeyDown = React.useCallback(
			(event: React.KeyboardEvent<HTMLTextAreaElement>) => {
				handleFormattingShortcut(event);
				handleEscapeKey(event);
			},
			[handleFormattingShortcut, handleEscapeKey],
		);

		const handleSubmit = React.useCallback(() => {
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

		React.useEffect(() => {
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

		React.useEffect(() => {
			if (!canAttachFiles) return;
			const unsubscribe = ComponentDispatch.subscribe('TEXTAREA_UPLOAD_FILE', (payload?: unknown) => {
				const {channelId} = (payload ?? {}) as {channelId?: string};
				if (channelId && channelId !== channel.id) return;
				handleFileButtonClick();
			});
			return unsubscribe;
		}, [channel.id, canAttachFiles]);

		React.useLayoutEffect(() => {
			if (!containerRef.current) return;

			const checkButtonVisibility = () => {
				if (!containerRef.current) return;
				const containerWidthLocal = containerRef.current.offsetWidth;
				const shouldShowAll = containerWidthLocal > 500;
				setShowAllButtons(shouldShowAll);
				setContainerWidth(containerWidthLocal);
			};

			const resizeObserver = new ResizeObserver(checkButtonVisibility);
			resizeObserver.observe(containerRef.current);
			checkButtonVisibility();

			return () => {
				resizeObserver.disconnect();
			};
		}, [mobileLayout.enabled]);

		const handleCancelEdit = React.useCallback(() => {
			setValue('');
			clearSegments();
		}, [clearSegments]);

		const handleMessageInputButtonContextMenu = React.useCallback(
			(event: React.MouseEvent) => {
				event.preventDefault();
				event.stopPropagation();

				ContextMenuActionCreators.openFromEvent(event, () => (
					<MessageInputButtonsContextMenu canSchedule={canScheduleMessage} onSchedule={handleOpenScheduleModal} />
				));
			},
			[canScheduleMessage, handleOpenScheduleModal],
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
							hasStackedSections ? wrapperStyles.roundedBottom : wrapperStyles.roundedAll,
							wrapperStyles.bottomSpacing,
							disabled && wrapperStyles.disabled,
						)}
						style={{minHeight: 'var(--input-container-min-height)'}}
					>
						{showAttachments && renderSection(<ChannelAttachmentArea channelId={channel.id} />)}
						{showStickers &&
							renderSection(<ChannelStickersArea channelId={channel.id} hasAttachments={hasAttachments} />)}

						{renderSection(
							<div className={clsx(styles.mainWrapperDense, disabled && wrapperStyles.disabled)}>
								{!disabled && showUploadButton && canAttachFiles && (
									<div className={clsx(styles.uploadButtonColumn, styles.sideButtonPadding)}>
										<TextareaButton
											icon={PlusCircleIcon}
											label={t`Upload file`}
											onClick={handleFileButtonClick}
											onContextMenu={handleMessageInputButtonContextMenu}
											keybindAction="upload_file"
										/>
									</div>
								)}

								<div className={styles.contentAreaDense}>
									<Scroller ref={scrollerRef} fade={true} className={styles.scroller} key="channel-textarea-scroller">
										<div style={{display: 'flex', flexDirection: 'column'}}>
											<TextareaInputField
												disabled={disabled}
												isMobile={mobileLayout.enabled}
												value={value}
												placeholder={placeholderText}
												textareaRef={textareaRef}
												scrollerRef={scrollerRef}
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
												onHeightChange={setTextareaHeight}
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
									showUploadButton={showUploadButton}
									showGiftButton={showGiftButton}
									showGifButton={showGifButton}
									showMemesButton={showMemesButton}
									showStickersButton={showStickersButton}
									showEmojiButton={showEmojiButton}
									showMessageSendButton={showMessageSendButton}
									expressionPickerOpen={expressionPickerOpen}
									selectedTab={selectedTab}
									isMobile={mobileLayout.enabled}
									shouldShowMobileGiftButton={shouldShowMobileGiftButton}
									isComposing={isComposing}
									isSlowmodeActive={isSlowmodeActive}
									isOverLimit={isOverCharacterLimit}
									hasContent={!!value.trim()}
									hasAttachments={uploadAttachments.length > 0}
									expressionPickerTriggerRef={expressionPickerTriggerRef}
									invisibleExpressionPickerTriggerRef={invisibleExpressionPickerTriggerRef}
									onExpressionPickerToggle={handleExpressionPickerTabToggle}
									onSubmit={handleSubmit}
									disableSendButton={isEditingScheduledMessage}
									onContextMenu={handleMessageInputButtonContextMenu}
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
							currentLength={value.length}
							maxLength={maxMessageLength}
							isPremium={currentUser?.isPremium() ?? false}
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
					<ExpressionPickerSheet
						isOpen={expressionPickerOpen}
						onClose={() => setExpressionPickerOpen(false)}
						channelId={channel.id}
						onEmojiSelect={handleEmojiSelect}
					/>
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

	return (
		<ChannelTextareaContent
			key={channel.id}
			channel={channel}
			disabled={disabled}
			canAttachFiles={canAttachFiles}
			draft={draft}
		/>
	);
});
