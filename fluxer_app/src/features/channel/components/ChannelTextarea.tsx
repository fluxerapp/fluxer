// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Limits} from '@app/features/app/utils/UserLimits';
import {Autocomplete} from '@app/features/channel/components/Autocomplete';
import {ChannelAttachmentArea} from '@app/features/channel/components/ChannelAttachmentArea';
import {EditBar} from '@app/features/channel/components/ChannelEditBar';
import {ReplyBar} from '@app/features/channel/components/ChannelReplyBar';
import {ChannelStickersArea} from '@app/features/channel/components/ChannelStickersArea';
import {
	CHANNEL_DESCRIPTOR,
	MESSAGE_2_DESCRIPTOR,
	MESSAGE_DESCRIPTOR,
	OPEN_MENU_DESCRIPTOR,
	RESCHEDULE_MESSAGE_DESCRIPTOR,
	THIS_WILL_MODIFY_THE_EXISTING_SCHEDULED_MESSAGE_RATHER_DESCRIPTOR,
	UPDATE_DESCRIPTOR,
	YOU_DO_NOT_HAVE_PERMISSION_TO_SEND_MESSAGES_DESCRIPTOR,
} from '@app/features/channel/components/channel_textarea/shared';
import {
	getMentionDescription,
	getMentionTitle,
	MentionEveryonePopout,
} from '@app/features/channel/components/MentionEveryonePopout';
import {MessageCharacterCounter} from '@app/features/channel/components/MessageCharacterCounter';
import {ScheduledMessageEditBar} from '@app/features/channel/components/ScheduledMessageEditBar';
import wrapperStyles from '@app/features/channel/components/textarea/InputWrapper.module.css';
import {MobileTextareaLayout} from '@app/features/channel/components/textarea/MobileTextareaLayout';
import {MobileTextareaPlusBottomSheet} from '@app/features/channel/components/textarea/MobileTextareaPlusBottomSheet';
import {TextareaButton} from '@app/features/channel/components/textarea/TextareaButton';
import {TextareaButtons} from '@app/features/channel/components/textarea/TextareaButtons';
import styles from '@app/features/channel/components/textarea/TextareaInput.module.css';
import {TextareaInputField} from '@app/features/channel/components/textarea/TextareaInputField';
import {TextareaPlusMenu} from '@app/features/channel/components/textarea/TextareaPlusMenu';
import type {Channel} from '@app/features/channel/models/Channel';
import ChannelSticker from '@app/features/channel/state/ChannelSticker';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import * as CommandUtils from '@app/features/devtools/utils/CommandUtils';
import {ExpressionPickerSheet} from '@app/features/expressions/components/modals/ExpressionPickerSheet';
import {CANCEL_DESCRIPTOR, CONTINUE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as DraftCommands from '@app/features/messaging/commands/DraftCommands';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import * as ScheduledMessageCommands from '@app/features/messaging/commands/ScheduledMessageCommands';
import {TooManyAttachmentsModal} from '@app/features/messaging/components/alerts/TooManyAttachmentsModal';
import {ScheduleMessageModal} from '@app/features/messaging/components/modals/ScheduleMessageModal';
import {useTextareaAttachments} from '@app/features/messaging/hooks/useCloudUpload';
import {useMarkdownFormattingShortcut, useMarkdownKeybinds} from '@app/features/messaging/hooks/useMarkdownKeybinds';
import {type SendMessageFunction, useMessageSubmission} from '@app/features/messaging/hooks/useMessageSubmission';
import {useTextareaAutocomplete} from '@app/features/messaging/hooks/useTextareaAutocomplete';
import {useTextareaDraftAndTyping} from '@app/features/messaging/hooks/useTextareaDraftAndTyping';
import {useTextareaEditing} from '@app/features/messaging/hooks/useTextareaEditing';
import {useTextareaEmojiPicker} from '@app/features/messaging/hooks/useTextareaEmojiPicker';
import {useTextareaExpressionHandlers} from '@app/features/messaging/hooks/useTextareaExpressionHandlers';
import {useTextareaExpressionPicker} from '@app/features/messaging/hooks/useTextareaExpressionPicker';
import {useTextareaKeyboard} from '@app/features/messaging/hooks/useTextareaKeyboard';
import {useTextareaPaste} from '@app/features/messaging/hooks/useTextareaPaste';
import {useTextareaSegments} from '@app/features/messaging/hooks/useTextareaSegments';
import {useTextareaSubmit} from '@app/features/messaging/hooks/useTextareaSubmit';
import {
	createMentionConfirmationSnapshot,
	type MentionConfirmationEvent,
	type MentionConfirmationInfo,
	selectMentionConfirmationModel,
	transitionMentionConfirmationSnapshot,
} from '@app/features/messaging/state/MentionConfirmationStateMachine';
import MessageEdit from '@app/features/messaging/state/MessageEdit';
import MessageEditMobile from '@app/features/messaging/state/MessageEditMobile';
import MessageReply from '@app/features/messaging/state/MessageReply';
import Drafts from '@app/features/messaging/state/MessagingDrafts';
import Messages from '@app/features/messaging/state/MessagingMessages';
import ScheduledMessageEditor from '@app/features/messaging/state/ScheduledMessageEditor';
import TextareaSelection from '@app/features/messaging/state/TextareaSelection';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import * as FileUploadUtils from '@app/features/messaging/utils/FileUploadUtils';
import {hasVisibleMessageContent, normalizeMessageContent} from '@app/features/messaging/utils/MessageRequestUtils';
import * as MessageSubmitUtils from '@app/features/messaging/utils/MessageSubmitUtils';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {
	captureTextareaSelection,
	focusTextareaWithSelection,
} from '@app/features/messaging/utils/TextareaSelectionUtils';
import {clearTextareaWithInputEvent} from '@app/features/messaging/utils/TextareaUndoUtils';
import {resolveTypedEmojiShortcodes} from '@app/features/messaging/utils/TypedEmojiShortcodeUtils';
import Permission from '@app/features/permissions/state/Permission';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {useSlowmode} from '@app/features/slowmode/hooks/useSlowmode';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {openPopout} from '@app/features/ui/popover/PopoverPopout';
import ContextMenuState from '@app/features/ui/state/ContextMenu';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import * as PlaceholderUtils from '@app/features/ui/utils/PlaceholderUtils';
import Users from '@app/features/user/state/Users';
import {openVoiceMessageComposerModal} from '@app/features/voice/components/VoiceMessageComposerModal';
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
import {useCallback, useEffect, useId, useMemo, useRef, useState} from 'react';

const PLUS_MENU_DOUBLE_CLICK_MS = 500;

const ChannelTextareaContent = observer(
	({
		channel,
		draft,
		draftSegments,
		disabled,
		inputSuppressed = false,
		canAttachFiles,
		canSendFavoriteMemeId,
	}: {
		channel: Channel;
		draft: string | null;
		draftSegments: ReadonlyArray<MentionSegment>;
		disabled: boolean;
		inputSuppressed?: boolean;
		canAttachFiles: boolean;
		canSendFavoriteMemeId: boolean;
	}) => {
		const {i18n} = useLingui();
		const [isFocused, setIsFocused] = useState(false);
		const [isInputAreaFocused, setIsInputAreaFocused] = useState(false);
		const [value, setValue] = useState('');
		const [showAllButtons, setShowAllButtons] = useState(true);
		const [mentionConfirmationSnapshot, setMentionConfirmationSnapshot] = useState(createMentionConfirmationSnapshot);
		const mentionConfirmationModel = selectMentionConfirmationModel(mentionConfirmationSnapshot);
		const pendingMentionConfirmation = mentionConfirmationModel.pending;
		const mentionPopoutKey = useMemo(() => `mention-everyone-${channel.id}`, [channel.id]);
		const mentionModalKey = useMemo(() => `mention-everyone-modal-${channel.id}`, [channel.id]);
		const [isScheduleModalOpen, setIsScheduleModalOpen] = useState(false);
		const [mobilePlusSheetOpen, setMobilePlusSheetOpen] = useState(false);
		const autocompleteListId = useId();
		const textareaRef = useRef<HTMLTextAreaElement>(null);
		const expressionPickerTriggerRef = useRef<HTMLButtonElement>(null);
		const invisibleExpressionPickerTriggerRef = useRef<HTMLDivElement>(null);
		const containerRef = useRef<HTMLDivElement>(null);
		const scrollerRef = useRef<ScrollerHandle>(null);
		const plusButtonRef = useRef<HTMLButtonElement | null>(null);
		const plusMenuOpenedAtRef = useRef(0);
		const plusBackdropPressHandledAtRef = useRef(0);
		const plusPressRef = useRef<{wasOpen: boolean; openedAt: number}>({wasOpen: false, openedAt: 0});
		const textareaInputDisabled = disabled || inputSuppressed;
		useMarkdownKeybinds(isFocused && !inputSuppressed, {preserveEditableFocusActions: true});
		const plusContextMenuOpen = useContextMenuHoverState(plusButtonRef);
		const textareaHeightRef = useRef<number>(0);
		const handleTextareaHeightChange = useCallback((height: number) => {
			textareaHeightRef.current = height;
		}, []);
		useEffect(() => {
			if (!inputSuppressed) return;
			if (document.activeElement === textareaRef.current) {
				textareaRef.current?.blur();
			}
			setIsFocused(false);
			setIsInputAreaFocused(false);
		}, [inputSuppressed]);
		const showGifButton = Accessibility.showGifButton && RuntimeConfig.gifEnabled;
		const showMemesButton = Accessibility.showMemesButton;
		const showStickersButton = Accessibility.showStickersButton;
		const showEmojiButton = Accessibility.showEmojiButton;
		const showMessageSendButton = Accessibility.showMessageSendButton;
		const editingMessageId = MessageEdit.getEditingMessageId(channel.id);
		const editingMobileMessageId = MessageEditMobile.getEditingMobileMessageId(channel.id);
		const isEditingMessageInComposer = editingMobileMessageId != null;
		const isEditingAnyMessage = isEditingMessageInComposer || editingMessageId != null;
		const mobileLayout = MobileLayout;
		const replyingMessage = MessageReply.getReplyingMessage(channel.id);
		const referencedMessage = MessageReply.getReferencedMessage(channel.id);
		const editingMessage = editingMobileMessageId ? Messages.getMessage(channel.id, editingMobileMessageId) : null;
		const currentUser = Users.getCurrentUser();
		const maxMessageLength = currentUser?.maxMessageLength ?? MAX_MESSAGE_LENGTH_NON_PREMIUM;
		const premiumMaxLength = Limits.getPremiumValue('max_message_length', MAX_MESSAGE_LENGTH_PREMIUM);
		const maxAttachments = currentUser?.maxAttachmentsPerMessage ?? MAX_ATTACHMENTS_PER_MESSAGE;
		const uploadAttachments = useTextareaAttachments(channel.id);
		const {isSlowmodeActive} = useSlowmode(channel);
		const {
			segmentManagerRef,
			previousValueRef,
			displayToActual,
			rememberSegmentsForValue,
			prepareTextChange,
			insertSegment,
			handleTextChange,
			clearSegments,
		} = useTextareaSegments();
		const {handleEmojiSelect} = useTextareaEmojiPicker({
			setValue,
			textareaRef,
			segmentManagerRef,
			previousValueRef,
			prepareTextChange,
			channelId: channel.id,
		});
		const scheduledMessageEditorState = ScheduledMessageEditor.getEditingState();
		const isEditingScheduledMessage = ScheduledMessageEditor.isEditingChannel(channel.id);
		const editingScheduledMessage = isEditingScheduledMessage ? scheduledMessageEditorState : null;
		const hasMessageSchedulingAccess = Users.getCurrentUser()?.isStaff() ?? false;
		const rememberTextareaSelection = useCallback(() => {
			const textarea = textareaRef.current;
			if (!textarea) {
				return;
			}
			const snapshot = captureTextareaSelection(textarea);
			if (editingMobileMessageId) {
				if (!MessageEditMobile.isEditingMobile(channel.id, editingMobileMessageId)) {
					return;
				}
				TextareaSelection.setEditingSelection(channel.id, editingMobileMessageId, snapshot);
				return;
			}
			TextareaSelection.setChannelSelection(channel.id, snapshot);
		}, [channel.id, editingMobileMessageId]);
		useEffect(() => {
			if (textareaInputDisabled) return;
			const textarea = textareaRef.current;
			if (!textarea) return;
			const rememberActiveTextareaSelection = () => {
				if (document.activeElement !== textarea) return;
				rememberTextareaSelection();
			};
			textarea.addEventListener('blur', rememberTextareaSelection);
			textarea.addEventListener('input', rememberActiveTextareaSelection);
			textarea.addEventListener('keyup', rememberActiveTextareaSelection);
			textarea.addEventListener('mouseup', rememberActiveTextareaSelection);
			textarea.addEventListener('select', rememberActiveTextareaSelection);
			document.addEventListener('selectionchange', rememberActiveTextareaSelection);
			return () => {
				rememberTextareaSelection();
				textarea.removeEventListener('blur', rememberTextareaSelection);
				textarea.removeEventListener('input', rememberActiveTextareaSelection);
				textarea.removeEventListener('keyup', rememberActiveTextareaSelection);
				textarea.removeEventListener('mouseup', rememberActiveTextareaSelection);
				textarea.removeEventListener('select', rememberActiveTextareaSelection);
				document.removeEventListener('selectionchange', rememberActiveTextareaSelection);
			};
		}, [rememberTextareaSelection, textareaInputDisabled, mobileLayout.enabled]);
		const {sendMessage, sendOptimisticMessage} = useMessageSubmission({
			channel,
			referencedMessage: referencedMessage ?? null,
			replyingMessage,
			clearSegments,
		});
		const handleCancelScheduledEdit = useCallback(() => {
			ScheduledMessageEditor.stopEditing();
			DraftCommands.deleteDraft(channel.id);
			setValue('');
			clearSegments();
		}, [channel.id, clearSegments, setValue]);
		const handleSendMessage: SendMessageFunction = useCallback(
			(...args) => {
				const textarea = textareaRef.current;
				rememberSegmentsForValue(value);
				if (!textarea || !clearTextareaWithInputEvent(textarea)) {
					setValue('');
				}
				clearSegments();
				sendMessage(...args);
			},
			[sendMessage, clearSegments, rememberSegmentsForValue, setValue, value],
		);
		const sendMentionConfirmationEvent = useCallback((event: MentionConfirmationEvent) => {
			setMentionConfirmationSnapshot((snapshot) => transitionMentionConfirmationSnapshot(snapshot, event));
		}, []);
		const currentMentionConfirmationSourceContent = useMemo(
			() => displayToActual(value).trim(),
			[displayToActual, value],
		);
		const currentMentionConfirmationSourceContentRef = useRef(currentMentionConfirmationSourceContent);
		const pendingMentionConfirmationRef = useRef<MentionConfirmationInfo | null>(pendingMentionConfirmation);
		const handleSendMessageRef = useRef(handleSendMessage);
		currentMentionConfirmationSourceContentRef.current = currentMentionConfirmationSourceContent;
		pendingMentionConfirmationRef.current = pendingMentionConfirmation;
		handleSendMessageRef.current = handleSendMessage;
		useEffect(() => {
			sendMentionConfirmationEvent({type: 'mentionConfirmation.reset'});
		}, [channel.id, sendMentionConfirmationEvent]);
		useEffect(() => {
			sendMentionConfirmationEvent({
				type: 'mentionConfirmation.composerChanged',
				sourceContent: currentMentionConfirmationSourceContent,
			});
		}, [currentMentionConfirmationSourceContent, sendMentionConfirmationEvent]);
		const handleMentionConfirmationNeeded = useCallback(
			(info: MentionConfirmationInfo) => {
				sendMentionConfirmationEvent({
					type: 'mentionConfirmation.requested',
					info,
					currentSourceContent: currentMentionConfirmationSourceContentRef.current,
				});
			},
			[sendMentionConfirmationEvent],
		);
		const handleMentionConfirm = useCallback(() => {
			const pending = pendingMentionConfirmationRef.current;
			if (pending) {
				sendMentionConfirmationEvent({type: 'mentionConfirmation.confirmed'});
				handleSendMessageRef.current(pending.content, false, pending.tts);
			}
		}, [sendMentionConfirmationEvent]);
		const handleMentionCancel = useCallback(() => {
			sendMentionConfirmationEvent({type: 'mentionConfirmation.dismissed'});
			textareaRef.current?.focus();
		}, [sendMentionConfirmationEvent]);
		useEffect(() => {
			if (!pendingMentionConfirmation) {
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
				ModalCommands.pushWithKey(
					modal(() => (
						<ConfirmModal
							title={title}
							description={description}
							primaryText={i18n._(CONTINUE_DESCRIPTOR)}
							secondaryText={i18n._(CANCEL_DESCRIPTOR)}
							onPrimary={() => {
								handleMentionConfirm();
							}}
							onSecondary={() => {
								handleMentionCancel();
							}}
							data-flx="channel.channel-textarea.channel-textarea-content.confirm-modal"
						/>
					)),
					mentionModalKey,
				);
				return () => {
					ModalCommands.popWithKey(mentionModalKey);
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
							data-flx="channel.channel-textarea.channel-textarea-content.mention-everyone-popout"
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
				PopoutCommands.close(mentionPopoutKey);
			};
		}, [
			pendingMentionConfirmation,
			mentionPopoutKey,
			mentionModalKey,
			handleMentionConfirm,
			handleMentionCancel,
			textareaRef,
			mobileLayout.enabled,
			i18n,
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
			prepareTextChange,
		});
		const isAutocompleteVisible = !inputSuppressed && isAutocompleteAttached;
		useEffect(() => {
			ComponentDispatch.safeDispatch('TEXTAREA_AUTOCOMPLETE_CHANGED', {
				channelId: channel.id,
				open: isAutocompleteVisible,
			});
		}, [channel.id, isAutocompleteVisible]);
		const resolveTypedEmojiContent = useCallback(
			(content: string): string => {
				return resolveTypedEmojiShortcodes({
					content,
					channel,
					i18n,
				});
			},
			[channel, i18n],
		);
		const trimmedMessageContent = useMemo(
			() => resolveTypedEmojiContent(displayToActual(value).trim()),
			[displayToActual, resolveTypedEmojiContent, value],
		);
		const hasMessageContent = useMemo(() => hasVisibleMessageContent(trimmedMessageContent), [trimmedMessageContent]);
		const isSubmissionBlockedBySlowmode = useMemo(() => {
			if (!isSlowmodeActive || isEditingAnyMessage) return false;
			const actualContent = displayToActual(value).trim();
			const parsedCommand = CommandUtils.isCommand(actualContent) ? CommandUtils.parseCommand(actualContent) : null;
			return !parsedCommand || CommandUtils.doesCommandSendCurrentChannelMessage(parsedCommand);
		}, [displayToActual, isEditingAnyMessage, isSlowmodeActive, value]);
		const hasScheduleContent = hasMessageContent || uploadAttachments.length > 0;
		const canScheduleMessage = hasMessageSchedulingAccess && !textareaInputDisabled && hasScheduleContent;
		const handlePasteExceedsLimit = useCallback(
			async (pastedText: string) => {
				const result = await FileUploadUtils.convertTextToFile(
					channel.id,
					pastedText,
					uploadAttachments.length,
					maxAttachments,
				);
				if (!result.success && result.error === 'too_many_attachments') {
					ModalCommands.push(
						modal(() => (
							<TooManyAttachmentsModal data-flx="channel.channel-textarea.handle-paste-exceeds-limit.too-many-attachments-modal" />
						)),
					);
				}
			},
			[channel.id, uploadAttachments.length, maxAttachments],
		);
		const handlePasteFiles = useCallback(
			async (files: Array<File>) => {
				const result = await FileUploadUtils.handleFileUpload(
					channel.id,
					files,
					uploadAttachments.length,
					maxAttachments,
				);
				if (!result.success && result.error === 'too_many_attachments') {
					ModalCommands.push(
						modal(() => (
							<TooManyAttachmentsModal data-flx="channel.channel-textarea.handle-paste-files.too-many-attachments-modal" />
						)),
					);
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
			prepareTextChange,
			maxMessageLength,
			onPasteExceedsLimit: canAttachFiles ? handlePasteExceedsLimit : undefined,
			onPasteFiles: canAttachFiles ? handlePasteFiles : undefined,
			allowExceedingLimit: true,
			disabled: textareaInputDisabled,
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
				const actualContent = trimmedMessageContent;
				if (!hasVisibleMessageContent(actualContent) && uploadAttachments.length === 0) {
					return;
				}
				const normalized = normalizeMessageContent(actualContent, undefined);
				if (editingScheduledMessage) {
					await ScheduledMessageCommands.updateScheduledMessage(i18n, {
						channelId: channel.id,
						scheduledMessageId: editingScheduledMessage.scheduledMessageId,
						scheduledLocalAt,
						timezone,
						normalized,
						payload: editingScheduledMessage.payload,
						replyMentioning: replyingMessage?.mentioning,
					});
					ScheduledMessageEditor.stopEditing();
				} else {
					await ScheduledMessageCommands.scheduleMessage(i18n, {
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
				editingScheduledMessage,
				referencedMessage,
				replyingMessage?.mentioning,
				setIsScheduleModalOpen,
				setValue,
				trimmedMessageContent,
				uploadAttachments.length,
			],
		);
		const handleFileButtonClick = useCallback(async () => {
			if (textareaInputDisabled || !canAttachFiles) {
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
				ModalCommands.push(
					modal(() => (
						<TooManyAttachmentsModal data-flx="channel.channel-textarea.handle-file-button-click.too-many-attachments-modal" />
					)),
				);
				return;
			}
			if (files.length > 0) {
				textareaRef.current?.focus();
			}
		}, [canAttachFiles, channel.id, textareaInputDisabled, maxAttachments, uploadAttachments.length]);
		const handleUploadMessageAsFile = useCallback(async () => {
			if (textareaInputDisabled || !canAttachFiles) {
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
					ModalCommands.push(
						modal(() => (
							<TooManyAttachmentsModal data-flx="channel.channel-textarea.handle-upload-message-as-file.too-many-attachments-modal" />
						)),
					);
				}
				return;
			}
			setValue('');
			DraftCommands.deleteDraft(channel.id);
			textareaRef.current?.focus();
		}, [textareaInputDisabled, canAttachFiles, value, channel.id, uploadAttachments.length, maxAttachments]);
		useTextareaExpressionHandlers({
			setValue,
			textareaRef,
			canSendFavoriteMemeId,
			insertSegment,
			previousValueRef,
			prepareTextChange,
			segmentManagerRef,
			sendOptimisticMessage,
			enabled: !textareaInputDisabled,
		});
		const {expressionPickerOpen, setExpressionPickerOpen, handleExpressionPickerTabToggle, selectedTab} =
			useTextareaExpressionPicker({
				channelId: channel.id,
				onEmojiSelect: handleEmojiSelect,
				expressionPickerTriggerRef,
				invisibleExpressionPickerTriggerRef,
				textareaRef,
				enabled: !textareaInputDisabled,
			});
		useTextareaEditing({
			channelId: channel.id,
			editingMessageId: editingMessageId ?? null,
			editingMessage: editingMessage ?? null,
			isMobileEditMode: mobileLayout.enabled,
			value,
			setValue,
			textareaRef,
			previousValueRef,
		});
		const hasPendingSticker = ChannelSticker.getPendingSticker(channel.id) !== null;
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
				if (event.shiftKey) return;
				if (hasAttachments || hasPendingSticker || replyingMessage) {
					event.preventDefault();
					if (hasAttachments) {
						CloudUpload.clearTextarea(channel.id);
					}
					if (hasPendingSticker) {
						ChannelSticker.removePendingSticker(channel.id);
					}
					if (replyingMessage) {
						MessageCommands.stopReply(channel.id);
					}
					return;
				}
				if (isInputAreaFocused && KeyboardMode.keyboardModeEnabled) {
					event.preventDefault();
					KeyboardMode.exitKeyboardMode();
					return;
				}
				if (Accessibility.escapeExitsKeyboardMode) {
					KeyboardMode.exitKeyboardMode();
				}
			},
			[
				channel.id,
				hasAttachments,
				hasPendingSticker,
				replyingMessage,
				isInputAreaFocused,
				KeyboardMode.keyboardModeEnabled,
				Accessibility.escapeExitsKeyboardMode,
			],
		);
		const handleFormattingShortcut = useMarkdownFormattingShortcut({
			textareaRef,
			value,
			setValue,
			handleTextChange,
			previousValueRef,
		});
		const handleTextareaKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLTextAreaElement>) => {
				handleFormattingShortcut(event);
				handleEscapeKey(event);
			},
			[handleFormattingShortcut, handleEscapeKey],
		);
		const handleSubmit = useCallback(() => {
			if (textareaInputDisabled || isOverCharacterLimit || isEditingScheduledMessage) {
				return;
			}
			onSubmit();
		}, [textareaInputDisabled, isOverCharacterLimit, onSubmit, isEditingScheduledMessage]);
		useTextareaDraftAndTyping({
			channelId: channel.id,
			value,
			setValue,
			draft,
			draftSegments,
			previousValueRef,
			segmentManagerRef,
			isAutocompleteAttached,
			enabled: !disabled,
			typingEnabled: !textareaInputDisabled,
			isEditingMessageInComposer,
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
			getLastEditableMessage: () => Messages.getLastEditableMessage(channel.id) || null,
			enabled: !textareaInputDisabled,
		});
		const messageLabel = i18n._(MESSAGE_DESCRIPTOR);
		const messagePrefix = `${messageLabel} `;
		const placeholderText = disabled
			? i18n._(YOU_DO_NOT_HAVE_PERMISSION_TO_SEND_MESSAGES_DESCRIPTOR)
			: channel.guildId != null
				? PlaceholderUtils.getChannelPlaceholder(
						`#${channel.name || i18n._(CHANNEL_DESCRIPTOR)}`,
						messagePrefix,
						Number.MAX_SAFE_INTEGER,
					)
				: PlaceholderUtils.getDMPlaceholder(
						ChannelUtils.getDMDisplayName(channel),
						channel.isDM() ? i18n._(MESSAGE_2_DESCRIPTOR) : messagePrefix,
						Number.MAX_SAFE_INTEGER,
					);
		useEffect(() => {
			const unsubscribe = ComponentDispatch.subscribe('FOCUS_TEXTAREA', (payload?: unknown) => {
				const {channelId, enterKeyboardMode} = (payload ?? {}) as {channelId?: string; enterKeyboardMode?: boolean};
				if (channelId && channelId !== channel.id) return;
				if (textareaInputDisabled) return false;
				if (editingMessageId && !mobileLayout.enabled) return false;
				const textarea = textareaRef.current;
				if (textarea) {
					if (enterKeyboardMode) {
						KeyboardMode.enterKeyboardMode(true);
					} else {
						KeyboardMode.exitKeyboardMode();
					}
					const selection = editingMobileMessageId
						? TextareaSelection.getEditingSelection(channel.id, editingMobileMessageId)
						: TextareaSelection.getChannelSelection(channel.id);
					focusTextareaWithSelection(textarea, selection);
				}
				return true;
			});
			return unsubscribe;
		}, [channel.id, editingMessageId, editingMobileMessageId, textareaInputDisabled, mobileLayout.enabled]);
		useEffect(() => {
			if (!canAttachFiles || textareaInputDisabled) return;
			const unsubscribe = ComponentDispatch.subscribe('TEXTAREA_UPLOAD_FILE', (payload?: unknown) => {
				const {channelId} = (payload ?? {}) as {channelId?: string};
				if (channelId && channelId !== channel.id) return;
				handleFileButtonClick();
			});
			return unsubscribe;
		}, [canAttachFiles, channel.id, textareaInputDisabled, handleFileButtonClick]);
		useEffect(() => {
			const unsubscribe = ComponentDispatch.subscribe('TEXTAREA_SEND_VOICE_MESSAGE', (payload?: unknown) => {
				const {channelId} = (payload ?? {}) as {channelId?: string};
				if (channelId && channelId !== channel.id) return undefined;
				if (mobileLayout.enabled || !canAttachFiles || textareaInputDisabled) return false;
				openVoiceMessageComposerModal(channel.id);
				return true;
			});
			return unsubscribe;
		}, [canAttachFiles, channel.id, textareaInputDisabled, mobileLayout.enabled]);
		useEffect(() => {
			if (mobileLayout.enabled) {
				setShowAllButtons(true);
				return;
			}
			if (!containerRef.current || typeof ResizeObserver === 'undefined') return;
			let lastWidth = -1;
			let rafId: number | null = null;
			let pendingWidth: number | null = null;
			const updateButtonVisibility = () => {
				rafId = null;
				const containerWidthLocal = pendingWidth ?? containerRef.current?.clientWidth ?? 0;
				pendingWidth = null;
				if (containerWidthLocal === lastWidth) return;
				lastWidth = containerWidthLocal;
				const shouldShowAll = containerWidthLocal > 500;
				setShowAllButtons(shouldShowAll);
			};
			const scheduleButtonVisibilityCheck = (width?: number) => {
				if (typeof width === 'number') {
					pendingWidth = Math.round(width);
				}
				if (rafId != null) return;
				rafId = requestAnimationFrame(updateButtonVisibility);
			};
			const resizeObserver = new ResizeObserver((entries) => {
				const entry = entries[0];
				scheduleButtonVisibilityCheck(entry?.contentRect.width);
			});
			resizeObserver.observe(containerRef.current);
			scheduleButtonVisibilityCheck(containerRef.current.clientWidth);
			return () => {
				if (rafId != null) {
					cancelAnimationFrame(rafId);
				}
				resizeObserver.disconnect();
			};
		}, [mobileLayout.enabled]);
		const handleCancelEdit = useCallback(() => {
			setValue('');
			clearSegments();
		}, [clearSegments]);
		const isPlusContextMenuOpen = useCallback(() => {
			const plusButton = plusButtonRef.current;
			return Boolean(plusButton && ContextMenuState.contextMenu?.target.target === plusButton);
		}, []);
		const isFastPlusMenuRepeatPress = useCallback((timeStamp: number, openedAt = plusMenuOpenedAtRef.current) => {
			if (openedAt <= 0) return false;
			const elapsed = timeStamp - openedAt;
			return elapsed >= 0 && elapsed <= PLUS_MENU_DOUBLE_CLICK_MS;
		}, []);
		const handlePlusMenuClosed = useCallback(() => {
			plusMenuOpenedAtRef.current = 0;
		}, []);
		const closePlusMenuFromRepeatPress = useCallback(
			(timeStamp: number, openedAt = plusMenuOpenedAtRef.current) => {
				ContextMenuCommands.close();
				if (canAttachFiles && isFastPlusMenuRepeatPress(timeStamp, openedAt)) {
					void handleFileButtonClick();
				}
			},
			[canAttachFiles, handleFileButtonClick, isFastPlusMenuRepeatPress],
		);
		const handlePlusMenuBackdropMouseDown = useCallback(
			(event: React.MouseEvent<HTMLDivElement>) => {
				if (textareaInputDisabled) {
					return false;
				}
				const plusButton = plusButtonRef.current;
				if (!plusButton) {
					return false;
				}
				const rect = plusButton.getBoundingClientRect();
				const isOnPlusButton =
					event.clientX >= rect.left &&
					event.clientX <= rect.right &&
					event.clientY >= rect.top &&
					event.clientY <= rect.bottom;
				if (!isOnPlusButton) {
					return false;
				}
				plusBackdropPressHandledAtRef.current = event.timeStamp;
				closePlusMenuFromRepeatPress(event.timeStamp);
				return true;
			},
			[closePlusMenuFromRepeatPress, textareaInputDisabled],
		);
		const openPlusMenu = useCallback(
			(openedAt: number) => {
				if (textareaInputDisabled) {
					return;
				}
				const plusButton = plusButtonRef.current;
				if (!plusButton) {
					return;
				}
				const rect = plusButton.getBoundingClientRect();
				const scrollX = window.scrollX || window.pageXOffset || 0;
				const scrollY = window.scrollY || window.pageYOffset || 0;
				const point = {x: rect.left + scrollX, y: rect.top + scrollY};
				plusMenuOpenedAtRef.current = openedAt;
				plusBackdropPressHandledAtRef.current = 0;
				ContextMenuCommands.openForElement(
					plusButton,
					() => (
						<TextareaPlusMenu
							onUploadFile={handleFileButtonClick}
							onSchedule={handleOpenScheduleModal}
							canSchedule={canScheduleMessage}
							canAttachFiles={canAttachFiles}
							canSendMessages={!textareaInputDisabled}
							textareaValue={value}
							onUploadAsFile={handleUploadMessageAsFile}
							onSendVoiceMessage={
								mobileLayout.enabled
									? undefined
									: () => {
											ContextMenuCommands.close();
											if (!textareaInputDisabled) {
												openVoiceMessageComposerModal(channel.id);
											}
										}
							}
							data-flx="channel.channel-textarea.open-plus-menu.textarea-plus-menu"
						/>
					),
					{
						point,
						config: {
							align: 'bottom-left',
							onClose: handlePlusMenuClosed,
							onBackdropMouseDown: handlePlusMenuBackdropMouseDown,
						},
					},
				);
			},
			[
				canAttachFiles,
				canScheduleMessage,
				channel.id,
				handleFileButtonClick,
				handleOpenScheduleModal,
				handlePlusMenuBackdropMouseDown,
				handlePlusMenuClosed,
				handleUploadMessageAsFile,
				mobileLayout.enabled,
				textareaInputDisabled,
				value,
			],
		);
		const handlePlusMenuMouseDown = useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				const wasOpen = isPlusContextMenuOpen();
				plusPressRef.current = {wasOpen, openedAt: plusMenuOpenedAtRef.current};
				if (wasOpen) {
					event.stopPropagation();
				}
			},
			[isPlusContextMenuOpen],
		);
		const handlePlusMenuClick = useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				event.preventDefault();
				event.stopPropagation();
				if (textareaInputDisabled) {
					return;
				}
				if (
					plusBackdropPressHandledAtRef.current > 0 &&
					Math.abs(event.timeStamp - plusBackdropPressHandledAtRef.current) < 250
				) {
					plusBackdropPressHandledAtRef.current = 0;
					return;
				}
				const press = plusPressRef.current;
				plusPressRef.current = {wasOpen: false, openedAt: 0};
				const wasOpen = press.wasOpen || isPlusContextMenuOpen();
				const openedAt = press.openedAt || plusMenuOpenedAtRef.current;
				if (wasOpen) {
					closePlusMenuFromRepeatPress(event.timeStamp, openedAt);
					return;
				}
				openPlusMenu(event.timeStamp);
			},
			[closePlusMenuFromRepeatPress, isPlusContextMenuOpen, textareaInputDisabled, openPlusMenu],
		);
		const hasStackedSections = Boolean(
			referencedMessage ||
				(editingMessage && mobileLayout.enabled) ||
				uploadAttachments.length > 0 ||
				hasPendingSticker,
		);
		const topBarContent =
			editingMessage && mobileLayout.enabled ? (
				<EditBar
					channel={channel}
					onCancel={handleCancelEdit}
					data-flx="channel.channel-textarea.channel-textarea-content.edit-bar"
				/>
			) : (
				referencedMessage && (
					<ReplyBar
						replyingMessageObject={referencedMessage}
						shouldReplyMention={replyingMessage?.mentioning ?? false}
						setShouldReplyMention={(mentioning) => MessageCommands.setReplyMentioning(channel.id, mentioning)}
						channel={channel}
						data-flx="channel.channel-textarea.channel-textarea-content.reply-bar"
					/>
				)
			);
		const renderSection = (content: React.ReactNode, sectionClassName?: string) => (
			<div
				className={clsx(wrapperStyles.stackSection, sectionClassName)}
				data-flx="channel.channel-textarea.render-section.div"
			>
				{content}
			</div>
		);
		return (
			<>
				{topBarContent &&
					renderSection(
						<div
							className={wrapperStyles.topBarContainer}
							data-flx="channel.channel-textarea.channel-textarea-content.div"
						>
							{topBarContent}
						</div>,
					)}
				{hasMessageSchedulingAccess &&
					editingScheduledMessage &&
					renderSection(
						<ScheduledMessageEditBar
							scheduledLocalAt={editingScheduledMessage.scheduledLocalAt}
							timezone={editingScheduledMessage.timezone}
							onCancel={handleCancelScheduledEdit}
							data-flx="channel.channel-textarea.channel-textarea-content.scheduled-message-edit-bar"
						/>,
					)}
				<FocusRing
					focusTarget={textareaRef}
					ringTarget={containerRef}
					offset={0}
					enabled={!textareaInputDisabled && Accessibility.showTextareaFocusRing}
					ringClassName={styles.textareaFocusRing}
					data-flx="channel.channel-textarea.channel-textarea-content.focus-ring"
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
							textareaInputDisabled && wrapperStyles.disabled,
							!mobileLayout.enabled && styles.textareaOuterMinHeight,
						)}
						data-flx="channel.channel-textarea.channel-textarea-content.textarea-outer"
					>
						{showAttachments &&
							renderSection(
								<ChannelAttachmentArea
									channelId={channel.id}
									data-flx="channel.channel-textarea.channel-textarea-content.channel-attachment-area"
								/>,
								styles.collapsibleSection,
							)}
						{showStickers &&
							renderSection(
								<ChannelStickersArea
									channelId={channel.id}
									hasAttachments={hasAttachments}
									data-flx="channel.channel-textarea.channel-textarea-content.channel-stickers-area"
								/>,
								styles.collapsibleSection,
							)}
						{mobileLayout.enabled
							? renderSection(
									<MobileTextareaLayout
										disabled={textareaInputDisabled}
										canAttachFiles={canAttachFiles}
										value={value}
										placeholderText={placeholderText}
										textareaRef={textareaRef}
										scrollerRef={scrollerRef}
										isFocused={isFocused}
										isAutocompleteAttached={isAutocompleteVisible}
										autocompleteListId={autocompleteListId}
										autocompleteOptions={autocompleteOptions}
										selectedIndex={selectedIndex}
										channelId={channel.id}
										isSlowmodeActive={isSubmissionBlockedBySlowmode}
										isOverCharacterLimit={isOverCharacterLimit}
										isEditingMessage={isEditingAnyMessage}
										hasContent={hasMessageContent}
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
										onChange={(newValue, inputType, hint) => {
											handleTextChange(newValue, previousValueRef.current, inputType, hint);
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
										data-flx="channel.channel-textarea.channel-textarea-content.mobile-textarea-layout.submit"
									/>,
									styles.inputSection,
								)
							: renderSection(
									<div
										className={clsx(styles.mainWrapperDense, textareaInputDisabled && wrapperStyles.disabled)}
										data-flx="channel.channel-textarea.channel-textarea-content.main-wrapper-dense"
									>
										<div
											className={clsx(styles.uploadButtonColumn, styles.sideButtonPadding)}
											data-flx="channel.channel-textarea.channel-textarea-content.upload-button-column"
										>
											<TextareaButton
												icon={PlusCircleIcon}
												label={i18n._(OPEN_MENU_DESCRIPTOR)}
												disabled={textareaInputDisabled}
												aria-hidden={textareaInputDisabled ? true : undefined}
												onMouseDown={handlePlusMenuMouseDown}
												onClick={handlePlusMenuClick}
												forceHover={plusContextMenuOpen}
												className={plusContextMenuOpen ? styles.plusButtonAboveBackdrop : undefined}
												ref={plusButtonRef}
												data-flx="channel.channel-textarea.channel-textarea-content.textarea-button.plus-menu-click"
											/>
										</div>
										<div
											className={styles.contentAreaDense}
											data-flx="channel.channel-textarea.channel-textarea-content.content-area-dense"
										>
											<Scroller
												ref={scrollerRef}
												fade={true}
												className={styles.scroller}
												key="channel-textarea-scroller"
												data-flx="channel.channel-textarea.channel-textarea-content.scroller"
											>
												<div
													className={styles.flexColumn}
													data-flx="channel.channel-textarea.channel-textarea-content.flex-column"
												>
													<TextareaInputField
														channelId={channel.id}
														disabled={textareaInputDisabled}
														isMobile={mobileLayout.enabled}
														value={value}
														placeholder={placeholderText}
														textareaRef={textareaRef}
														isFocused={isFocused}
														isAutocompleteAttached={isAutocompleteVisible}
														autocompleteListId={autocompleteListId}
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
														onChange={(newValue, inputType, hint) => {
															handleTextChange(newValue, previousValueRef.current, inputType, hint);
															setValue(newValue);
														}}
														onHeightChange={handleTextareaHeightChange}
														onCursorMove={onCursorMove}
														onArrowUp={handleArrowUp}
														onEnter={handleSubmit}
														onAutocompleteSelect={handleSelect}
														setSelectedIndex={setSelectedIndex}
														onKeyDown={handleTextareaKeyDown}
														data-flx="channel.channel-textarea.channel-textarea-content.textarea-input-field.text-change"
													/>
												</div>
											</Scroller>
										</div>
										<TextareaButtons
											disabled={textareaInputDisabled}
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
											isSlowmodeActive={isSubmissionBlockedBySlowmode}
											isOverLimit={isOverCharacterLimit}
											hasContent={hasMessageContent}
											hasAttachments={uploadAttachments.length > 0}
											expressionPickerTriggerRef={expressionPickerTriggerRef}
											invisibleExpressionPickerTriggerRef={invisibleExpressionPickerTriggerRef}
											onExpressionPickerToggle={handleExpressionPickerTabToggle}
											onSubmit={handleSubmit}
											disableSendButton={isEditingScheduledMessage}
											channelId={channel.id}
											data-flx="channel.channel-textarea.channel-textarea-content.textarea-buttons.submit"
										/>
										{isScheduleModalOpen && hasMessageSchedulingAccess && (
											<ScheduleMessageModal
												onClose={() => setIsScheduleModalOpen(false)}
												onSubmit={handleScheduleSubmit}
												initialScheduledLocalAt={editingScheduledMessage?.scheduledLocalAt}
												initialTimezone={editingScheduledMessage?.timezone}
												title={isEditingScheduledMessage ? i18n._(RESCHEDULE_MESSAGE_DESCRIPTOR) : undefined}
												submitLabel={isEditingScheduledMessage ? i18n._(UPDATE_DESCRIPTOR) : undefined}
												helpText={
													isEditingScheduledMessage
														? i18n._(THIS_WILL_MODIFY_THE_EXISTING_SCHEDULED_MESSAGE_RATHER_DESCRIPTOR)
														: undefined
												}
												data-flx="channel.channel-textarea.channel-textarea-content.schedule-message-modal.schedule-submit"
											/>
										)}
									</div>,
									styles.inputSection,
								)}
						<MessageCharacterCounter
							currentLength={trimmedMessageContent.length}
							maxLength={maxMessageLength}
							canUpgrade={maxMessageLength < premiumMaxLength}
							premiumMaxLength={premiumMaxLength}
							data-flx="channel.channel-textarea.channel-textarea-content.message-character-counter"
						/>
						{isAutocompleteVisible && (
							<Autocomplete
								type={autocompleteType}
								onSelect={handleSelect}
								selectedIndex={selectedIndex}
								options={autocompleteOptions}
								setSelectedIndex={setSelectedIndex}
								referenceElement={containerRef.current}
								query={autocompleteQuery}
								attached={true}
								listboxId={autocompleteListId}
								data-flx="channel.channel-textarea.channel-textarea-content.autocomplete.select"
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
							data-flx="channel.channel-textarea.channel-textarea-content.expression-picker-sheet"
						/>
						<MobileTextareaPlusBottomSheet
							isOpen={mobilePlusSheetOpen}
							onClose={handleCloseMobilePlusSheet}
							onUploadFile={handleFileButtonClick}
							textareaValue={value}
							onUploadAsFile={handleUploadMessageAsFile}
							data-flx="channel.channel-textarea.channel-textarea-content.mobile-textarea-plus-bottom-sheet"
						/>
					</>
				)}
			</>
		);
	},
);
export const ChannelTextarea = observer(
	({channel, inputSuppressed = false}: {channel: Channel; inputSuppressed?: boolean}) => {
		const draft = Drafts.getDraft(channel.id);
		const draftSegments = Drafts.getDraftSegments(channel.id);
		const forceNoSendMessages = DeveloperOptions.forceNoSendMessages;
		const forceNoAttachFiles = DeveloperOptions.forceNoAttachFiles;
		const disabled = channel.isPrivate()
			? forceNoSendMessages
			: forceNoSendMessages ||
				!Permission.can(Permissions.SEND_MESSAGES, channel) ||
				GuildMembers.isUserTimedOut(channel.guildId ?? null, Users.currentUser?.id);
		const canAttachFiles = channel.isPrivate()
			? !forceNoAttachFiles
			: !forceNoAttachFiles && Permission.can(Permissions.ATTACH_FILES, channel);
		const canEmbedLinks = channel.isPrivate() ? true : Permission.can(Permissions.EMBED_LINKS, channel);
		const canSendFavoriteMemeId = canAttachFiles && canEmbedLinks;
		return (
			<ChannelTextareaContent
				key={channel.id}
				channel={channel}
				disabled={disabled}
				inputSuppressed={inputSuppressed}
				canAttachFiles={canAttachFiles}
				canSendFavoriteMemeId={canSendFavoriteMemeId}
				draft={draft}
				draftSegments={draftSegments}
				data-flx="channel.channel-textarea.channel-textarea-content"
			/>
		);
	},
);
