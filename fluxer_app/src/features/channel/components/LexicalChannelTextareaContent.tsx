// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Limits} from '@app/features/app/utils/UserLimits';
import {fetchSlowmodeState} from '@app/features/channel/commands/ChannelCommands';
import {ChannelAttachmentArea} from '@app/features/channel/components/ChannelAttachmentArea';
import {
	type ChannelComposerDismissalRequest,
	requestChannelComposerAffordanceDismissal,
} from '@app/features/channel/components/ChannelComposerDismissal';
import {EditBar} from '@app/features/channel/components/ChannelEditBar';
import {ReplyBar} from '@app/features/channel/components/ChannelReplyBar';
import {ChannelStickersArea} from '@app/features/channel/components/ChannelStickersArea';
import {
	CHANNEL_DESCRIPTOR,
	MESSAGE_2_DESCRIPTOR,
	MESSAGE_DESCRIPTOR,
	OPEN_MENU_DESCRIPTOR,
	YOU_DO_NOT_HAVE_PERMISSION_TO_SEND_MESSAGES_DESCRIPTOR,
} from '@app/features/channel/components/channel_textarea/shared';
import lexicalStyles from '@app/features/channel/components/LexicalChannelTextareaContent.module.css';
import {
	getMentionDescription,
	getMentionTitle,
	MentionEveryonePopout,
} from '@app/features/channel/components/MentionEveryonePopout';
import {MessageCharacterCounter} from '@app/features/channel/components/MessageCharacterCounter';
import {SlashCommandParamBar} from '@app/features/channel/components/SlashCommandParamBar';
import {SlowmodeIndicator} from '@app/features/channel/components/SlowmodeIndicator';
import {TypingUsers, usePresentableTypingUsers} from '@app/features/channel/components/TypingUsers';
import wrapperStyles from '@app/features/channel/components/textarea/InputWrapper.module.css';
import {MobileTextareaPlusBottomSheet} from '@app/features/channel/components/textarea/MobileTextareaPlusBottomSheet';
import {TextareaButton} from '@app/features/channel/components/textarea/TextareaButton';
import {TextareaButtons} from '@app/features/channel/components/textarea/TextareaButtons';
import styles from '@app/features/channel/components/textarea/TextareaInput.module.css';
import {TextareaPlusMenu} from '@app/features/channel/components/textarea/TextareaPlusMenu';
import {useChannelComposerGlobalShortcuts} from '@app/features/channel/components/useChannelComposerGlobalShortcuts';
import {useChannelComposerPaste} from '@app/features/channel/components/useChannelComposerPaste';
import type {Channel} from '@app/features/channel/models/Channel';
import ChannelSearch from '@app/features/channel/state/ChannelSearch';
import ChannelSticker from '@app/features/channel/state/ChannelSticker';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelDisplayUtils from '@app/features/channel/utils/ChannelDisplayUtils';
import * as CommandUtils from '@app/features/devtools/utils/CommandUtils';
import * as ExpressionPickerCommands from '@app/features/emoji/commands/ExpressionPickerCommands';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionPickerSheet} from '@app/features/expressions/components/modals/ExpressionPickerSheet';
import GuildGuilds from '@app/features/guild/state/Guilds';
import {CANCEL_DESCRIPTOR, CONTINUE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Keybind from '@app/features/input/state/InputKeybind';
import type {ComposerHandle, ComposerSelectionRange} from '@app/features/lexical/composer/ComposerHandle';
import {insertComposerEmoji} from '@app/features/lexical/composer/ComposerInsertion';
import {LexicalComposerInput} from '@app/features/lexical/composer/LexicalComposerInput';
import {
	LexicalMessageCommandResolutionStatus,
	LexicalMessageCommandResolver,
} from '@app/features/lexical/composer/LexicalMessageCommand';
import type {SlashCommandComposerState} from '@app/features/lexical/composer/slashSlots';
import type {SlashSlotResolvers} from '@app/features/lexical/composer/slashSlotValidation';
import {useLexicalAutocomplete} from '@app/features/lexical/composer/useLexicalAutocomplete';
import * as DraftCommands from '@app/features/messaging/commands/DraftCommands';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {showAttachmentPermissionDeniedModal} from '@app/features/messaging/components/alerts/AttachmentPermissionDeniedModal';
import {FileSizeTooLargeModal} from '@app/features/messaging/components/alerts/FileSizeTooLargeModal';
import {TooManyAttachmentsModal} from '@app/features/messaging/components/alerts/TooManyAttachmentsModal';
import {useTextareaAttachments} from '@app/features/messaging/hooks/useCloudUpload';
import {doesEventMatchShortcut, useMarkdownKeybinds} from '@app/features/messaging/hooks/useMarkdownKeybinds';
import {type SendMessageFunction, useMessageSubmission} from '@app/features/messaging/hooks/useMessageSubmission';
import {useTextareaDraftAndTyping} from '@app/features/messaging/hooks/useTextareaDraftAndTyping';
import {useTextareaEditing} from '@app/features/messaging/hooks/useTextareaEditing';
import {useTextareaExpressionHandlers} from '@app/features/messaging/hooks/useTextareaExpressionHandlers';
import {useTextareaExpressionPicker} from '@app/features/messaging/hooks/useTextareaExpressionPicker';
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
import Messages from '@app/features/messaging/state/MessagingMessages';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {canAttachFilesInChannel} from '@app/features/messaging/utils/AttachmentPermissionUtils';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import * as FileUploadUtils from '@app/features/messaging/utils/FileUploadUtils';
import {hasVisibleMessageContent} from '@app/features/messaging/utils/MessageRequestUtils';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {
	resolveTypedEmojiShortcodes,
	resolveTypedEmojiToken,
} from '@app/features/messaging/utils/TypedEmojiShortcodeUtils';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {useSlowmode} from '@app/features/slowmode/hooks/useSlowmode';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {SlashCommandIcon} from '@app/features/ui/components/icons/SlashCommandIcon';
import {openPopout} from '@app/features/ui/popover/PopoverPopout';
import ContextMenuState from '@app/features/ui/state/ContextMenu';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import * as PlaceholderUtils from '@app/features/ui/utils/PlaceholderUtils';
import Users from '@app/features/user/state/Users';
import {openVoiceMessageComposerModal} from '@app/features/voice/components/VoiceMessageComposerModal';
import {flxElementClassName} from '@app/lib/react';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useLayoutEffect, useMemo, useRef, useState} from 'react';

const PLUS_MENU_DOUBLE_CLICK_MS = 500;
const MESSAGE_SCROLLER_SELECTOR = '[data-fluxer-scroll-container="true"]';
const MESSAGE_SCROLLER_BOTTOM_THRESHOLD = 16;
const getActiveMessageScroller = (): HTMLElement | null =>
	document.querySelector<HTMLElement>(MESSAGE_SCROLLER_SELECTOR);
const isMessageScrollerNearBottom = (scrollerElement: HTMLElement): boolean =>
	scrollerElement.scrollHeight <=
	scrollerElement.scrollTop + scrollerElement.clientHeight + MESSAGE_SCROLLER_BOTTOM_THRESHOLD;
const CLEAR_COMMAND_DESCRIPTOR = msg({
	message: 'Clear command',
	comment: 'Accessible label for the composer button that clears the slash command currently being composed.',
});

const PLUS_ICON_PROPS = {weight: 'bold'} as const;

export const LexicalChannelTextareaContent = observer(
	({
		channel,
		draft,
		draftSegments,
		disabled,
		inputSuppressed = false,
		messageSafetyGateActive = false,
	}: {
		channel: Channel;
		draft: string | null;
		draftSegments: ReadonlyArray<MentionSegment>;
		disabled: boolean;
		inputSuppressed?: boolean;
		messageSafetyGateActive?: boolean;
	}) => {
		const {i18n} = useLingui();
		const editingMessageId = MessageEdit.getEditingMessageId(channel.id);
		const editingMobileMessageId = MessageEditMobile.getEditingMobileMessageId(channel.id);
		const isEditingMessageInComposer = editingMobileMessageId != null;
		let initialDraftDisplay = '';
		if (!disabled && !isEditingMessageInComposer && draft !== null) {
			initialDraftDisplay = draft;
		}
		const [isFocused, setIsFocused] = useState(false);
		const [isInputAreaFocused, setIsInputAreaFocused] = useState(false);
		const initialDraftRef = useRef({
			display: initialDraftDisplay,
			segments: initialDraftDisplay.length === 0 ? [] : draftSegments.map((segment) => ({...segment})),
		});
		const [value, setValue] = useState(() => initialDraftRef.current.display);
		const [slashCommandState, setSlashCommandState] = useState<SlashCommandComposerState>({
			hasSlots: false,
			activeSlot: null,
		});
		const [showAllButtons, setShowAllButtons] = useState(true);
		const [mentionConfirmationSnapshot, setMentionConfirmationSnapshot] = useState(createMentionConfirmationSnapshot);
		const mentionConfirmationModel = selectMentionConfirmationModel(mentionConfirmationSnapshot);
		const pendingMentionConfirmation = mentionConfirmationModel.pending;
		const mentionPopoutKey = useMemo(() => `mention-everyone-${channel.id}`, [channel.id]);
		const mentionModalKey = useMemo(() => `mention-everyone-modal-${channel.id}`, [channel.id]);
		const [mobilePlusSheetOpen, setMobilePlusSheetOpen] = useState(false);
		const autocompleteListId = useId();
		const handleRef = useRef<ComposerHandle | null>(null);
		const editorDisplayRef = useRef(initialDraftRef.current.display);
		const editableRef = useRef<HTMLDivElement | null>(null);
		const nullTextareaRef = useRef<HTMLTextAreaElement>(null);
		const expressionPickerTriggerRef = useRef<HTMLButtonElement>(null);
		const invisibleExpressionPickerTriggerRef = useRef<HTMLDivElement>(null);
		const containerRef = useRef<HTMLDivElement>(null);
		const contentAreaRef = useRef<HTMLElement | null>(null);
		const plusButtonRef = useRef<HTMLButtonElement | null>(null);
		const plusMenuOpenedAtRef = useRef(0);
		const plusBackdropPressHandledAtRef = useRef(0);
		const plusPressRef = useRef<{wasOpen: boolean; openedAt: number}>({wasOpen: false, openedAt: 0});
		const textareaInputDisabled = disabled || inputSuppressed || messageSafetyGateActive;
		useMarkdownKeybinds(isFocused && !textareaInputDisabled, {preserveEditableFocusActions: true});
		const plusContextMenuOpen = useContextMenuHoverState(plusButtonRef);
		useEffect(() => {
			editableRef.current = containerRef.current
				? containerRef.current.querySelector<HTMLDivElement>('[data-channel-textarea]')
				: null;
		}, []);
		useEffect(() => {
			const contentArea = contentAreaRef.current;
			if (contentArea == null) {
				return;
			}
			const handleContentMouseDown = (event: MouseEvent): void => {
				const editable = contentArea.querySelector<HTMLElement>('[data-channel-textarea]');
				const target = event.target;
				const defaultView = contentArea.ownerDocument.defaultView;
				if (editable == null || defaultView == null || !(target instanceof defaultView.Node)) {
					return;
				}
				if (target === editable || editable.contains(target)) {
					return;
				}
				event.preventDefault();
				const handle = handleRef.current;
				if (handle != null) {
					handle.focus();
				}
			};
			contentArea.addEventListener('mousedown', handleContentMouseDown);
			return () => contentArea.removeEventListener('mousedown', handleContentMouseDown);
		}, []);
		useEffect(() => {
			if (!inputSuppressed) return;
			if (editableRef.current && document.activeElement === editableRef.current) {
				editableRef.current.blur();
			}
			setIsFocused(false);
			setIsInputAreaFocused(false);
		}, [inputSuppressed]);
		const showGifButton = Accessibility.showGifButton && RuntimeConfig.gifEnabled;
		const showMemesButton = Accessibility.showMemesButton;
		const showStickersButton = Accessibility.showStickersButton;
		const showEmojiButton = Accessibility.showEmojiButton;
		const showMessageSendButton = Accessibility.showMessageSendButton;
		const mobileLayout = MobileLayout;
		const replyingMessage = MessageReply.getReplyingMessage(channel.id);
		const referencedMessage = MessageReply.getReferencedMessage(channel.id);
		const editingMessage = editingMobileMessageId ? Messages.getMessage(channel.id, editingMobileMessageId) : null;
		const editingMessageForComposer = editingMessage === undefined ? null : editingMessage;
		const maxMessageLength = Limits.getMaxMessageLength();
		const premiumMaxLength = Limits.getStockValue('max_message_length', maxMessageLength);
		const maxAttachments = Limits.getMaxAttachmentsPerMessage();
		const uploadAttachments = useTextareaAttachments(channel.id);
		const {isSlowmodeActive, slowmodeRemaining, isSlowmodeEnabled, isSlowmodeImmune} = useSlowmode(channel);
		const rateLimitPerUser = channel.rateLimitPerUser === undefined ? 0 : channel.rateLimitPerUser;
		const shouldFetchSlowmode = Boolean(channel.guildId) && rateLimitPerUser > 0;
		useEffect(() => {
			if (!shouldFetchSlowmode) return;
			void fetchSlowmodeState(channel.id);
		}, [channel.id, shouldFetchSlowmode]);
		const {
			segmentManagerRef,
			previousValueRef,
			displayToActual,
			rememberSegmentsForValue,
			prepareTextChange,
			insertSegment,
			clearSegments,
		} = useTextareaSegments();
		const segmentsSeededRef = useRef(false);
		if (!segmentsSeededRef.current) {
			segmentsSeededRef.current = true;
			segmentManagerRef.current.setSegments(initialDraftRef.current.segments.map((segment) => ({...segment})));
			previousValueRef.current = initialDraftRef.current.display;
			rememberSegmentsForValue(initialDraftRef.current.display, initialDraftRef.current.segments);
		}
		const handleEditorChange = useCallback(
			(display: string, segments: Array<MentionSegment>) => {
				editorDisplayRef.current = display;
				segmentManagerRef.current.setSegments(segments);
				previousValueRef.current = display;
				rememberSegmentsForValue(display, segments);
				setValue(display);
			},
			[previousValueRef, rememberSegmentsForValue, segmentManagerRef],
		);
		useLayoutEffect(() => {
			if (value === editorDisplayRef.current) {
				return;
			}
			editorDisplayRef.current = value;
			const handle = handleRef.current;
			if (handle !== null) {
				handle.hydrate(value, segmentManagerRef.current.getSegmentsCopy());
			}
		}, [value, segmentManagerRef]);
		const handleEmojiSelect = useCallback(
			(emoji: FlatEmoji, shiftKey?: boolean): boolean => {
				const didInsert = insertComposerEmoji(handleRef.current, emoji);
				if (didInsert && !shiftKey) {
					ExpressionPickerCommands.close();
					PopoutCommands.close(`expression-picker-${channel.id}`);
				}
				return didInsert;
			},
			[channel.id],
		);
		const {sendMessage, sendOptimisticMessage} = useMessageSubmission({
			channel,
			referencedMessage,
			replyingMessage,
			clearSegments,
		});
		const handleSendMessage: SendMessageFunction = useCallback(
			(...args) => {
				if (!sendMessage(...args)) {
					return false;
				}
				rememberSegmentsForValue(value);
				const handle = handleRef.current;
				if (handle !== null) {
					handle.clear();
				}
				setValue('');
				clearSegments();
				return true;
			},
			[sendMessage, clearSegments, rememberSegmentsForValue, value],
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
			if (!pending) {
				return;
			}
			const pendingSticker = ChannelSticker.getPendingSticker(channel.id);
			const stickerItems = pendingSticker ? [pendingSticker.toJSON()] : undefined;
			let didSend = false;
			if (pending.tts) {
				didSend = handleSendMessageRef.current(pending.content, false, true, stickerItems);
			} else if (stickerItems) {
				didSend = handleSendMessageRef.current(pending.content, false, stickerItems);
			} else {
				didSend = handleSendMessageRef.current(pending.content, false);
			}
			if (!didSend) return;
			sendMentionConfirmationEvent({type: 'mentionConfirmation.confirmed'});
			if (pendingSticker) {
				ChannelSticker.removePendingSticker(channel.id);
			}
		}, [channel.id, sendMentionConfirmationEvent]);
		const handleMentionCancel = useCallback(() => {
			sendMentionConfirmationEvent({type: 'mentionConfirmation.dismissed'});
			const handle = handleRef.current;
			if (handle !== null) {
				handle.focus();
			}
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
						/>
					),
					position: 'top-start',
					offsetMainAxis: 8,
					shouldAutoUpdate: true,
					returnFocusRef: editableRef,
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
			mobileLayout.enabled,
			i18n,
		]);
		const {
			autocompleteQuery,
			autocompleteOptions,
			autocompleteType,
			isAutocompleteAttached,
			isSlotMenu,
			onCursorMove,
			handleSelect,
		} = useLexicalAutocomplete({
			channel,
			handleRef,
			i18n,
		});
		const isAutocompleteVisible = !textareaInputDisabled && isAutocompleteAttached;
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
		const composerEmojiResolver = useCallback(
			(shortcodeName: string) => {
				let guildId: string | null = null;
				if (channel.guildId !== undefined) {
					guildId = channel.guildId;
				}
				return resolveTypedEmojiToken(shortcodeName, channel, guildId, i18n);
			},
			[channel, i18n],
		);
		const trimmedMessageContent = useMemo(
			() => resolveTypedEmojiContent(displayToActual(value).trim()),
			[displayToActual, resolveTypedEmojiContent, value],
		);
		const hasMessageContent = useMemo(() => hasVisibleMessageContent(trimmedMessageContent), [trimmedMessageContent]);
		const isSubmissionBlockedBySlowmode = useMemo(() => {
			if (!isSlowmodeActive || isEditingMessageInComposer) {
				return false;
			}
			const resolution = LexicalMessageCommandResolver.resolve(handleRef.current);
			if (resolution.status === LexicalMessageCommandResolutionStatus.NO_COMMAND) {
				return true;
			}
			if (resolution.status === LexicalMessageCommandResolutionStatus.INVALID_COMMAND) {
				return false;
			}
			return CommandUtils.doesCommandSendCurrentChannelMessage(resolution.command);
		}, [handleRef, isEditingMessageInComposer, isSlowmodeActive, value]);
		useChannelComposerPaste({
			channel,
			handleRef,
			editableRef,
			isFocused,
			maxMessageLength,
			maxAttachments,
			uploadAttachments,
			textareaInputDisabled,
		});
		const handleOpenMobilePlusSheet = useCallback(() => {
			setMobilePlusSheetOpen(true);
		}, []);
		const handleCloseMobilePlusSheet = useCallback(() => {
			setMobilePlusSheetOpen(false);
		}, []);
		const handleFileButtonClick = useCallback(async () => {
			if (textareaInputDisabled) {
				return;
			}
			const files = await openFilePicker({multiple: true});
			if (files.length === 0) {
				return;
			}
			if (!canAttachFilesInChannel(channel)) {
				showAttachmentPermissionDeniedModal(channel);
				return;
			}
			const result = await FileUploadUtils.handleFileUpload(
				channel.id,
				files,
				uploadAttachments.length,
				maxAttachments,
			);
			if (!result.success) {
				if (result.error === 'too_many_attachments') {
					ModalCommands.push(modal(() => <TooManyAttachmentsModal />));
				} else if (result.error === 'file_size_too_large') {
					ModalCommands.push(modal(() => <FileSizeTooLargeModal oversizedFileCount={result.oversizedFileCount} />));
				}
				return;
			}
			if (files.length > 0) {
				const handle = handleRef.current;
				if (handle !== null) {
					handle.focus();
				}
			}
		}, [channel, textareaInputDisabled, maxAttachments, uploadAttachments.length]);
		const handleUploadMessageAsFile = useCallback(async () => {
			if (textareaInputDisabled) {
				return;
			}
			if (!canAttachFilesInChannel(channel)) {
				showAttachmentPermissionDeniedModal(channel);
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
					ModalCommands.push(modal(() => <TooManyAttachmentsModal />));
				} else if (result.error === 'file_size_too_large') {
					ModalCommands.push(modal(() => <FileSizeTooLargeModal oversizedFileCount={result.oversizedFileCount} />));
				}
				return;
			}
			const handle = handleRef.current;
			if (handle !== null) {
				handle.clear();
			}
			setValue('');
			DraftCommands.deleteDraft(channel.id);
			if (handle !== null) {
				handle.focus();
			}
		}, [textareaInputDisabled, value, channel, uploadAttachments.length, maxAttachments]);
		useTextareaExpressionHandlers({
			setValue,
			textareaRef: nullTextareaRef,
			canSendFavoriteMemeId: true,
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
				textareaRef: editableRef,
				enabled: !textareaInputDisabled,
			});
		useTextareaEditing({
			channelId: channel.id,
			editingMessageId,
			editingMessage: editingMessageForComposer,
			isMobileEditMode: mobileLayout.enabled,
			value,
			setValue,
			textareaRef: nullTextareaRef,
			previousValueRef,
		});
		const hasPendingSticker = ChannelSticker.getPendingSticker(channel.id) !== null;
		const hasAttachments = uploadAttachments.length > 0;
		const previousComposerBoundaryState = useRef({channelId: channel.id, hasAttachments, hasPendingSticker});
		const wasAtBottomBeforeComposerBoundaryChange = useRef(true);
		useEffect(() => {
			const scrollerElement = getActiveMessageScroller();
			if (!scrollerElement) return undefined;
			const updateWasAtBottom = () => {
				wasAtBottomBeforeComposerBoundaryChange.current = isMessageScrollerNearBottom(scrollerElement);
			};
			updateWasAtBottom();
			scrollerElement.addEventListener('scroll', updateWasAtBottom, {passive: true});
			return () => {
				scrollerElement.removeEventListener('scroll', updateWasAtBottom);
			};
		}, [channel.id]);
		useEffect(() => {
			const previous = previousComposerBoundaryState.current;
			const isSameChannel = previous.channelId === channel.id;
			const attachmentBoundaryChanged = isSameChannel && previous.hasAttachments !== hasAttachments;
			const stickerBoundaryChanged = isSameChannel && previous.hasPendingSticker !== hasPendingSticker;
			if (
				wasAtBottomBeforeComposerBoundaryChange.current &&
				(stickerBoundaryChanged || (attachmentBoundaryChanged && Messages.getMessages(channel.id).hasMoreAfter))
			) {
				ComponentDispatch.dispatch('FORCE_JUMP_TO_PRESENT', {channelId: channel.id});
			}
			previousComposerBoundaryState.current = {channelId: channel.id, hasAttachments, hasPendingSticker};
			const scrollerElement = getActiveMessageScroller();
			if (scrollerElement) {
				wasAtBottomBeforeComposerBoundaryChange.current = isMessageScrollerNearBottom(scrollerElement);
			}
		}, [channel.id, hasAttachments, hasPendingSticker]);
		const showAttachments = hasAttachments;
		const showStickers = hasPendingSticker;
		const isOverCharacterLimit = trimmedMessageContent.length > maxMessageLength;
		const canSubmit =
			!textareaInputDisabled &&
			!isSubmissionBlockedBySlowmode &&
			!isOverCharacterLimit &&
			(hasMessageContent || hasAttachments || hasPendingSticker);
		const {onSubmit} = useTextareaSubmit({
			channelId: channel.id,
			guildId: channel.guildId === undefined ? null : channel.guildId,
			editingMessage: editingMessageForComposer,
			isMobileEditMode: mobileLayout.enabled,
			uploadAttachmentsLength: uploadAttachments.length,
			hasPendingSticker,
			value,
			setValue,
			displayToActual,
			composerHandleRef: handleRef,
			clearSegments,
			isSlowmodeActive,
			handleSendMessage,
			onMentionConfirmationNeeded: handleMentionConfirmationNeeded,
			i18n: i18n,
		});
		const handleClearSlashCommand = useCallback(() => {
			const handle = handleRef.current;
			if (handle !== null) {
				handle.clear();
			}
			setValue('');
			clearSegments();
			DraftCommands.deleteDraft(channel.id);
			if (handle !== null) {
				handle.focus();
			}
		}, [channel.id, clearSegments]);
		const handleCancelEdit = useCallback(() => {
			setValue('');
			clearSegments();
		}, [clearSegments]);
		const focusComposer = useCallback(() => {
			const handle = handleRef.current;
			if (handle !== null) {
				handle.focus();
			}
		}, []);
		const dismissTopmostComposerAffordance = useCallback((): boolean => {
			const editingInline = MessageEdit.getEditingMessageId(channel.id) !== null;
			if (editingInline) {
				MessageCommands.stopEdit(channel.id);
				return true;
			}
			if (hasPendingSticker) {
				ChannelSticker.removePendingSticker(channel.id);
				focusComposer();
				return true;
			}
			if (hasAttachments) {
				CloudUpload.clearTextarea(channel.id);
				focusComposer();
				return true;
			}
			const slashCommandResolution = LexicalMessageCommandResolver.resolve(handleRef.current);
			if (slashCommandResolution.status !== LexicalMessageCommandResolutionStatus.NO_COMMAND) {
				handleClearSlashCommand();
				return true;
			}
			if (editingMessageForComposer !== null && mobileLayout.enabled) {
				MessageCommands.stopEditMobile(channel.id);
				handleCancelEdit();
				focusComposer();
				return true;
			}
			if (replyingMessage !== null) {
				MessageCommands.stopReply(channel.id);
				focusComposer();
				return true;
			}
			return false;
		}, [
			channel.id,
			editingMessageForComposer,
			focusComposer,
			handleCancelEdit,
			handleClearSlashCommand,
			hasAttachments,
			hasPendingSticker,
			mobileLayout.enabled,
			replyingMessage,
		]);
		useEffect(() => {
			return ComponentDispatch.subscribe('TEXTAREA_DISMISS_AFFORDANCE', (request?: unknown) => {
				const dismissalRequest = request as ChannelComposerDismissalRequest | undefined;
				if (dismissalRequest?.channelId !== channel.id) {
					return false;
				}
				return dismissTopmostComposerAffordance();
			});
		}, [channel.id, dismissTopmostComposerAffordance]);
		const handleEscapeKey = useCallback(
			(event: React.KeyboardEvent<HTMLElement>) => {
				if (event.defaultPrevented || event.nativeEvent.isComposing) return;
				if (event.key !== 'Escape' || event.shiftKey) return;
				if (requestChannelComposerAffordanceDismissal(channel.id)) {
					event.preventDefault();
					event.stopPropagation();
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
			[channel.id, isInputAreaFocused, KeyboardMode.keyboardModeEnabled, Accessibility.escapeExitsKeyboardMode],
		);
		const slotResolvers = useMemo<SlashSlotResolvers>(() => {
			const guildId = channel.guildId;
			return {
				resolveUser: (query) => {
					const user = Users.getUserByTag(query);
					return user ? {id: user.id} : null;
				},
				resolveChannel: (query) => {
					if (!guildId) {
						return null;
					}
					const found = Channels.getGuildChannels(guildId).find((candidate) => {
						const name = candidate.name === undefined ? '' : candidate.name;
						return name.toLowerCase() === query.toLowerCase();
					});
					return found ? {id: found.id} : null;
				},
				resolveRole: (query) => {
					if (!guildId) {
						return null;
					}
					const found = GuildGuilds.getGuildRoles(guildId).find(
						(role) => role.name.toLowerCase() === query.toLowerCase(),
					);
					return found ? {id: found.id} : null;
				},
			};
		}, [channel.guildId]);
		const handleEditorKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLElement>) => {
				const handle = handleRef.current;
				let selection: ComposerSelectionRange | null = null;
				if (handle !== null) {
					selection = handle.getSelection();
				}
				const hasSelectionRange = selection ? selection.start !== selection.end : false;
				const inboxCombo = Keybind.getByAction('chat_toggle_inbox').combo;
				if (doesEventMatchShortcut(event, inboxCombo) && !hasSelectionRange && value.trim().length === 0) {
					event.preventDefault();
					event.stopPropagation();
					ComponentDispatch.dispatch('INBOX_OPEN');
					return;
				}
				handleEscapeKey(event);
			},
			[handleEscapeKey, value],
		);
		const handleSubmit = useCallback(() => {
			if (!canSubmit) {
				return;
			}
			if (hasAttachments && !canAttachFilesInChannel(channel)) {
				showAttachmentPermissionDeniedModal(channel);
				return;
			}
			onSubmit();
		}, [canSubmit, channel, hasAttachments, onSubmit]);
		const handleArrowUpEmpty = useCallback(() => {
			if (KeyboardMode.keyboardModeEnabled) {
				ComponentDispatch.dispatch('FOCUS_BOTTOMMOST_MESSAGE', {channelId: channel.id});
				return;
			}
			const message = Messages.getLastEditableMessage(channel.id);
			if (!message) {
				return;
			}
			MessageCommands.startEdit(channel.id, message.id, message.content);
		}, [channel.id]);
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
		useChannelComposerGlobalShortcuts({
			channel,
			handleRef,
			editableRef,
			textareaInputDisabled,
			isFocused,
			handleArrowUpEmpty,
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
						ChannelDisplayUtils.getDMDisplayName(channel),
						channel.isDM() ? i18n._(MESSAGE_2_DESCRIPTOR) : messagePrefix,
						Number.MAX_SAFE_INTEGER,
					);
		useEffect(() => {
			const unsubscribe = ComponentDispatch.subscribe('FOCUS_TEXTAREA', (payload?: unknown) => {
				const payloadValue = payload === null || payload === undefined ? {} : payload;
				const {channelId, enterKeyboardMode} = payloadValue as {
					channelId?: string;
					enterKeyboardMode?: boolean;
				};
				if (channelId && channelId !== channel.id) return;
				if (textareaInputDisabled) return false;
				if (editingMessageId && !mobileLayout.enabled) return false;
				if (enterKeyboardMode) {
					KeyboardMode.enterKeyboardMode(true);
				} else {
					KeyboardMode.exitKeyboardMode();
				}
				const handle = handleRef.current;
				if (handle !== null) {
					handle.focus();
				}
				return true;
			});
			return unsubscribe;
		}, [channel.id, editingMessageId, textareaInputDisabled, mobileLayout.enabled]);
		const wasEditingInlineRef = useRef(false);
		useEffect(() => {
			const isEditingInline = editingMessageId != null && !mobileLayout.enabled;
			if (wasEditingInlineRef.current && !isEditingInline && !textareaInputDisabled) {
				const handle = handleRef.current;
				if (handle !== null) {
					handle.focus();
				}
			}
			wasEditingInlineRef.current = isEditingInline;
		}, [editingMessageId, mobileLayout.enabled, textareaInputDisabled]);
		useEffect(() => {
			if (textareaInputDisabled) return;
			const unsubscribe = ComponentDispatch.subscribe('TEXTAREA_UPLOAD_FILE', (payload?: unknown) => {
				const payloadValue = payload === null || payload === undefined ? {} : payload;
				const {channelId} = payloadValue as {channelId?: string};
				if (channelId && channelId !== channel.id) return;
				handleFileButtonClick();
			});
			return unsubscribe;
		}, [channel.id, textareaInputDisabled, handleFileButtonClick]);
		useEffect(() => {
			const unsubscribe = ComponentDispatch.subscribe('TEXTAREA_SEND_VOICE_MESSAGE', (payload?: unknown) => {
				const payloadValue = payload === null || payload === undefined ? {} : payload;
				const {channelId} = payloadValue as {channelId?: string};
				if (channelId && channelId !== channel.id) return undefined;
				if (mobileLayout.enabled || textareaInputDisabled) return false;
				openVoiceMessageComposerModal(channel.id);
				return true;
			});
			return unsubscribe;
		}, [channel.id, textareaInputDisabled, mobileLayout.enabled]);
		useEffect(() => {
			if (mobileLayout.enabled) {
				setShowAllButtons(true);
				return;
			}
			const container = containerRef.current;
			if (container === null || typeof ResizeObserver === 'undefined') return;
			let lastWidth = -1;
			let rafId: number | null = null;
			let pendingWidth: number | null = null;
			const updateButtonVisibility = () => {
				rafId = null;
				let containerWidthLocal = 0;
				if (pendingWidth !== null) {
					containerWidthLocal = pendingWidth;
				} else {
					const currentContainer = containerRef.current;
					if (currentContainer !== null) containerWidthLocal = currentContainer.clientWidth;
				}
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
				if (entry !== undefined) {
					scheduleButtonVisibilityCheck(entry.contentRect.width);
				} else {
					scheduleButtonVisibilityCheck();
				}
			});
			resizeObserver.observe(container);
			scheduleButtonVisibilityCheck(container.clientWidth);
			return () => {
				if (rafId != null) {
					cancelAnimationFrame(rafId);
				}
				resizeObserver.disconnect();
			};
		}, [mobileLayout.enabled]);
		const isPlusContextMenuOpen = useCallback(() => {
			const plusButton = plusButtonRef.current;
			const contextMenu = ContextMenuState.contextMenu;
			return Boolean(
				plusButton !== null &&
					contextMenu !== null &&
					contextMenu !== undefined &&
					contextMenu.target.target === plusButton,
			);
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
				if (isFastPlusMenuRepeatPress(timeStamp, openedAt)) {
					void handleFileButtonClick();
				}
			},
			[handleFileButtonClick, isFastPlusMenuRepeatPress],
		);
		const handlePlusMenuBackdropMouseDown = useCallback(
			(event: React.MouseEvent<HTMLElement>) => {
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
							canAttachFiles={canAttachFilesInChannel(channel)}
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
				channel.id,
				handleFileButtonClick,
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
				if (mobileLayout.enabled) {
					handleOpenMobilePlusSheet();
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
			[
				closePlusMenuFromRepeatPress,
				handleOpenMobilePlusSheet,
				isPlusContextMenuOpen,
				mobileLayout.enabled,
				textareaInputDisabled,
				openPlusMenu,
			],
		);
		const isSlashParamBarVisible = slashCommandState.activeSlot != null;
		const isMobileEditBarVisible = editingMessage && mobileLayout.enabled;
		const isReplyBarVisible = !isMobileEditBarVisible && referencedMessage != null;
		const presentableTypingUsers = usePresentableTypingUsers(channel);
		const isTypingStatusVisible = !isAutocompleteVisible && presentableTypingUsers.length > 0;
		const isSlowmodeIndicatorVisible = isSlowmodeEnabled;
		const hasLeadingStatusContent = isMobileEditBarVisible || isReplyBarVisible || isSlashParamBarVisible;
		let shouldReplyMention = false;
		if (replyingMessage !== null && replyingMessage !== undefined) {
			shouldReplyMention = replyingMessage.mentioning;
		}
		let topBarContent: React.ReactNode = null;
		if (isMobileEditBarVisible) {
			topBarContent = <EditBar channel={channel} onCancel={handleCancelEdit} />;
		} else if (referencedMessage !== null) {
			topBarContent = (
				<ReplyBar
					replyingMessageObject={referencedMessage}
					shouldReplyMention={shouldReplyMention}
					setShouldReplyMention={(mentioning) => MessageCommands.setReplyMentioning(channel.id, mentioning)}
					channel={channel}
				/>
			);
		}
		const renderSection = (content: React.ReactNode, sectionClassName?: string) => (
			<flx-channel-textarea-section className={flxElementClassName(wrapperStyles.stackSection, sectionClassName)}>
				{content}
			</flx-channel-textarea-section>
		);
		return (
			<>
				<flx-channel-textarea
					ref={containerRef}
					className={flxElementClassName(
						wrapperStyles.box,
						wrapperStyles.composerRoot,
						wrapperStyles.wrapperSides,
						styles.textareaOuter,
						mobileLayout.enabled && styles.textareaOuterMobile,
						wrapperStyles.roundedAll,
						textareaInputDisabled && wrapperStyles.disabled,
						!mobileLayout.enabled && styles.textareaOuterRow,
					)}
				>
					<flx-channel-textarea-status-rail className={flxElementClassName(wrapperStyles.statusRail)}>
						<flx-channel-textarea-status-rail-left className={flxElementClassName(wrapperStyles.statusRailLeft)}>
							{isTypingStatusVisible && (
								<flx-channel-textarea-typing-slot className={flxElementClassName(wrapperStyles.statusTypingSlot)}>
									<TypingUsers channel={channel} withText={true} showAvatars={true} />
								</flx-channel-textarea-typing-slot>
							)}
						</flx-channel-textarea-status-rail-left>
						{isSlowmodeIndicatorVisible && (
							<flx-channel-textarea-slowmode-slot className={flxElementClassName(wrapperStyles.statusSlowmodeSlot)}>
								<SlowmodeIndicator
									slowmodeRemaining={slowmodeRemaining}
									slowmodeDuration={channel.rateLimitPerUser * 1000}
									isImmune={isSlowmodeImmune}
								/>
							</flx-channel-textarea-slowmode-slot>
						)}
					</flx-channel-textarea-status-rail>
					{hasLeadingStatusContent && (
						<div className={wrapperStyles.composerActionStack} data-flx="channel.textarea.composer-action-stack">
							{topBarContent !== null && (
								<div className={wrapperStyles.composerActionRow} data-flx="channel.textarea.composer-action-row">
									{topBarContent}
								</div>
							)}
							{slashCommandState.activeSlot !== null && (
								<div className={wrapperStyles.composerActionRow} data-flx="channel.textarea.slash-command-action-row">
									<SlashCommandParamBar activeSlot={slashCommandState.activeSlot} onClear={handleClearSlashCommand} />
								</div>
							)}
						</div>
					)}
					{showAttachments &&
						renderSection(<ChannelAttachmentArea channelId={channel.id} />, styles.collapsibleSection)}
					{showStickers &&
						renderSection(
							<ChannelStickersArea channelId={channel.id} hasAttachments={hasAttachments} />,
							styles.collapsibleSection,
						)}
					{renderSection(
						<flx-channel-textarea-box
							className={flxElementClassName(styles.mainWrapperDense, textareaInputDisabled && wrapperStyles.disabled)}
						>
							<flx-channel-textarea-upload-column
								className={flxElementClassName(styles.uploadButtonColumn, styles.sideButtonPadding)}
							>
								<TextareaButton
									iconProps={PLUS_ICON_PROPS}
									icon={slashCommandState.hasSlots ? SlashCommandIcon : PlusIcon}
									label={slashCommandState.hasSlots ? i18n._(CLEAR_COMMAND_DESCRIPTOR) : i18n._(OPEN_MENU_DESCRIPTOR)}
									disabled={textareaInputDisabled}
									aria-hidden={textareaInputDisabled ? true : undefined}
									onMouseDown={slashCommandState.hasSlots ? undefined : handlePlusMenuMouseDown}
									onClick={slashCommandState.hasSlots ? handleClearSlashCommand : handlePlusMenuClick}
									forceHover={!slashCommandState.hasSlots && plusContextMenuOpen}
									className={plusContextMenuOpen ? styles.plusButtonAboveBackdrop : undefined}
									ref={plusButtonRef}
								/>
							</flx-channel-textarea-upload-column>
							<flx-channel-textarea-content
								ref={contentAreaRef}
								className={flxElementClassName(styles.contentAreaDense)}
							>
								<flx-channel-textarea-composer className={flxElementClassName(lexicalStyles.composerHost)}>
									<LexicalComposerInput
										placeholder={placeholderText}
										disabled={textareaInputDisabled}
										handleRef={handleRef}
										initialValue={initialDraftRef.current.display}
										initialSegments={initialDraftRef.current.segments}
										slotResolvers={slotResolvers}
										emojiShortcodeResolver={composerEmojiResolver}
										channelId={channel.id}
										guildId={channel.guildId}
										submitOnEnter={!mobileLayout.enabled}
										focusRingTarget={containerRef}
										focusRingEnabled={!textareaInputDisabled && Accessibility.showTextareaFocusRing}
										className={lexicalStyles.composerEditable}
										autocompleteOptions={autocompleteOptions}
										autocompleteType={autocompleteType}
										autocompleteQuery={autocompleteQuery}
										autocompleteEnabled={!textareaInputDisabled}
										slotMenuActive={isSlotMenu}
										autocompleteReferenceElement={containerRef.current}
										autocompleteListboxId={autocompleteListId}
										onAutocompleteSelect={handleSelect}
										onChange={handleEditorChange}
										onCursorMove={onCursorMove}
										onEnter={handleSubmit}
										onArrowUp={handleArrowUpEmpty}
										onKeyDown={handleEditorKeyDown}
										onFocus={() => {
											setIsFocused(true);
											setIsInputAreaFocused(true);
											ChannelSearch.setInputFocused(channel.id, false);
										}}
										onBlur={() => {
											setIsFocused(false);
											setIsInputAreaFocused(false);
										}}
										onSlashCommandStateChange={setSlashCommandState}
									/>
								</flx-channel-textarea-composer>
							</flx-channel-textarea-content>
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
								channelId={channel.id}
							/>
						</flx-channel-textarea-box>,
						styles.inputSection,
					)}
					<MessageCharacterCounter
						currentLength={trimmedMessageContent.length}
						maxLength={maxMessageLength}
						canUpgrade={maxMessageLength < premiumMaxLength}
						premiumMaxLength={premiumMaxLength}
					/>
				</flx-channel-textarea>
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
