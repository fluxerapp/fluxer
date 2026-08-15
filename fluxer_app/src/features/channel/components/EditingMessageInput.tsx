// SPDX-License-Identifier: AGPL-3.0-or-later

import {Limits} from '@app/features/app/utils/UserLimits';
import editingStyles from '@app/features/channel/components/EditingMessageInput.module.css';
import {MessageCharacterCounter} from '@app/features/channel/components/MessageCharacterCounter';
import wrapperStyles from '@app/features/channel/components/textarea/InputWrapper.module.css';
import {TextareaButton} from '@app/features/channel/components/textarea/TextareaButton';
import styles from '@app/features/channel/components/textarea/TextareaInput.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionPickerSheet} from '@app/features/expressions/components/modals/ExpressionPickerSheet';
import {ExpressionPickerPopout} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import GuildVerification, {VerificationFailureReason} from '@app/features/guild/state/GuildVerification';
import {
	CANNOT_SEND_MESSAGES_IN_CHANNEL_DESCRIPTOR,
	EDIT_MESSAGE_DESCRIPTOR,
	EMOJIS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Keybind from '@app/features/input/state/InputKeybind';
import {LexicalRichInput, type LexicalRichInputHandle} from '@app/features/lexical/composer/LexicalRichInput';
import {doesEventMatchShortcut, useMarkdownKeybinds} from '@app/features/messaging/hooks/useMarkdownKeybinds';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MessageEdit from '@app/features/messaging/state/MessageEdit';
import {applyMarkdownSegments} from '@app/features/messaging/utils/MarkdownToSegmentUtils';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {openPopout} from '@app/features/ui/popover/PopoverPopout';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {MAX_MESSAGE_LENGTH_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {SmileyIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const EDITING_ALLOWED_TRIGGERS = ['emoji', 'mention', 'channel'] as const;

interface EditingFocusRequest {
	requestedChannelId: string | null;
	enterKeyboardMode: boolean | null;
	channelId: string;
	messageId: string;
	editingDisabled: boolean;
	composer: LexicalRichInputHandle | null;
}

function focusEditingInput(request: EditingFocusRequest): boolean | null {
	if (
		request.requestedChannelId != null &&
		request.requestedChannelId !== '' &&
		request.requestedChannelId !== request.channelId
	) {
		return null;
	}
	if (request.editingDisabled) {
		return false;
	}
	if (!MessageEdit.isEditing(request.channelId, request.messageId)) {
		return false;
	}
	if (request.enterKeyboardMode === true) {
		KeyboardMode.enterKeyboardMode(true);
	} else {
		KeyboardMode.exitKeyboardMode();
	}
	if (request.composer != null) {
		request.composer.focus();
	}
	return true;
}

export const EditingMessageInput = observer(
	({
		channel,
		message,
		onCancel,
		onSubmit,
	}: {
		channel: Channel;
		message: Message;
		onCancel: () => void;
		onSubmit: (actualContent?: string) => void;
	}) => {
		const {i18n} = useLingui();
		const maxMessageLength = Limits.getMaxMessageLength();
		const premiumMaxLength = Limits.getPremiumValue('max_message_length', MAX_MESSAGE_LENGTH_PREMIUM);
		const [initial] = useState(() => {
			let wire = MessageEdit.getEditingContent(channel.id, message.id);
			if (wire == null) {
				wire = MessageEdit.getDraftContent(message.id);
			}
			if (wire == null) {
				wire = message.content;
			}
			const segments = new TextareaSegmentManager();
			const display = applyMarkdownSegments(wire, channel.guildId, segments);
			return {display, segments: segments.getSegmentsCopy(), wire};
		});
		const [actualContent, setActualContent] = useState(initial.wire);
		const composerRef = useRef<LexicalRichInputHandle | null>(null);
		const containerRef = useRef<HTMLDivElement>(null);
		const editableRef = useRef<HTMLElement | null>(null);
		const expressionPickerTriggerRef = useRef<HTMLButtonElement>(null);
		const [expressionPickerOpen, setExpressionPickerOpen] = useState(false);
		const [isFocused, setIsFocused] = useState(false);
		const hasFocusedInitiallyRef = useRef(false);
		const mobileLayout = MobileLayout;
		useMarkdownKeybinds(isFocused, {preserveEditableFocusActions: false});
		const editingDisabled =
			channel.guildId != null &&
			GuildVerification.getFailureReason(channel.guildId) === VerificationFailureReason.TIMED_OUT;
		let placeholderText = '';
		if (editingDisabled) {
			placeholderText = i18n._(CANNOT_SEND_MESSAGES_IN_CHANNEL_DESCRIPTOR);
		}
		useEffect(() => {
			const container = containerRef.current;
			if (container == null) {
				editableRef.current = null;
				return;
			}
			const editable = container.querySelector<HTMLElement>('[data-channel-textarea]');
			if (editable == null) {
				editableRef.current = null;
				return;
			}
			editableRef.current = editable;
		}, []);
		const handleChange = useCallback(
			(_display: string, _segments: Array<MentionSegment>, wire: string) => {
				setActualContent(wire);
				MessageEdit.setEditingContent(channel.id, message.id, wire);
			},
			[channel.id, message.id],
		);
		const handleEmojiSelect = useCallback((emoji: FlatEmoji) => {
			const composer = composerRef.current;
			if (composer != null) {
				composer.insertEmoji(emoji);
			}
		}, []);
		const handleSubmit = useCallback(() => {
			if (editingDisabled || actualContent.length > maxMessageLength) {
				return;
			}
			onSubmit(actualContent);
		}, [actualContent, editingDisabled, maxMessageLength, onSubmit]);
		const handleKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLElement>) => {
				if (event.defaultPrevented) {
					return;
				}
				const composer = composerRef.current;
				const selection = composer == null ? null : composer.getSelection();
				const hasSelectionRange = selection != null && selection.start !== selection.end;
				const inboxCombo = Keybind.getByAction('chat_toggle_inbox').combo;
				if (doesEventMatchShortcut(event, inboxCombo) && !hasSelectionRange && actualContent.trim().length === 0) {
					event.preventDefault();
					event.stopPropagation();
					ComponentDispatch.dispatch('INBOX_OPEN');
					return;
				}
				if (event.key === 'Escape' && !event.shiftKey && !event.defaultPrevented && !event.nativeEvent.isComposing) {
					event.preventDefault();
					event.stopPropagation();
					onCancel();
				}
			},
			[actualContent, onCancel],
		);
		useEffect(() => {
			if (editingDisabled || hasFocusedInitiallyRef.current) {
				return;
			}
			hasFocusedInitiallyRef.current = true;
			requestAnimationFrame(() => {
				const composer = composerRef.current;
				if (composer != null) {
					composer.focus();
				}
			});
		}, [editingDisabled]);
		useEffect(() => {
			if (!editingDisabled) {
				return;
			}
			const container = containerRef.current;
			const active = document.activeElement;
			if (container != null && active instanceof HTMLElement && container.contains(active)) {
				active.blur();
			}
			PopoutCommands.close(`editing-expression-picker-${channel.id}`);
			setExpressionPickerOpen(false);
		}, [channel.id, editingDisabled]);
		useEffect(() => {
			const unsubscribe = ComponentDispatch.subscribe('FOCUS_TEXTAREA', (payload?: unknown) => {
				const data = payload as {channelId?: string; enterKeyboardMode?: boolean} | undefined;
				let requestedChannelId: string | null = null;
				let enterKeyboardMode: boolean | null = null;
				if (data != null) {
					if (data.channelId != null) {
						requestedChannelId = data.channelId;
					}
					if (data.enterKeyboardMode != null) {
						enterKeyboardMode = data.enterKeyboardMode;
					}
				}
				return focusEditingInput({
					requestedChannelId,
					enterKeyboardMode,
					channelId: channel.id,
					messageId: message.id,
					editingDisabled,
					composer: composerRef.current,
				});
			});
			return unsubscribe;
		}, [channel.id, editingDisabled, message.id]);
		const handleExpressionPickerToggle = useCallback(() => {
			if (editingDisabled) {
				return;
			}
			const triggerElement = expressionPickerTriggerRef.current;
			if (triggerElement == null) {
				return;
			}
			const popoutKey = `editing-expression-picker-${channel.id}`;
			if (expressionPickerOpen) {
				PopoutCommands.close(popoutKey);
				setExpressionPickerOpen(false);
				return;
			}
			openPopout(
				triggerElement,
				{
					render: ({onClose}) => (
						<ExpressionPickerPopout
							channelId={channel.id}
							onEmojiSelect={handleEmojiSelect}
							onClose={onClose}
							visibleTabs={['emojis']}
						/>
					),
					position: 'top-end',
					animationType: 'none',
					offsetCrossAxis: 16,
					onOpen: () => setExpressionPickerOpen(true),
					onClose: () => setExpressionPickerOpen(false),
					returnFocusRef: editableRef,
				},
				popoutKey,
			);
		}, [channel.id, editingDisabled, expressionPickerOpen, handleEmojiSelect]);
		useEffect(() => {
			const unsubscribe = ComponentDispatch.subscribe('EDITING_EXPRESSION_PICKER_TAB_TOGGLE', (payload?: unknown) => {
				const data = payload as {channelId?: string; messageId?: string; tab?: string} | undefined;
				if (data == null || data.channelId !== channel.id || data.messageId !== message.id || data.tab !== 'emojis') {
					return;
				}
				handleExpressionPickerToggle();
			});
			return unsubscribe;
		}, [channel.id, handleExpressionPickerToggle, message.id]);
		let expressionPickerButtonRef: React.Ref<HTMLButtonElement> | undefined;
		if (!mobileLayout.enabled) {
			expressionPickerButtonRef = expressionPickerTriggerRef;
		}
		let handleExpressionPickerButtonClick = handleExpressionPickerToggle;
		if (mobileLayout.enabled) {
			handleExpressionPickerButtonClick = () => setExpressionPickerOpen(true);
		}
		let displayContentLength = actualContent.length;
		if (editingDisabled) {
			displayContentLength = 0;
		}
		return (
			<>
				<FocusRing within={true} offset={-2} data-flx="channel.editing-message-input.focus-ring">
					<div
						ref={containerRef}
						className={styles.textareaContainer}
						data-flx="channel.editing-message-input.textarea-container"
					>
						<div
							className={clsx(styles.mainWrapperEditing, editingDisabled && wrapperStyles.disabled)}
							data-flx="channel.editing-message-input.main-wrapper-editing"
						>
							<div className={styles.contentAreaEditing} data-flx="channel.editing-message-input.content-area-editing">
								<LexicalRichInput
									initialValue={initial.display}
									initialSegments={initial.segments}
									placeholder={placeholderText}
									disabled={editingDisabled}
									channel={channel}
									allowedTriggers={[...EDITING_ALLOWED_TRIGGERS]}
									markdown={true}
									singleLine={!mobileLayout.enabled}
									size="chat"
									className={editingStyles.editor}
									autocompleteAnchor={containerRef.current}
									ariaLabel={i18n._(EDIT_MESSAGE_DESCRIPTOR)}
									richInputRef={composerRef}
									onChange={handleChange}
									onSubmit={handleSubmit}
									onKeyDown={handleKeyDown}
									onFocus={() => setIsFocused(true)}
									onBlur={() => setIsFocused(false)}
									i18n={i18n}
								/>
							</div>
							<div
								className={styles.buttonContainerEditing}
								data-flx="channel.editing-message-input.button-container-editing"
							>
								<TextareaButton
									ref={expressionPickerButtonRef}
									icon={SmileyIcon}
									iconProps={{weight: 'fill'}}
									label={i18n._(EMOJIS_DESCRIPTOR)}
									isSelected={expressionPickerOpen}
									compact={true}
									disabled={editingDisabled}
									data-expression-picker-tab="emojis"
									onClick={handleExpressionPickerButtonClick}
									data-flx="channel.editing-message-input.textarea-button.set-expression-picker-open"
								/>
							</div>
						</div>
						<MessageCharacterCounter
							currentLength={displayContentLength}
							maxLength={maxMessageLength}
							canUpgrade={maxMessageLength < premiumMaxLength}
							premiumMaxLength={premiumMaxLength}
							data-flx="channel.editing-message-input.message-character-counter"
						/>
					</div>
				</FocusRing>
				<div className={editingStyles.footer} data-flx="channel.editing-message-input.footer">
					<div data-flx="channel.editing-message-input.hints">
						<Trans>
							escape to{' '}
							<FocusRing offset={-2} data-flx="channel.editing-message-input.focus-ring--2">
								<button type="button" className={editingStyles.footerLink} onClick={onCancel} key="cancel">
									cancel
								</button>
							</FocusRing>
						</Trans>
						<div
							aria-hidden={true}
							className={editingStyles.separator}
							data-flx="channel.editing-message-input.separator"
						/>
						<Trans>
							enter to{' '}
							<FocusRing offset={-2} enabled={!editingDisabled} data-flx="channel.editing-message-input.focus-ring--3">
								<button
									type="button"
									className={editingStyles.footerLink}
									onClick={handleSubmit}
									disabled={editingDisabled}
									key="save"
								>
									save
								</button>
							</FocusRing>
						</Trans>
					</div>
				</div>
				{mobileLayout.enabled && !editingDisabled && (
					<ExpressionPickerSheet
						isOpen={expressionPickerOpen}
						onClose={() => setExpressionPickerOpen(false)}
						channelId={channel.id}
						onEmojiSelect={handleEmojiSelect}
						visibleTabs={['emojis']}
						selectedTab="emojis"
						data-flx="channel.editing-message-input.expression-picker-sheet"
					/>
				)}
			</>
		);
	},
);
