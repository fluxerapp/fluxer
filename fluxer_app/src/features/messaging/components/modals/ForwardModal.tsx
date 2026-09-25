// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import type {ForwardDestination} from '@app/features/app/components/dialogs/shared/ForwardDefaultDestinations';
import {MAX_FORWARD_DESTINATIONS} from '@app/features/app/components/dialogs/shared/ForwardDestinationSelection';
import selectorStyles from '@app/features/app/components/dialogs/shared/SelectorModalStyles.module.css';
import {
	type ForwardDestinationOption,
	useForwardDestinations,
} from '@app/features/app/components/dialogs/shared/UseForwardDestinations';
import {Limits} from '@app/features/app/utils/UserLimits';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {MessageCharacterCounter} from '@app/features/channel/components/MessageCharacterCounter';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionPickerSheet} from '@app/features/expressions/components/modals/ExpressionPickerSheet';
import {ExpressionPickerPopout} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {dropTrailingEmptyBlockquoteLines} from '@app/features/lexical/composer/blockquoteLines';
import {LexicalRichInput, type LexicalRichInputHandle} from '@app/features/lexical/composer/LexicalRichInput';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {MessageForwardFailedModal} from '@app/features/messaging/components/alerts/MessageForwardFailedModal';
import {showMessagingErrorModal} from '@app/features/messaging/components/alerts/MessagingErrorModalUtils';
import {ForwardDestinationIcon} from '@app/features/messaging/components/modals/ForwardDestinationIcon';
import {ForwardMessagePreview} from '@app/features/messaging/components/modals/ForwardMessagePreview';
import modalStyles from '@app/features/messaging/components/modals/ForwardModal.module.css';
import {shouldNavigateAfterForward} from '@app/features/messaging/components/modals/ForwardModalUtils';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {shouldDisableAutofocusOnMobile} from '@app/features/platform/utils/AutofocusUtils';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import type {User} from '@app/features/user/models/User';
import {MAX_MESSAGE_LENGTH_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, SmileyIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type MouseEvent, useCallback, useEffect, useId, useRef, useState} from 'react';

const MESSAGE_IS_TOO_LONG_DESCRIPTOR = msg({
	message: 'Message is too long',
	comment: 'Error modal title in the forward modal when the optional comment exceeds the message length limit.',
});
const SHORTEN_THE_MESSAGE_AND_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Shorten the message and try again.',
	comment: 'Body of the error modal shown when the forward modal comment exceeds the message length limit.',
});
const COMMENTS_ARE_DISABLED_BECAUSE_SLOWMODE_IS_ON_IN_DESCRIPTOR = msg({
	message:
		'Comments are disabled because slowmode is on in {forwardChannelDisplayName}. The comment would be blocked by slowmode right after the forward.',
	comment:
		'Helper text in the forward modal explaining why the optional comment field is disabled for a single selected channel with slowmode.',
});
const COMMENTS_ARE_DISABLED_BECAUSE_ONE_OR_MORE_SELECTED_DESCRIPTOR = msg({
	message:
		'Comments are disabled because one or more selected channels have slowmode on. The comment would be blocked by slowmode right after the forward.',
	comment:
		'Helper text in the forward modal explaining why the optional comment field is disabled when at least one selected target has slowmode active.',
});
const WAITING_FOR_SLOWMODE_IN_TO_EXPIRE_DESCRIPTOR = msg({
	message: 'Waiting for slowmode in {forwardChannelDisplayName} to expire.',
	comment:
		'Inline notice in the forward modal when slowmode is active in the selected target channel. forwardChannelDisplayName is the channel name.',
});
const WAITING_FOR_SLOWMODE_IN_ONE_OR_MORE_SELECTED_DESCRIPTOR = msg({
	message: 'Waiting for slowmode in one or more selected channels to expire.',
	comment: 'Inline notice in the forward modal when slowmode is active in one or more selected target channels.',
});
const FORWARD_MESSAGE_DESCRIPTOR = msg({
	message: 'Forward message',
	comment: 'Title of the forward message modal.',
});
const SEARCH_DESCRIPTOR = msg({
	message: 'Search',
	comment:
		'Placeholder and accessible label of the search input in the forward modal. It finds people, channels and group DMs to forward to.',
});
const COMMENTS_ARE_UNAVAILABLE_WHILE_SLOWMODE_IS_ON_DESCRIPTOR = msg({
	message: 'Comments are unavailable while slowmode is on.',
	comment: 'Tooltip on the disabled comment field in the forward modal when slowmode blocks comments.',
});
const ADD_A_COMMENT_OPTIONAL_DESCRIPTOR = msg({
	message: 'Add a comment (optional)',
	comment: 'Placeholder text in the optional comment field of the forward modal.',
});
const OPEN_EMOJI_PICKER_DESCRIPTOR = msg({
	message: 'Open emoji picker',
	comment: 'Accessible label for the emoji picker trigger button in the forward modal comment field.',
});
const SEND_SELECTED_COUNT_DESCRIPTOR = msg({
	message: 'Send ({selectedCount}/{selectionLimit})',
	comment:
		'Primary button label in the forward message modal. selectedCount is the number of selected destinations; selectionLimit is the maximum allowed.',
});
const logger = new Logger('ForwardModal');

interface ForwardModalProps {
	message: Message;
	mediaSelection?: MessageCommands.ForwardMediaSelection;
	onForwardSuccess?: (result: ForwardModalSuccess) => void;
	sourceChannel?: Channel | null;
	user: User;
}

export interface ForwardModalSuccess {
	forwardedChannelIds: ReadonlyArray<string>;
	shouldNavigate: boolean;
}

function resolveForwardSourceChannel(
	sourceChannel: Channel | null | undefined,
	messageChannelId: string,
): Channel | null {
	if (sourceChannel != null) {
		return sourceChannel;
	}
	const storedChannel = Channels.getChannel(messageChannelId);
	if (storedChannel == null) {
		return null;
	}
	return storedChannel;
}

function resolveForwardReferenceGuildId(channel: Channel | null, message: Message): string | null {
	if (channel != null && channel.guildId != null) {
		return channel.guildId;
	}
	if (message.guildId != null) {
		return message.guildId;
	}
	return null;
}

async function openForwardDestinations(destinations: ReadonlyArray<ForwardDestination>): Promise<Array<string>> {
	const channelIds: Array<string> = [];
	for (const destination of destinations) {
		if (destination.type === 'channel') {
			channelIds.push(destination.id);
			continue;
		}
		channelIds.push(await PrivateChannelCommands.ensureDMChannel(destination.id));
	}
	return channelIds;
}

function resolveCommentBlockedNotice(
	i18n: I18n,
	slowmodeEnabledOptions: ReadonlyArray<ForwardDestinationOption>,
): string | null {
	if (slowmodeEnabledOptions.length === 0) {
		return null;
	}
	if (slowmodeEnabledOptions.length === 1) {
		return i18n._(COMMENTS_ARE_DISABLED_BECAUSE_SLOWMODE_IS_ON_IN_DESCRIPTOR, {
			forwardChannelDisplayName: slowmodeEnabledOptions[0].displayName,
		});
	}
	return i18n._(COMMENTS_ARE_DISABLED_BECAUSE_ONE_OR_MORE_SELECTED_DESCRIPTOR);
}

function resolveSendBlockedNotice(
	i18n: I18n,
	slowmodeActiveOptions: ReadonlyArray<ForwardDestinationOption>,
): string | null {
	if (slowmodeActiveOptions.length === 0) {
		return null;
	}
	if (slowmodeActiveOptions.length === 1) {
		return i18n._(WAITING_FOR_SLOWMODE_IN_TO_EXPIRE_DESCRIPTOR, {
			forwardChannelDisplayName: slowmodeActiveOptions[0].displayName,
		});
	}
	return i18n._(WAITING_FOR_SLOWMODE_IN_ONE_OR_MORE_SELECTED_DESCRIPTOR);
}

function focusForwardComment(handle: LexicalRichInputHandle | null): void {
	if (handle != null) {
		handle.focus();
	}
}

function insertForwardEmoji(handle: LexicalRichInputHandle | null, emoji: FlatEmoji): boolean {
	if (handle == null) {
		return false;
	}
	return handle.insertEmoji(emoji);
}

const EMPTY_FORWARD_COMMENT_SEGMENTS: ReadonlyArray<MentionSegment> = Object.freeze([]);

export const ForwardModal = observer(
	({message, mediaSelection, onForwardSuccess, sourceChannel, user}: ForwardModalProps) => {
		const {i18n} = useLingui();
		const {
			composerChannel,
			options,
			searchQuery,
			selected,
			selectedKeys,
			setSearchQuery,
			slowmodeActiveSelectedOptions,
			slowmodeEnabledSelectedOptions,
			toggleDestination,
		} = useForwardDestinations({message, mediaSelection});
		const [actualOptionalMessage, setActualOptionalMessage] = useState('');
		const [isForwarding, setIsForwarding] = useState(false);
		const [expressionPickerOpen, setExpressionPickerOpen] = useState(false);
		const searchInputRef = useRef<HTMLInputElement>(null);
		const containerRef = useRef<HTMLDivElement>(null);
		const [containerElement, setContainerElement] = useState<HTMLDivElement | null>(null);
		const setContainerNode = useCallback((node: HTMLDivElement | null) => {
			containerRef.current = node;
			setContainerElement(node);
		}, []);
		const richInputRef = useRef<LexicalRichInputHandle>(null);
		const commentNoticeId = useId();
		const premiumMaxLength = Limits.getPremiumValue('max_message_length', MAX_MESSAGE_LENGTH_PREMIUM);
		const isMobileLayout = MobileLayout.enabled;
		const shouldFocusSearch = !shouldDisableAutofocusOnMobile();
		useEffect(() => {
			if (searchQuery !== '' || shouldDisableAutofocusOnMobile()) {
				return;
			}
			searchInputRef.current?.focus();
		}, [searchQuery]);
		const handleOptionalMessageExceedsLimit = useCallback(() => {
			showMessagingErrorModal({
				title: i18n._(MESSAGE_IS_TOO_LONG_DESCRIPTOR),
				message: i18n._(SHORTEN_THE_MESSAGE_AND_TRY_AGAIN_DESCRIPTOR),
				dataFlx: 'messaging.forward-modal.optional-message-too-long.generic-error-modal',
			});
		}, [i18n]);
		const sourceForwardChannel = resolveForwardSourceChannel(sourceChannel, message.channelId);
		const handleCommentChange = useCallback((_display: string, _segments: Array<MentionSegment>, wire: string) => {
			setActualOptionalMessage(dropTrailingEmptyBlockquoteLines(wire));
		}, []);
		const isAtSelectionLimit = selected.length >= MAX_FORWARD_DESTINATIONS;
		const isCommentOverLimit = actualOptionalMessage.length > user.maxMessageLength;
		const isSendBlockedBySlowmode = slowmodeActiveSelectedOptions.length > 0;
		const isCommentBlockedBySlowmode = slowmodeEnabledSelectedOptions.length > 0;
		const isCommentComposerDisabled = isCommentBlockedBySlowmode;
		let commentNotice = resolveSendBlockedNotice(i18n, slowmodeActiveSelectedOptions);
		const commentBlockedNotice = resolveCommentBlockedNotice(i18n, slowmodeEnabledSelectedOptions);
		if (commentBlockedNotice != null) {
			commentNotice = commentBlockedNotice;
		}
		let commentNoticeDescriptionId: string | undefined;
		if (commentNotice != null) {
			commentNoticeDescriptionId = commentNoticeId;
		}
		let commentPlaceholder = i18n._(ADD_A_COMMENT_OPTIONAL_DESCRIPTOR);
		if (isCommentComposerDisabled) {
			commentPlaceholder = i18n._(COMMENTS_ARE_UNAVAILABLE_WHILE_SLOWMODE_IS_ON_DESCRIPTOR);
		}
		const isCommentCounterVisible = actualOptionalMessage.length > user.maxMessageLength * 0.8;
		const handleForward = async (skipNavigation = false) => {
			if (selected.length === 0) return;
			if (isForwarding) return;
			if (isSendBlockedBySlowmode) return;
			if (!isCommentComposerDisabled && isCommentOverLimit) {
				handleOptionalMessageExceedsLimit();
				return;
			}
			setIsForwarding(true);
			try {
				let actualMessage: string | undefined;
				const trimmedOptionalMessage = actualOptionalMessage.trim();
				if (!isCommentComposerDisabled && trimmedOptionalMessage.length > 0) {
					actualMessage = trimmedOptionalMessage;
				}
				const guildId = resolveForwardReferenceGuildId(sourceForwardChannel, message);
				let attachmentIds: ReadonlyArray<string> | undefined;
				let embedIndices: ReadonlyArray<number> | undefined;
				if (mediaSelection !== undefined) {
					attachmentIds = mediaSelection.attachmentIds;
					embedIndices = mediaSelection.embedIndices;
				}
				const forwardedChannelIds = await openForwardDestinations(selected);
				const forwarded = await MessageCommands.forward(
					forwardedChannelIds,
					{
						message_id: message.id,
						channel_id: message.channelId,
						guild_id: guildId,
						attachment_ids: attachmentIds,
						embed_indices: embedIndices,
					},
					actualMessage,
				);
				if (!forwarded) {
					return;
				}
				ToastCommands.createToast({
					type: 'success',
					children: <Trans>Message forwarded</Trans>,
				});
				ModalCommands.pop();
				const shouldNavigate = shouldNavigateAfterForward(skipNavigation, forwardedChannelIds.length);
				if (onForwardSuccess !== undefined) {
					onForwardSuccess({forwardedChannelIds, shouldNavigate});
				}
				if (shouldNavigate) {
					const forwardedChannelId = forwardedChannelIds[0];
					if (forwardedChannelId == null) return;
					const forwardedChannel = Channels.getChannel(forwardedChannelId);
					if (forwardedChannel) {
						let forwardedGuildId: string | undefined;
						if (forwardedChannel.guildId != null) {
							forwardedGuildId = forwardedChannel.guildId;
						}
						NavigationCommands.selectChannel(forwardedGuildId, forwardedChannelId);
						focusChannelTextareaAfterNavigation(forwardedChannelId);
					}
				}
			} catch (error) {
				logger.error('Failed to forward message:', error);
				ModalCommands.push(
					modal(() => (
						<MessageForwardFailedModal data-flx="messaging.forward-modal.handle-forward.message-forward-failed-modal" />
					)),
				);
			} finally {
				setIsForwarding(false);
			}
		};
		return (
			<Modal.Root
				size="small"
				centered
				initialFocusRef={shouldFocusSearch ? searchInputRef : undefined}
				data-flx="messaging.forward-modal.modal-root"
			>
				<Modal.Header title={i18n._(FORWARD_MESSAGE_DESCRIPTOR)} data-flx="messaging.forward-modal.modal-header">
					<div className={selectorStyles.headerSearch} data-flx="messaging.forward-modal.div">
						<Input
							ref={searchInputRef}
							type="text"
							value={searchQuery}
							onChange={(event) => setSearchQuery(event.target.value)}
							placeholder={i18n._(SEARCH_DESCRIPTOR)}
							aria-label={i18n._(SEARCH_DESCRIPTOR)}
							maxLength={100}
							leftIcon={
								<MagnifyingGlassIcon
									className={selectorStyles.searchIcon}
									weight="bold"
									data-flx="messaging.forward-modal.magnifying-glass-icon"
								/>
							}
							className={selectorStyles.headerSearchInput}
							data-flx="messaging.forward-modal.input.set-search-query.text"
						/>
					</div>
				</Modal.Header>
				<Modal.Content
					padding="none"
					className={modalStyles.destinationList}
					data-flx="messaging.forward-modal.modal-content"
				>
					<div className={selectorStyles.listContainer} data-flx="messaging.forward-modal.div--2">
						<Scroller
							className={selectorStyles.scroller}
							key="forward-modal-channel-list-scroller"
							fade={false}
							data-flx="messaging.forward-modal.scroller"
						>
							{options.length === 0 ? (
								<div className={selectorStyles.emptyState} data-flx="messaging.forward-modal.div--3">
									<Trans>No channels found</Trans>
								</div>
							) : (
								<div className={selectorStyles.itemList} data-flx="messaging.forward-modal.div--4">
									{options.map((option) => {
										const isSelected = selectedKeys.has(option.key);
										const isDisabled = !isSelected && (isAtSelectionLimit || option.disableReason != null);
										const secondaryText = option.disableReason ?? option.detail;
										return (
											<FocusRing
												key={option.key}
												offset={-2}
												enabled={!isDisabled}
												data-flx="messaging.forward-modal.focus-ring"
											>
												<button
													type="button"
													onClick={() => toggleDestination(option.destination)}
													disabled={isDisabled}
													aria-pressed={isSelected}
													className={clsx(
														selectorStyles.itemButton,
														isSelected && selectorStyles.itemButtonSelected,
														isDisabled && selectorStyles.itemButtonDisabled,
													)}
													data-flx="messaging.forward-modal.button"
												>
													<div className={selectorStyles.itemContent} data-flx="messaging.forward-modal.div--5">
														<ForwardDestinationIcon
															option={option}
															data-flx="messaging.forward-modal.forward-destination-icon"
														/>
														<div className={selectorStyles.itemInfo} data-flx="messaging.forward-modal.div--6">
															<span className={selectorStyles.itemName} data-flx="messaging.forward-modal.span">
																{option.displayName}
															</span>
															{secondaryText != null && (
																<span
																	className={selectorStyles.itemSecondary}
																	data-flx="messaging.forward-modal.span--2"
																>
																	{secondaryText}
																</span>
															)}
														</div>
													</div>
													<div className={selectorStyles.itemAction} data-flx="messaging.forward-modal.div--7">
														<Checkbox
															checked={isSelected}
															disabled={isDisabled}
															aria-hidden={true}
															data-flx="messaging.forward-modal.checkbox"
														/>
													</div>
												</button>
											</FocusRing>
										);
									})}
								</div>
							)}
						</Scroller>
					</div>
				</Modal.Content>
				<div className={modalStyles.footerSection} data-flx="messaging.forward-modal.div--8">
					<ForwardMessagePreview
						message={message}
						mediaSelection={mediaSelection}
						embedChannel={composerChannel}
						data-flx="messaging.forward-modal.forward-message-preview"
					/>
					<FocusRing
						within
						ringTarget={containerRef}
						offset={-2}
						enabled={!isCommentComposerDisabled}
						data-flx="messaging.forward-modal.focus-ring"
					>
						<div
							ref={setContainerNode}
							className={clsx(
								modalStyles.messageInputContainer,
								isCommentComposerDisabled && modalStyles.messageInputContainerDisabled,
								commentNotice != null && modalStyles.messageInputContainerWithNotice,
								isCommentCounterVisible && modalStyles.messageInputContainerWithCounter,
							)}
							data-flx="messaging.forward-modal.div--9"
						>
							<LexicalRichInput
								richInputRef={richInputRef}
								initialValue=""
								initialSegments={EMPTY_FORWARD_COMMENT_SEGMENTS}
								className={modalStyles.richInput}
								channel={composerChannel}
								disabled={isCommentComposerDisabled}
								markdown={true}
								singleLine={false}
								submitOnEnter={true}
								size="form"
								autocompleteAnchor={containerElement}
								maxLength={user.maxMessageLength}
								onExceedMaxLength={handleOptionalMessageExceedsLimit}
								ariaLabel={i18n._(ADD_A_COMMENT_OPTIONAL_DESCRIPTOR)}
								ariaDescribedBy={commentNoticeDescriptionId}
								placeholder={commentPlaceholder}
								onChange={handleCommentChange}
								onSubmit={() => {
									void handleForward(false);
								}}
								i18n={i18n}
								data-flx="messaging.forward-modal.lexical-rich-input.comment-change"
							/>
							<MessageCharacterCounter
								currentLength={actualOptionalMessage.length}
								maxLength={user.maxMessageLength}
								canUpgrade={user.maxMessageLength < premiumMaxLength}
								premiumMaxLength={premiumMaxLength}
								data-flx="messaging.forward-modal.message-character-counter"
							/>
							{commentNotice != null && (
								<div
									id={commentNoticeId}
									className={modalStyles.slowmodeNotice}
									data-flx="messaging.forward-modal.div--10"
								>
									{commentNotice}
								</div>
							)}
							<div className={modalStyles.messageInputActions} data-flx="messaging.forward-modal.div--12">
								{isMobileLayout ? (
									<FocusRing offset={-2} data-flx="messaging.forward-modal.focus-ring--2">
										<button
											type="button"
											onClick={() => setExpressionPickerOpen(true)}
											disabled={isCommentBlockedBySlowmode}
											className={clsx(
												modalStyles.emojiPickerButton,
												expressionPickerOpen && modalStyles.emojiPickerButtonActive,
											)}
											aria-label={i18n._(OPEN_EMOJI_PICKER_DESCRIPTOR)}
											aria-haspopup="dialog"
											aria-expanded={expressionPickerOpen}
											data-flx="messaging.forward-modal.button.set-expression-picker-open"
										>
											<SmileyIcon
												className={modalStyles.emojiIcon}
												weight="fill"
												data-flx="messaging.forward-modal.smiley-icon"
											/>
										</button>
									</FocusRing>
								) : (
									<Popout
										position="top-end"
										animationType="none"
										offsetMainAxis={8}
										offsetCrossAxis={0}
										onOpen={() => setExpressionPickerOpen(true)}
										onClose={() => {
											setExpressionPickerOpen(false);
											focusForwardComment(richInputRef.current);
										}}
										render={({onClose}) => (
											<ExpressionPickerPopout
												channelId={message.channelId}
												onEmojiSelect={(emoji, shiftKey) => {
													const didInsert = insertForwardEmoji(richInputRef.current, emoji);
													if (didInsert) {
														focusForwardComment(richInputRef.current);
													}
													if (didInsert && shiftKey !== true) {
														onClose();
													}
												}}
												onClose={onClose}
												visibleTabs={['emojis']}
												data-flx="messaging.forward-modal.expression-picker-popout"
											/>
										)}
										data-flx="messaging.forward-modal.popout"
									>
										<FocusRing offset={-2} data-flx="messaging.forward-modal.focus-ring--3">
											<button
												type="button"
												disabled={isCommentBlockedBySlowmode}
												className={clsx(
													modalStyles.emojiPickerButton,
													expressionPickerOpen && modalStyles.emojiPickerButtonActive,
												)}
												aria-label={i18n._(OPEN_EMOJI_PICKER_DESCRIPTOR)}
												aria-haspopup="dialog"
												aria-expanded={expressionPickerOpen}
												data-flx="messaging.forward-modal.button--2"
											>
												<SmileyIcon
													className={modalStyles.emojiIcon}
													weight="fill"
													data-flx="messaging.forward-modal.smiley-icon--2"
												/>
											</button>
										</FocusRing>
									</Popout>
								)}
							</div>
						</div>
					</FocusRing>
				</div>
				<Modal.Footer data-flx="messaging.forward-modal.modal-footer">
					<Button variant="secondary" onClick={() => ModalCommands.pop()} data-flx="messaging.forward-modal.button.pop">
						<Trans>Cancel</Trans>
					</Button>
					<Button
						onClick={(event: MouseEvent<HTMLButtonElement>) => handleForward(event.shiftKey)}
						disabled={
							selected.length === 0 ||
							isForwarding ||
							isSendBlockedBySlowmode ||
							(!isCommentComposerDisabled && isCommentOverLimit)
						}
						data-flx="messaging.forward-modal.button.forward"
					>
						{i18n._(SEND_SELECTED_COUNT_DESCRIPTOR, {
							selectedCount: selected.length,
							selectionLimit: MAX_FORWARD_DESTINATIONS,
						})}
					</Button>
				</Modal.Footer>
				{isMobileLayout && (
					<ExpressionPickerSheet
						isOpen={expressionPickerOpen}
						onClose={() => setExpressionPickerOpen(false)}
						channelId={message.channelId}
						onEmojiSelect={(emoji, shiftKey) => {
							const didInsert = insertForwardEmoji(richInputRef.current, emoji);
							if (didInsert && shiftKey !== true) {
								setExpressionPickerOpen(false);
							}
						}}
						visibleTabs={['emojis']}
						selectedTab="emojis"
						zIndex={30000}
						data-flx="messaging.forward-modal.expression-picker-sheet"
					/>
				)}
			</Modal.Root>
		);
	},
);
