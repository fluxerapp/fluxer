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

import * as MessageActionCreators from '@app/actions/MessageActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as NavigationActionCreators from '@app/actions/NavigationActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {MessageForwardFailedModal} from '@app/components/alerts/MessageForwardFailedModal';
import {Autocomplete} from '@app/components/channel/Autocomplete';
import {MessageCharacterCounter} from '@app/components/channel/MessageCharacterCounter';
import {GroupDMAvatar} from '@app/components/common/GroupDMAvatar';
import {Input} from '@app/components/form/Input';
import {ExpressionPickerSheet} from '@app/components/modals/ExpressionPickerSheet';
import modalStyles from '@app/components/modals/ForwardModal.module.css';
import * as Modal from '@app/components/modals/Modal';
import {
	getForwardChannelCategoryName,
	getForwardChannelDisplayName,
	getForwardChannelGuildName,
	useForwardChannelSelection,
} from '@app/components/modals/shared/ForwardChannelSelection';
import selectorStyles from '@app/components/modals/shared/SelectorModalStyles.module.css';
import {ExpressionPickerPopout} from '@app/components/popouts/ExpressionPickerPopout';
import {Button} from '@app/components/uikit/button/Button';
import {Checkbox} from '@app/components/uikit/checkbox/Checkbox';
import FocusRing from '@app/components/uikit/focus_ring/FocusRing';
import {Popout} from '@app/components/uikit/popout/Popout';
import {Scroller} from '@app/components/uikit/Scroller';
import {StatusAwareAvatar} from '@app/components/uikit/StatusAwareAvatar';
import {useTextareaAutocomplete} from '@app/hooks/useTextareaAutocomplete';
import {useTextareaEmojiPicker} from '@app/hooks/useTextareaEmojiPicker';
import {useTextareaPaste} from '@app/hooks/useTextareaPaste';
import {useTextareaSegments} from '@app/hooks/useTextareaSegments';
import {Logger} from '@app/lib/Logger';
import {TextareaAutosize} from '@app/lib/TextareaAutosize';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import type {MessageRecord} from '@app/records/MessageRecord';
import type {UserRecord} from '@app/records/UserRecord';
import ChannelStore from '@app/stores/ChannelStore';
import MobileLayoutStore from '@app/stores/MobileLayoutStore';
import UserStore from '@app/stores/UserStore';
import {Limits} from '@app/utils/limits/UserLimits';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {MAX_MESSAGE_LENGTH_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {HashIcon, MagnifyingGlassIcon, NotePencilIcon, SmileyIcon, SpeakerHighIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useRef, useState} from 'react';

const logger = new Logger('ForwardModal');

interface ForwardModalProps {
	message: MessageRecord;
	user: UserRecord;
}

export const ForwardModal = observer(({message, user}: ForwardModalProps) => {
	const {t} = useLingui();
	const {filteredChannels, handleToggleChannel, isChannelDisabled, searchQuery, selectedChannelIds, setSearchQuery} =
		useForwardChannelSelection({excludedChannelId: message.channelId});
	const [optionalMessage, setOptionalMessage] = useState('');
	const [isForwarding, setIsForwarding] = useState(false);
	const [expressionPickerOpen, setExpressionPickerOpen] = useState(false);
	const textareaRef = useRef<HTMLTextAreaElement>(null);
	const containerRef = useRef<HTMLDivElement>(null);
	const premiumMaxLength = Limits.getPremiumValue('max_message_length', MAX_MESSAGE_LENGTH_PREMIUM);
	const mobileLayout = MobileLayoutStore;

	const {segmentManagerRef, previousValueRef, displayToActual, handleTextChange} = useTextareaSegments();
	const handleOptionalMessageExceedsLimit = useCallback(() => {
		ToastActionCreators.error(t`Message is too long`);
	}, [t]);
	const {handleEmojiSelect} = useTextareaEmojiPicker({
		setValue: setOptionalMessage,
		textareaRef,
		segmentManagerRef,
		previousValueRef,
		maxActualLength: user.maxMessageLength,
		onExceedMaxLength: handleOptionalMessageExceedsLimit,
	});

	const channel = ChannelStore.getChannel(message.channelId)!;
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
		value: optionalMessage,
		setValue: setOptionalMessage,
		textareaRef,
		segmentManagerRef,
		previousValueRef,
		maxActualLength: user.maxMessageLength,
		onExceedMaxLength: handleOptionalMessageExceedsLimit,
	});

	useTextareaPaste({
		channel,
		textareaRef,
		segmentManagerRef,
		setValue: setOptionalMessage,
		previousValueRef,
		maxMessageLength: user.maxMessageLength,
		onPasteExceedsLimit: () => handleOptionalMessageExceedsLimit(),
	});

	const actualOptionalMessage = useMemo(() => displayToActual(optionalMessage), [displayToActual, optionalMessage]);
	const optionalMessageDisplayMaxLength = useMemo(() => {
		return Math.max(0, optionalMessage.length + (user.maxMessageLength - actualOptionalMessage.length));
	}, [actualOptionalMessage.length, user.maxMessageLength, optionalMessage.length]);

	const handleForward = async () => {
		if (selectedChannelIds.size === 0 || isForwarding) return;

		setIsForwarding(true);
		try {
			const actualMessage = optionalMessage.trim() ? actualOptionalMessage : undefined;
			await MessageActionCreators.forward(
				Array.from(selectedChannelIds),
				{
					message_id: message.id,
					channel_id: message.channelId,
					guild_id: channel.guildId,
				},
				actualMessage,
			);
			ToastActionCreators.createToast({
				type: 'success',
				children: <Trans>Message forwarded</Trans>,
			});
			ModalActionCreators.pop();

			if (selectedChannelIds.size === 1) {
				const forwardedChannelId = Array.from(selectedChannelIds)[0];
				const forwardedChannel = ChannelStore.getChannel(forwardedChannelId);
				if (forwardedChannel) {
					NavigationActionCreators.selectChannel(forwardedChannel.guildId ?? undefined, forwardedChannelId);
				}
			}
		} catch (error) {
			logger.error('Failed to forward message:', error);
			ModalActionCreators.push(modal(() => <MessageForwardFailedModal />));
		} finally {
			setIsForwarding(false);
		}
	};

	const getChannelIcon = (ch: ChannelRecord) => {
		const iconSize = 32;

		if (ch.type === ChannelTypes.DM_PERSONAL_NOTES) {
			return <NotePencilIcon className={selectorStyles.itemIcon} weight="fill" size={iconSize} />;
		}
		if (ch.type === ChannelTypes.DM) {
			const recipientId = ch.recipientIds[0];
			const user = UserStore.getUser(recipientId);
			if (!user) return null;
			return (
				<div className={selectorStyles.avatar}>
					<StatusAwareAvatar user={user} size={iconSize} />
				</div>
			);
		}
		if (ch.type === ChannelTypes.GROUP_DM) {
			return (
				<div className={selectorStyles.avatar}>
					<GroupDMAvatar channel={ch} size={iconSize} />
				</div>
			);
		}
		if (ch.type === ChannelTypes.GUILD_VOICE) {
			return <SpeakerHighIcon className={selectorStyles.itemIcon} weight="fill" size={iconSize} />;
		}
		return <HashIcon className={selectorStyles.itemIcon} weight="bold" size={iconSize} />;
	};

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Forward Message`}>
				<div className={selectorStyles.headerSearch}>
					<Input
						type="text"
						value={searchQuery}
						onChange={(e) => setSearchQuery(e.target.value)}
						placeholder={t`Search channels or DMs`}
						maxLength={100}
						leftIcon={<MagnifyingGlassIcon className={selectorStyles.searchIcon} weight="bold" />}
						className={selectorStyles.headerSearchInput}
					/>
				</div>
			</Modal.Header>
			<Modal.Content className={selectorStyles.selectorContent}>
				<div className={selectorStyles.listContainer}>
					<Scroller className={selectorStyles.scroller} key="forward-modal-channel-list-scroller" fade={false}>
						{filteredChannels.length === 0 ? (
							<div className={selectorStyles.emptyState}>
								<Trans>No channels found</Trans>
							</div>
						) : (
							<div className={selectorStyles.itemList}>
								{filteredChannels.map((ch: ChannelRecord | null) => {
									if (!ch) return null;
									const isSelected = selectedChannelIds.has(ch.id);
									const isDisabled = isChannelDisabled(ch.id);
									const displayName = getForwardChannelDisplayName(ch);
									const categoryName = getForwardChannelCategoryName(ch);
									const guildName = getForwardChannelGuildName(ch);

									return (
										<FocusRing key={ch.id} offset={-2} enabled={!isDisabled}>
											<button
												type="button"
												onClick={() => !isDisabled && handleToggleChannel(ch.id)}
												disabled={isDisabled}
												className={clsx(
													selectorStyles.itemButton,
													isSelected && selectorStyles.itemButtonSelected,
													isDisabled && selectorStyles.itemButtonDisabled,
												)}
											>
												<div className={selectorStyles.itemContent}>
													{getChannelIcon(ch)}
													<div className={selectorStyles.itemInfo}>
														<span className={selectorStyles.itemName}>{displayName}</span>
														{ch.type === ChannelTypes.GUILD_TEXT ? (
															<span className={selectorStyles.itemSecondary}>
																{categoryName ? categoryName : t`No Category`}
																{guildName && ` • ${guildName}`}
															</span>
														) : (
															guildName && <span className={selectorStyles.itemSecondary}>{guildName}</span>
														)}
													</div>
												</div>
												<div className={selectorStyles.itemAction}>
													<Checkbox checked={isSelected} disabled={isDisabled} aria-hidden={true} />
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
			<div className={modalStyles.inputAreaContainer}>
				{isAutocompleteAttached && (
					<Autocomplete
						type={autocompleteType}
						onSelect={handleSelect}
						selectedIndex={selectedIndex}
						options={autocompleteOptions}
						setSelectedIndex={setSelectedIndex}
						referenceElement={containerRef.current}
						zIndex={20000}
						query={autocompleteQuery}
					/>
				)}

				<div ref={containerRef} className={modalStyles.messageInputContainer}>
					<TextareaAutosize
						className={clsx(modalStyles.messageInput, modalStyles.messageInputBase)}
						maxLength={optionalMessageDisplayMaxLength}
						ref={textareaRef}
						value={optionalMessage}
						onChange={(e) => {
							const newValue = e.target.value;
							handleTextChange(newValue, previousValueRef.current);
							setOptionalMessage(newValue);
						}}
						onKeyDown={(e) => {
							onCursorMove();
							if (isAutocompleteAttached) {
								if (e.key === 'ArrowUp' || e.key === 'ArrowDown') {
									e.preventDefault();
									setSelectedIndex((prevIndex) => {
										const newIndex = e.key === 'ArrowUp' ? prevIndex - 1 : prevIndex + 1;
										return (newIndex + autocompleteOptions.length) % autocompleteOptions.length;
									});
								} else if (e.key === 'Tab' || (e.key === 'Enter' && !e.shiftKey)) {
									e.preventDefault();
									const selectedOption = autocompleteOptions[selectedIndex];
									if (selectedOption) {
										handleSelect(selectedOption);
									}
								}
							}
						}}
						placeholder={t`Add a comment (optional)`}
					/>
					<MessageCharacterCounter
						currentLength={actualOptionalMessage.length}
						maxLength={user.maxMessageLength}
						canUpgrade={user.maxMessageLength < premiumMaxLength}
						premiumMaxLength={premiumMaxLength}
					/>
					<div className={modalStyles.messageInputActions}>
						{mobileLayout.enabled ? (
							<FocusRing offset={-2}>
								<button
									type="button"
									onClick={() => setExpressionPickerOpen(true)}
									className={clsx(
										modalStyles.emojiPickerButton,
										expressionPickerOpen && modalStyles.emojiPickerButtonActive,
									)}
								>
									<SmileyIcon className={modalStyles.emojiIcon} weight="fill" />
								</button>
							</FocusRing>
						) : (
							<Popout
								position="top-end"
								animationType="none"
								offsetMainAxis={8}
								offsetCrossAxis={0}
								onOpen={() => setExpressionPickerOpen(true)}
								onClose={() => setExpressionPickerOpen(false)}
								returnFocusRef={textareaRef}
								render={({onClose}) => (
									<ExpressionPickerPopout
										channelId={message.channelId}
										onEmojiSelect={(emoji, shiftKey) => {
											const didInsert = handleEmojiSelect(emoji, shiftKey);
											if (didInsert && !shiftKey) {
												onClose();
											}
										}}
										onClose={onClose}
										visibleTabs={['emojis']}
									/>
								)}
							>
								<FocusRing offset={-2}>
									<button
										type="button"
										className={clsx(
											modalStyles.emojiPickerButton,
											expressionPickerOpen && modalStyles.emojiPickerButtonActive,
										)}
									>
										<SmileyIcon className={modalStyles.emojiIcon} weight="fill" />
									</button>
								</FocusRing>
							</Popout>
						)}
					</div>
				</div>
			</div>
			<Modal.Footer>
				<div className={selectorStyles.actionRow}>
					<Button variant="secondary" onClick={() => ModalActionCreators.pop()} className={selectorStyles.actionButton}>
						<Trans>Cancel</Trans>
					</Button>
					<Button
						onClick={handleForward}
						disabled={selectedChannelIds.size === 0 || isForwarding}
						className={selectorStyles.actionButton}
					>
						<Trans>Send ({selectedChannelIds.size}/5)</Trans>
					</Button>
				</div>
			</Modal.Footer>
			{mobileLayout.enabled && (
				<ExpressionPickerSheet
					isOpen={expressionPickerOpen}
					onClose={() => setExpressionPickerOpen(false)}
					channelId={message.channelId}
					onEmojiSelect={(emoji, shiftKey) => {
						const didInsert = handleEmojiSelect(emoji, shiftKey);
						if (didInsert && !shiftKey) {
							setExpressionPickerOpen(false);
						}
						return didInsert;
					}}
					visibleTabs={['emojis']}
					selectedTab="emojis"
					zIndex={30000}
				/>
			)}
		</Modal.Root>
	);
});
