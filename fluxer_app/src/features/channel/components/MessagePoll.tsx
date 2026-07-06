// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/MessagePoll.module.css';
import {
	getPollOptionPercentage,
	getPollOptionRank,
	getPollOptionResultValue,
	getPollTotalResultValue,
	getSelectedPollOptionIds,
	isPollClosed,
	togglePollOptionSelection,
} from '@app/features/channel/utils/MessagePollState';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import Permission from '@app/features/permissions/state/Permission';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_POLL_OPTION_TEXT_LENGTH} from '@fluxer/constants/src/LimitConstants';
import type {PollOptionResponse} from '@fluxer/schema/src/domains/message/PollSchemas';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckIcon, ImageIcon, LockIcon, PlusIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {type ChangeEvent, type FormEvent, useCallback, useEffect, useMemo, useRef, useState} from 'react';

const ANONYMOUS_DESCRIPTOR = msg({
	message: 'Anonymous',
	comment: 'Short poll visibility label in a chat message.',
});
const PUBLIC_DESCRIPTOR = msg({
	message: 'Public',
	comment: 'Short poll visibility label in a chat message.',
});
const RANKED_DESCRIPTOR = msg({
	message: 'Ranked',
	comment: 'Short poll mode label in a chat message.',
});
const CLOSED_DESCRIPTOR = msg({
	message: 'Closed',
	comment: 'Short poll state label in a chat message.',
});
const VOTE_DESCRIPTOR = msg({
	message: 'Vote',
	comment: 'Button label for submitting a poll vote.',
});
const CLEAR_VOTE_DESCRIPTOR = msg({
	message: 'Clear vote',
	comment: 'Button label for removing your poll vote.',
});
const CLOSE_POLL_DESCRIPTOR = msg({
	message: 'Close poll',
	comment: 'Button label for closing a poll.',
});
const ADD_OPTION_DESCRIPTOR = msg({
	message: 'Add option',
	comment: 'Button label for adding a custom poll option.',
});
const ADD_OPTION_IMAGE_DESCRIPTOR = msg({
	message: 'Add image',
	comment: 'Button label for selecting an image for a custom poll option.',
});
const REMOVE_OPTION_IMAGE_DESCRIPTOR = msg({
	message: 'Remove image',
	comment: 'Button label for removing a selected image from a custom poll option before submitting.',
});
const CUSTOM_OPTION_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Add an answer',
	comment: 'Placeholder for the custom poll option input.',
});
const POLL_ACTION_FAILED_DESCRIPTOR = msg({
	message: 'Poll action failed. Try again.',
	comment: 'Toast shown when a poll request fails.',
});
const OPTION_ARIA_DESCRIPTOR = msg({
	message: '{optionText}: {resultText}, {percentage} percent',
	comment: 'Accessible label for a poll option button. Preserve placeholders.',
});

function getOptionImageUrl(message: Message, option: PollOptionResponse): string | null {
	if (!option.attachment_id) {
		return null;
	}
	const attachment = message.attachments.find((item) => item.id === option.attachment_id);
	if (!attachment?.content_type?.toLowerCase().startsWith('image/')) {
		return null;
	}
	return attachment.proxy_url ?? attachment.url ?? null;
}

export const MessagePoll = ({message}: {message: Message}) => {
	const {i18n} = useLingui();
	const poll = message.poll;
	const [selectedOptionIds, setSelectedOptionIds] = useState<Array<string>>(() =>
		poll ? getSelectedPollOptionIds(poll) : [],
	);
	const [customOptionText, setCustomOptionText] = useState('');
	const [customOptionImage, setCustomOptionImage] = useState<File | null>(null);
	const [pendingAction, setPendingAction] = useState<'vote' | 'clear' | 'close' | 'custom' | null>(null);
	const customOptionImageInputRef = useRef<HTMLInputElement>(null);
	useEffect(() => {
		setSelectedOptionIds(poll ? getSelectedPollOptionIds(poll) : []);
	}, [poll]);
	const currentVoteOptionIds = useMemo(() => (poll ? getSelectedPollOptionIds(poll) : []), [poll]);
	const totalResultValue = useMemo(() => (poll ? getPollTotalResultValue(poll) : 0), [poll]);
	const closed = poll ? isPollClosed(poll) : false;
	const canClosePoll =
		poll != null &&
		!closed &&
		(message.isCurrentUserAuthor() || Permission.can(Permissions.MANAGE_MESSAGES, {channelId: message.channelId}));
	const canSubmitVote = poll != null && !closed && selectedOptionIds.length > 0 && pendingAction == null;
	const handlePollError = useCallback(() => {
		ToastCommands.error(i18n._(POLL_ACTION_FAILED_DESCRIPTOR));
	}, [i18n]);
	const submitVote = useCallback(
		async (optionIds: ReadonlyArray<string>) => {
			if (!poll || closed || optionIds.length === 0 || pendingAction) {
				return;
			}
			setPendingAction('vote');
			try {
				await MessageCommands.votePoll(message.channelId, message.id, optionIds);
			} catch {
				handlePollError();
			} finally {
				setPendingAction(null);
			}
		},
		[closed, handlePollError, message.channelId, message.id, pendingAction, poll],
	);
	const handleOptionClick = useCallback(
		(optionId: string) => {
			if (!poll || closed || pendingAction) {
				return;
			}
			const nextSelection = togglePollOptionSelection(selectedOptionIds, optionId, poll.allow_ranked_choice);
			setSelectedOptionIds(nextSelection);
			if (!poll.allow_ranked_choice) {
				void submitVote(nextSelection);
			}
		},
		[closed, pendingAction, poll, selectedOptionIds, submitVote],
	);
	const handleSubmitRankedVote = useCallback(() => {
		void submitVote(selectedOptionIds);
	}, [selectedOptionIds, submitVote]);
	const handleClearVote = useCallback(async () => {
		if (!poll || pendingAction) {
			return;
		}
		if (currentVoteOptionIds.length === 0) {
			setSelectedOptionIds([]);
			return;
		}
		setPendingAction('clear');
		try {
			await MessageCommands.removeOwnPollVote(message.channelId, message.id);
		} catch {
			handlePollError();
		} finally {
			setPendingAction(null);
		}
	}, [currentVoteOptionIds.length, handlePollError, message.channelId, message.id, pendingAction, poll]);
	const handleClosePoll = useCallback(async () => {
		if (!poll || !canClosePoll || pendingAction) {
			return;
		}
		setPendingAction('close');
		try {
			await MessageCommands.closePoll(message.channelId, message.id);
		} catch {
			handlePollError();
		} finally {
			setPendingAction(null);
		}
	}, [canClosePoll, handlePollError, message.channelId, message.id, pendingAction, poll]);
	const handleCustomOptionSubmit = useCallback(
		async (event: FormEvent<HTMLFormElement>) => {
			event.preventDefault();
			const text = customOptionText.trim();
			if (!poll || closed || pendingAction || !text) {
				return;
			}
			setPendingAction('custom');
			try {
				await MessageCommands.addCustomPollOption(message.channelId, message.id, text, customOptionImage);
				setCustomOptionText('');
				setCustomOptionImage(null);
				if (customOptionImageInputRef.current) {
					customOptionImageInputRef.current.value = '';
				}
			} catch {
				handlePollError();
			} finally {
				setPendingAction(null);
			}
		},
		[closed, customOptionImage, customOptionText, handlePollError, message.channelId, message.id, pendingAction, poll],
	);
	const handleCustomOptionImageChange = useCallback((event: ChangeEvent<HTMLInputElement>) => {
		setCustomOptionImage(event.currentTarget.files?.[0] ?? null);
	}, []);
	const handleChooseCustomOptionImage = useCallback(() => {
		customOptionImageInputRef.current?.click();
	}, []);
	const handleClearCustomOptionImage = useCallback(() => {
		setCustomOptionImage(null);
		if (customOptionImageInputRef.current) {
			customOptionImageInputRef.current.value = '';
		}
	}, []);
	if (!poll) {
		return null;
	}
	return (
		<div className={styles.poll} data-flx="channel.message-poll.poll">
			<div className={styles.header} data-flx="channel.message-poll.header">
				<h4 className={styles.title} data-flx="channel.message-poll.title">
					{poll.title}
				</h4>
				<div className={styles.metaRow} data-flx="channel.message-poll.meta-row">
					<span className={styles.metaPill} data-flx="channel.message-poll.meta-pill.visibility">
						{poll.anonymous ? i18n._(ANONYMOUS_DESCRIPTOR) : i18n._(PUBLIC_DESCRIPTOR)}
					</span>
					{poll.allow_ranked_choice && (
						<span className={styles.metaPill} data-flx="channel.message-poll.meta-pill.ranked">
							{i18n._(RANKED_DESCRIPTOR)}
						</span>
					)}
					{closed && (
						<span className={styles.metaPill} data-flx="channel.message-poll.meta-pill.closed">
							<LockIcon size={12} weight="bold" data-flx="channel.message-poll.lock-icon" />
							{'\u00a0'}
							{i18n._(CLOSED_DESCRIPTOR)}
						</span>
					)}
				</div>
			</div>
			<div className={styles.options} data-flx="channel.message-poll.options">
				{poll.options.map((option) => {
					const resultValue = getPollOptionResultValue(option, poll.allow_ranked_choice);
					const percentage = getPollOptionPercentage(resultValue, totalResultValue);
					const resultText = poll.allow_ranked_choice
						? plural(
								{count: resultValue},
								{
									one: '# point',
									other: '# points',
								},
							)
						: plural(
								{count: option.vote_count},
								{
									one: '# vote',
									other: '# votes',
								},
							);
					const firstChoiceText = plural(
						{count: option.vote_count},
						{
							one: '# first choice',
							other: '# first choices',
						},
					);
					const optionImageUrl = getOptionImageUrl(message, option);
					const rank = getPollOptionRank(selectedOptionIds, option.id);
					const selected = rank != null;
					return (
						<button
							key={option.id}
							type="button"
							className={clsx(styles.optionButton, selected && styles.optionButtonSelected)}
							aria-label={i18n._(OPTION_ARIA_DESCRIPTOR, {
								optionText: option.text,
								resultText,
								percentage,
							})}
							aria-pressed={selected}
							disabled={closed || pendingAction != null}
							onClick={() => handleOptionClick(option.id)}
							data-flx="channel.message-poll.option-button.select"
						>
							<span
								className={styles.optionFill}
								style={{width: `${percentage}%`}}
								aria-hidden="true"
								data-flx="channel.message-poll.option-fill"
							/>
							<span className={styles.optionContent} data-flx="channel.message-poll.option-content">
								{rank != null && (
									<span className={styles.rankBadge} data-flx="channel.message-poll.rank-badge">
										{rank}
									</span>
								)}
								{optionImageUrl && (
									<img
										src={optionImageUrl}
										alt=""
										className={styles.optionImage}
										data-flx="channel.message-poll.option-image"
									/>
								)}
								<span className={styles.optionText} data-flx="channel.message-poll.option-text">
									{option.text}
								</span>
							</span>
							<span className={styles.optionStats} data-flx="channel.message-poll.option-stats">
								<span data-flx="channel.message-poll.option-percentage">{percentage}%</span>
								<span data-flx="channel.message-poll.option-votes">{resultText}</span>
								{poll.allow_ranked_choice && (
									<span data-flx="channel.message-poll.option-first-choices">{firstChoiceText}</span>
								)}
							</span>
						</button>
					);
				})}
			</div>
			<div className={styles.actions} data-flx="channel.message-poll.actions">
				{poll.allow_ranked_choice && !closed && (
					<button
						type="button"
						className={styles.actionButton}
						disabled={!canSubmitVote}
						onClick={handleSubmitRankedVote}
						data-flx="channel.message-poll.vote-button.submit"
					>
						<CheckIcon size={14} weight="bold" data-flx="channel.message-poll.vote-button.check-icon" />
						<span data-flx="channel.message-poll.vote-button-label">{i18n._(VOTE_DESCRIPTOR)}</span>
					</button>
				)}
				{(currentVoteOptionIds.length > 0 || selectedOptionIds.length > 0) && !closed && (
					<button
						type="button"
						className={styles.actionButton}
						disabled={pendingAction != null}
						onClick={handleClearVote}
						data-flx="channel.message-poll.clear-vote-button.clear"
					>
						<XIcon size={14} weight="bold" data-flx="channel.message-poll.clear-vote-button.x-icon" />
						<span data-flx="channel.message-poll.clear-vote-label">{i18n._(CLEAR_VOTE_DESCRIPTOR)}</span>
					</button>
				)}
				{canClosePoll && (
					<button
						type="button"
						className={styles.actionButton}
						disabled={pendingAction != null}
						onClick={handleClosePoll}
						data-flx="channel.message-poll.close-poll-button.close"
					>
						<LockIcon size={14} weight="bold" data-flx="channel.message-poll.close-poll-button.lock-icon" />
						<span data-flx="channel.message-poll.close-poll-label">{i18n._(CLOSE_POLL_DESCRIPTOR)}</span>
					</button>
				)}
				{poll.allow_custom_answers && !closed && (
					<form
						className={styles.customOptionForm}
						onSubmit={handleCustomOptionSubmit}
						data-flx="channel.message-poll.custom-option-form.submit"
					>
						<input
							className={styles.customOptionInput}
							value={customOptionText}
							disabled={pendingAction != null}
							maxLength={MAX_POLL_OPTION_TEXT_LENGTH}
							placeholder={i18n._(CUSTOM_OPTION_PLACEHOLDER_DESCRIPTOR)}
							onChange={(event) => setCustomOptionText(event.target.value)}
							data-flx="channel.message-poll.custom-option-input"
						/>
						<input
							ref={customOptionImageInputRef}
							className={styles.customOptionFileInput}
							type="file"
							accept="image/*"
							disabled={pendingAction != null}
							onChange={handleCustomOptionImageChange}
							data-flx="channel.message-poll.custom-option-image-input"
						/>
						<button
							type="button"
							className={styles.iconButton}
							aria-label={i18n._(ADD_OPTION_IMAGE_DESCRIPTOR)}
							disabled={pendingAction != null}
							onClick={handleChooseCustomOptionImage}
							data-flx="channel.message-poll.add-option-image-button.choose"
						>
							<ImageIcon size={14} weight="bold" data-flx="channel.message-poll.add-option-image-button.image-icon" />
						</button>
						{customOptionImage && (
							<button
								type="button"
								className={styles.customOptionImageChip}
								aria-label={i18n._(REMOVE_OPTION_IMAGE_DESCRIPTOR)}
								disabled={pendingAction != null}
								onClick={handleClearCustomOptionImage}
								data-flx="channel.message-poll.custom-option-image-chip.remove"
							>
								<span data-flx="channel.message-poll.custom-option-image-name">{customOptionImage.name}</span>
								<XIcon size={12} weight="bold" data-flx="channel.message-poll.custom-option-image-remove-icon" />
							</button>
						)}
						<button
							type="submit"
							className={styles.iconButton}
							aria-label={i18n._(ADD_OPTION_DESCRIPTOR)}
							disabled={pendingAction != null || customOptionText.trim().length === 0}
							data-flx="channel.message-poll.add-option-button.add"
						>
							<PlusIcon size={14} weight="bold" data-flx="channel.message-poll.add-option-button.plus-icon" />
						</button>
					</form>
				)}
			</div>
		</div>
	);
};
