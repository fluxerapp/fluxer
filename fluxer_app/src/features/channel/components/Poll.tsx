// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/Poll.module.css';
import Emoji from '@app/features/emoji/state/Emoji';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import type {Guild} from '@app/features/guild/models/Guild';
import {PollAnswerVotersModal} from '@app/features/messaging/components/modals/poll_modal/PollAnswerVotersModal';
import Permission from '@app/features/permissions/state/Permission';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import Users from '@app/features/user/state/Users';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {
	MessagePoll,
	MessagePollAnswerCount,
	MessagePollEmoji,
} from '@fluxer/schema/src/domains/message/PollSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useState} from 'react';

const SELECT_ONE_ANSWER_DESCRIPTOR = msg({
	message: 'Select one answer',
	comment: 'Small explanatory text instructing the user to select one answer on the poll.',
});
const SELECT_ONE_OR_MULTIPLE_ANSWERS_DESCRIPTOR = msg({
	message: 'Select one or multiple answers',
	comment: 'Small explanatory text instructing the user to select one or multiple answers on the poll.',
});
const GO_BACK_TO_VOTE_DESCRIPTOR = msg({
	message: 'Go back to vote',
	comment: 'Label of the button to go back to vote when the user is looking at poll results.',
});
const REMOVE_VOTE_DESCRIPTOR = msg({
	message: 'Remove vote',
	comment: "Label of the button to remove the user's vote.",
});
const VOTE_DESCRIPTOR = msg({
	message: 'Vote',
	comment: 'Label of the vote button.',
});
const SHOW_RESULTS_DESCRIPTOR = msg({
	message: 'Show results',
	comment: 'Label of the button to show poll answer results.',
});
const THIS_POLL_IS_ANONYMOUS_DESCRIPTOR = msg({
	message: 'This poll is anonymous.',
	comment: 'Small disclaimer explaining to the user that this poll is anonymous.',
});
const THIS_POLL_IS_ANONYMOUS_BUT_YOU_CAN_SEE_VOTES_DESCRIPTOR = msg({
	message: 'This poll is anonymous, but you can see votes.',
	comment:
		'Small disclaimer explaining to the user that this poll is anonymous, but they can see votes because they have the permission for it.',
});
const VERIFY_YOU_EMAIL_ADDRESS_DESCRIPTOR = msg({
	message: 'You need to verify your email address to vote on a poll.',
	comment: 'Small disclaimer explaining to the user that email verification is necessary to vote on a poll.',
});
const LESS_THAN_ONE_MINUTE_LEFT_DESCRIPTOR = msg({
	message: '<1m left',
	comment: 'Very short text informing the user that the poll ends in less than one minute.',
});
const POLL_CLOSED_DESCRIPTOR = msg({
	message: 'Poll closed',
	comment: 'Small text informing the user that the poll is closed.',
});
const POLL_NOT_SENT_DESCRIPTOR = msg({
	message: 'Poll not sent',
	comment: 'Small text informing the user that the poll message has not been sent yet.',
});
export const VOTES_DESCRIPTOR = msg({
	message: '{count, plural, one {# vote} other {# votes}}',
	comment: 'Small text indicating the number of votes on the poll or on a specific answer of the poll.',
});
const WINNING_ANSWER = msg({
	message: 'Winning answer',
	comment: 'Label showing or describing the answer with the most votes on the poll.',
});
const SELECTED_ANSWER = msg({
	message: 'Selected answer',
	comment: 'Label showing the answer with the most votes on the poll.',
});
const ARIA_LABEL_ANSWER_QUALIFIER_SEPARATOR = msg({
	message: ', ',
	comment:
		'Separator that goes between answer qualifiers that are only readable for screenreaders. Example: "Winning answer[, ]Selected answer".',
});

function renderPollEmoji(pollEmoji?: MessagePollEmoji) {
	if (!pollEmoji) return undefined;

	const emoji = pollEmoji.id
		? Emoji.getEmojiById(pollEmoji.id)
		: pollEmoji.name
			? UnicodeEmojis.getByName(pollEmoji.name)
			: undefined;

	if (!emoji) return undefined;
	return <img src={emoji.url} alt={emoji.name} width="24" height="24" data-flx="poll.answer.emoji.img" />;
}

interface Answer {
	id: number;
	emoji?: MessagePollEmoji;
	text: string;
	me: boolean;
	votes: number;
	percentage: number;
	winner: boolean;
}

interface PollProps {
	guild?: Guild;
	channelId: string;
	messageId: string;
	isMobile: boolean;
	poll: MessagePoll;
	messageState: string;
	onVote?: (add: boolean, selectedAnswers: Array<number>) => void;
}

export const Poll = observer(({guild, channelId, messageId, isMobile, poll, messageState, onVote}: PollProps) => {
	const {i18n} = useLingui();

	const isSent = messageState === 'SENT';

	const currentUser = Users.getCurrentUser();
	const isVerified = useMemo(() => currentUser?.verified ?? false, [currentUser]);

	const canManageMessages = Permission.can(Permissions.MANAGE_MESSAGES, {
		guildId: guild?.id,
		channelId: channelId,
	});
	const canSeeVotesOnAnonymousPolls = Permission.can(Permissions.SEE_VOTES_ON_ANONYMOUS_POLLS, {
		guildId: guild?.id,
		channelId: channelId,
	});

	const canSeeVotes = useMemo(() => !poll.anonymous_voting || canManageMessages || canSeeVotesOnAnonymousPolls, [poll]);

	const answerCounts = poll.results?.answer_counts ?? [];
	const hasVoted = answerCounts.find((answerCount) => answerCount.me_voted) !== undefined;

	const [selectedAnswers, setSelectedAnswers] = useState<Array<number>>(
		answerCounts
			.filter((answerCount) => answerCount.id !== undefined && answerCount.me_voted)
			.map((answerCount) => answerCount.id ?? 0),
	);
	const [isVoting, setIsVoting] = useState(!hasVoted);
	const [isViewingResults, setIsViewingResults] = useState(false);

	const [now, setNow] = useState(Date.now());

	const totalVoteCount = useMemo(() => {
		let acc = 0;
		for (const answerCount of poll.results?.answer_counts ?? []) acc += answerCount.count ?? 0;
		return acc;
	}, [poll.results]);

	const secondsLeft = useMemo(() => {
		if (!poll.expiry) return 0;

		const expiryUts = Date.parse(poll.expiry) / 1000;
		const nowUts = now / 1000;

		return expiryUts - nowUts;
	}, [poll.expiry, now]);

	const isFinalized = useMemo(() => poll.results?.is_finalized, [poll]);
	const inVoteScreen = useMemo(
		() => isVoting && !isViewingResults && !isFinalized && isVerified,
		[isVoting, isViewingResults, isFinalized, isVerified],
	);

	if (secondsLeft > 0 && !isFinalized) {
		setTimeout(
			() => setNow(Date.now()),
			secondsLeft < 1.5 * 3600 ? 60_000 : secondsLeft < 1.5 * 86400 ? 3600_000 : 86400_000,
		);
	}

	function timeLeft(secondsLeft: number): React.ReactNode {
		if (secondsLeft < 60) return <>{i18n._(LESS_THAN_ONE_MINUTE_LEFT_DESCRIPTOR)}</>;
		if (secondsLeft < 3600) return <Trans>{Math.round(secondsLeft / 60)}m left</Trans>;
		if (secondsLeft < 86400) return <Trans>{Math.round(secondsLeft / 3600)}h left</Trans>;
		return <Trans>{Math.floor(secondsLeft / 86400)}d left</Trans>;
	}

	const answers = useMemo<Array<Answer>>(() => {
		const answerCountById: Array<MessagePollAnswerCount> = [];
		for (const answerCount of poll.results?.answer_counts ?? []) {
			if (answerCount) answerCountById[answerCount.id ?? 0] = answerCount;
		}

		const answers = (poll.answers ?? []).map((answer) => {
			const votes = answerCountById[answer.answer_id ?? 0] ?? 0;
			return {
				id: answer.answer_id ?? 0,
				emoji: answer.poll_media?.emoji,
				text: answer.poll_media?.text ?? '',
				me: votes.me_voted ?? false,
				votes: votes.count ?? 0,
				percentage: totalVoteCount > 0 ? ((votes.count ?? 0) * 100.0) / totalVoteCount : 0,
				winner: false,
			};
		});

		if (isFinalized) {
			let maxPercentage = 0;
			for (const answer of answers) maxPercentage = Math.max(maxPercentage, answer.percentage);

			if (maxPercentage > 0) {
				for (const answer of answers) {
					if (answer.percentage === maxPercentage) answer.winner = true;
				}
			}
		}

		return answers;
	}, [poll]);

	const openPollAnswerVotersModal = useCallback((initialAnswerId: number) => {
		ModalCommands.push(
			modal(() => (
				<PollAnswerVotersModal
					guild={guild}
					channelId={channelId}
					messageId={messageId}
					poll={poll}
					openToAnswerId={initialAnswerId}
					key={`poll-answers-modal-${messageId}`}
				/>
			)),
		);
	}, []);

	function answerAriaLabel(answer: Answer): string {
		const qualifiers = [];
		if (answer.me) qualifiers.push(i18n._(SELECTED_ANSWER));
		if (answer.winner) qualifiers.push(i18n._(WINNING_ANSWER));
		return qualifiers.join(i18n._(ARIA_LABEL_ANSWER_QUALIFIER_SEPARATOR));
	}

	return (
		<div data-flx="poll" className={styles.pollContainer} data-open={!isFinalized} data-state={messageState}>
			<h2 data-flx="poll.question">{poll.question?.text ?? ''}</h2>
			<p data-flx="poll.description">
				<small>
					{i18n._(poll.allow_multiselect ? SELECT_ONE_OR_MULTIPLE_ANSWERS_DESCRIPTOR : SELECT_ONE_ANSWER_DESCRIPTOR)}
				</small>
			</p>
			{answers.map((answer) => (
				<FocusRing key={answer.id} offset={-2} enabled={inVoteScreen} data-flx="poll.answer.focus-ring">
					<button
						type="button"
						key={answer.id}
						className={styles.answerButton}
						disabled={!isSent}
						onClick={() => {
							if (!inVoteScreen) return;
							setSelectedAnswers((prevSelectedAnswers) =>
								poll.allow_multiselect
									? prevSelectedAnswers.find((prevId) => prevId === answer.id) !== undefined
										? prevSelectedAnswers.filter((prevId) => prevId !== answer.id)
										: [...prevSelectedAnswers, answer.id]
									: [answer.id],
							);
						}}
						data-variant={answer.winner ? 'winner' : answer.me ? (isFinalized ? 'me-finalized' : 'me') : undefined}
						data-voting={inVoteScreen}
						data-checked={answer.me}
						aria-label={answerAriaLabel(answer)}
						data-flx="poll.answer.button"
					>
						{inVoteScreen ? undefined : (
							<div
								className={styles.answerPercentageBar}
								style={{
									width: `${Math.round(answer.percentage)}%`,
								}}
								data-flx="poll.answer.bar"
							/>
						)}
						<div className={styles.answerLayout}>
							{inVoteScreen ? (
								<Checkbox
									className={styles.answerCheckbox}
									type={poll.allow_multiselect ? 'box' : 'round'}
									checked={selectedAnswers.find((id) => id === answer.id) !== undefined}
									aria-hidden={true}
									data-flx="poll.answer.checkbox"
								/>
							) : undefined}
							<section className={styles.answerText} data-flx="poll.answer.section.text">
								{renderPollEmoji(answer.emoji)}
								<p data-flx="poll.answer.text">{answer.text}</p>
							</section>
							{inVoteScreen ? undefined : (
								<section data-flx="poll.answer.section.votes">
									<a
										role="button"
										// biome-ignore lint/a11y/useValidAnchor: Apparently I can't nest a button inside of a button because it's bad for hydration or something
										onClick={() => {
											if (canSeeVotes) openPollAnswerVotersModal(answer.id);
										}}
										aria-disabled={!canSeeVotes}
										className={styles.answerVotes}
										data-flx="poll.answer.vote-count"
									>
										{i18n._(VOTES_DESCRIPTOR, {count: answer.votes})}
									</a>
									<h2 className={styles.answerPercentage} data-flx="poll.answer.vote-percentage">
										{Math.round(answer.percentage)}%
									</h2>
									{answer.me ? (
										<CheckCircleIcon weight="fill" className={styles.answerMeSuccess} data-flx="poll.answer.me-check" />
									) : undefined}
								</section>
							)}
						</div>
					</button>
				</FocusRing>
			))}
			{poll.anonymous_voting && (
				<section>
					<p>
						<small>
							{i18n._(
								canSeeVotes
									? THIS_POLL_IS_ANONYMOUS_BUT_YOU_CAN_SEE_VOTES_DESCRIPTOR
									: THIS_POLL_IS_ANONYMOUS_DESCRIPTOR,
							)}
						</small>
					</p>
				</section>
			)}
			{isVerified || isFinalized ? undefined : (
				<section>
					<p>
						<small>{i18n._(VERIFY_YOU_EMAIL_ADDRESS_DESCRIPTOR)}</small>
					</p>
				</section>
			)}
			<footer data-flx="poll.footer" data-mobile={isMobile}>
				{isFinalized ? undefined : (
					<FocusRing data-flx="poll.footer.vote.button.focus-ring">
						<Button
							variant={isVoting ? 'primary' : 'secondary'}
							disabled={isViewingResults || (isVoting && selectedAnswers.length === 0) || !isSent || !isVerified}
							onClick={() => {
								setIsVoting((prevIsVoting) => !prevIsVoting);
								if (onVote) onVote(isVoting, selectedAnswers);
							}}
							data-flx="poll.footer.vote.button"
						>
							{i18n._(isVoting ? VOTE_DESCRIPTOR : REMOVE_VOTE_DESCRIPTOR)}
						</Button>
					</FocusRing>
				)}
				<section>
					<div className={styles.answerTotalVotes} data-flx="poll.footer.total-vote-count">
						{isFinalized
							? i18n._(POLL_CLOSED_DESCRIPTOR)
							: poll.expiry
								? timeLeft(secondsLeft)
								: i18n._(POLL_NOT_SENT_DESCRIPTOR)}{' '}
						·{' '}
						<button
							type="button"
							onClick={() => openPollAnswerVotersModal(1)}
							disabled={!canSeeVotes}
							className={styles.answerVotes}
							data-flx="poll.footer.vote-count"
						>
							{i18n._(VOTES_DESCRIPTOR, {count: totalVoteCount})}
						</button>
					</div>
					{isFinalized || !isVoting || !isVerified ? undefined : (
						<Button
							variant="secondary"
							disabled={!isSent}
							onClick={() => setIsViewingResults((prevIsViewingResults) => !prevIsViewingResults)}
							data-flx="poll.footer.show-results.button"
						>
							{i18n._(isViewingResults ? GO_BACK_TO_VOTE_DESCRIPTOR : SHOW_RESULTS_DESCRIPTOR)}
						</Button>
					)}
				</section>
			</footer>
		</div>
	);
});
