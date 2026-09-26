// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {VOTES_DESCRIPTOR} from '@app/features/channel/components/Poll';
import {PreloadableUserPopout} from '@app/features/channel/components/PreloadableUserPopout';
import type {Guild} from '@app/features/guild/models/Guild';
import {useMessagePollAnswerVotersState} from '@app/features/messaging/hooks/useMessagePollAnswerVotersState';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {MessagePoll} from '@fluxer/schema/src/domains/message/PollSchemas';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, type UIEvent} from 'react';
import * as styles from './PollAnswerVotersModal.module.css';

interface PollAnswerVotersModalProps {
	guild?: Guild;
	channelId: string;
	messageId: string;
	message?: Message | null;
	poll: MessagePoll;
	openToAnswerId: number;
}

export const PollAnswerVotersModal = observer(
	({guild, channelId, messageId, message: messageFallback, poll, openToAnswerId}: PollAnswerVotersModalProps) => {
		const {i18n} = useLingui();
		const {
			message,
			selectedAnswerId,
			setSelectedAnswerId,
			votes,
			voters,
			isLoading,
			hasMore,
			loadMore,
			guildId,
			voterScrollerKey,
		} = useMessagePollAnswerVotersState({
			channelId,
			messageId,
			message: messageFallback,
			openToAnswerId,
			poll,
			isOpen: true,
			onMissingMessage: () => ModalCommands.pop(),
		});

		const handleScroll = useCallback(
			(event: UIEvent<HTMLDivElement>) => {
				if (!loadMore || !hasMore || isLoading) {
					return;
				}
				const target = event.currentTarget;
				const scrollPercentage = (target.scrollTop + target.offsetHeight) / target.scrollHeight;
				if (scrollPercentage > 0.8) {
					loadMore();
				}
			},
			[hasMore, isLoading, loadMore],
		);

		const answerVoteArray = useMemo(() => {
			const array = [];
			for (const answerCount of votes ?? []) {
				array[answerCount.id ?? 0] = answerCount.count ?? 0;
			}
			return array;
		}, [votes]);

		const totalVotes = useMemo(() => {
			let total = 0;
			for (const answerCount of votes ?? []) {
				total += answerCount.count ?? 0;
			}
			return total;
		}, [votes]);

		if (!message || !selectedAnswerId) {
			return null;
		}

		return (
			<Modal.Root size="medium" centered data-flx="messaging.poll-answer-voters-modal.modal-root">
				<Modal.Header title={poll.question?.text ?? ''} data-flx="messaging.poll-answer-voters-modal.modal-header">
					<div className={styles.smallText} data-flx="messaging.poll-answer-voters-modal.modal-header.total-votes">
						{i18n._(VOTES_DESCRIPTOR, {count: totalVotes})}
					</div>
				</Modal.Header>
				<div className={styles.contentSplit} data-flx="messaging.poll-answer-voters-modal.content-split">
					<div className={styles.answerList} data-flx="messaging.poll-answer-voters-modal.answer-list">
						<Scroller
							className={styles.scrollerPadding}
							contentClassName={styles.scrollerContent}
							data-flx="messaging.poll-answer-voters-modal.answer-list.scroller"
						>
							{(poll.answers ?? []).map((answer) => {
								const answerId = Number(answer.answer_id ?? 0);
								return (
									<button
										type="button"
										key={answerId}
										className={styles.answer}
										data-selected={answerId === selectedAnswerId}
										data-answer-id={answerId}
										onClick={() => setSelectedAnswerId(answerId)}
										data-flx="messaging.poll-answer-voters-modal.answer.button"
									>
										<div className={styles.answerText} data-flx="messaging.poll-answer-voters-modal.answer.text">
											{answer.poll_media?.text ?? ''}
										</div>
										<div className={styles.smallText} data-flx="messaging.poll-answer-voters-modal.answer.votes">
											{i18n._(VOTES_DESCRIPTOR, {count: answerVoteArray[answerId] ?? 0})}
										</div>
									</button>
								);
							})}
							<div className={styles.scrollerVoid} />
						</Scroller>
					</div>
					<div className={styles.voterList}>
						<Scroller
							className={styles.scrollerPadding}
							key={voterScrollerKey}
							onScroll={handleScroll}
							contentClassName={styles.scrollerContent}
							data-flx="messaging.poll-answer-voters-modal.voter-list.scroller"
						>
							{voters.map((user) => (
								<PreloadableUserPopout
									user={user}
									key={user.id}
									isWebhook={false}
									guildId={guild?.id}
									channelId={channelId}
									enableLongPressActions={false}
									data-flx="channel.message-avatar.preloadable-user-popout"
								>
									<FocusRing data-flx="channel.message-avatar.focus-ring">
										<div className={styles.voter}>
											<Avatar user={user} guildId={guild?.id} size={32} />
											<div>
												<div>{NicknameUtils.getNickname(user, guildId, channelId)}</div>
												<div className={styles.smallText}>{user.tag}</div>
											</div>
										</div>
									</FocusRing>
								</PreloadableUserPopout>
							))}
						</Scroller>
					</div>
				</div>
			</Modal.Root>
		);
	},
);
