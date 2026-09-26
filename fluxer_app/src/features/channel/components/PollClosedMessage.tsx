// SPDX-License-Identifier: AGPL-3.0-or-later

import pollStyles from '@app/features/channel/components/Poll.module.css';
import styles from '@app/features/channel/components/PollClosedMessage.module.css';
import {SystemMessage} from '@app/features/channel/components/SystemMessage';
import {SystemMessageUsername} from '@app/features/channel/components/SystemMessageUsername';
import {useSystemMessageData} from '@app/features/messaging/hooks/useSystemMessageData';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react';
import {Trans} from '@lingui/react/macro';
import {CheckCircleIcon, TableIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import {SystemMessageMessageLink} from './SystemMessageMessageLink';

interface PollClosedMessage {
	message: Message;
}

const NOBODY_VOTED_DESCRIPTOR = msg({
	message: "Nobody voted!",
	comment: 'Small explanatory text informing the user that nobody voted on the poll.',
});
const ITS_A_DRAW_DESCRIPTOR = msg({
	message: "It's a draw",
	comment: 'Small explanatory text informing the user that the poll resulted in a draw.',
});
const VIEW_POLL_DESCRIPTOR = msg({
	message: 'View poll',
	comment: 'Title of the button that redirects to the poll message.',
});

export const PollClosedMessage = observer(({message}: PollClosedMessage) => {
	const {i18n} = useLingui();
	const {author, channel, guild} = useSystemMessageData(message);
	if (!channel) {
		return null;
	}

	const fields = message.embeds[0]?.fields;
	const question = fields?.find((field) => field.name === 'poll_question_text')?.value ?? '';
	const victorAnswerVotes = Number(fields?.find((field) => field.name === 'victor_answer_votes')?.value ?? '0');
	const totalVotes = Number(fields?.find((field) => field.name === 'total_votes')?.value ?? '0');
	const percentage = totalVotes && totalVotes > 0 ? Math.floor((victorAnswerVotes * 100) / totalVotes) : 0;

	let isDraw = false;
	let winningAnswer: string | undefined;
	const poll = message.referencedMessage?.poll;
	if (poll) {
		for (const answerCount of poll.results?.answer_counts ?? []) {
			if (answerCount.count === victorAnswerVotes) {
				if (winningAnswer) isDraw = true;
				winningAnswer = poll.answers?.find((answer) => answer.answer_id === answerCount.id)?.poll_media?.text;
			}
		}
	}

	return (
		<SystemMessage
			icon={TableIcon}
			iconWeight="bold"
			message={message}
			messageContent={
				<Trans>
					<SystemMessageUsername
						key={author.id}
						author={author}
						guild={guild}
						message={message}
						data-flx="channel.poll-closed-message.system-message-username"
					/>
					's poll{' '}
					<SystemMessageMessageLink
						key={message.id}
						linkText={question}
						guildId={guild?.id}
						linkedMessage={message.referencedMessage}
						data-flx="channel.poll-closed-message.system-message-link-to-poll"
					/>{' '}
					has closed.
				</Trans>
			}
			additionalContent={
				<div
					data-flx="channel.poll-closed-message.embed"
					className={clsx(pollStyles.pollContainer, styles.pollResults)}
				>
					<section>
						<div className={styles.resultDescription} data-flx="channel.poll-closed-message.embed.result-description">
							{totalVotes === 0 ? i18n._(NOBODY_VOTED_DESCRIPTOR) : isDraw ? i18n._(ITS_A_DRAW_DESCRIPTOR) : winningAnswer}
							{isDraw || totalVotes === 0 ? undefined : (
								<CheckCircleIcon
									weight="fill"
									className={pollStyles.answerMeSuccess}
									data-variant="winner"
									data-flx="poll.answer.me-check"
								/>
							)}
						</div>
						{totalVotes > 0 && (
							<div className={styles.victorPercentage} data-flx="channel.poll-closed-message.embed.victor-percentage">
								{percentage}%
							</div>
						)}
					</section>
					<Button
						variant="secondary"
						onClick={() => {
							const linkedMessage = message.referencedMessage;
							if (linkedMessage)
								NavigationCommands.navigateToMessage(guild?.id, linkedMessage.channelId, linkedMessage.id, 'push');
						}}
						data-flx="channel.poll-closed-message.embed.button.view-poll"
					>
						{i18n._(VIEW_POLL_DESCRIPTOR)}
					</Button>
				</div>
			}
			data-flx="channel.recipient-add-message.system-message"
		/>
	);
});
