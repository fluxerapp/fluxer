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

import {Trans, useLingui} from '@lingui/react/macro';
import {AtIcon, XCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import GuildStore from '~/stores/GuildStore';
import * as NicknameUtils from '~/utils/NicknameUtils';
import styles from './ReplyBar.module.css';
import wrapperStyles from './textarea/InputWrapper.module.css';

interface ReplyBarProps {
	replyingMessageObject: MessageRecord;
	shouldReplyMention: boolean;
	setShouldReplyMention: (mentioning: boolean) => void;
	channel: ChannelRecord;
}

export const ReplyBar = observer(function ReplyBar({
	replyingMessageObject,
	shouldReplyMention: initialShouldMention,
	setShouldReplyMention,
	channel,
}: ReplyBarProps) {
	const {t} = useLingui();
	const guild = GuildStore.getGuild(channel.guildId ?? '');
	const currentUserId = AuthenticationStore.currentUserId;
	const isOwnMessage = replyingMessageObject.author.id === currentUserId;
	const isInGuild = channel.guildId != null;
	const isWebhook = replyingMessageObject.webhookId != null;

	const canMention = !isOwnMessage && isInGuild && !isWebhook;
	const shouldMention = initialShouldMention && canMention;
	const authorNickname = NicknameUtils.getNickname(replyingMessageObject.author, guild?.id);

	const handleStopReply = () => {
		MessageActionCreators.stopReply(channel.id);
	};

	const toggleMention = () => {
		setShouldReplyMention(!shouldMention);
	};

	const handleKeyDown = (handler: () => void) => (event: React.KeyboardEvent) => {
		if (event.key === 'Enter') handler();
	};

	return (
		<div
			className={clsx(
				wrapperStyles.box,
				wrapperStyles.wrapperSides,
				wrapperStyles.roundedTop,
				wrapperStyles.noBottomBorder,
				styles.topBorder,
			)}
		>
			<div className={clsx(wrapperStyles.barInner)} style={{gridTemplateColumns: '1fr auto'}}>
				<div className={styles.text}>
					<Trans>
						Replying to <span className={styles.authorName}>{authorNickname}</span>
					</Trans>
				</div>

				<div className={styles.controls}>
					{canMention && (
						<Tooltip
							text={
								shouldMention
									? t`Click to disable pinging the user you're replying to.`
									: t`Click to enable pinging the user you're replying to.`
							}
						>
							<FocusRing offset={-2}>
								<div
									role="switch"
									aria-checked={shouldMention}
									tabIndex={0}
									onClick={toggleMention}
									onKeyDown={handleKeyDown(toggleMention)}
								>
									<div
										className={clsx(
											styles.mentionToggle,
											shouldMention ? styles.mentionToggleOn : styles.mentionToggleOff,
										)}
									>
										<AtIcon weight="bold" className={styles.mentionIcon} />
										{shouldMention ? t`On` : t`Off`}
									</div>
								</div>
							</FocusRing>
						</Tooltip>
					)}

					<FocusRing offset={-2}>
						<button type="button" className={styles.closeButton} onClick={handleStopReply}>
							<XCircleIcon className={styles.closeIcon} />
						</button>
					</FocusRing>
				</div>
			</div>
			<div className={wrapperStyles.separator} />
		</div>
	);
});
