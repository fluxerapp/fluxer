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

import {useLingui} from '@lingui/react/macro';
import {ArrowBendUpLeftIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {PreloadableUserPopout} from '~/components/channel/PreloadableUserPopout';
import {Avatar} from '~/components/uikit/Avatar';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {SafeMarkdown} from '~/lib/markdown';
import {MarkdownContext} from '~/lib/markdown/renderers';
import type {MessageRecord} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import MessageReferenceStore, {MessageReferenceState} from '~/stores/MessageReferenceStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import markupStyles from '~/styles/Markup.module.css';
import styles from '~/styles/Message.module.css';
import {goToMessage} from '~/utils/MessageNavigator';
import * as NicknameUtils from '~/utils/NicknameUtils';

export const ReplyPreview = observer(
	({message, channelId, animateEmoji}: {message: MessageRecord; channelId: string; animateEmoji: boolean}) => {
		const {t} = useLingui();

		const {message: referencedMessage, state: messageState} = MessageReferenceStore.getMessageReference(
			message.messageReference?.channel_id ?? '',
			message.messageReference?.message_id ?? '',
		);

		const guildId = ChannelStore.getChannel(channelId)!.guildId;
		const messageDisplayCompact = UserSettingsStore.getMessageDisplayCompact();

		const jumpToRepliedMessage = React.useCallback(() => {
			if (message.messageReference?.message_id) {
				goToMessage(message.channelId, message.messageReference.message_id);
			}
		}, [message.channelId, message.messageReference]);

		if (!message.messageReference) return null;

		if (messageState !== MessageReferenceState.LOADED || !referencedMessage) {
			return (
				<div className={clsx(styles.repliedMessage, messageDisplayCompact && styles.repliedMessageCompact)}>
					<div className={styles.repliedIconContainer}>
						<ArrowBendUpLeftIcon weight="bold" className={styles.repliedIcon} />
					</div>
					<button type="button" disabled className={clsx(styles.repliedTextPreview, styles.unstyled)} tabIndex={-1}>
						{messageState === MessageReferenceState.DELETED ? (
							<span className={styles.repliedItalic}>{t`Original message was deleted`}</span>
						) : (
							<span className={styles.repliedItalic}>{t`Original message failed to load`}</span>
						)}
					</button>
				</div>
			);
		}

		return (
			<div className={clsx(styles.repliedMessage, messageDisplayCompact && styles.repliedMessageCompact)}>
				{!messageDisplayCompact ? (
					<PreloadableUserPopout
						user={referencedMessage.author}
						isWebhook={referencedMessage.webhookId != null}
						guildId={guildId}
						channelId={channelId}
					>
						<Avatar
							user={referencedMessage.author}
							size={16}
							className={styles.repliedAvatar}
							guildId={guildId}
							data-user-id={referencedMessage.author.id}
							data-guild-id={guildId}
						/>
					</PreloadableUserPopout>
				) : (
					<div className={styles.repliedIconContainer}>
						<ArrowBendUpLeftIcon weight="bold" className={styles.repliedIcon} />
					</div>
				)}

				<PreloadableUserPopout
					user={referencedMessage.author}
					isWebhook={referencedMessage.webhookId != null}
					guildId={guildId}
					channelId={channelId}
				>
					<span
						className={styles.repliedUsername}
						style={{
							color: GuildMemberStore.getMember(guildId ?? '', referencedMessage.author.id)?.getColorString(),
						}}
						data-user-id={referencedMessage.author.id}
						data-guild-id={guildId}
					>
						{message.mentions.some((mention) => mention.id === referencedMessage.author.id) && '@'}
						{NicknameUtils.getNickname(referencedMessage.author, guildId)}
					</span>
				</PreloadableUserPopout>

				<FocusRing offset={-2}>
					<button
						type="button"
						className={clsx(styles.repliedTextPreview, styles.unstyled)}
						onClick={jumpToRepliedMessage}
						tabIndex={0}
						onKeyDown={(e) => {
							if (e.key === 'Enter' || e.key === ' ') {
								e.preventDefault();
								jumpToRepliedMessage();
							}
						}}
					>
						{referencedMessage.content ? (
							<span className={clsx(styles.repliedTextContent, markupStyles.markup)}>
								<SafeMarkdown
									content={referencedMessage.content}
									options={{
										context: MarkdownContext.RESTRICTED_INLINE_REPLY,
										messageId: referencedMessage.id,
										channelId,
										disableAnimatedEmoji: !animateEmoji,
									}}
								/>
							</span>
						) : (
							<span className={clsx(styles.repliedTextContent, styles.repliedItalic)}>
								{t`Message contains attached media`}
							</span>
						)}
					</button>
				</FocusRing>
			</div>
		);
	},
);
