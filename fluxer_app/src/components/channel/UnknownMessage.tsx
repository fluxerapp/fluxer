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
import {WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {MessageAvatar} from '~/components/channel/MessageAvatar';
import {MessageUsername} from '~/components/channel/MessageUsername';
import {TimestampWithTooltip} from '~/components/channel/TimestampWithTooltip';
import {UserTag} from '~/components/channel/UserTag';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import UserStore from '~/stores/UserStore';
import styles from '~/styles/Message.module.css';
import * as DateUtils from '~/utils/DateUtils';
import {useMessageViewContext} from './MessageViewContext';

export const UnknownMessage = observer(() => {
	const {i18n} = useLingui();
	const {message, channel, shouldGroup, isHovering, previewContext, previewOverrides} = useMessageViewContext();
	const userAuthor = UserStore.getUser(message.author.id);
	const author = message.webhookId != null ? message.author : (userAuthor ?? message.author);
	const formattedDate = DateUtils.getRelativeDateString(message.timestamp, i18n);
	const guild = GuildStore.getGuild(channel.guildId ?? '');
	const member = GuildMemberStore.getMember(guild?.id ?? '', author?.id ?? '');

	return (
		<>
			{!shouldGroup && (
				<>
					<div className={styles.messageGutterLeft} />
					<MessageAvatar
						user={author}
						message={message}
						guildId={guild?.id}
						size={40}
						className={styles.messageAvatar}
						isHovering={isHovering}
						isPreview={!!previewContext}
					/>
					<div className={styles.messageGutterRight} />
				</>
			)}

			<div className={styles.messageContent}>
				<h3 className={styles.messageAuthorInfo}>
					<span className={styles.authorContainer}>
						<MessageUsername
							user={author}
							message={message}
							guild={guild}
							member={member ?? undefined}
							className={styles.messageUsername}
							isPreview={!!previewContext}
							previewColor={previewOverrides?.usernameColor}
							previewName={previewOverrides?.displayName}
						/>
						{author.bot && <UserTag className={styles.userTagOffset} system={author.system} />}
					</span>
					<TimestampWithTooltip date={message.timestamp} className={styles.messageTimestamp}>
						{formattedDate}
					</TimestampWithTooltip>
				</h3>
				<div className={styles.messageText}>
					<div className={styles.unknownMessageWarning}>
						<WarningCircleIcon size={16} weight="fill" />
						<span>
							<Trans>Please update Fluxer to view this message.</Trans>
						</span>
					</div>
				</div>
			</div>
		</>
	);
});
