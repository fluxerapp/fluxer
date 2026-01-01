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

import {Trans} from '@lingui/react/macro';
import {ClockIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type {MessagePreviewContext} from '~/Constants';
import {MessageAvatar} from '~/components/channel/MessageAvatar';
import {MessageUsername} from '~/components/channel/MessageUsername';
import {TimestampWithTooltip} from '~/components/channel/TimestampWithTooltip';
import {UserTag} from '~/components/channel/UserTag';
import {Tooltip} from '~/components/uikit/Tooltip';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import type {UserRecord} from '~/records/UserRecord';
import styles from '~/styles/Message.module.css';
import * as DateUtils from '~/utils/DateUtils';

export const MessageAuthorInfo = observer(
	({
		message,
		author,
		guild,
		member,
		shouldGroup,
		shouldAppearAuthorless,
		messageDisplayCompact,
		showUserAvatarsInCompactMode,
		mobileLayoutEnabled,
		isHovering,
		formattedDate,
		previewContext,
		previewOverrides,
	}: {
		message: MessageRecord;
		author: UserRecord;
		guild?: GuildRecord;
		member?: GuildMemberRecord;
		shouldGroup: boolean;
		shouldAppearAuthorless: boolean;
		messageDisplayCompact: boolean;
		showUserAvatarsInCompactMode: boolean;
		mobileLayoutEnabled: boolean;
		isHovering: boolean;
		formattedDate: string;
		previewContext?: keyof typeof MessagePreviewContext;
		previewOverrides?: {
			usernameColor?: string;
			displayName?: string;
		};
	}) => {
		if (shouldAppearAuthorless) return null;

		const isPreview = !!previewContext;
		const timeoutUntil = member?.communicationDisabledUntil ?? null;
		const isMemberTimedOut = Boolean(member?.isTimedOut());
		const timeoutIndicator =
			timeoutUntil && isMemberTimedOut ? (
				<Tooltip
					text={() => (
						<Trans>
							Timeout ends {DateUtils.getShortRelativeDateString(timeoutUntil)} (
							{DateUtils.getFormattedDateTime(timeoutUntil)})
						</Trans>
					)}
					position="top"
					maxWidth="none"
				>
					<span className={styles.messageTimeoutIndicator}>
						<ClockIcon size={16} weight="bold" />
					</span>
				</Tooltip>
			) : null;

		if (messageDisplayCompact && !shouldGroup) {
			return (
				<h3 className={styles.messageAuthorInfoCompact}>
					<TimestampWithTooltip date={message.timestamp} className={styles.messageTimestampCompact}>
						<span className={styles.messageAssistiveText} aria-hidden="true">
							{'['}
						</span>
						{DateUtils.getFormattedTime(message.timestamp)}
						<span className={styles.messageAssistiveText} aria-hidden="true">
							{'] '}
						</span>
					</TimestampWithTooltip>
					{author.bot && <UserTag className={styles.userTagCompact} system={author.system} />}
					{showUserAvatarsInCompactMode && (
						<MessageAvatar
							user={author}
							message={message}
							guildId={guild?.id}
							size={16}
							className={styles.messageAvatarCompact}
							isHovering={isHovering}
							isPreview={isPreview}
						/>
					)}
					<span className={styles.authorContainer}>
						{timeoutIndicator}
						<MessageUsername
							user={author}
							message={message}
							guild={guild}
							member={member}
							className={styles.messageUsername}
							isPreview={isPreview}
							previewColor={previewOverrides?.usernameColor}
							previewName={previewOverrides?.displayName}
						/>
						<span className={styles.messageAssistiveText} aria-hidden="true">
							{':'}
						</span>
					</span>
				</h3>
			);
		}

		if (messageDisplayCompact && shouldGroup) {
			if (mobileLayoutEnabled) return null;

			return (
				<h3 className={styles.messageAuthorInfoCompact}>
					<TimestampWithTooltip date={message.timestamp} className={styles.messageTimestampCompactHover}>
						<span className={styles.messageAssistiveText} aria-hidden="true">
							{'['}
						</span>
						{DateUtils.getFormattedTime(message.timestamp)}
						<span className={styles.messageAssistiveText} aria-hidden="true">
							{'] '}
						</span>
					</TimestampWithTooltip>
					{author.bot && <UserTag className={styles.userTagCompact} system={author.system} />}
					{showUserAvatarsInCompactMode && (
						<MessageAvatar
							user={author}
							message={message}
							guildId={guild?.id}
							size={16}
							className={styles.messageAvatarCompact}
							isHovering={isHovering}
							isPreview={isPreview}
						/>
					)}
					<span className={styles.authorContainer}>
						{timeoutIndicator}
						<MessageUsername
							user={author}
							message={message}
							guild={guild}
							member={member}
							className={styles.messageUsername}
							isPreview={isPreview}
							previewColor={previewOverrides?.usernameColor}
							previewName={previewOverrides?.displayName}
						/>
						<span className={styles.messageAssistiveText} aria-hidden="true">
							{':'}
						</span>
					</span>
				</h3>
			);
		}

		if (!shouldGroup) {
			return (
				<>
					<div className={styles.messageGutterLeft} />
					<MessageAvatar
						user={author}
						message={message}
						guildId={guild?.id}
						size={40}
						className={styles.messageAvatar}
						isHovering={isHovering}
						isPreview={isPreview}
					/>
					<div className={styles.messageGutterRight} />
					<h3 className={styles.messageAuthorInfo}>
						<span className={styles.authorContainer}>
							{timeoutIndicator}
							<MessageUsername
								user={author}
								message={message}
								guild={guild}
								member={member}
								className={styles.messageUsername}
								isPreview={isPreview}
								previewColor={previewOverrides?.usernameColor}
								previewName={previewOverrides?.displayName}
							/>
							{author.bot && <UserTag className={styles.userTagOffset} system={author.system} />}
						</span>

						<TimestampWithTooltip date={message.timestamp} className={styles.messageTimestamp}>
							<span className={styles.messageAssistiveText} aria-hidden="true">
								{' — '}
							</span>
							{formattedDate}
						</TimestampWithTooltip>
					</h3>
				</>
			);
		}

		if (mobileLayoutEnabled) return null;

		return (
			<>
				<div className={styles.messageGutterLeft} />
				<TimestampWithTooltip date={message.timestamp} className={styles.messageTimestampHover}>
					<span className={styles.messageAssistiveText} aria-hidden="true">
						{'['}
					</span>
					{DateUtils.getFormattedTime(message.timestamp)}
					<span className={styles.messageAssistiveText} aria-hidden="true">
						{']'}
					</span>
				</TimestampWithTooltip>
				<div className={styles.messageGutterRight} />
			</>
		);
	},
);
