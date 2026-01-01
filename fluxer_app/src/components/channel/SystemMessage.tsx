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
import type {Icon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {MessageReactions} from '~/components/channel/MessageReactions';
import {TimestampWithTooltip} from '~/components/channel/TimestampWithTooltip';
import type {MessageRecord} from '~/records/MessageRecord';
import UserSettingsStore from '~/stores/UserSettingsStore';
import styles from '~/styles/Message.module.css';
import * as DateUtils from '~/utils/DateUtils';

export const SystemMessage = observer(
	({
		icon: Icon,
		iconWeight,
		iconClassname,
		message,
		messageContent,
	}: {
		icon: Icon;
		iconWeight: 'bold' | 'fill';
		iconClassname?: string;
		message: MessageRecord;
		messageContent: React.ReactNode;
	}) => {
		const {i18n} = useLingui();
		const messageDisplayCompact = UserSettingsStore.getMessageDisplayCompact();
		const formattedDate = messageDisplayCompact
			? DateUtils.getFormattedTime(message.timestamp)
			: DateUtils.getRelativeDateString(message.timestamp, i18n);

		if (messageDisplayCompact) {
			return (
				<div className={styles.systemMessageCompactContent}>
					<TimestampWithTooltip date={message.timestamp} className={styles.messageTimestampCompact}>
						{formattedDate}
					</TimestampWithTooltip>
					<div className={styles.systemMessageIconCompact}>
						<Icon weight={iconWeight} className={clsx(styles.systemMessageIconSvg, iconClassname)} />
					</div>
					<div className={styles.systemMessageContentWrapper}>
						<div className={styles.systemMessageContent}>{messageContent}</div>
						{UserSettingsStore.getRenderReactions() && message.reactions.length > 0 && (
							<div className={styles.container}>
								<MessageReactions message={message} />
							</div>
						)}
					</div>
				</div>
			);
		}

		return (
			<>
				<div className={styles.messageGutterLeft} />
				<div className={styles.systemMessageIconWrapper}>
					<Icon weight={iconWeight} className={clsx(styles.systemMessageIconSvg, iconClassname)} />
				</div>
				<div className={styles.messageGutterRight} />
				<div className={styles.systemMessageContent}>
					{messageContent}{' '}
					<TimestampWithTooltip
						date={message.timestamp}
						className={clsx(styles.messageTimestamp, styles.systemMessageTimestamp)}
					>
						{formattedDate}
					</TimestampWithTooltip>
				</div>
				{UserSettingsStore.getRenderReactions() && message.reactions.length > 0 && (
					<div className={styles.container}>
						<MessageReactions message={message} />
					</div>
				)}
			</>
		);
	},
);
