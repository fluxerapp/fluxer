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
import {FlagCheckeredIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ScheduledMessageActionCreators from '~/actions/ScheduledMessageActionCreators';
import previewStyles from '~/components/shared/MessagePreview.module.css';
import {Scroller} from '~/components/uikit/Scroller';
import ScheduledMessagesStore from '~/stores/ScheduledMessagesStore';
import styles from './ScheduledMessagesContent.module.css';

export const ScheduledMessagesContent = observer(() => {
	const {t, i18n} = useLingui();
	const {scheduledMessages, fetched, fetching} = ScheduledMessagesStore;
	const [cancellingId, setCancellingId] = React.useState<string | null>(null);

	React.useEffect(() => {
		if (!fetched && !fetching) {
			ScheduledMessageActionCreators.fetchScheduledMessages();
		}
	}, [fetched, fetching]);

	const handleCancel = React.useCallback(
		async (messageId: string) => {
			setCancellingId(messageId);
			try {
				await ScheduledMessageActionCreators.cancelScheduledMessage(i18n, messageId);
			} finally {
				setCancellingId(null);
			}
		},
		[i18n],
	);

	if (scheduledMessages.length === 0) {
		return (
			<div className={previewStyles.emptyState}>
				<div className={previewStyles.emptyStateContent}>
					<FlagCheckeredIcon className={previewStyles.emptyStateIcon} />
					<div className={previewStyles.emptyStateTextContainer}>
						<h3 className={previewStyles.emptyStateTitle}>
							{fetching ? t`Loading scheduled messages` : t`No Scheduled Messages`}
						</h3>
						<p className={previewStyles.emptyStateDescription}>
							{fetching
								? t`Hang on while we check for scheduled messages.`
								: t`Right-click the send button to schedule a message.`}
						</p>
					</div>
				</div>
			</div>
		);
	}

	const formatScheduledAt = (message: (typeof scheduledMessages)[number]) => {
		try {
			const formatter = new Intl.DateTimeFormat(undefined, {
				dateStyle: 'medium',
				timeStyle: 'short',
				timeZone: message.timezone,
			});
			return formatter.format(message.scheduledAt);
		} catch {
			return `${message.scheduledLocalAt} (${message.timezone})`;
		}
	};

	return (
		<Scroller className={previewStyles.scroller} key="scheduled-messages-scroller" reserveScrollbarTrack>
			{scheduledMessages.map((message) => (
				<div key={message.id} className={previewStyles.previewCard}>
					<div className={styles.cardHeader}>
						<span className={`${styles.statusBadge} ${message.status === 'invalid' ? styles.statusInvalid : ''}`}>
							{message.status === 'invalid' ? t`Invalid` : t`Scheduled`}
						</span>
						<span className={styles.timestamp}>{formatScheduledAt(message)}</span>
					</div>

					<p className={styles.messageText}>
						{message.payload.content ??
							(message.payload.attachments?.length ? t`Attachment only message` : t`(No content)`)}
					</p>

					{message.payload.attachments?.length ? (
						<div className={styles.attachmentsInfo}>
							{t`Attachments`}: {message.payload.attachments.length}
						</div>
					) : null}

					{message.status === 'invalid' && message.statusReason ? (
						<div className={styles.statusReason}>
							<WarningCircleIcon className={styles.warningIcon} weight="fill" />
							<span>{message.statusReason}</span>
						</div>
					) : null}

					<div className={previewStyles.actionButtons}>
						<button
							type="button"
							className={previewStyles.actionButton}
							onClick={() => handleCancel(message.id)}
							disabled={cancellingId === message.id}
						>
							{message.status === 'invalid' ? t`Remove` : t`Cancel`}
						</button>
					</div>
				</div>
			))}

			<div className={previewStyles.endState}>
				<div className={previewStyles.endStateContent}>
					<FlagCheckeredIcon className={previewStyles.endStateIcon} />
					<div className={previewStyles.endStateTextContainer}>
						<h3 className={previewStyles.endStateTitle}>{t`You're caught up`}</h3>
						<p className={previewStyles.endStateDescription}>{t`No more scheduled messages.`}</p>
					</div>
				</div>
			</div>
		</Scroller>
	);
});
