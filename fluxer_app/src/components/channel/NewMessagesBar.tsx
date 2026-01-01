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
import {CheckIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import * as DateUtils from '~/utils/DateUtils';
import styles from './Messages.module.css';

export const NewMessagesBar = observer(function NewMessagesBar({
	unreadCount,
	oldestUnreadTimestamp,
	isEstimated,
	onJumpToNewMessages,
}: {
	unreadCount: number;
	oldestUnreadTimestamp: number;
	isEstimated: boolean;
	onJumpToNewMessages: () => void;
}) {
	const {t} = useLingui();

	const isMobile = MobileLayoutStore.isMobileLayout();
	const sameDay = DateUtils.isSameDay(oldestUnreadTimestamp);
	const compactTime = DateUtils.getFormattedCompactDateTime(oldestUnreadTimestamp);
	const shortTime = sameDay ? DateUtils.getFormattedTime(oldestUnreadTimestamp) : compactTime;

	return (
		<button type="button" className={styles.newMessagesBar} onClick={onJumpToNewMessages}>
			<span className={styles.newMessagesBarText}>
				{isEstimated
					? isMobile
						? t`${unreadCount}+ new since ${shortTime}`
						: t`${unreadCount}+ new messages since ${compactTime}`
					: isMobile
						? t`${unreadCount} new since ${shortTime}`
						: unreadCount === 1
							? t`${unreadCount} new message since ${compactTime}`
							: t`${unreadCount} new messages since ${compactTime}`}
			</span>

			<span className={styles.newMessagesBarAction}>
				<span>{isMobile ? t`Mark Read` : t`Mark as Read`}</span>
				<CheckIcon weight="bold" size={16} />
			</span>
		</button>
	);
});
