// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/ThreadItem.module.css';
import * as ThreadCommands from '@app/features/channel/commands/ThreadCommands';
import {openThreadContextMenu} from '@app/features/channel/components/ThreadContextMenu';
import type {Thread} from '@app/features/channel/state/Threads';
import Threads from '@app/features/channel/state/Threads';
import type {Guild} from '@app/features/guild/models/Guild';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const THREAD_DESCRIPTOR = msg({
	message: 'thread',
	comment: 'Lowercase channel type label for threads in accessible text.',
});

interface ThreadItemProps {
	guild: Guild;
	thread: Thread;
	isSelectedByPath: boolean;
}

export const ThreadItem = observer(({guild, thread, isSelectedByPath}: ThreadItemProps) => {
	const {i18n} = useLingui();
	const isJoined = Threads.isJoined(thread.id);

	const handleClick = useCallback(async () => {
		if (!isJoined) {
			await ThreadCommands.join(thread.threadParentChannelId, thread.id);
		}
		NavigationCommands.selectChannel(guild.id, thread.id);
	}, [guild.id, thread.id, thread.threadParentChannelId, isJoined]);

	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			openThreadContextMenu(event, {thread, guildId: guild.id, i18n});
		},
		[thread, guild.id, i18n],
	);

	const ariaLabel = `${thread.name ?? ''}, ${i18n._(THREAD_DESCRIPTOR)}`;

	return (
		<FocusRing data-flx="app.thread-item.focus-ring">
			<div
				role="button"
				tabIndex={0}
				aria-label={ariaLabel}
				aria-current={isSelectedByPath ? 'page' : undefined}
				className={clsx(
					styles.threadItem,
					isSelectedByPath && styles.threadItemSelected,
				)}
				onClick={handleClick}
				onContextMenu={handleContextMenu}
				onKeyDown={(e) => {
					if (e.key === 'Enter' || e.key === ' ') {
						e.preventDefault();
						void handleClick();
					}
				}}
				data-flx="app.thread-item.thread-item.click"
				data-channel-id={thread.id}
				data-channel-list-focus-item="true"
			>
				<div className={styles.connector} data-flx="app.thread-item.connector" />
				<span
					className={clsx(styles.name, isSelectedByPath && styles.nameSelected)}
					data-flx="app.thread-item.name"
				>
					{thread.name}
				</span>
			</div>
		</FocusRing>
	);
});
