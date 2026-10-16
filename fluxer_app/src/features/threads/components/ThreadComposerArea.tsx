// SPDX-License-Identifier: AGPL-3.0-or-later

import {BarrierBase} from '@app/features/channel/components/barriers/BarrierComponents';
import {ChannelTextarea} from '@app/features/channel/components/ChannelTextarea';
import type {Channel} from '@app/features/channel/models/Channel';
import {getPostForum} from '@app/features/forum/utils/ForumChannelUtils';
import * as PostD from '@app/features/forum/utils/ForumMessageDescriptors';
import * as ThreadCommands from '@app/features/threads/commands/ThreadCommands';
import styles from '@app/features/threads/components/ThreadComposerArea.module.css';
import {reportThreadActionError} from '@app/features/threads/hooks/useThreadMenuData';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import {canJoinThreadChannel, canWriteInThread} from '@app/features/threads/utils/ThreadActionRules';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {useLingui} from '@lingui/react/macro';
import {ArchiveIcon, ChatsIcon, LockSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

export const ThreadComposerArea = observer(({thread}: {thread: Channel}) => {
	const {i18n} = useLingui();
	const [joining, setJoining] = useState(false);
	const joined = ThreadMemberships.isMember(thread.id);
	const post = getPostForum(thread) !== undefined;
	const handleJoin = useCallback(() => {
		setJoining(true);
		void ThreadCommands.joinThread(thread)
			.catch((error) => reportThreadActionError(i18n, error))
			.finally(() => setJoining(false));
	}, [i18n, thread]);
	if (thread.isLocked && !canWriteInThread(thread, 'send')) {
		return (
			<BarrierBase
				icon={<LockSimpleIcon className={styles.lockedIcon} data-flx="threads.thread-composer-area.locked-icon" />}
				message={i18n._(D.THREAD_LOCKED_NOTICE_DESCRIPTOR)}
				data-flx="threads.thread-composer-area.barrier-base.locked"
			/>
		);
	}
	return (
		<>
			{thread.isArchived && (
				<div className={styles.notice} role="status" data-flx="threads.thread-composer-area.notice.archived">
					<ArchiveIcon className={styles.noticeIcon} data-flx="threads.thread-composer-area.archived-icon" />
					<span className={styles.noticeText} data-flx="threads.thread-composer-area.notice-text.archived">
						{i18n._(D.THREAD_ARCHIVED_NOTICE_DESCRIPTOR)}
					</span>
				</div>
			)}
			{!post && !joined && !thread.isArchived && canJoinThreadChannel(thread) && (
				<div className={styles.notice} data-flx="threads.thread-composer-area.notice.join">
					<ChatsIcon className={styles.noticeIcon} data-flx="threads.thread-composer-area.join-icon" />
					<span className={styles.noticeText} data-flx="threads.thread-composer-area.notice-text.join">
						{i18n._(D.THREAD_JOIN_NOTICE_DESCRIPTOR)}
					</span>
					<Button
						small
						fitContent
						submitting={joining}
						onClick={handleJoin}
						data-flx="threads.thread-composer-area.button.join"
					>
						{i18n._(D.JOIN_THREAD_DESCRIPTOR)}
					</Button>
				</div>
			)}
			<ChannelTextarea
				channel={thread}
				placeholder={post ? i18n._(PostD.SEND_MESSAGE_IN_POST_DESCRIPTOR, {name: thread.name ?? ''}) : undefined}
				data-flx="threads.thread-composer-area.channel-textarea"
			/>
		</>
	);
});
