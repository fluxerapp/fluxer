// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {getPostForum} from '@app/features/forum/utils/ForumChannelUtils';
import styles from '@app/features/threads/components/ThreadWelcomeSection.module.css';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export const ThreadWelcomeSection = observer(({thread}: {thread: Channel}) => {
	const {i18n} = useLingui();
	const forum = getPostForum(thread);
	const owner = !forum && thread.ownerId ? Users.getUser(thread.ownerId) : null;
	return (
		<div className={styles.container} data-flx="threads.thread-welcome-section.container">
			<div className={styles.icon} data-flx="threads.thread-welcome-section.icon">
				{ChannelUtils.getIcon(forum ?? thread, {className: styles.iconGlyph})}
			</div>
			<h1 className={styles.heading} data-flx="threads.thread-welcome-section.heading">
				{thread.name}
			</h1>
			{owner && (
				<p className={styles.description} data-flx="threads.thread-welcome-section.description">
					{i18n._(D.THREAD_STARTED_BY_DESCRIPTOR, {
						name: NicknameUtils.getNickname(owner, thread.guildId, thread.id),
					})}
				</p>
			)}
		</div>
	);
});
