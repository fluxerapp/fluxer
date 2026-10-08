// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import styles from '@app/features/threads/components/ThreadParentCrumb.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {CaretRightIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

export const ThreadParentCrumb = observer(({thread, iconClassName}: {thread: Channel; iconClassName?: string}) => {
	const parent = thread.parentId ? Channels.getChannel(thread.parentId) : undefined;
	if (!parent || !thread.guildId) return null;
	const guildId = thread.guildId;
	return (
		<>
			<FocusRing offset={-2} data-flx="threads.thread-parent-crumb.focus-ring">
				<button
					type="button"
					className={styles.crumb}
					onClick={() => NavigationCommands.selectChannel(guildId, parent.id)}
					data-flx="threads.thread-parent-crumb.crumb.select-parent"
				>
					{ChannelUtils.getIcon(parent, {className: iconClassName})}
					<span className={styles.crumbName} data-flx="threads.thread-parent-crumb.crumb-name">
						{parent.name}
					</span>
				</button>
			</FocusRing>
			<CaretRightIcon className={styles.separator} weight="bold" data-flx="threads.thread-parent-crumb.separator" />
		</>
	);
});
