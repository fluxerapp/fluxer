// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ThreadCommands from '@app/features/threads/commands/ThreadCommands';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadRoster from '@app/features/threads/state/ThreadRoster';
import {canRemoveThreadMembers} from '@app/features/threads/utils/ThreadActionRules';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import {KickMemberIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {User} from '@app/features/user/models/User';
import * as FormUtils from '@app/lib/forms';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export function getRemovableThread(guildId: string, channelId: string | undefined, userId: string): Channel | null {
	if (!channelId || !ThreadGuilds.isActive(guildId) || userId === Authentication.currentUserId) return null;
	const thread = Channels.getChannel(channelId);
	if (!thread?.isThread()) return null;
	if (!ThreadRoster.getMembers(thread.id)?.some((member) => member.userId === userId)) return null;
	return canRemoveThreadMembers(thread) ? thread : null;
}

interface RemoveFromThreadMenuItemProps {
	thread: Channel;
	user: User;
	onClose: () => void;
}

export const RemoveFromThreadMenuItem: React.FC<RemoveFromThreadMenuItemProps> = observer(({thread, user, onClose}) => {
	const {i18n} = useLingui();
	const handleRemove = () => {
		onClose();
		void ThreadCommands.removeThreadMember(thread, user.id).catch((error) =>
			ToastCommands.createToast({
				type: 'error',
				children: i18n._(D.THREAD_ACTION_FAILED_DESCRIPTOR, {detail: FormUtils.extractErrorMessage(i18n, error)}),
			}),
		);
	};
	return (
		<MenuItem
			icon={<KickMemberIcon size={16} data-flx="threads.remove-from-thread-menu-item.icon" />}
			onClick={handleRemove}
			danger
			data-flx="threads.remove-from-thread-menu-item.menu-item"
		>
			{i18n._(D.REMOVE_FROM_THREAD_DESCRIPTOR)}
		</MenuItem>
	);
});
