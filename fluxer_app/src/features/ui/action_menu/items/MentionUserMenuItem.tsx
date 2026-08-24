// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {MentionUserIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import type {User} from '@app/features/user/models/User';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const MENTION_DESCRIPTOR = msg({
	message: 'Mention',
	comment: 'Action that inserts a mention of the selected user into the message composer.',
});

interface MentionUserMenuItemProps {
	user: User;
	onClose: () => void;
}

export const MentionUserMenuItem: React.FC<MentionUserMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const handleMentionUser = useCallback(() => {
		onClose();
		ComponentBus.dispatch('INSERT_MENTION', {userId: user.id});
	}, [user.id, onClose]);
	return (
		<MenuItem
			icon={<MentionUserIcon size={16} data-flx="ui.action-menu.items.mention-user-menu-item.mention-user-icon" />}
			onClick={handleMentionUser}
			data-flx="ui.action-menu.items.mention-user-menu-item.menu-item.mention-user"
		>
			{i18n._(MENTION_DESCRIPTOR)}
		</MenuItem>
	);
});
