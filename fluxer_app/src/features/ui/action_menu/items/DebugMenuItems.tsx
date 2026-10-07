// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {ChannelDebugModal} from '@app/features/devtools/components/debug/ChannelDebugModal';
import {GuildMemberDebugModal} from '@app/features/devtools/components/debug/GuildMemberDebugModal';
import {UserDebugModal} from '@app/features/devtools/components/debug/UserDebugModal';
import {
	CHANNEL_DEBUG_DESCRIPTOR,
	DEBUG_CHANNEL_DESCRIPTOR,
	DEBUG_USER_DESCRIPTOR,
	USER_DEBUG_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import {DebugIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {User} from '@app/features/user/models/User';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const COMMUNITY_MEMBER_DEBUG_DESCRIPTOR = msg({
	message: 'Community member debug',
	comment: 'Title of the developer-mode community member debug modal.',
});
const DEBUG_MEMBER_DESCRIPTOR = msg({
	message: 'Debug member',
	comment: 'Developer-mode action that opens the community member debug modal.',
});

interface BaseDebugMenuItemProps {
	onClose: () => void;
}

type DebugUserMenuItemProps = BaseDebugMenuItemProps & {
	user: User;
};

export const DebugUserMenuItem: React.FC<DebugUserMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const handleDebug = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<UserDebugModal
					title={i18n._(USER_DEBUG_DESCRIPTOR)}
					user={user}
					data-flx="ui.action-menu.items.debug-menu-items.handle-debug.user-debug-modal"
				/>
			)),
		);
		onClose();
	}, [user, onClose]);
	return (
		<MenuItem
			icon={<DebugIcon data-flx="ui.action-menu.items.debug-menu-items.debug-user-menu-item.debug-icon" />}
			onClick={handleDebug}
			data-flx="ui.action-menu.items.debug-menu-items.debug-user-menu-item.menu-item.debug"
		>
			{i18n._(DEBUG_USER_DESCRIPTOR)}
		</MenuItem>
	);
});

type DebugChannelMenuItemProps = BaseDebugMenuItemProps & {
	channel: Channel;
};

export const DebugChannelMenuItem: React.FC<DebugChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const handleDebug = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ChannelDebugModal
					title={i18n._(CHANNEL_DEBUG_DESCRIPTOR)}
					channel={channel}
					data-flx="ui.action-menu.items.debug-menu-items.handle-debug.channel-debug-modal"
				/>
			)),
		);
		onClose();
	}, [channel, onClose]);
	return (
		<MenuItem
			icon={<DebugIcon data-flx="ui.action-menu.items.debug-menu-items.debug-channel-menu-item.debug-icon" />}
			onClick={handleDebug}
			data-flx="ui.action-menu.items.debug-menu-items.debug-channel-menu-item.menu-item.debug"
		>
			{i18n._(DEBUG_CHANNEL_DESCRIPTOR)}
		</MenuItem>
	);
});
type DebugGuildMemberMenuItemProps = BaseDebugMenuItemProps & {
	member: GuildMember;
};

export const DebugGuildMemberMenuItem: React.FC<DebugGuildMemberMenuItemProps> = observer(({member, onClose}) => {
	const {i18n} = useLingui();
	const handleDebug = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<GuildMemberDebugModal
					title={i18n._(COMMUNITY_MEMBER_DEBUG_DESCRIPTOR)}
					member={member}
					data-flx="ui.action-menu.items.debug-menu-items.handle-debug.guild-member-debug-modal"
				/>
			)),
		);
		onClose();
	}, [member, onClose]);
	return (
		<MenuItem
			icon={<DebugIcon data-flx="ui.action-menu.items.debug-menu-items.debug-guild-member-menu-item.debug-icon" />}
			onClick={handleDebug}
			data-flx="ui.action-menu.items.debug-menu-items.debug-guild-member-menu-item.menu-item.debug"
		>
			{i18n._(DEBUG_MEMBER_DESCRIPTOR)}
		</MenuItem>
	);
});
