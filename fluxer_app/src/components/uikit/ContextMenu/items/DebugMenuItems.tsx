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
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {ChannelDebugModal} from '~/components/debug/ChannelDebugModal';
import {GuildDebugModal} from '~/components/debug/GuildDebugModal';
import {GuildMemberDebugModal} from '~/components/debug/GuildMemberDebugModal';
import {UserDebugModal} from '~/components/debug/UserDebugModal';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {UserRecord} from '~/records/UserRecord';
import {DebugIcon} from '../ContextMenuIcons';
import {MenuItem} from '../MenuItem';

interface BaseDebugMenuItemProps {
	onClose: () => void;
}

type DebugUserMenuItemProps = BaseDebugMenuItemProps & {
	user: UserRecord;
};

export const DebugUserMenuItem: React.FC<DebugUserMenuItemProps> = observer(({user, onClose}) => {
	const {t} = useLingui();
	const handleDebug = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <UserDebugModal title={t`User Debug`} user={user} />));
		onClose();
	}, [user, onClose]);

	return (
		<MenuItem icon={<DebugIcon />} onClick={handleDebug}>
			{t`Debug User`}
		</MenuItem>
	);
});

type DebugChannelMenuItemProps = BaseDebugMenuItemProps & {
	channel: ChannelRecord;
};

export const DebugChannelMenuItem: React.FC<DebugChannelMenuItemProps> = observer(({channel, onClose}) => {
	const {t} = useLingui();
	const handleDebug = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <ChannelDebugModal title={t`Channel Debug`} channel={channel} />));
		onClose();
	}, [channel, onClose]);

	return (
		<MenuItem icon={<DebugIcon />} onClick={handleDebug}>
			{t`Debug Channel`}
		</MenuItem>
	);
});

type DebugGuildMenuItemProps = BaseDebugMenuItemProps & {
	guild: GuildRecord;
};

export const DebugGuildMenuItem: React.FC<DebugGuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const handleDebug = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <GuildDebugModal title={t`Community Debug`} guild={guild} />));
		onClose();
	}, [guild, onClose]);

	return (
		<MenuItem icon={<DebugIcon />} onClick={handleDebug}>
			{t`Debug Guild`}
		</MenuItem>
	);
});

type DebugGuildMemberMenuItemProps = BaseDebugMenuItemProps & {
	member: GuildMemberRecord;
};

export const DebugGuildMemberMenuItem: React.FC<DebugGuildMemberMenuItemProps> = observer(({member, onClose}) => {
	const {t} = useLingui();
	const handleDebug = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <GuildMemberDebugModal title={t`Community Member Debug`} member={member} />));
		onClose();
	}, [member, onClose]);

	return (
		<MenuItem icon={<DebugIcon />} onClick={handleDebug}>
			{t`Debug Member`}
		</MenuItem>
	);
});
