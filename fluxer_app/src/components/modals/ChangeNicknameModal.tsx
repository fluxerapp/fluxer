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

import {observer} from 'mobx-react-lite';
import React from 'react';
import * as GuildMemberActionCreators from '~/actions/GuildMemberActionCreators';
import {BaseChangeNicknameModal} from '~/components/modals/BaseChangeNicknameModal';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';

interface ChangeNicknameModalProps {
	guildId: string;
	user: UserRecord;
	member: GuildMemberRecord;
}

export const ChangeNicknameModal: React.FC<ChangeNicknameModalProps> = observer(({guildId, user, member}) => {
	const currentUserId = AuthenticationStore.currentUserId;
	const isCurrentUser = user.id === currentUserId;

	const handleSave = React.useCallback(
		async (nick: string | null) => {
			if (isCurrentUser) {
				await GuildMemberActionCreators.updateProfile(guildId, {nick});
			} else {
				await GuildMemberActionCreators.update(guildId, user.id, {nick});
			}
		},
		[guildId, user.id, isCurrentUser],
	);

	return <BaseChangeNicknameModal currentNick={member.nick || ''} displayName={user.displayName} onSave={handleSave} />;
});
