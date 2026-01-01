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
import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import {BaseChangeNicknameModal} from '~/components/modals/BaseChangeNicknameModal';
import type {UserRecord} from '~/records/UserRecord';
import ChannelStore from '~/stores/ChannelStore';

interface ChangeGroupDMNicknameModalProps {
	channelId: string;
	user: UserRecord;
}

export const ChangeGroupDMNicknameModal: React.FC<ChangeGroupDMNicknameModalProps> = observer(({channelId, user}) => {
	const channel = ChannelStore.getChannel(channelId);
	const currentNick = channel?.nicks?.[user.id] || '';

	const handleSave = React.useCallback(
		async (nick: string | null) => {
			await ChannelActionCreators.updateGroupDMNickname(channelId, user.id, nick);
		},
		[channelId, user.id],
	);

	return <BaseChangeNicknameModal currentNick={currentNick} displayName={user.displayName} onSave={handleSave} />;
});
