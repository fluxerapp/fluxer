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
import {ChatCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import type {UserRecord} from '~/records/UserRecord';
import {MenuItem} from '../MenuItem';

interface MessageUserMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const MessageUserMenuItem: React.FC<MessageUserMenuItemProps> = observer(({user, onClose}) => {
	const {t} = useLingui();
	const handleMessageUser = React.useCallback(async () => {
		onClose();

		try {
			await PrivateChannelActionCreators.openDMChannel(user.id);
		} catch (error) {
			console.error('Failed to open DM channel:', error);
		}
	}, [user.id, onClose]);

	return (
		<MenuItem icon={<ChatCircleIcon size={16} />} onClick={handleMessageUser}>
			{t`Message`}
		</MenuItem>
	);
});
