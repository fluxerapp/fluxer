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
import {UserIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import type {UserRecord} from '~/records/UserRecord';
import {MenuItem} from '../MenuItem';

interface UserProfileMenuItemProps {
	user: UserRecord;
	guildId?: string;
	onClose: () => void;
}

export const UserProfileMenuItem: React.FC<UserProfileMenuItemProps> = observer(({user, guildId, onClose}) => {
	const {t} = useLingui();
	const handleViewProfile = React.useCallback(() => {
		onClose();
		UserProfileActionCreators.openUserProfile(user.id, guildId);
	}, [onClose, user.id, guildId]);

	return (
		<MenuItem icon={<UserIcon size={16} />} onClick={handleViewProfile}>
			{t`View Profile`}
		</MenuItem>
	);
});
