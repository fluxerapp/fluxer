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

import {Trans} from '@lingui/react/macro';
import {EyeIcon, ProhibitIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {GuildBan} from '~/actions/GuildActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {BanDetailsModal} from '~/components/modals/BanDetailsModal';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';

interface BannedUserContextMenuProps {
	ban: GuildBan;
	onClose: () => void;
	onRevoke: () => void;
}

export const BannedUserContextMenu: React.FC<BannedUserContextMenuProps> = observer(({ban, onClose, onRevoke}) => {
	const handleViewDetails = () => {
		onClose();
		ModalActionCreators.push(modal(() => <BanDetailsModal ban={ban} onRevoke={onRevoke} />));
	};

	const handleRevokeBan = () => {
		onClose();
		onRevoke();
	};

	return (
		<>
			<MenuGroup>
				<MenuItem icon={<EyeIcon weight="bold" />} onClick={handleViewDetails}>
					<Trans>View Details</Trans>
				</MenuItem>
			</MenuGroup>
			<MenuGroup>
				<MenuItem icon={<ProhibitIcon weight="bold" />} danger onClick={handleRevokeBan}>
					<Trans>Revoke Ban</Trans>
				</MenuItem>
			</MenuGroup>
		</>
	);
});
