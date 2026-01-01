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
import {FlagIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import type {IARContext} from '~/components/modals/IARModal';
import {IARModal} from '~/components/modals/IARModal';
import type {GuildRecord} from '~/records/GuildRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import {MenuItem} from '../MenuItem';

interface ReportGuildMenuItemProps {
	guild: GuildRecord;
	onClose: () => void;
}

export const ReportGuildMenuItem: React.FC<ReportGuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const isOwner = guild.ownerId === AuthenticationStore.currentUserId;

	const handleReportGuild = React.useCallback(() => {
		onClose();
		const context: IARContext = {
			type: 'guild',
			guild,
		};
		ModalActionCreators.push(modal(() => <IARModal context={context} />));
	}, [guild, onClose]);

	if (isOwner) {
		return null;
	}

	return (
		<MenuItem icon={<FlagIcon size={16} />} onClick={handleReportGuild} danger>
			{t`Report Community`}
		</MenuItem>
	);
});
