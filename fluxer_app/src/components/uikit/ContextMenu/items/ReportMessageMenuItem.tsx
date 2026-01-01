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
import type {MessageRecord} from '~/records/MessageRecord';
import {MenuItem} from '../MenuItem';

interface ReportMessageMenuItemProps {
	message: MessageRecord;
	onClose: () => void;
}

export const ReportMessageMenuItem: React.FC<ReportMessageMenuItemProps> = observer(({message, onClose}) => {
	const {t} = useLingui();
	const handleReportMessage = React.useCallback(() => {
		onClose();
		const context: IARContext = {
			type: 'message',
			message,
		};
		ModalActionCreators.push(modal(() => <IARModal context={context} />));
	}, [message, onClose]);

	if (message.isCurrentUserAuthor()) {
		return null;
	}

	return (
		<MenuItem icon={<FlagIcon size={16} />} onClick={handleReportMessage} danger>
			{t`Report Message`}
		</MenuItem>
	);
});
