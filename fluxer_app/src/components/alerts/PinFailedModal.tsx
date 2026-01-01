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
import {GenericErrorModal} from './GenericErrorModal';

export type PinFailureReason = 'dm_restricted' | 'generic';

interface PinFailedModalProps {
	isUnpin?: boolean;
	reason?: PinFailureReason;
}

export const PinFailedModal: React.FC<PinFailedModalProps> = observer(({isUnpin, reason = 'generic'}) => {
	const {t} = useLingui();
	const title = isUnpin ? t`Failed to unpin message` : t`Failed to pin message`;

	let message: string;
	switch (reason) {
		case 'dm_restricted':
			message = t`You cannot interact with this user right now.`;
			break;
		default:
			message = t`Something went wrong. Please try again later.`;
	}

	return <GenericErrorModal title={title} message={message} />;
});
