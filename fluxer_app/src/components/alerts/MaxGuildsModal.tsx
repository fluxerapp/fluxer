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

import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import type {UserRecord} from '@app/records/UserRecord';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

interface MaxGuildsModalProps {
	user: UserRecord;
}

export const MaxGuildsModal = observer(({user}: MaxGuildsModalProps) => {
	const {t} = useLingui();
	const maxGuilds = user.maxGuilds;

	return (
		<ConfirmModal
			title={t`Too Many Communities`}
			description={
				maxGuilds === 1
					? t`You've reached the maximum number of communities you can join (${maxGuilds} community). Please leave a community before joining another one.`
					: t`You've reached the maximum number of communities you can join (${maxGuilds} communities). Please leave a community before joining another one.`
			}
			primaryText={t`Understood`}
			onPrimary={() => {}}
		/>
	);
});
