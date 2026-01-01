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

import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import UserStore from '~/stores/UserStore';

export const MaxGuildsModal = observer(() => {
	const {t} = useLingui();
	const currentUser = UserStore.currentUser!;
	const maxGuilds = currentUser.maxGuilds;

	return (
		<ConfirmModal
			title={t`Too Many Communities`}
			description={
				<Trans>
					You've reached the maximum number of communities you can join (
					<Plural value={maxGuilds} one="# community" other="# communities" />
					). Please leave a community before joining another one.
				</Trans>
			}
			primaryText={t`Understood`}
			onPrimary={() => {}}
		/>
	);
});
