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
import {observer} from 'mobx-react-lite';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {Nagbar} from '~/components/layout/Nagbar';
import {NagbarButton} from '~/components/layout/NagbarButton';
import {NagbarContent} from '~/components/layout/NagbarContent';
import {ClaimAccountModal} from '~/components/modals/ClaimAccountModal';
import UserStore from '~/stores/UserStore';

export const UnclaimedAccountNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const user = UserStore.currentUser;
	if (!user) {
		return null;
	}

	const handleClaimAccount = () => {
		ModalActionCreators.push(modal(() => <ClaimAccountModal />));
	};

	return (
		<Nagbar isMobile={isMobile} backgroundColor="#ea580c" textColor="#ffffff">
			<NagbarContent
				isMobile={isMobile}
				message={<Trans>Hey {user.displayName}, claim your account to prevent losing access.</Trans>}
				actions={
					<NagbarButton isMobile={isMobile} onClick={handleClaimAccount}>
						<Trans>Claim Account</Trans>
					</NagbarButton>
				}
			/>
		</Nagbar>
	);
});
