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
import {ClaimAccountModal} from '~/components/modals/ClaimAccountModal';
import {Button} from '~/components/uikit/Button/Button';
import {WarningAlert} from '~/components/uikit/WarningAlert/WarningAlert';

export const UnclaimedAccountAlert = observer(() => {
	return (
		<WarningAlert
			title={<Trans>Unclaimed Account</Trans>}
			actions={
				<Button small={true} onClick={() => ModalActionCreators.push(modal(() => <ClaimAccountModal />))}>
					<Trans>Claim Account</Trans>
				</Button>
			}
		>
			<Trans>
				Your account is not yet claimed. Without an email and password, you won't be able to sign in from other devices
				and you could lose access to your account. Claim your account now to secure it.
			</Trans>
		</WarningAlert>
	);
});
