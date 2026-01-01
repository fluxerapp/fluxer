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
import type React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {PlutoniumUpsell} from '~/components/uikit/PlutoniumUpsell/PlutoniumUpsell';

export const PlutoniumUpsellBanner: React.FC = observer(() => {
	return (
		<PlutoniumUpsell
			buttonText={<Trans>View Plans</Trans>}
			onButtonClick={() => {
				ModalActionCreators.pop();
				ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="plutonium" />));
			}}
		>
			<Trans>Get Plutonium for yourself and unlock higher limits and exclusive features.</Trans>
		</PlutoniumUpsell>
	);
});
