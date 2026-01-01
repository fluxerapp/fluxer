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

import {Plural, Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {Nagbar} from '~/components/layout/Nagbar';
import {NagbarButton} from '~/components/layout/NagbarButton';
import {NagbarContent} from '~/components/layout/NagbarContent';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import UserStore from '~/stores/UserStore';

export const GiftInventoryNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const currentUser = UserStore.currentUser;
	const unreadCount = currentUser?.unreadGiftInventoryCount ?? 1;

	const handleOpenGiftInventory = () => {
		ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="gift_inventory" />));
	};

	return (
		<Nagbar isMobile={isMobile} backgroundColor="var(--brand-primary)" textColor="var(--text-on-brand-primary)">
			<NagbarContent
				isMobile={isMobile}
				message={
					<Plural
						value={unreadCount}
						one="You have a new gift code waiting in your Gift Inventory."
						other="You have # new gift codes waiting in your Gift Inventory."
					/>
				}
				actions={
					<NagbarButton isMobile={isMobile} onClick={handleOpenGiftInventory}>
						<Trans>View Gift Inventory</Trans>
					</NagbarButton>
				}
			/>
		</Nagbar>
	);
});
