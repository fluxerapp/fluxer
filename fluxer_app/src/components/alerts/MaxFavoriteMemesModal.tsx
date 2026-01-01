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
import {modal, push} from '~/actions/ModalActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {PremiumModal} from '~/components/modals/PremiumModal';
import UserStore from '~/stores/UserStore';

export const MaxFavoriteMemesModal = observer(function MaxFavoriteMemesModal() {
	const {t} = useLingui();
	const currentUser = UserStore.currentUser;
	const isPremium = currentUser?.isPremium() ?? false;

	if (isPremium) {
		return (
			<ConfirmModal
				title={t`Saved Media Limit Reached`}
				description={t`You've reached the maximum limit of 500 saved media items for Plutonium users. To add more, you'll need to remove some existing items from your collection.`}
				secondaryText={t`Close`}
			/>
		);
	}

	return (
		<ConfirmModal
			title={t`Saved Media Limit Reached`}
			description={t`You've reached the maximum limit of 50 saved media items for free users. Upgrade to Plutonium to increase your limit to 500 saved media items!`}
			primaryText={t`Upgrade to Plutonium`}
			primaryVariant="primary"
			onPrimary={() => push(modal(() => <PremiumModal />))}
			secondaryText={t`Maybe Later`}
		/>
	);
});
