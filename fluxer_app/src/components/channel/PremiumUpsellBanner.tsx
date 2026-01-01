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
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import {PlutoniumUpsell} from '~/components/uikit/PlutoniumUpsell/PlutoniumUpsell';
import DismissedUpsellStore from '~/stores/DismissedUpsellStore';
import styles from './PremiumUpsellBanner.module.css';

export const PremiumUpsellBanner = observer(({message}: {message?: string}) => {
	if (DismissedUpsellStore.pickerPremiumUpsellDismissed) {
		return null;
	}

	const handleClick = () => {
		PremiumModalActionCreators.open();
	};

	const handleDismiss = () => {
		DismissedUpsellStore.dismissPickerPremiumUpsell();
	};

	return (
		<PlutoniumUpsell className={styles.banner} onButtonClick={handleClick} dismissible={true} onDismiss={handleDismiss}>
			{message ?? <Trans>Unlock all custom emojis and stickers across all communities with Plutonium</Trans>}
		</PlutoniumUpsell>
	);
});
