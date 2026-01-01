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
import {GiftIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Button} from '~/components/uikit/Button/Button';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import type {UserRecord} from '~/records/UserRecord';
import styles from './GiftInventoryBanner.module.css';

interface GiftInventoryBannerProps {
	currentUser: UserRecord;
}

export const GiftInventoryBanner: React.FC<GiftInventoryBannerProps> = observer(({currentUser}) => {
	if (!currentUser.hasUnreadGiftInventory) return null;

	return (
		<div className={styles.banner}>
			<div className={styles.content}>
				<GiftIcon className={styles.icon} weight="fill" />
				<div className={styles.textContainer}>
					<p className={styles.title}>
						<Plural
							value={currentUser.unreadGiftInventoryCount ?? 1}
							one="You have a new gift code waiting for you!"
							other="You have # new gift codes waiting for you!"
						/>
					</p>
				</div>
				<Button
					variant="inverted"
					small
					onClick={() => ComponentDispatch.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'gift_inventory'})}
				>
					<Trans>View Gift Inventory</Trans>
				</Button>
			</div>
		</div>
	);
});
