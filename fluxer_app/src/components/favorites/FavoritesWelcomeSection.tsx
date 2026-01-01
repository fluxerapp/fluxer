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

import {Trans, useLingui} from '@lingui/react/macro';
import {StarIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as FavoritesActionCreators from '~/actions/FavoritesActionCreators';
import {Button} from '~/components/uikit/Button/Button';
import {Routes} from '~/Routes';
import * as RouterUtils from '~/utils/RouterUtils';
import styles from './FavoritesWelcomeSection.module.css';

export const FavoritesWelcomeSection: React.FC = observer(function FavoritesWelcomeSection() {
	const {i18n} = useLingui();

	const handleDisableFavorites = React.useCallback(() => {
		FavoritesActionCreators.confirmHideFavorites(() => {
			RouterUtils.transitionTo(Routes.ME);
		}, i18n);
	}, [i18n]);

	return (
		<div className={styles.welcomeSection}>
			<div className={styles.iconSection}>
				<div className={styles.iconWrapper}>
					<StarIcon className={styles.icon} weight="fill" />
				</div>
			</div>

			<div className={styles.contentSection}>
				<h1 className={styles.heading}>
					<Trans>Welcome to Favorites</Trans>
				</h1>

				<p className={styles.description}>
					<Trans>
						Your personal space for quick access to channels, DMs, and groups you love. Press the star on any channel to
						add it here.
					</Trans>
				</p>

				<p className={styles.tip}>
					<Trans>Not your cup of tea? You can disable this feature anytime.</Trans>
				</p>

				<div className={styles.actionSection}>
					<Button variant="secondary" onClick={handleDisableFavorites}>
						<Trans>Disable Favorites</Trans>
					</Button>
				</div>
			</div>
		</div>
	);
});
