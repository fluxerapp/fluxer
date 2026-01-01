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
import type React from 'react';
import * as DeveloperOptionsActionCreators from '~/actions/DeveloperOptionsActionCreators';
import {Switch} from '~/components/form/Switch';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import styles from './GeneralTab.module.css';
import {getToggleGroups} from './shared';

export const GeneralTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const toggleGroups = getToggleGroups();

	return (
		<>
			{toggleGroups.map((group, gi) => (
				<div
					key={group.title.id ?? `toggle-group-${gi}`}
					className={gi > 0 ? styles.toggleGroup : styles.toggleGroupFirst}
				>
					<div className={styles.groupTitle}>{t(group.title)}</div>
					<div className={styles.toggleList}>
						{group.items.map(({key, label, description}) => (
							<Switch
								key={String(key)}
								label={t(label)}
								description={description ? t(description) : undefined}
								value={Boolean(DeveloperOptionsStore[key])}
								onChange={(value) => {
									DeveloperOptionsActionCreators.updateOption(key, value);
									if (key === 'selfHostedModeOverride') {
										window.location.reload();
									}
								}}
							/>
						))}
					</div>
				</div>
			))}
		</>
	);
});
