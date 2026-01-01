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
import {Slider} from '~/components/uikit/Slider';
import styles from './PushSettings.module.css';

interface PushSettingsProps {
	afkTimeout: number;
	onAfkTimeoutChange: (value: number) => void;
}

export const PushSettings: React.FC<PushSettingsProps> = observer(({afkTimeout, onAfkTimeoutChange}) => {
	const {t} = useLingui();
	return (
		<div className={styles.container}>
			<h2 className={styles.title}>{t`Push Notification Inactive Timeout`}</h2>
			<p className={styles.description}>
				{t`Fluxer avoids sending push notifications to your mobile devices when you are at your computer. Use this setting to control how long you need to be inactive on desktop for before you receive push notifications, ranging from 1 minute to 10 minutes.`}
			</p>
			<Slider
				defaultValue={afkTimeout / 60}
				factoryDefaultValue={10}
				onValueChange={onAfkTimeoutChange}
				minValue={1}
				maxValue={10}
				markers={[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}
				stickToMarkers={true}
			/>
		</div>
	);
});
