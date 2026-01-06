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
import {PushPinIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {ChannelPinsContent} from '~/components/shared/ChannelPinsContent';
import type {ChannelRecord} from '~/records/ChannelRecord';
import styles from './ChannelPinsPopout.module.css';

export const ChannelPinsPopout = observer(({channel}: {channel: ChannelRecord}) => {
	const {t} = useLingui();
	return (
		<div className={styles.container}>
			<div className={styles.header}>
				<PushPinIcon className={styles.iconLarge} />
				<h1 className={styles.title}>{t`Pinned Messages`}</h1>
			</div>
			<div className={styles.body}>
				<ChannelPinsContent channel={channel} />
			</div>
		</div>
	);
});
