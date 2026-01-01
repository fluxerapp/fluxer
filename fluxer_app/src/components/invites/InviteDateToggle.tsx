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
import React from 'react';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import styles from './InviteDateToggle.module.css';

interface InviteDateToggleProps {
	showCreatedDate: boolean;
	onToggle: (showCreatedDate: boolean) => void;
}

export const InviteDateToggle: React.FC<InviteDateToggleProps> = observer(({showCreatedDate, onToggle}) => {
	const handleChange = React.useCallback(
		(isChecked: boolean) => {
			onToggle(isChecked);
		},
		[onToggle],
	);

	return (
		<div className={styles.container}>
			<Checkbox checked={showCreatedDate} onChange={handleChange} size="small">
				<span className={styles.label}>
					<Trans>Show creation date instead of expiration date</Trans>
				</span>
			</Checkbox>
		</div>
	);
});
