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
import {WarningCircleIcon} from '@phosphor-icons/react';
import type React from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';

export const UserProfileDataWarning: React.FC = () => {
	const {t} = useLingui();
	const WARNING_TOOLTIP = t`We failed to retrieve the full information about this user at this time.`;

	return (
		<Tooltip text={WARNING_TOOLTIP} maxWidth="xl">
			<FocusRing offset={-2}>
				<span
					role="img"
					aria-label={WARNING_TOOLTIP}
					style={{
						display: 'inline-flex',
						alignItems: 'center',
						justifyContent: 'center',
						width: 28,
						height: 28,
					}}
				>
					<WarningCircleIcon weight="fill" size={18} color="var(--status-warning)" />
				</span>
			</FocusRing>
		</Tooltip>
	);
};
