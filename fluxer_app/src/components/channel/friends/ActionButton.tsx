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

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import styles from './ActionButton.module.css';

export const ActionButton = observer(
	({
		tooltip,
		onClick,
		className,
		danger = false,
		children,
	}: {
		tooltip: string;
		onClick: (e: React.MouseEvent) => void;
		className?: string;
		danger?: boolean;
		children: React.ReactNode;
	}) => (
		<Tooltip text={tooltip} position="top">
			<FocusRing>
				<button
					type="button"
					className={clsx(styles.button, danger && styles.danger, !danger && className)}
					onClick={(e) => {
						e.stopPropagation();
						onClick(e);
					}}
				>
					{children}
				</button>
			</FocusRing>
		</Tooltip>
	),
);
