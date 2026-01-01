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

import type {Icon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {stopPropagationOnEnterSpace} from '~/utils/KeyboardUtils';
import styles from './ChannelItemIcon.module.css';

interface ChannelItemIconProps {
	icon: Icon;
	label: string;
	onClick?: () => void;
	className?: string;
	selected?: boolean;
}

export const ChannelItemIcon = observer(
	({icon: Icon, label, onClick, className, selected = false}: ChannelItemIconProps) => {
		return (
			<Tooltip text={label}>
				<FocusRing offset={-2} ringClassName={styles.iconFocusRing}>
					<button
						type="button"
						className={clsx(
							styles.iconButton,
							selected ? styles.iconButtonSelected : styles.iconButtonDefault,
							className,
						)}
						aria-label={label}
						onClick={(e) => {
							e.stopPropagation();
							onClick?.();
						}}
						onKeyDown={stopPropagationOnEnterSpace}
					>
						<Icon className={styles.icon} />
					</button>
				</FocusRing>
			</Tooltip>
		);
	},
);
