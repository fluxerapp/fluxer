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
import {clsx} from 'clsx';
import React from 'react';
import styles from './UserTag.module.css';

interface UserTagProps extends React.ComponentPropsWithoutRef<'span'> {
	className?: string;
	system?: boolean;
	size?: 'sm' | 'lg';
}

export const UserTag = React.forwardRef<HTMLSpanElement, UserTagProps>(
	({className, system, size = 'sm', ...props}, ref) => {
		return (
			<span className={clsx(styles.tag, size === 'lg' ? styles.tagLg : styles.tagSm, className)} ref={ref} {...props}>
				<span className={clsx(styles.text, size === 'lg' ? styles.textLg : styles.textSm)}>
					{system ? <Trans>System</Trans> : <Trans>Bot</Trans>}
				</span>
			</span>
		);
	},
);

UserTag.displayName = 'UserTag';
