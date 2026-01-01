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
import type {MotionStyle} from 'framer-motion';
import React from 'react';
import styles from './NativeDragRegion.module.css';

type ElementType = React.ElementType;

type NativeDragRegionProps = Omit<React.HTMLAttributes<HTMLElement>, 'style'> & {
	as?: ElementType;
	disabled?: boolean;
	style?: React.CSSProperties | MotionStyle;
};

export const NativeDragRegion = React.forwardRef<HTMLElement, NativeDragRegionProps>(
	function NativeDragRegionInner(props, ref) {
		const {as, disabled = false, className, ...rest} = props;
		const Component = (as ?? 'div') as ElementType;

		return (
			<Component
				ref={ref as React.Ref<HTMLElement>}
				className={clsx(className, !disabled && styles.nativeDragRegion)}
				{...rest}
			/>
		);
	},
);

NativeDragRegion.displayName = 'NativeDragRegion';
