// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/NativeDragRegion.module.css';
import {clsx} from 'clsx';
import type {MotionStyle} from 'framer-motion';
import React from 'react';

type ElementType = React.ElementType;
type NativeDragRegionProps = Omit<React.HTMLAttributes<HTMLElement>, 'style'> & {
	as?: ElementType;
	disabled?: boolean;
	style?: React.CSSProperties | MotionStyle;
};

export const NativeDragRegion = React.forwardRef<HTMLElement, NativeDragRegionProps>(
	function NativeDragRegionInner(props, ref) {
		const {as, disabled = false, className, ...rest} = props;
		let Component: ElementType = 'div';
		if (as != null) {
			Component = as;
		}
		return React.createElement(Component, {
			...rest,
			ref: ref as React.Ref<HTMLElement>,
			className: clsx(className, !disabled && styles.nativeDragRegion),
			'data-flx': 'app.native-drag-region.native-drag-region-inner.native-drag-region',
		});
	},
);

NativeDragRegion.displayName = 'NativeDragRegion';
