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
import React from 'react';
import {FrameContext, type FrameSides} from './FrameContext';
import styles from './OutlineFrame.module.css';

interface OutlineFrameProps {
	sidebarDivider?: boolean;
	hideTopBorder?: boolean;
	sides?: FrameSides;
	nagbar?: React.ReactNode;
	topBanner?: React.ReactNode;
	children: React.ReactNode;
	className?: string;
}

export const OutlineFrame: React.FC<OutlineFrameProps> = ({
	sidebarDivider = false,
	hideTopBorder = false,
	sides,
	topBanner,
	nagbar,
	children,
	className,
}) => {
	const ctxSides = React.useMemo<FrameSides>(() => {
		return {
			top: !hideTopBorder,
			right: true,
			bottom: true,
			left: true,
			...sides,
		};
	}, [hideTopBorder, sides]);

	const showTopBorder = ctxSides.top !== false;
	const frameStyle = React.useMemo<React.CSSProperties>(() => {
		return {
			borderLeft: ctxSides.left === false ? 'none' : undefined,
			borderRight: ctxSides.right === false ? 'none' : undefined,
			borderBottom: ctxSides.bottom === false ? 'none' : undefined,
		};
	}, [ctxSides.bottom, ctxSides.left, ctxSides.right]);

	return (
		<div
			className={clsx(
				styles.frame,
				showTopBorder && styles.frameShowTop,
				!showTopBorder && styles.frameHideTop,
				className,
			)}
			style={frameStyle}
		>
			<FrameContext.Provider value={ctxSides}>
				{topBanner}
				{nagbar}
				<div className={styles.contentWrapper}>
					{sidebarDivider && <div className={styles.divider} aria-hidden />}
					<div className={styles.body}>{children}</div>
				</div>
			</FrameContext.Provider>
		</div>
	);
};
