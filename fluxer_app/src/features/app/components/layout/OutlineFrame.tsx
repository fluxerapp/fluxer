// SPDX-License-Identifier: AGPL-3.0-or-later

import {FrameContext, type FrameSides} from '@app/features/app/components/layout/FrameContext';
import styles from '@app/features/app/components/layout/OutlineFrame.module.css';
import {SidebarResizeHandle} from '@app/features/app/components/layout/SidebarResizeHandle';
import {clsx} from 'clsx';
import type React from 'react';
import {useMemo} from 'react';

interface OutlineFrameProps {
	sidebarDivider?: boolean;
	sidebarResizeHandle?: boolean;
	hideTopBorder?: boolean;
	sides?: FrameSides;
	nagbar?: React.ReactNode;
	topBanner?: React.ReactNode;
	children: React.ReactNode;
	className?: string;
}

export const OutlineFrame: React.FC<OutlineFrameProps> = ({
	sidebarDivider = false,
	sidebarResizeHandle = false,
	hideTopBorder = false,
	sides,
	topBanner,
	nagbar,
	children,
	className,
}) => {
	const ctxSides = useMemo<FrameSides>(() => {
		return {
			top: !hideTopBorder,
			right: true,
			bottom: true,
			left: true,
			...sides,
		};
	}, [hideTopBorder, sides]);
	const showTopBorder = ctxSides.top !== false;
	const frameStyle = useMemo<React.CSSProperties>(() => {
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
			data-flx="app.outline-frame.frame"
		>
			<FrameContext.Provider value={ctxSides}>
				{topBanner}
				{nagbar}
				<div className={styles.contentWrapper} data-flx="app.outline-frame.content-wrapper">
					{sidebarDivider && <div className={styles.divider} aria-hidden data-flx="app.outline-frame.divider" />}
					{sidebarResizeHandle && <SidebarResizeHandle />}
					<div className={styles.body} data-flx="app.outline-frame.body">
						{children}
					</div>
				</div>
			</FrameContext.Provider>
		</div>
	);
};
