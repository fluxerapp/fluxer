// SPDX-License-Identifier: AGPL-3.0-or-later

import {NAGBAR_TONES} from '@app/features/app/components/layout/NagbarTones';
import styles from '@app/features/app/components/skeleton/NagbarSkeleton.module.css';
import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import type {RememberedSkeletonNagbarRow} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import {flxElementClassName} from '@app/lib/react';
import type React from 'react';

const MESSAGE_WIDTH_DESKTOP = 'min(18rem, 60%)';
const MESSAGE_WIDTH_MOBILE = 'min(12rem, 72%)';
const MESSAGE_HEIGHT = '0.75rem';
const ACTION_WIDTH = '6rem';
const ACTION_HEIGHT = '2rem';

interface NagbarSkeletonProps {
	readonly isMobile: boolean;
	readonly rows: ReadonlyArray<RememberedSkeletonNagbarRow>;
}

function resolveRowStyle(row: RememberedSkeletonNagbarRow): React.CSSProperties {
	const tone = NAGBAR_TONES[row.tone];
	return {backgroundColor: tone.backgroundColor, color: tone.textColor};
}

export const NagbarSkeleton: React.FC<NagbarSkeletonProps> = ({isMobile, rows}) => {
	if (rows.length === 0) {
		return null;
	}
	let messageWidth = MESSAGE_WIDTH_DESKTOP;
	if (isMobile) {
		messageWidth = MESSAGE_WIDTH_MOBILE;
	}
	return (
		<flx-app-nagbar-skeleton
			className={flxElementClassName(styles.stack)}
			aria-hidden
			data-flx="app.skeleton.nagbar-skeleton.stack"
		>
			{rows.map((row, index) => (
				<flx-app-nagbar-skeleton-row
					key={index}
					className={flxElementClassName(
						styles.row,
						row.dismissible && styles.rowDismissible,
						isMobile && styles.rowMobile,
						isMobile && row.hasActions && styles.rowMobileWithActions,
					)}
					style={resolveRowStyle(row)}
					data-flx="app.skeleton.nagbar-skeleton.row"
				>
					<SkeletonBlock
						width={messageWidth}
						height={MESSAGE_HEIGHT}
						radius={SkeletonRadius.PILL}
						emphasis={SkeletonEmphasis.MUTED}
						className={styles.tinted}
						data-flx="app.skeleton.nagbar-skeleton.tinted"
					/>
					{isMobile && row.hasActions && (
						<SkeletonBlock
							width={ACTION_WIDTH}
							height={ACTION_HEIGHT}
							radius={SkeletonRadius.MEDIUM}
							emphasis={SkeletonEmphasis.MUTED}
							className={styles.tinted}
							data-flx="app.skeleton.nagbar-skeleton.tinted--2"
						/>
					)}
				</flx-app-nagbar-skeleton-row>
			))}
		</flx-app-nagbar-skeleton>
	);
};
