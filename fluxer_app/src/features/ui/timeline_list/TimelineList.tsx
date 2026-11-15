// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/ui/timeline_list/TimelineList.module.css';
import {clsx} from 'clsx';
import type React from 'react';

interface TimelineListItem {
	key: string;
	content: React.ReactNode;
}

interface TimelineListProps {
	items: ReadonlyArray<TimelineListItem>;
	ordered?: boolean;
	clampLines?: boolean;
	className?: string;
	'data-flx'?: string;
}

export const TimelineList: React.FC<TimelineListProps> = ({
	items,
	ordered = false,
	clampLines = false,
	className,
	'data-flx': dataFlx,
}) => {
	const List = ordered ? 'ol' : 'ul';
	return (
		<List className={clsx(styles.list, className)} data-flx={dataFlx ?? 'ui.timeline-list.timeline-list.list'}>
			{items.map((item) => (
				<li key={item.key} className={styles.item} data-flx="ui.timeline-list.timeline-list.item">
					<span
						className={clsx(styles.text, clampLines && styles.clamped)}
						data-flx="ui.timeline-list.timeline-list.text"
					>
						{item.content}
					</span>
				</li>
			))}
		</List>
	);
};
