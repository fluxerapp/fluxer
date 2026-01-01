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

import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {UIEvent} from 'react';
import {Scroller} from '~/components/uikit/Scroller';
import styles from './MemberListContainer.module.css';

interface MemberListContainerProps {
	channelId: string;
	children: React.ReactNode;
	onScroll?: (event: UIEvent<HTMLDivElement>) => void;
}

export const MemberListContainer: React.FC<MemberListContainerProps> = observer(({channelId, children, onScroll}) => {
	return (
		<div className={styles.memberListContainer}>
			<Scroller className={styles.memberListScroller} key={`member-list-scroller-${channelId}`} onScroll={onScroll}>
				<div className={styles.scrollerSpacer} />
				{children}
			</Scroller>
		</div>
	);
});
