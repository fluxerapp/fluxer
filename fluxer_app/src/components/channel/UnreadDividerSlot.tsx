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
import {observer} from 'mobx-react-lite';
import dividerStyles from './Divider.module.css';
import styles from './Messages.module.css';

type UnreadDividerSlotProps =
	| {beforeId: string; afterId?: never; visible: boolean}
	| {afterId: string; beforeId?: never; visible: boolean};

export const UnreadDividerSlot = observer(function UnreadDividerSlot(props: UnreadDividerSlotProps) {
	const data: Record<string, string> = {'data-divider-slot': 'unread'};
	if ('beforeId' in props && props.beforeId !== undefined) data['data-before-id'] = props.beforeId;
	if ('afterId' in props && props.afterId !== undefined) data['data-after-id'] = props.afterId;

	return (
		<div
			className={styles.unreadSlot}
			aria-hidden="true"
			id={props.visible ? 'new-messages-bar' : undefined}
			data-visible={props.visible ? '1' : undefined}
			{...(data as any)}
		>
			<div className={dividerStyles.unreadContainer}>
				<div className={dividerStyles.unreadLine} />
				<span className={dividerStyles.unreadBadge}>
					<Trans>New</Trans>
				</span>
			</div>
		</div>
	);
});
