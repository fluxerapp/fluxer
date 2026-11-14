// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/ChannelItem.module.css';
import {useTextOverflow} from '@app/features/ui/hooks/useTextOverflow';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

interface ChannelItemContentProps {
	icon?: React.ReactNode;
	name: string;
	actions?: React.ReactNode;
	isCategory?: boolean;
	nameClassName?: string;
}

export const ChannelItemContent: React.FC<ChannelItemContentProps> = observer(
	({icon, name, actions, isCategory = false, nameClassName}) => {
		const nameRef = useRef<HTMLSpanElement>(null);
		const isNameOverflowing = useTextOverflow(nameRef, {content: name});
		if (isCategory) {
			return (
				<>
					<div className={styles.categoryContent} data-flx="app.channel-item-content.category-content">
						<Tooltip text={isNameOverflowing && name ? name : ''} data-flx="app.channel-item-content.tooltip">
							<span
								ref={nameRef}
								className={clsx(styles.categoryName, nameClassName)}
								data-flx="app.channel-item-content.category-name"
							>
								{name}
							</span>
						</Tooltip>
					</div>
					{actions && (
						<div className={styles.channelItemActions} data-flx="app.channel-item-content.channel-item-actions">
							{actions}
						</div>
					)}
				</>
			);
		}
		return (
			<>
				{icon && (
					<Tooltip text={name} data-flx="app.channel-item-content.tooltip--2">
						<div data-flx="app.channel-item-content.div">{icon}</div>
					</Tooltip>
				)}
				<Tooltip text={isNameOverflowing && name ? name : ''} data-flx="app.channel-item-content.tooltip--3">
					<span
						ref={nameRef}
						className={clsx(styles.channelName, nameClassName)}
						data-flx="app.channel-item-content.channel-name"
					>
						{name}
					</span>
				</Tooltip>
				{actions && (
					<div className={styles.channelItemActions} data-flx="app.channel-item-content.channel-item-actions--2">
						{actions}
					</div>
				)}
			</>
		);
	},
);
