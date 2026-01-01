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
import {Button} from '~/components/uikit/Button/Button';
import styles from './StatusSlate.module.css';

interface StatusAction {
	text: React.ReactNode;
	onClick: () => void;
	variant?: React.ComponentProps<typeof Button>['variant'];
	fitContent?: boolean;
	fitContainer?: boolean;
}

interface StatusSlateProps {
	Icon: React.ComponentType<React.ComponentProps<'svg'>>;
	title: React.ReactNode;
	description: React.ReactNode;
	actions?: Array<StatusAction>;
	fullHeight?: boolean;
}

export const StatusSlate: React.FC<StatusSlateProps> = observer(
	({Icon, title, description, actions = [], fullHeight = false}) => {
		return (
			<div className={`${styles.container} ${fullHeight ? styles.fullHeight : ''}`}>
				<Icon className={styles.icon} aria-hidden />
				<h3 className={styles.title}>{title}</h3>
				<p className={styles.description}>{description}</p>
				{actions.length > 0 && (
					<div className={styles.actions}>
						{actions.map((action, index) => (
							<Button
								key={index}
								variant={action.variant ?? 'primary'}
								fitContent={action.fitContent ?? true}
								fitContainer={action.fitContainer ?? false}
								onClick={action.onClick}
								submitting={false}
							>
								{action.text}
							</Button>
						))}
					</div>
				)}
			</div>
		);
	},
);
