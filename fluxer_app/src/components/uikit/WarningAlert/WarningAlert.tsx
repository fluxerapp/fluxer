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

import {WarningIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import styles from './WarningAlert.module.css';

interface WarningAlertProps {
	title?: React.ReactNode;
	children: React.ReactNode;
	link?: {
		label: React.ReactNode;
		onClick: () => void;
	};
	actions?: React.ReactNode;
	className?: string;
}

export const WarningAlert: React.FC<WarningAlertProps> = ({title, children, link, actions, className}) => {
	return (
		<div className={clsx(styles.alert, className)}>
			<WarningIcon size={16} weight="fill" className={styles.icon} />
			<div className={styles.content}>
				{title && <h4 className={styles.title}>{title}</h4>}
				<p className={styles.text}>{children}</p>
				{link && (
					<button type="button" className={styles.link} onClick={link.onClick}>
						{link.label}
					</button>
				)}
				{actions && <div className={styles.actions}>{actions}</div>}
			</div>
		</div>
	);
};
