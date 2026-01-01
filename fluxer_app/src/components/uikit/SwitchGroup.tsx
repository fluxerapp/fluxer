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
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Switch} from '~/components/form/Switch';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import styles from './SwitchGroup.module.css';

interface SwitchGroupItemProps {
	label: React.ReactNode;
	value: boolean;
	onChange: (value: boolean) => void;
	shortcut?: React.ReactNode;
	disabled?: boolean;
}

export const SwitchGroupItem = observer(({label, value, onChange, shortcut, disabled}: SwitchGroupItemProps) => {
	const handleClick = () => !disabled && onChange(!value);
	const handleKeyDown = (event: React.KeyboardEvent) => {
		if (event.key === 'Enter' || event.key === ' ') {
			event.preventDefault();
			handleClick();
		}
	};

	return (
		<div className={styles.item}>
			<div className={styles.itemContent}>
				<FocusRing offset={-2}>
					<button
						type="button"
						tabIndex={disabled ? -1 : 0}
						className={clsx(styles.itemLabel, {
							[styles.disabled]: disabled,
						})}
						onClick={handleClick}
						onKeyDown={handleKeyDown}
					>
						<span className={styles.labelText}>{label}</span>
						{shortcut && <span className={styles.shortcut}>{shortcut}</span>}
					</button>
				</FocusRing>
				<Switch label="" value={value} onChange={onChange} disabled={disabled} />
			</div>
		</div>
	);
});

interface SwitchGroupProps {
	children: React.ReactNode;
}

export const SwitchGroup = observer(({children}: SwitchGroupProps) => {
	return <div className={styles.container}>{children}</div>;
});

interface SwitchGroupCustomItemProps {
	label: React.ReactNode;
	value: boolean;
	onChange: (value: boolean) => void;
	disabled?: boolean;
	extraContent?: React.ReactNode;
	onClick?: () => void;
	clickDisabled?: boolean;
}

export const SwitchGroupCustomItem = observer(
	({label, value, onChange, disabled, extraContent, onClick, clickDisabled}: SwitchGroupCustomItemProps) => {
		const handleClick = () => {
			if (onClick && !clickDisabled) {
				onClick();
			}
		};

		return (
			<div className={clsx('group', styles.item)}>
				<div className={styles.itemContent}>
					<FocusRing offset={-2}>
						<button
							type="button"
							className={clsx(styles.itemLabel, {
								[styles.disabled]: clickDisabled,
							})}
							onClick={handleClick}
							disabled={clickDisabled}
						>
							<span className={styles.labelText}>{label}</span>
						</button>
					</FocusRing>
					<div className={styles.extraContent}>
						{extraContent}
						<Switch label="" value={value} onChange={onChange} disabled={disabled} />
					</div>
				</div>
			</div>
		);
	},
);
