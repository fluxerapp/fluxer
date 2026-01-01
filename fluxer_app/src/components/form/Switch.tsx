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

import * as SwitchPrimitive from '@radix-ui/react-switch';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import styles from './Switch.module.css';

interface SwitchProps {
	label?: React.ReactNode;
	description?: React.ReactNode;
	value: boolean;
	onChange: (value: boolean) => void;
	disabled?: boolean;
	ariaLabel?: string;
	className?: string;
	compact?: boolean;
}
export const Switch = observer(
	({label, description, value, onChange, disabled, ariaLabel, className, compact}: SwitchProps) => {
		const baseId = React.useId();
		const labelId = `${baseId}-switch-label`;
		const descriptionId = `${baseId}-switch-description`;

		const hasLabel = label !== undefined && label !== null && !(typeof label === 'string' && label.trim().length === 0);

		const hasDescription =
			description !== undefined &&
			description !== null &&
			!(typeof description === 'string' && description.trim().length === 0);

		const rootRef = React.useRef<React.ElementRef<typeof SwitchPrimitive.Root>>(null);

		const valueChange = React.useCallback(
			(next: boolean) => {
				if (disabled) return;
				onChange(next);
			},
			[disabled, onChange],
		);

		const handleLabelToggle = React.useCallback(() => {
			if (disabled) return;
			onChange(!value);
			rootRef.current?.focus();
		}, [disabled, onChange, value]);

		const handleLabelKeyDown = React.useCallback(
			(event: React.KeyboardEvent<HTMLDivElement>) => {
				if (event.key === 'Enter' || event.key === ' ') {
					event.preventDefault();
					handleLabelToggle();
				}
			},
			[handleLabelToggle],
		);

		return (
			<div className={clsx(styles.container, compact && styles.compact, className)}>
				{(hasLabel || hasDescription) && (
					<div
						className={clsx(styles.labelContainer, !disabled && styles.clickable)}
						onClick={handleLabelToggle}
						onKeyDown={handleLabelKeyDown}
						tabIndex={disabled ? -1 : 0}
						role="button"
						aria-disabled={disabled}
					>
						{hasLabel && (
							<span id={labelId} className={clsx(styles.label, disabled && styles.disabled)}>
								{label}
							</span>
						)}
						{hasDescription && (
							<p id={descriptionId} className={styles.description}>
								{description}
							</p>
						)}
					</div>
				)}

				<FocusRing focusTarget={rootRef} ringTarget={rootRef} offset={-2}>
					<SwitchPrimitive.Root
						ref={rootRef}
						checked={value}
						onCheckedChange={valueChange}
						disabled={disabled}
						className={clsx(styles.switchRoot, disabled && styles.disabled)}
						aria-label={!hasLabel ? ariaLabel : undefined}
						aria-labelledby={hasLabel ? labelId : undefined}
						aria-describedby={hasDescription ? descriptionId : undefined}
					>
						<SwitchPrimitive.Thumb className={styles.switchThumb}>
							{value ? (
								<svg
									xmlns="http://www.w3.org/2000/svg"
									width="12"
									height="12"
									fill="currentColor"
									viewBox="0 0 256 256"
									className={styles.iconChecked}
									aria-hidden="true"
								>
									<path d="M232.49,80.49l-128,128a12,12,0,0,1-17,0l-56-56a12,12,0,1,1,17-17L96,183,215.51,63.51a12,12,0,0,1,17,17Z" />
								</svg>
							) : (
								<svg
									xmlns="http://www.w3.org/2000/svg"
									width="10"
									height="10"
									fill="currentColor"
									viewBox="0 0 256 256"
									className={styles.iconUnchecked}
									aria-hidden="true"
								>
									<path d="M208.49,191.51a12,12,0,0,1-17,17L128,145,64.49,208.49a12,12,0,0,1-17-17L111,128,47.51,64.49a12,12,0,0,1,17-17L128,111l63.51-63.52a12,12,0,0,1,17,17L145,128Z" />
								</svg>
							)}
						</SwitchPrimitive.Thumb>
					</SwitchPrimitive.Root>
				</FocusRing>
			</div>
		);
	},
);
