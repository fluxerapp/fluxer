// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/ui/components/form/FormSwitch.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as SwitchPrimitive from '@radix-ui/react-switch';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {forwardRef, useCallback, useId, useMemo, useRef} from 'react';

interface SwitchProps {
	label?: React.ReactNode;
	description?: React.ReactNode;
	value: boolean;
	onChange: (value: boolean) => void;
	disabled?: boolean;
	ariaLabel?: string;
	ariaLabelledBy?: string;
	ariaDescribedBy?: string;
	className?: string;
	compact?: boolean;
}

export const Switch = observer(
	forwardRef<HTMLDivElement, SwitchProps>(function Switch(
		{label, description, value, onChange, disabled, ariaLabel, ariaLabelledBy, ariaDescribedBy, className, compact},
		forwardedRef,
	) {
		const baseId = useId();
		const labelId = useMemo(() => `${baseId}-switch-label`, [baseId]);
		const descriptionId = useMemo(() => `${baseId}-switch-description`, [baseId]);
		const hasLabel = useMemo(
			() => label !== undefined && label !== null && !(typeof label === 'string' && label.trim().length === 0),
			[label],
		);
		const hasDescription = useMemo(
			() =>
				description !== undefined &&
				description !== null &&
				!(typeof description === 'string' && description.trim().length === 0),
			[description],
		);
		const resolvedLabelledBy = hasLabel ? labelId : ariaLabelledBy;
		const resolvedDescribedBy = [hasDescription ? descriptionId : null, ariaDescribedBy].filter(Boolean).join(' ');
		const rootRef = useRef<React.ComponentRef<typeof SwitchPrimitive.Root>>(null);
		const valueChange = useCallback(
			(next: boolean) => {
				if (disabled) return;
				onChange(next);
			},
			[disabled, onChange],
		);
		const handleLabelToggle = useCallback(() => {
			if (disabled) return;
			onChange(!value);
			rootRef.current?.focus();
		}, [disabled, onChange, value]);
		return (
			<div
				ref={forwardedRef}
				className={clsx(styles.container, compact && styles.compact, className)}
				data-flx="ui.form.switch.container"
			>
				{(hasLabel || hasDescription) && (
					<button
						type="button"
						className={clsx(styles.labelContainer, !disabled && styles.clickable)}
						disabled={disabled}
						onClick={handleLabelToggle}
						aria-pressed={value}
						aria-labelledby={hasLabel ? labelId : undefined}
						aria-describedby={hasDescription ? descriptionId : undefined}
						data-flx="ui.form.switch.label-container.label-toggle"
					>
						{hasLabel && (
							<span
								id={labelId}
								className={clsx(styles.label, disabled && styles.disabled)}
								data-flx="ui.form.switch.label"
							>
								{label}
							</span>
						)}
						{hasDescription && (
							<span id={descriptionId} className={styles.description} data-flx="ui.form.switch.description">
								{description}
							</span>
						)}
					</button>
				)}
				<FocusRing focusTarget={rootRef} ringTarget={rootRef} offset={-2} data-flx="ui.form.switch.focus-ring">
					<SwitchPrimitive.Root
						ref={rootRef}
						checked={value}
						onCheckedChange={valueChange}
						disabled={disabled}
						className={clsx(styles.switchRoot, disabled && styles.disabled)}
						aria-label={!resolvedLabelledBy ? ariaLabel : undefined}
						aria-labelledby={resolvedLabelledBy}
						aria-describedby={resolvedDescribedBy || undefined}
						data-flx="ui.form.switch.switch-root"
					>
						<SwitchPrimitive.Thumb className={styles.switchThumb} data-flx="ui.form.switch.switch-thumb">
							{value ? (
								<svg
									xmlns="http://www.w3.org/2000/svg"
									width="12"
									height="12"
									fill="currentColor"
									viewBox="0 0 256 256"
									className={styles.iconChecked}
									aria-hidden="true"
									data-flx="ui.form.switch.icon-checked"
								>
									<path
										d="M232.49,80.49l-128,128a12,12,0,0,1-17,0l-56-56a12,12,0,1,1,17-17L96,183,215.51,63.51a12,12,0,0,1,17,17Z"
										data-flx="ui.form.switch.path"
									/>
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
									data-flx="ui.form.switch.icon-unchecked"
								>
									<path
										d="M208.49,191.51a12,12,0,0,1-17,17L128,145,64.49,208.49a12,12,0,0,1-17-17L111,128,47.51,64.49a12,12,0,0,1,17-17L128,111l63.51-63.52a12,12,0,0,1,17,17L145,128Z"
										data-flx="ui.form.switch.path--2"
									/>
								</svg>
							)}
						</SwitchPrimitive.Thumb>
					</SwitchPrimitive.Root>
				</FocusRing>
			</div>
		);
	}),
);
