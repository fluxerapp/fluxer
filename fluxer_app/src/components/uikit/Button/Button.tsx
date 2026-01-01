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
import React from 'react';
import styles from '~/components/uikit/Button/Button.module.css';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';

interface BaseButtonProps
	extends Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'onClick' | 'type' | 'disabled' | 'className'> {
	className?: string;
	contentClassName?: string;
	disabled?: boolean;
	leftIcon?: React.ReactNode;
	rightIcon?: React.ReactNode;

	onClick?:
		| ((event: React.MouseEvent<HTMLButtonElement>) => void)
		| ((event: React.KeyboardEvent<HTMLButtonElement>) => void);

	small?: boolean;
	compact?: boolean;
	superCompact?: boolean;
	submitting?: boolean;
	type?: 'button' | 'submit';
	variant?: 'primary' | 'secondary' | 'danger-primary' | 'danger-secondary' | 'inverted' | 'inverted-outline';
	fitContainer?: boolean;
	fitContent?: boolean;
	recording?: boolean;
	matchSkeletonHeight?: boolean;
}

export interface SquareButtonProps extends BaseButtonProps {
	square: true;
	children?: never;
	icon: React.ReactNode;
}

export interface RegularButtonProps extends BaseButtonProps {
	square?: false;
	children?: React.ReactNode;
}

export type ButtonProps = SquareButtonProps | RegularButtonProps;

export const Button = React.forwardRef<HTMLButtonElement, ButtonProps>((props, ref) => {
	const {
		children,
		className,
		contentClassName,
		disabled,
		leftIcon,
		rightIcon,
		onClick,
		small,
		compact,
		superCompact,
		square,
		submitting,
		type = 'button',
		variant = 'primary',
		fitContainer = false,
		fitContent = false,
		recording = false,
		matchSkeletonHeight = false,
		onKeyDown: userOnKeyDown,
		...buttonProps
	} = props;

	const icon = square ? (props as SquareButtonProps).icon : undefined;

	const handleClick = (event: React.MouseEvent<HTMLButtonElement>) => {
		if (submitting) {
			event.preventDefault();
			return;
		}
		(onClick as ((e: React.MouseEvent<HTMLButtonElement>) => void) | undefined)?.(event);
	};

	const handleKeyDown = (event: React.KeyboardEvent<HTMLButtonElement>) => {
		userOnKeyDown?.(event);

		if (submitting) {
			event.preventDefault();
			return;
		}

		const isSpaceKey = event.key === ' ' || event.key === 'Spacebar';

		if (isSpaceKey) {
			event.preventDefault();
			(onClick as ((e: React.KeyboardEvent<HTMLButtonElement>) => void) | undefined)?.(event);
		}
	};

	const spinnerItemClass = clsx(styles.spinnerItem, {
		[styles.spinnerItemInverted]: variant === 'inverted',
	});

	const variantClass =
		variant === 'inverted-outline'
			? 'invertedOutline'
			: variant === 'danger-primary'
				? 'dangerPrimary'
				: variant === 'danger-secondary'
					? 'dangerSecondary'
					: variant;

	return (
		<FocusRing offset={-2}>
			<button
				ref={ref}
				className={clsx(
					styles.button,
					styles[variantClass],
					{
						[styles.small]: small,
						[styles.compact]: compact,
						[styles.superCompact]: superCompact,
						[styles.square]: square,
						[styles.fitContainer]: fitContainer,
						[styles.fitContent]: fitContent,
						[styles.recording]: recording,
						[styles.matchSkeletonHeight]: matchSkeletonHeight,
					},
					className,
				)}
				disabled={disabled}
				onClick={handleClick}
				onKeyDown={handleKeyDown}
				type={type}
				tabIndex={disabled ? -1 : 0}
				{...buttonProps}
			>
				<div className={clsx(contentClassName)}>
					<div className={styles.grid}>
						<div className={clsx(styles.iconWrapper, {[styles.hidden]: submitting})}>
							{square ? (
								icon
							) : (
								<>
									{leftIcon}
									{children}
									{rightIcon}
								</>
							)}
						</div>
						<div className={clsx(styles.spinnerWrapper, {[styles.hidden]: !submitting})}>
							<span className={styles.spinner}>
								<span className={styles.spinnerInner}>
									<span className={spinnerItemClass} />
									<span className={spinnerItemClass} />
									<span className={spinnerItemClass} />
								</span>
							</span>
						</div>
					</div>
				</div>
			</button>
		</FocusRing>
	);
});

Button.displayName = 'Button';
