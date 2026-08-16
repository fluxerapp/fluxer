// SPDX-License-Identifier: AGPL-3.0-or-later

import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import styles from '@app/features/ui/button/Button.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Button as BaseButton} from '@base-ui/react/button';
import {clsx} from 'clsx';
import React from 'react';

export type ButtonVariant = 'primary' | 'secondary' | 'danger' | 'inverted' | 'inverted-outline' | 'ghost';

interface BaseButtonProps
	extends Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'onClick' | 'type' | 'disabled' | 'className' | 'title'> {
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
	variant?: ButtonVariant;
	fitContainer?: boolean;
	fitContent?: boolean;
	recording?: boolean;
	matchSkeletonHeight?: boolean;
	title?: never;
}

type SquareButtonAccessibleName =
	| {
			'aria-label': string;
			'aria-labelledby'?: string;
	  }
	| {
			'aria-label'?: string;
			'aria-labelledby': string;
	  };
export type SquareButtonProps = BaseButtonProps &
	SquareButtonAccessibleName & {
		square: true;
		children?: never;
		icon: React.ReactNode;
	};

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
		if (submitting && isKeyboardActivationKey(event.key)) {
			event.preventDefault();
		}
	};
	const variantClass = variant === 'inverted-outline' ? 'invertedOutline' : variant;
	const baseButtonRef = ref as React.ForwardedRef<HTMLElement>;
	return (
		<FocusRing offset={-2} data-flx="ui.button.button.focus-ring">
			<BaseButton
				ref={baseButtonRef}
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
				data-flx="ui.button.button.button.click"
				{...buttonProps}
			>
				<div className={clsx(contentClassName)} data-flx="ui.button.button.div">
					<div className={styles.grid} data-flx="ui.button.button.grid">
						<div
							className={clsx(styles.iconWrapper, {[styles.hidden]: submitting})}
							data-flx="ui.button.button.icon-wrapper"
						>
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
						<div
							className={clsx(styles.spinnerWrapper, {[styles.hidden]: !submitting})}
							data-flx="ui.button.button.spinner-wrapper"
						>
							<span className={styles.spinner} data-flx="ui.button.button.spinner">
								<span className={styles.spinnerInner} data-flx="ui.button.button.spinner-inner">
									<span className={styles.spinnerItem} data-flx="ui.button.button.span" />
									<span className={styles.spinnerItem} data-flx="ui.button.button.span--2" />
									<span className={styles.spinnerItem} data-flx="ui.button.button.span--3" />
								</span>
							</span>
						</div>
					</div>
				</div>
			</BaseButton>
		</FocusRing>
	);
});

Button.displayName = 'Button';
