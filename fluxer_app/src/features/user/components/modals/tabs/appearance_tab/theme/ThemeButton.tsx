// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import styles from '@app/features/user/components/modals/tabs/appearance_tab/ThemeTab.module.css';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {CheckIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';

export interface ThemeButtonProps {
	themeType: ThemeType;
	currentTheme: ThemeType;
	label: string;
	backgroundColor: string;
	isLight: boolean;
	disabled?: boolean;
	onKeyDown: (event: React.KeyboardEvent, themeType: ThemeType) => void;
	onClick: (themeType: ThemeType) => void;
	icon?: React.ReactElement<Record<string, unknown>>;
}

export const ThemeButton = observer(
	React.forwardRef<HTMLButtonElement, ThemeButtonProps>(
		({themeType, currentTheme, label, backgroundColor, isLight, disabled = false, onKeyDown, onClick, icon}, ref) => {
			const isSelected = currentTheme === themeType;
			const getButtonClassName = () => {
				const classes = [styles.themeButton];
				if (isSelected) {
					classes.push(styles.themeButtonSelected);
				} else if (isLight) {
					classes.push(styles.themeButtonLight);
				} else {
					classes.push(styles.themeButtonDark);
				}
				return clsx(classes);
			};
			return (
				<FocusRing offset={-2} data-flx="user.appearance-tab.theme.theme-button.focus-ring">
					<button
						ref={ref}
						type="button"
						onClick={() => onClick(themeType)}
						onKeyDown={(e) => onKeyDown(e, themeType)}
						className={getButtonClassName()}
						style={{backgroundColor}}
						disabled={disabled}
						role="radio"
						aria-checked={isSelected}
						aria-disabled={disabled}
						aria-label={label}
						tabIndex={disabled ? -1 : isSelected ? 0 : -1}
						data-flx="user.appearance-tab.theme.theme-button.radio.click.button"
					>
						{icon && (
							<div
								className={styles.themeButtonIcon}
								aria-hidden="true"
								data-flx="user.appearance-tab.theme.theme-button.theme-button-icon"
							>
								{React.cloneElement(icon, {
									size: 24,
									weight: 'bold',
									style: {color: isLight ? '#000000' : '#ffffff'},
								})}
							</div>
						)}
						{isSelected && (
							<div
								className={styles.themeButtonCheckmark}
								aria-hidden="true"
								data-flx="user.appearance-tab.theme.theme-button.theme-button-checkmark"
							>
								<CheckIcon
									weight="bold"
									className={styles.themeButtonCheckmarkIcon}
									size={remFromPx(12)}
									data-flx="user.appearance-tab.theme.theme-button.theme-button-checkmark-icon"
								/>
							</div>
						)}
					</button>
				</FocusRing>
			);
		},
	),
);
