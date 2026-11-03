// SPDX-License-Identifier: AGPL-3.0-or-later

import {THEME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import Theme from '@app/features/theme/state/Theme';
import {ThemeButton} from '@app/features/theme_settings/components/ThemeButton';
import styles from '@app/features/theme_settings/components/ThemeSelector.module.css';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {SYSTEM_THEME_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowsCounterClockwiseIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef} from 'react';

const DARK_THEME_DESCRIPTOR = msg({
	message: 'Dark theme',
	comment: 'Short label in the theme tab content. Keep it concise.',
});
const USE_DARK_THEME_DESCRIPTOR = msg({
	message: 'Use dark theme',
	comment: 'Short label in the theme tab content. Keep it concise.',
});
const DARK_LEGACY_THEME_DESCRIPTOR = msg({
	message: 'Dark (legacy) theme',
	comment: 'Short label in the theme tab content. Keep it concise.',
});
const USE_LEGACY_DARK_THEME_ORIGINAL_NEUTRAL_GRAY_PALETTE_DESCRIPTOR = msg({
	message: 'Use legacy dark theme (original neutral gray palette)',
	comment: 'Label in the theme tab content.',
});
const COAL_THEME_DESCRIPTOR = msg({
	message: 'Coal theme',
	comment: 'Short label in the theme tab content. Keep it concise.',
});
const USE_COAL_THEME_PITCH_BLACK_SURFACES_DESCRIPTOR = msg({
	message: 'Use coal theme (pitch-black surfaces)',
	comment: 'Label in the theme tab content.',
});
const LIGHT_THEME_DESCRIPTOR = msg({
	message: 'Light theme',
	comment: 'Short label in the theme tab content. Keep it concise.',
});
const USE_LIGHT_THEME_DESCRIPTOR = msg({
	message: 'Use light theme',
	comment: 'Short label in the theme tab content. Keep it concise.',
});

const SYSTEM_DARK_THEME_AUTOMATICALLY_SYNC_WITH_YOUR_SYSTEM_DESCRIPTOR = msg({
	message: "System: dark theme (automatically sync with your system's dark/light preference)",
	comment: 'Description text in the theme tab content.',
});
const SYSTEM_LIGHT_THEME_AUTOMATICALLY_SYNC_WITH_YOUR_SYSTEM_DESCRIPTOR = msg({
	message: "System: light theme (automatically sync with your system's dark/light preference)",
	comment: 'Description text in the theme tab content.',
});
interface ThemeSelectorProps {
	value: ThemeType;
	onChange: (theme: ThemeType) => void;
	className?: string;
	ariaLabel?: string;
}

export const ThemeSelector: React.FC<ThemeSelectorProps> = observer(({value, onChange, className, ariaLabel}) => {
	const {i18n} = useLingui();
	const currentSelectedTheme: ThemeType = value;
	const systemPrefersDark = Theme.systemPrefersDark;
	const buttonRefs = useRef<Record<string, HTMLButtonElement | null>>({});
	const themeToFocusRef = useRef<string | null>(null);
	const handleThemeChange = useCallback(
		(newTheme: ThemeType) => {
			if (newTheme === currentSelectedTheme) return;
			themeToFocusRef.current = newTheme;
			onChange(newTheme);
		},
		[currentSelectedTheme, onChange],
	);
	const themeOptions = useMemo(
		() =>
			[
				{
					type: ThemeTypes.DARK,
					label: i18n._(DARK_THEME_DESCRIPTOR),
					backgroundColor: 'hsl(258, calc(10% * var(--saturation-factor)), 12.04%)',
					isLight: false,
					icon: null,
					tooltip: i18n._(USE_DARK_THEME_DESCRIPTOR),
				},
				{
					type: ThemeTypes.DARK_LEGACY,
					label: i18n._(DARK_LEGACY_THEME_DESCRIPTOR),
					backgroundColor: 'hsl(220, calc(13% * var(--saturation-factor)), 11.18%)',
					isLight: false,
					icon: null,
					tooltip: i18n._(USE_LEGACY_DARK_THEME_ORIGINAL_NEUTRAL_GRAY_PALETTE_DESCRIPTOR),
				},
				{
					type: ThemeTypes.COAL,
					label: i18n._(COAL_THEME_DESCRIPTOR),
					backgroundColor: 'hsl(258, 10%, 2%)',
					isLight: false,
					icon: null,
					tooltip: i18n._(USE_COAL_THEME_PITCH_BLACK_SURFACES_DESCRIPTOR),
				},
				{
					type: ThemeTypes.LIGHT,
					label: i18n._(LIGHT_THEME_DESCRIPTOR),
					backgroundColor: 'hsl(220, 10%, 98.5%)',
					isLight: true,
					icon: null,
					tooltip: i18n._(USE_LIGHT_THEME_DESCRIPTOR),
				},
				{
					type: ThemeTypes.SYSTEM,
					label: i18n._(SYSTEM_THEME_DESCRIPTOR),
					backgroundColor: systemPrefersDark
						? 'hsl(258, calc(10% * var(--saturation-factor)), 5%)'
						: 'hsl(220, 10%, 98.5%)',
					isLight: !systemPrefersDark,
					icon: (
						<ArrowsCounterClockwiseIcon
							size={remFromPx(12)}
							data-flx="user.appearance-tab.theme.theme-tab-content.theme-options.arrows-counter-clockwise-icon"
						/>
					),
					tooltip: systemPrefersDark
						? i18n._(SYSTEM_DARK_THEME_AUTOMATICALLY_SYNC_WITH_YOUR_SYSTEM_DESCRIPTOR)
						: i18n._(SYSTEM_LIGHT_THEME_AUTOMATICALLY_SYNC_WITH_YOUR_SYSTEM_DESCRIPTOR),
				},
			] satisfies ReadonlyArray<{
				type: ThemeType;
				label: string;
				backgroundColor: string;
				isLight: boolean;
				icon: React.ReactNode | null;
				tooltip: string;
			}>,
		[systemPrefersDark, i18n.locale],
	);
	const handleKeyDown = useCallback(
		(event: React.KeyboardEvent, targetTheme: ThemeType) => {
			if (isKeyboardActivationKey(event.key)) {
				event.preventDefault();
				handleThemeChange(targetTheme);
			} else if (event.key === 'ArrowRight' || event.key === 'ArrowLeft') {
				event.preventDefault();
				const order = themeOptions.map((option) => option.type);
				const direction = event.key === 'ArrowRight' ? 1 : -1;
				const currentIndex = Math.max(order.indexOf(currentSelectedTheme as (typeof order)[number]), 0);
				const nextIndex = (currentIndex + direction + order.length) % order.length;
				const nextTheme = order[nextIndex];
				if (nextTheme) {
					handleThemeChange(nextTheme);
				}
			}
		},
		[themeOptions, currentSelectedTheme, handleThemeChange],
	);
	useEffect(() => {
		if (themeToFocusRef.current) {
			const node = buttonRefs.current[themeToFocusRef.current];
			if (node) {
				node.focus();
			}
			themeToFocusRef.current = null;
		}
	}, [currentSelectedTheme]);
	const groupClassName = className ? `${styles.themeButtonGroup} ${className}` : styles.themeButtonGroup;
	return (
		<div
			className={groupClassName}
			role="radiogroup"
			aria-label={ariaLabel ?? i18n._(THEME_DESCRIPTOR)}
			data-flx="user.appearance-tab.theme.theme-tab-content.theme-button-group"
		>
			{themeOptions.map((option) => (
				<Tooltip
					key={option.type}
					text={option.tooltip}
					position="top"
					delay={200}
					data-flx="user.appearance-tab.theme.theme-tab-content.tooltip"
				>
					<div data-flx="user.appearance-tab.theme.theme-tab-content.div">
						<ThemeButton
							ref={(el) => {
								buttonRefs.current[option.type] = el;
							}}
							themeType={option.type}
							currentTheme={currentSelectedTheme}
							label={option.label}
							backgroundColor={option.backgroundColor}
							isLight={option.isLight}
							icon={option.icon ?? undefined}
							onClick={handleThemeChange}
							onKeyDown={handleKeyDown}
							data-flx="user.appearance-tab.theme.theme-tab-content.theme-button.theme-change"
						/>
					</div>
				</Tooltip>
			))}
		</div>
	);
});
