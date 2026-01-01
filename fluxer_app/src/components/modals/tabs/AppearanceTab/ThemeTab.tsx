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

import {useLingui} from '@lingui/react/macro';
import {ArrowsCounterClockwiseIcon, CheckIcon, ShareNetworkIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {ThemeTypes} from '~/Constants';
import {ColorPickerField} from '~/components/form/ColorPickerField';
import {Input, Textarea} from '~/components/form/Input';
import {Switch} from '~/components/form/Switch';
import {ShareThemeModal} from '~/components/modals/ShareThemeModal';
import {Accordion} from '~/components/uikit/Accordion/Accordion';
import {Button} from '~/components/uikit/Button/Button';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Spinner} from '~/components/uikit/Spinner';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import AccessibilityStore from '~/stores/AccessibilityStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import styles from './ThemeTab.module.css';

interface ThemeButtonProps {
	themeType: string;
	currentTheme: string;
	label: string;
	backgroundColor: string;
	onKeyDown: (event: React.KeyboardEvent, themeType: string) => void;
	onClick: (themeType: string) => void;
	icon?: React.ReactElement<Record<string, unknown>>;
}

const ThemeButton = observer(
	React.forwardRef<HTMLButtonElement, ThemeButtonProps>(
		({themeType, currentTheme, label, backgroundColor, onKeyDown, onClick, icon}, ref) => {
			const isSelected = currentTheme === themeType;

			const getButtonClassName = () => {
				const classes = [styles.themeButton];
				if (isSelected) {
					classes.push(styles.themeButtonSelected);
				} else if (backgroundColor === 'hsl(210, 20%, 98%)') {
					classes.push(styles.themeButtonLight);
				} else {
					classes.push(styles.themeButtonDark);
				}
				return clsx(classes);
			};

			return (
				<FocusRing offset={-2}>
					<button
						ref={ref}
						type="button"
						onClick={() => onClick(themeType)}
						onKeyDown={(e) => onKeyDown(e, themeType)}
						className={getButtonClassName()}
						style={{backgroundColor}}
						role="radio"
						aria-checked={isSelected}
						aria-label={label}
						tabIndex={isSelected ? 0 : -1}
					>
						{icon && (
							<div className={styles.themeButtonIcon} aria-hidden="true">
								{React.cloneElement(icon, {
									size: 24,
									weight: 'bold',
									style: {color: backgroundColor === 'hsl(220, 13%, 5%)' ? '#ffffff' : '#000000'},
								})}
							</div>
						)}
						{isSelected && (
							<div className={styles.themeButtonCheckmark} aria-hidden="true">
								<CheckIcon weight="bold" className={styles.themeButtonCheckmarkIcon} size={12} />
							</div>
						)}
					</button>
				</FocusRing>
			);
		},
	),
);

const THEME_COLOR_VARIABLES: ReadonlyArray<string> = [
	'--background-primary',
	'--background-secondary',
	'--background-secondary-alt',
	'--background-tertiary',
	'--background-textarea',
	'--background-header-primary',
	'--background-header-primary-hover',
	'--background-header-secondary',
	'--background-modifier-hover',
	'--guild-list-foreground',
	'--background-modifier-selected',
	'--background-modifier-accent',
	'--background-modifier-accent-focus',
	'--brand-primary',
	'--brand-secondary',
	'--brand-primary-light',
	'--brand-primary-fill',
	'--status-online',
	'--status-idle',
	'--status-dnd',
	'--status-offline',
	'--status-danger',
	'--text-primary',
	'--text-secondary',
	'--text-tertiary',
	'--text-primary-muted',
	'--text-chat',
	'--text-chat-muted',
	'--text-link',
	'--text-on-brand-primary',
	'--text-tertiary-muted',
	'--text-tertiary-secondary',
	'--border-color',
	'--border-color-hover',
	'--border-color-focus',
	'--accent-primary',
	'--accent-success',
	'--accent-warning',
	'--accent-danger',
	'--accent-info',
	'--accent-purple',
	'--alert-note-color',
	'--alert-tip-color',
	'--alert-important-color',
	'--alert-warning-color',
	'--alert-caution-color',
	'--markup-mention-text',
	'--markup-mention-fill',
	'--markup-interactive-hover-text',
	'--markup-interactive-hover-fill',
	'--button-primary-fill',
	'--button-primary-active-fill',
	'--button-primary-text',
	'--button-secondary-fill',
	'--button-secondary-active-fill',
	'--button-secondary-text',
	'--button-secondary-active-text',
	'--button-danger-fill',
	'--button-danger-active-fill',
	'--button-danger-text',
	'--button-danger-outline-border',
	'--button-danger-outline-text',
	'--button-danger-outline-active-fill',
	'--button-danger-outline-active-border',
	'--button-ghost-text',
	'--button-inverted-fill',
	'--button-inverted-text',
	'--button-outline-border',
	'--button-outline-text',
	'--button-outline-active-fill',
	'--button-outline-active-border',
	'--bg-primary',
	'--bg-secondary',
	'--bg-tertiary',
	'--bg-hover',
	'--bg-active',
	'--bg-code',
	'--bg-code-block',
	'--bg-blockquote',
	'--bg-table-header',
	'--bg-table-row-odd',
	'--bg-table-row-even',
];

const THEME_FONT_VARIABLES: ReadonlyArray<string> = ['--font-sans', '--font-mono'];

function escapeRegExp(value: string): string {
	return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function extractThemeVariableOverrides(css: string): Record<string, string> {
	const overrides: Record<string, string> = {};
	const variablePattern = /--([a-zA-Z0-9_-]+)\s*:\s*([^;]+);/g;
	let match: RegExpExecArray | null;

	while ((match = variablePattern.exec(css)) !== null) {
		const variableName = `--${match[1] as string}`;
		const value = match[2] as string;
		overrides[variableName] = value.trim();
	}

	return overrides;
}

function updateCssForVariable(css: string, variableName: string, newValue: string | null): string {
	const variableNamePattern = escapeRegExp(variableName);
	const propertyPattern = new RegExp(`(--${variableNamePattern.replace(/^--/, '')}\\s*:[^;]*;)`);

	if (newValue === null) {
		return css.replace(propertyPattern, '');
	}

	if (propertyPattern.test(css)) {
		return css.replace(propertyPattern, `${variableName}: ${newValue};`);
	}

	const trimmedCss = css.trim();
	const prefix = trimmedCss.length > 0 && !trimmedCss.endsWith('\n') ? '\n' : '';
	return `${trimmedCss}${prefix}:root { ${variableName}: ${newValue}; }\n`;
}

function clampByte(value: number): number {
	return Math.max(0, Math.min(255, Math.round(value)));
}

function numberToHex(value: number): string {
	return `#${(value >>> 0).toString(16).padStart(6, '0').slice(-6)}`.toUpperCase();
}

function cssColorStringToNumber(color: string): number | null {
	const canvas = document.createElement('canvas');
	const context = canvas.getContext('2d');

	if (!context) return null;

	try {
		context.fillStyle = '#000';
		context.fillStyle = color;
		const parsed = String(context.fillStyle);

		const match = /^rgba?\((\d+),\s*(\d+),\s*(\d+)/i.exec(parsed);
		if (match) {
			const red = parseInt(match[1] ?? '0', 10);
			const green = parseInt(match[2] ?? '0', 10);
			const blue = parseInt(match[3] ?? '0', 10);
			const hex = `#${clampByte(red).toString(16).padStart(2, '0')}${clampByte(green)
				.toString(16)
				.padStart(2, '0')}${clampByte(blue).toString(16).padStart(2, '0')}`.toUpperCase();

			return Number.parseInt(hex.slice(1), 16) >>> 0;
		}

		if (/^#[0-9A-Fa-f]{6}$/.test(parsed)) {
			return Number.parseInt(parsed.slice(1), 16) >>> 0;
		}
	} catch {
		return null;
	}

	return null;
}

export const ThemeTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const {theme} = UserSettingsStore;
	const syncThemeAcrossDevices = AccessibilityStore.syncThemeAcrossDevices;
	const localThemeOverride = AccessibilityStore.localThemeOverride;
	const customThemeCss = AccessibilityStore.customThemeCss ?? '';

	const currentSelectedTheme = syncThemeAcrossDevices ? theme : localThemeOverride || theme;

	const [systemPrefersDark, setSystemPrefersDark] = React.useState(() => {
		return window.matchMedia?.('(prefers-color-scheme: dark)').matches;
	});
	const buttonRefs = React.useRef<Record<string, HTMLButtonElement | null>>({});
	const themeToFocusRef = React.useRef<string | null>(null);

	React.useEffect(() => {
		if (!window.matchMedia) return;
		const mediaQuery = window.matchMedia('(prefers-color-scheme: dark)');
		const handleChange = (event: MediaQueryListEvent) => {
			setSystemPrefersDark(event.matches);
		};
		mediaQuery.addEventListener('change', handleChange);
		return () => mediaQuery.removeEventListener('change', handleChange);
	}, []);

	const [defaultVariableValues, setDefaultVariableValues] = React.useState<Record<string, string> | null>(null);
	const beginThemeHydration = React.useCallback(() => {
		setDefaultVariableValues(null);
	}, []);

	const handleThemeChange = React.useCallback(
		(newTheme: string) => {
			if (newTheme === currentSelectedTheme) return;
			themeToFocusRef.current = newTheme;
			beginThemeHydration();
			if (newTheme === ThemeTypes.SYSTEM) {
				AccessibilityActionCreators.update({syncThemeAcrossDevices: false, localThemeOverride: newTheme});
			} else if (syncThemeAcrossDevices) {
				UserSettingsActionCreators.update({theme: newTheme});
			} else {
				AccessibilityActionCreators.update({localThemeOverride: newTheme});
			}
		},
		[currentSelectedTheme, beginThemeHydration, syncThemeAcrossDevices],
	);

	const themeOptions = [
		{
			type: ThemeTypes.DARK,
			label: t`Dark Theme`,
			backgroundColor: 'hsl(220, calc(13% * var(--saturation-factor)), 13.22%)',
			icon: null,
			tooltip: t`Use dark theme`,
		},
		{
			type: ThemeTypes.COAL,
			label: t`Coal Theme`,
			backgroundColor: 'hsl(220, 13%, 2%)',
			icon: null,
			tooltip: t`Use coal theme (pitch-black surfaces)`,
		},
		{
			type: ThemeTypes.LIGHT,
			label: t`Light Theme`,
			backgroundColor: 'hsl(210, 20%, 98%)',
			icon: null,
			tooltip: t`Use light theme`,
		},
		{
			type: ThemeTypes.SYSTEM,
			label: t`System Theme`,
			backgroundColor: systemPrefersDark ? 'hsl(220, 13%, 5%)' : 'hsl(210, 20%, 98%)',
			icon: <ArrowsCounterClockwiseIcon size={12} />,
			tooltip: systemPrefersDark
				? t`System: Dark theme (automatically sync with your system's dark/light preference)`
				: t`System: Light theme (automatically sync with your system's dark/light preference)`,
		},
	];

	const handleKeyDown = React.useCallback(
		(event: React.KeyboardEvent, targetTheme: string) => {
			if (event.key === ' ' || event.key === 'Enter') {
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
	const overrides = React.useMemo(() => extractThemeVariableOverrides(customThemeCss), [customThemeCss]);
	const hydrationKey =
		currentSelectedTheme === ThemeTypes.SYSTEM
			? `${currentSelectedTheme}-${systemPrefersDark ? 'dark' : 'light'}`
			: currentSelectedTheme;

	const lastHydrationKey = React.useRef(hydrationKey);

	React.useEffect(() => {
		if (hydrationKey !== lastHydrationKey.current) {
			beginThemeHydration();
			lastHydrationKey.current = hydrationKey;
		}

		const frameId = requestAnimationFrame(() => {
			const computed = getComputedStyle(document.documentElement);
			const nextDefaults: Record<string, string> = {};

			for (const variableName of [...THEME_COLOR_VARIABLES, ...THEME_FONT_VARIABLES]) {
				const raw = computed.getPropertyValue(variableName);
				if (raw && raw.trim().length > 0) {
					nextDefaults[variableName] = raw.trim();
				}
			}

			setDefaultVariableValues(nextDefaults);
		});

		return () => cancelAnimationFrame(frameId);
	}, [hydrationKey, customThemeCss, beginThemeHydration]);

	const handleShareTheme = React.useCallback(() => {
		const css = AccessibilityStore.customThemeCss ?? '';
		if (!css.trim()) {
			ToastActionCreators.error(t`You don't have any custom theme overrides to share yet.`);
			return;
		}

		ModalActionCreators.push(ModalActionCreators.modal(() => <ShareThemeModal themeCss={css} />));
	}, []);

	const handleResetAllOverrides = React.useCallback(() => {
		AccessibilityActionCreators.update({customThemeCss: null});
	}, []);

	React.useEffect(() => {
		if (defaultVariableValues !== null && themeToFocusRef.current) {
			const node = buttonRefs.current[themeToFocusRef.current];
			if (node) {
				node.focus();
			}
			themeToFocusRef.current = null;
		}
	}, [defaultVariableValues]);

	if (defaultVariableValues === null) {
		return (
			<div className={styles.loadingContainer}>
				<Spinner size="large" />
			</div>
		);
	}

	return (
		<>
			<div className={styles.themeButtonGroup} role="radiogroup" aria-labelledby="theme-label">
				{themeOptions.map((option) => (
					<Tooltip key={option.type} text={option.tooltip} position="top" delay={200}>
						<div>
							<ThemeButton
								ref={(el) => {
									buttonRefs.current[option.type] = el;
								}}
								themeType={option.type}
								currentTheme={currentSelectedTheme}
								label={option.label}
								backgroundColor={option.backgroundColor}
								icon={option.icon ?? undefined}
								onClick={handleThemeChange}
								onKeyDown={handleKeyDown}
							/>
						</div>
					</Tooltip>
				))}
			</div>
			<Switch
				label={t`Sync theme across devices`}
				description={
					(syncThemeAcrossDevices ? theme : localThemeOverride) === ThemeTypes.SYSTEM
						? t`System theme automatically disables sync to track your system's preference on this device.`
						: t`When enabled, theme changes will sync to all your devices. When disabled, this device will use its own theme setting.`
				}
				value={syncThemeAcrossDevices}
				disabled={(syncThemeAcrossDevices ? theme : localThemeOverride) === ThemeTypes.SYSTEM}
				onChange={(value) => {
					if (!value) {
						const currentTheme = syncThemeAcrossDevices ? theme : UserSettingsStore.getTheme();
						AccessibilityActionCreators.update({syncThemeAcrossDevices: false, localThemeOverride: currentTheme});
					} else {
						AccessibilityActionCreators.update({syncThemeAcrossDevices: true, localThemeOverride: null});
					}
				}}
			/>

			<Accordion
				id="custom-theme-tokens"
				title={t`Custom theme tokens`}
				description={t`Fine-tune core colors and fonts for this app. Changes here are stored as custom CSS overrides and sync with the editor below.`}
				defaultExpanded={false}
			>
				<div className={styles.colorGrid}>
					{THEME_FONT_VARIABLES.map((variableName) => {
						const overrideValue = overrides[variableName];
						const defaultValue = defaultVariableValues[variableName];
						const currentValue = overrideValue ?? defaultValue ?? '';

						return (
							<Input
								key={variableName}
								label={
									variableName === '--font-sans'
										? t`Body font family`
										: variableName === '--font-mono'
											? t`Monospace font family`
											: variableName
								}
								placeholder={defaultValue || undefined}
								value={currentValue}
								onChange={(event) => {
									const next = event.target.value.trim();
									const updatedCss =
										next.length === 0
											? updateCssForVariable(customThemeCss, variableName, null)
											: updateCssForVariable(customThemeCss, variableName, next);
									AccessibilityActionCreators.update({customThemeCss: updatedCss});
								}}
							/>
						);
					})}
				</div>

				<div className={styles.colorSection}>
					<div className={styles.colorSectionHeading}>{t`Colors`}</div>
					<div className={styles.colorGrid}>
						{THEME_COLOR_VARIABLES.map((variableName) => {
							const overrideCss = overrides[variableName];
							const defaultCss = defaultVariableValues[variableName];

							const defaultNumber = defaultCss !== undefined ? (cssColorStringToNumber(defaultCss) ?? 0) : undefined;
							const valueNumber =
								overrideCss !== undefined ? (cssColorStringToNumber(overrideCss) ?? defaultNumber ?? 0) : 0;

							const label = variableName.replace(/^--/, '').replace(/-/g, ' ');

							return (
								<ColorPickerField
									key={variableName}
									label={label}
									value={valueNumber ?? 0}
									defaultValue={defaultNumber}
									hideHelperText
									onChange={(nextValue) => {
										const updatedCss =
											nextValue === 0
												? updateCssForVariable(customThemeCss, variableName, null)
												: updateCssForVariable(customThemeCss, variableName, numberToHex(nextValue));
										AccessibilityActionCreators.update({customThemeCss: updatedCss});
									}}
								/>
							);
						})}
					</div>
				</div>

				<div className={styles.cssSection}>
					<Textarea
						label={t`Custom CSS overrides`}
						placeholder={t`Write custom CSS here to override any theme tokens. For example:\n:root { --background-primary: #1E1E2F; }`}
						minRows={4}
						maxRows={12}
						value={customThemeCss}
						onChange={(event) => {
							AccessibilityActionCreators.update({customThemeCss: event.target.value});
						}}
					/>
					<div className={styles.buttonGroup}>
						<Button variant="secondary" fitContent onClick={handleResetAllOverrides}>
							{t`Reset all overrides to theme default`}
						</Button>
						<Button variant="primary" fitContent leftIcon={<ShareNetworkIcon size={18} />} onClick={handleShareTheme}>
							{t`Share this theme`}
						</Button>
					</div>
				</div>
			</Accordion>
		</>
	);
});
