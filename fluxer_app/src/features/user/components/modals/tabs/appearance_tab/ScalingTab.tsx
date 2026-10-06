// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility, {
	ZOOM_LEVEL_MARKERS,
	ZOOM_LEVEL_MAX,
	ZOOM_LEVEL_MIN,
} from '@app/features/accessibility/state/Accessibility';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {APP_ZOOM_LEVEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import Keybind from '@app/features/input/state/InputKeybind';
import {formatKeyCombo} from '@app/features/input/utils/KeybindUtils';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Slider} from '@app/features/ui/components/Slider';
import {canResetSliderValue, SliderResetIconButton} from '@app/features/ui/components/slider/SliderResetIconButton';
import {shouldWarnAboutFirefoxWebZoomShortcuts} from '@app/features/ui/utils/AppZoomKeybindUtils';
import {formatRoundedPercentage, roundPercentage} from '@app/features/ui/utils/PercentageFormatting';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import {msg, ph} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo} from 'react';

const CHAT_FONT_SIZE_DESCRIPTOR = msg({
	message: 'Chat font size',
	comment: 'Accessible label for the chat font size select.',
});
const RESET_FONT_SIZE_DESCRIPTOR = msg({
	message: 'Reset font size',
	comment: 'Button for the chat font size setting. Restores chat font size to the default.',
});
const PERCENT_DESCRIPTOR = msg({
	message: '{zoomPercent} percent',
	comment: 'Accessible value for the app zoom level slider.',
});
const RESET_ZOOM_DESCRIPTOR = msg({
	message: 'Reset zoom',
	comment: 'Label for controls that reset app zoom to 100%, including the app zoom slider button.',
});
const ADJUST_THE_OVERALL_ZOOM_LEVEL_OF_THE_APP_DESCRIPTOR = msg({
	message:
		'Adjust the overall zoom level of the app. Firefox keeps the standard browser zoom shortcuts, so use the slider here for {productName} zoom.',
	comment: 'Description for the app zoom setting in Firefox on the web. productName is the app name.',
});
const ADJUST_THE_OVERALL_ZOOM_LEVEL_OF_THE_APP_2_DESCRIPTOR = msg({
	message: 'Adjust the overall zoom level of the app. Use {zoomIn} / {zoomOut} to adjust quickly.',
	comment: 'Description for the app zoom setting. The placeholders are keyboard shortcuts.',
});
const DEFAULT_FONT_SIZE = 16;
const DEFAULT_ZOOM_PERCENT = 100;
const FONT_SIZE_OPTIONS = [12, 14, 15, 16, 18, 20, 24] as const;

const getNearestFontSize = (value: number): number => {
	let nearest: number = FONT_SIZE_OPTIONS[0];
	let nearestDistance = Number.POSITIVE_INFINITY;
	for (const option of FONT_SIZE_OPTIONS) {
		const distance = Math.abs(option - value);
		if (distance < nearestDistance) {
			nearest = option;
			nearestDistance = distance;
		}
	}
	return nearest;
};

const resolveFontSizeInput = (
	inputValue: string,
	options: ReadonlyArray<ComboboxOption<number>>,
): number | undefined => {
	const numericMatch = inputValue.trim().match(/([0-9]+(?:\.[0-9]+)?)/);
	if (!numericMatch) return undefined;
	const parsedValue = Number(numericMatch[1]);
	if (!Number.isFinite(parsedValue)) return undefined;
	return options.reduce((nearest, option) =>
		Math.abs(option.value - parsedValue) < Math.abs(nearest.value - parsedValue) ? option : nearest,
	).value;
};

export function canResetFontSize(): boolean {
	return canResetSliderValue(Accessibility.fontSize, DEFAULT_FONT_SIZE);
}

export const FontSizeResetAction: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<SliderResetIconButton
			canReset={true}
			onReset={() => AccessibilityCommands.update({fontSize: DEFAULT_FONT_SIZE})}
			ariaLabel={i18n._(RESET_FONT_SIZE_DESCRIPTOR)}
			dataFlx="user.appearance-tab.scaling-tab.reset-button.font-size"
			data-flx="user.appearance-tab.scaling-tab.font-size-reset-action.slider-reset-icon-button"
		/>
	);
});

export const FontSizeTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const fontSize = Accessibility.fontSize;
	const options: ReadonlyArray<ComboboxOption<number>> = useMemo(
		() => FONT_SIZE_OPTIONS.map((value) => ({value, label: `${getCachedNumberFormat(i18n.locale).format(value)}px`})),
		[i18n.locale],
	);
	const selectedFontSize = getNearestFontSize(fontSize);
	useEffect(() => {
		if (fontSize !== selectedFontSize) AccessibilityCommands.update({fontSize: selectedFontSize});
	}, [fontSize, selectedFontSize]);
	return (
		<CompactComboboxRow<number>
			label={i18n._(CHAT_FONT_SIZE_DESCRIPTOR)}
			action={
				canResetFontSize() ? (
					<FontSizeResetAction data-flx="user.appearance-tab.scaling-tab.font-size-tab-content.font-size-reset-action" />
				) : null
			}
			value={selectedFontSize}
			options={options}
			onChange={(value) => AccessibilityCommands.update({fontSize: value})}
			autoSelectValueFromInput={resolveFontSizeInput}
			controlWidth="small"
			menuMinWidth={128}
			aria-label={i18n._(CHAT_FONT_SIZE_DESCRIPTOR)}
			dataFlx="user.appearance-tab.scaling-tab.font-size-tab-content.select"
			data-flx="user.appearance-tab.scaling-tab.font-size-tab-content.compact-select-row.update"
		/>
	);
});

export function canResetAppZoomLevel(): boolean {
	const zoomPercent = roundPercentage(Accessibility.zoomLevel * 100);
	return canResetSliderValue(zoomPercent, DEFAULT_ZOOM_PERCENT);
}

export const AppZoomLevelResetAction: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<SliderResetIconButton
			canReset={true}
			onReset={() => AccessibilityCommands.update({zoomLevel: 1})}
			ariaLabel={i18n._(RESET_ZOOM_DESCRIPTOR)}
			dataFlx="user.appearance-tab.scaling-tab.reset-button.app-zoom-level"
			data-flx="user.appearance-tab.scaling-tab.app-zoom-level-reset-action.slider-reset-icon-button"
		/>
	);
});

export const AppZoomLevelTabContent: React.FC = observer(() => {
	const zoomLevel = Accessibility.zoomLevel;
	const zoomPercent = roundPercentage(zoomLevel * 100);
	const markers = ZOOM_LEVEL_MARKERS.map((step) => roundPercentage(step * 100));
	const {i18n} = useLingui();
	const formatPercentage = useCallback(
		(value: number) => formatRoundedPercentage(i18n.locale, value),
		[i18n, i18n.locale],
	);
	return (
		<Slider
			defaultValue={zoomPercent}
			factoryDefaultValue={DEFAULT_ZOOM_PERCENT}
			minValue={Math.round(ZOOM_LEVEL_MIN * 100)}
			maxValue={Math.round(ZOOM_LEVEL_MAX * 100)}
			ariaLabel={i18n._(APP_ZOOM_LEVEL_DESCRIPTOR)}
			ariaValueText={i18n._(PERCENT_DESCRIPTOR, {zoomPercent: getCachedNumberFormat(i18n.locale).format(zoomPercent)})}
			step={1}
			markers={markers}
			stickToMarkers={false}
			asValueChanges={() => {}}
			onValueChange={(value) => AccessibilityCommands.update({zoomLevel: value / 100})}
			onMarkerRender={formatPercentage}
			onValueRender={(value) => (
				<Trans>{ph({zoomPercent: getCachedNumberFormat(i18n.locale).format(roundPercentage(value))})}%</Trans>
			)}
			data-flx="user.appearance-tab.scaling-tab.app-zoom-level-tab-content.slider"
		/>
	);
});

export function useAppZoomLevelDescription(): string {
	const {i18n} = useLingui();
	return useMemo(() => {
		if (shouldWarnAboutFirefoxWebZoomShortcuts()) {
			return i18n._(ADJUST_THE_OVERALL_ZOOM_LEVEL_OF_THE_APP_DESCRIPTOR, {productName: PRODUCT_NAME});
		}
		const zoomIn = formatKeyCombo(i18n, Keybind.getByAction('system_zoom_in').combo);
		const zoomOut = formatKeyCombo(i18n, Keybind.getByAction('system_zoom_out').combo);
		return i18n._(ADJUST_THE_OVERALL_ZOOM_LEVEL_OF_THE_APP_2_DESCRIPTOR, {zoomIn, zoomOut});
	}, [i18n.locale]);
}
