// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility, {DMMessagePreviewMode} from '@app/features/accessibility/state/Accessibility';
import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR, Slider} from '@app/features/ui/components/Slider';
import {canResetSliderValue, SliderResetIconButton} from '@app/features/ui/components/slider/SliderResetIconButton';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {formatRoundedPercentage} from '@app/features/ui/utils/PercentageFormatting';
import styles from '@app/features/user/components/modals/tabs/accessibility_tab/VisualTab.module.css';
import {DIM_STRIKETHROUGH_TEXT_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const ALL_MESSAGES_DESCRIPTOR = msg({
	message: 'All messages',
	comment: 'Short label in the visual tab. Keep it concise.',
});
const SHOW_MESSAGE_PREVIEWS_FOR_ALL_DM_CONVERSATIONS_DESCRIPTOR = msg({
	message: 'Show message previews for all DM conversations',
	comment: 'Label in the visual tab.',
});
const UNREAD_DMS_ONLY_DESCRIPTOR = msg({
	message: 'Unread DMs only',
	comment: 'Short label in the visual tab. Keep it concise.',
});
const ONLY_SHOW_MESSAGE_PREVIEWS_FOR_DMS_WITH_UNREAD_DESCRIPTOR = msg({
	message: 'Only show message previews for DMs with unread messages',
	comment: 'Label in the visual tab.',
});
const NONE_DESCRIPTOR = msg({
	message: 'None',
	comment: 'Short label in the visual tab. Keep it concise.',
});
const DON_T_SHOW_MESSAGE_PREVIEWS_IN_THE_DM_DESCRIPTOR = msg({
	message: "Don't show message previews in the DM list",
	comment: 'Label in the visual tab.',
});
const ALWAYS_UNDERLINE_LINKS_DESCRIPTOR = msg({
	message: 'Always underline links',
	comment: 'Short label in the visual tab. Keep it concise.',
});
const DM_MESSAGE_PREVIEW_MODE_DESCRIPTOR = msg({
	message: 'DM message preview mode',
	comment: 'Label in the visual tab.',
});
const dmMessagePreviewOptions = (i18n: I18n): ReadonlyArray<RadioOption<DMMessagePreviewMode>> => [
	{
		value: DMMessagePreviewMode.ALL,
		name: i18n._(ALL_MESSAGES_DESCRIPTOR),
		desc: i18n._(SHOW_MESSAGE_PREVIEWS_FOR_ALL_DM_CONVERSATIONS_DESCRIPTOR),
	},
	{
		value: DMMessagePreviewMode.UNREAD_ONLY,
		name: i18n._(UNREAD_DMS_ONLY_DESCRIPTOR),
		desc: i18n._(ONLY_SHOW_MESSAGE_PREVIEWS_FOR_DMS_WITH_UNREAD_DESCRIPTOR),
	},
	{
		value: DMMessagePreviewMode.NONE,
		name: i18n._(NONE_DESCRIPTOR),
		desc: i18n._(DON_T_SHOW_MESSAGE_PREVIEWS_IN_THE_DM_DESCRIPTOR),
	},
];
export const VisualTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const formatPercentage = useCallback(
		(value: number) => formatRoundedPercentage(i18n.locale, value),
		[i18n, i18n.locale],
	);
	const saturationFactor = Accessibility.saturationFactor;
	const alwaysUnderlineLinks = Accessibility.alwaysUnderlineLinks;
	const dimStrikethroughText = Accessibility.dimStrikethroughText;
	const saturationValue = saturationFactor * 100;
	return (
		<>
			<div
				className={styles.sliderSection}
				data-flx="user.accessibility-tab.visual-tab.visual-tab-content.slider-section"
			>
				<div
					className={styles.sliderHeader}
					data-flx="user.accessibility-tab.visual-tab.visual-tab-content.slider-header"
				>
					<div
						className={styles.sliderTitleRow}
						data-flx="user.accessibility-tab.visual-tab.visual-tab-content.slider-title-row"
					>
						<label
							htmlFor="saturation"
							className={styles.sliderLabel}
							data-flx="user.accessibility-tab.visual-tab.visual-tab-content.slider-label"
						>
							<Trans>Saturation</Trans>
						</label>
						<SliderResetIconButton
							canReset={canResetSliderValue(saturationValue, 100)}
							onReset={() => AccessibilityCommands.update({saturationFactor: 1})}
							ariaLabel={i18n._(RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR)}
							dataFlx="user.accessibility-tab.visual-tab.visual-tab-content.reset-button.saturation"
							data-flx="user.accessibility-tab.visual-tab.visual-tab-content.slider-reset-icon-button"
						/>
					</div>
				</div>
				<Slider
					defaultValue={saturationValue}
					factoryDefaultValue={100}
					minValue={0}
					maxValue={100}
					step={1}
					markers={[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]}
					stickToMarkers={false}
					onMarkerRender={formatPercentage}
					onValueRender={formatPercentage}
					onValueChange={(value) => AccessibilityCommands.update({saturationFactor: value / 100})}
					data-flx="user.accessibility-tab.visual-tab.visual-tab-content.slider"
				/>
			</div>
			<Switch
				label={i18n._(ALWAYS_UNDERLINE_LINKS_DESCRIPTOR)}
				value={alwaysUnderlineLinks}
				onChange={(value) => AccessibilityCommands.update({alwaysUnderlineLinks: value})}
				data-flx="user.accessibility-tab.visual-tab.visual-tab-content.switch.update"
			/>
			<Switch
				label={i18n._(DIM_STRIKETHROUGH_TEXT_DESCRIPTOR)}
				value={dimStrikethroughText}
				onChange={(value) => AccessibilityCommands.update({dimStrikethroughText: value})}
				data-flx="user.accessibility-tab.visual-tab.visual-tab-content.switch.update--2"
			/>
			<SettingsTabSection
				title={<Trans>DM message previews</Trans>}
				data-flx="user.accessibility-tab.visual-tab.visual-tab-content.settings-tab-section--2"
			>
				<RadioGroup
					options={dmMessagePreviewOptions(i18n)}
					value={Accessibility.dmMessagePreviewMode}
					onChange={(value) => AccessibilityCommands.update({dmMessagePreviewMode: value})}
					aria-label={i18n._(DM_MESSAGE_PREVIEW_MODE_DESCRIPTOR)}
					data-flx="user.accessibility-tab.visual-tab.visual-tab-content.radio-group.update"
				/>
			</SettingsTabSection>
		</>
	);
});
