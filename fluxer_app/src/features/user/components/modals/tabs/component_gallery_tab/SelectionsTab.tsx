// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {ENABLE_NOTIFICATIONS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {Slider} from '@app/features/ui/components/Slider';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {SubsectionTitle} from '@app/features/user/components/modals/tabs/component_gallery_tab/ComponentGalleryTabSubsectionTitle';
import styles from '@app/features/user/components/modals/tabs/component_gallery_tab/SelectionsTab.module.css';
import {DARK_MODE_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const CHOOSE_AN_OPTION_DESCRIPTOR = msg({
	message: 'Choose an option',
	comment: 'Button or menu action label in the selections tab. Keep it concise.',
});
const OPTION_ONE_DESCRIPTOR = msg({
	message: 'Option one',
	comment: 'Short label in the selections tab. Keep it concise.',
});
const OPTION_TWO_DESCRIPTOR = msg({
	message: 'Option two',
	comment: 'Short label in the selections tab. Keep it concise.',
});
const OPTION_THREE_DESCRIPTOR = msg({
	message: 'Option three',
	comment: 'Short label in the selections tab. Keep it concise.',
});
const OPTION_FOUR_DESCRIPTOR = msg({
	message: 'Option four',
	comment: 'Short label in the selections tab. Keep it concise.',
});
const SIZE_SELECTION_DESCRIPTOR = msg({
	message: 'Size selection',
	comment: 'Short label in the selections tab. Keep it concise.',
});
const SMALL_DESCRIPTOR = msg({
	message: 'Small',
	comment: 'Short label in the selections tab. Keep it concise.',
});
const MEDIUM_DESCRIPTOR = msg({
	message: 'Medium',
	comment: 'Short label in the selections tab. Keep it concise.',
});
const LARGE_DESCRIPTOR = msg({
	message: 'Large',
	comment: 'Short label in the selections tab. Keep it concise.',
});
const EXTRA_LARGE_DESCRIPTOR = msg({
	message: 'Extra large',
	comment: 'Short label in the selections tab. Keep it concise.',
});
const DISABLED_SELECT_DESCRIPTOR = msg({
	message: 'Disabled select',
	comment: 'Button or menu action label in the selections tab. Keep it concise.',
});
const THIS_IS_DISABLED_DESCRIPTOR = msg({
	message: 'This is disabled',
	comment: 'Short label in the selections tab. Keep it concise.',
});
const RECEIVE_NOTIFICATIONS_WHEN_SOMEONE_MENTIONS_YOU_DESCRIPTOR = msg({
	message: 'Receive notifications when someone mentions you',
	comment: 'Label in the selections tab.',
});

const USE_DARK_THEME_ACROSS_THE_APPLICATION_DESCRIPTOR = msg({
	message: 'Use dark theme across the application',
	comment: 'Label in the selections tab.',
});
const DISABLED_SWITCH_DESCRIPTOR = msg({
	message: 'Disabled switch',
	comment: 'Button or menu action label in the selections tab. Keep it concise.',
});
const THIS_SWITCH_IS_DISABLED_AND_CANNOT_BE_TOGGLED_DESCRIPTOR = msg({
	message: 'This switch is disabled and cannot be toggled',
	comment: 'Error message in the selections tab.',
});
const DISABLED_CHECKED_DESCRIPTOR = msg({
	message: 'Disabled (checked)',
	comment: 'Button or menu action label in the selections tab. Keep it concise.',
});
const THIS_SWITCH_IS_DISABLED_IN_THE_CHECKED_STATE_DESCRIPTOR = msg({
	message: 'This switch is disabled in the checked state',
	comment: 'Label in the selections tab.',
});
const SELECT_AN_OPTION_FROM_THE_RADIO_GROUP_DESCRIPTOR = msg({
	message: 'Select an option from the radio group',
	comment: 'Button or menu action label in the selections tab. Keep it concise.',
});

interface SelectionsTabProps {
	selectValue: string;
	setSelectValue: (value: string) => void;
	selectValue2: string;
	setSelectValue2: (value: string) => void;
	primarySwitch: boolean;
	setPrimarySwitch: (value: boolean) => void;
	dangerSwitch: boolean;
	setDangerSwitch: (value: boolean) => void;
	checkboxChecked: boolean;
	setCheckboxChecked: (value: boolean) => void;
	checkboxChecked2: boolean;
	setCheckboxChecked2: (value: boolean) => void;
	radioGroupValue: string;
	setRadioGroupValue: (value: string) => void;
	radioOptions: Array<RadioOption<string>>;
	sliderValue: number;
	setSliderValue: (value: number) => void;
	sliderValue2: number;
	setSliderValue2: (value: number) => void;
	sliderValue3: number;
	setSliderValue3: (value: number) => void;
	sliderValue4: number;
	setSliderValue4: (value: number) => void;
	sliderValue5: number;
}

export const SelectionsTab: React.FC<SelectionsTabProps> = observer(
	({
		selectValue,
		setSelectValue,
		selectValue2,
		setSelectValue2,
		primarySwitch,
		setPrimarySwitch,
		dangerSwitch,
		setDangerSwitch,
		checkboxChecked,
		setCheckboxChecked,
		checkboxChecked2,
		setCheckboxChecked2,
		radioGroupValue,
		setRadioGroupValue,
		radioOptions,
		sliderValue,
		setSliderValue,
		sliderValue2,
		setSliderValue2,
		sliderValue3,
		setSliderValue3,
		sliderValue4,
		setSliderValue4,
		sliderValue5,
	}) => {
		const {i18n} = useLingui();
		return (
			<SettingsTabContainer data-flx="user.component-gallery-tab.selections-tab.settings-tab-container">
				<SettingsTabContent data-flx="user.component-gallery-tab.selections-tab.settings-tab-content">
					<SettingsTabSection
						title={<Trans>Combobox</Trans>}
						description={<Trans>Open the menu and choose an option.</Trans>}
						data-flx="user.component-gallery-tab.selections-tab.settings-tab-section"
					>
						<div className={styles.gridDouble} data-flx="user.component-gallery-tab.selections-tab.grid-double">
							<Combobox<string>
								label={i18n._(CHOOSE_AN_OPTION_DESCRIPTOR)}
								value={selectValue}
								onChange={(value) => {
									setSelectValue(value);
								}}
								options={[
									{value: 'opt1', label: i18n._(OPTION_ONE_DESCRIPTOR)},
									{value: 'opt2', label: i18n._(OPTION_TWO_DESCRIPTOR)},
									{value: 'opt3', label: i18n._(OPTION_THREE_DESCRIPTOR)},
									{value: 'opt4', label: i18n._(OPTION_FOUR_DESCRIPTOR)},
								]}
								data-flx="user.component-gallery-tab.selections-tab.select"
							/>
							<Combobox<string>
								label={i18n._(SIZE_SELECTION_DESCRIPTOR)}
								value={selectValue2}
								onChange={(value) => {
									setSelectValue2(value);
								}}
								options={[
									{value: 'size-sm', label: i18n._(SMALL_DESCRIPTOR)},
									{value: 'size-md', label: i18n._(MEDIUM_DESCRIPTOR)},
									{value: 'size-lg', label: i18n._(LARGE_DESCRIPTOR)},
									{value: 'size-xl', label: i18n._(EXTRA_LARGE_DESCRIPTOR)},
								]}
								data-flx="user.component-gallery-tab.selections-tab.select--2"
							/>
						</div>
						<div className={styles.gridSingle} data-flx="user.component-gallery-tab.selections-tab.grid-single">
							<Combobox
								label={i18n._(DISABLED_SELECT_DESCRIPTOR)}
								value="disabled-opt"
								onChange={() => {}}
								disabled
								options={[{value: 'disabled-opt', label: i18n._(THIS_IS_DISABLED_DESCRIPTOR)}]}
								data-flx="user.component-gallery-tab.selections-tab.select--3"
							/>
						</div>
					</SettingsTabSection>
					<SettingsTabSection
						title={<Trans>Switches</Trans>}
						description={<Trans>Toggle switches on and off to see state changes.</Trans>}
						data-flx="user.component-gallery-tab.selections-tab.settings-tab-section--2"
					>
						<div className={styles.contentList} data-flx="user.component-gallery-tab.selections-tab.content-list">
							<Switch
								label={i18n._(ENABLE_NOTIFICATIONS_DESCRIPTOR)}
								description={i18n._(RECEIVE_NOTIFICATIONS_WHEN_SOMEONE_MENTIONS_YOU_DESCRIPTOR)}
								value={primarySwitch}
								onChange={(value) => {
									setPrimarySwitch(value);
								}}
								data-flx="user.component-gallery-tab.selections-tab.switch"
							/>
							<Switch
								label={i18n._(DARK_MODE_DESCRIPTOR)}
								description={i18n._(USE_DARK_THEME_ACROSS_THE_APPLICATION_DESCRIPTOR)}
								value={dangerSwitch}
								onChange={(value) => {
									setDangerSwitch(value);
								}}
								data-flx="user.component-gallery-tab.selections-tab.switch--2"
							/>
							<Switch
								label={i18n._(DISABLED_SWITCH_DESCRIPTOR)}
								description={i18n._(THIS_SWITCH_IS_DISABLED_AND_CANNOT_BE_TOGGLED_DESCRIPTOR)}
								value={false}
								onChange={() => {}}
								disabled
								data-flx="user.component-gallery-tab.selections-tab.switch--3"
							/>
							<Switch
								label={i18n._(DISABLED_CHECKED_DESCRIPTOR)}
								description={i18n._(THIS_SWITCH_IS_DISABLED_IN_THE_CHECKED_STATE_DESCRIPTOR)}
								value={true}
								onChange={() => {}}
								disabled
								data-flx="user.component-gallery-tab.selections-tab.switch--4"
							/>
						</div>
					</SettingsTabSection>
					<SettingsTabSection
						title={<Trans>Checkboxes</Trans>}
						description={<Trans>Click to check and uncheck. Available in square and round styles.</Trans>}
						data-flx="user.component-gallery-tab.selections-tab.settings-tab-section--3"
					>
						<SubsectionTitle data-flx="user.component-gallery-tab.selections-tab.subsection-title">
							<Trans>Square checkboxes</Trans>
						</SubsectionTitle>
						<div className={styles.contentList} data-flx="user.component-gallery-tab.selections-tab.content-list--2">
							<Checkbox
								checked={checkboxChecked}
								onChange={(checked) => {
									setCheckboxChecked(checked);
								}}
								data-flx="user.component-gallery-tab.selections-tab.checkbox"
							>
								<Trans>Interactive checkbox</Trans>
							</Checkbox>
							<Checkbox
								checked={checkboxChecked2}
								onChange={(checked) => {
									setCheckboxChecked2(checked);
								}}
								data-flx="user.component-gallery-tab.selections-tab.checkbox--2"
							>
								<Trans>Another checkbox</Trans>
							</Checkbox>
							<Checkbox checked={true} disabled data-flx="user.component-gallery-tab.selections-tab.checkbox--3">
								<Trans>Disabled (checked)</Trans>
							</Checkbox>
							<Checkbox checked={false} disabled data-flx="user.component-gallery-tab.selections-tab.checkbox--4">
								<Trans>Disabled (unchecked)</Trans>
							</Checkbox>
						</div>
						<SubsectionTitle data-flx="user.component-gallery-tab.selections-tab.subsection-title--2">
							<Trans>Round checkboxes</Trans>
						</SubsectionTitle>
						<div className={styles.contentList} data-flx="user.component-gallery-tab.selections-tab.content-list--3">
							<Checkbox
								checked={checkboxChecked}
								onChange={(checked) => setCheckboxChecked(checked)}
								type="round"
								data-flx="user.component-gallery-tab.selections-tab.checkbox.round"
							>
								<Trans>Round style checkbox</Trans>
							</Checkbox>
							<Checkbox
								checked={checkboxChecked2}
								onChange={(checked) => setCheckboxChecked2(checked)}
								type="round"
								data-flx="user.component-gallery-tab.selections-tab.checkbox.round--2"
							>
								<Trans>Another round checkbox</Trans>
							</Checkbox>
							<Checkbox
								checked={true}
								disabled
								type="round"
								data-flx="user.component-gallery-tab.selections-tab.checkbox.round--3"
							>
								<Trans>Disabled round (checked)</Trans>
							</Checkbox>
						</div>
					</SettingsTabSection>
					<SettingsTabSection
						title={<Trans>Radio group</Trans>}
						description={<Trans>Radio buttons allow selecting one option from a group.</Trans>}
						data-flx="user.component-gallery-tab.selections-tab.settings-tab-section--4"
					>
						<RadioGroup
							aria-label={i18n._(SELECT_AN_OPTION_FROM_THE_RADIO_GROUP_DESCRIPTOR)}
							options={radioOptions}
							value={radioGroupValue}
							onChange={(value) => {
								setRadioGroupValue(value);
							}}
							data-flx="user.component-gallery-tab.selections-tab.radio-group"
						/>
					</SettingsTabSection>
					<SettingsTabSection
						title={<Trans>Sliders</Trans>}
						description={
							<Trans>Drag the slider handles to adjust values. Click markers to jump to specific values.</Trans>
						}
						data-flx="user.component-gallery-tab.selections-tab.settings-tab-section--5"
					>
						<SubsectionTitle data-flx="user.component-gallery-tab.selections-tab.subsection-title--3">
							<Trans>Standard slider with markers</Trans>
						</SubsectionTitle>
						<div className={styles.sliderRow} data-flx="user.component-gallery-tab.selections-tab.slider-row">
							<div
								className={styles.sliderContainer}
								data-flx="user.component-gallery-tab.selections-tab.slider-container"
							>
								<Slider
									defaultValue={sliderValue}
									factoryDefaultValue={42}
									minValue={0}
									maxValue={100}
									onValueChange={(v) => setSliderValue(Math.round(v))}
									onValueRender={(v) => `${Math.round(v)}%`}
									markers={[0, 25, 50, 75, 100]}
									onMarkerRender={(m) => `${m}%`}
									data-flx="user.component-gallery-tab.selections-tab.slider"
								/>
							</div>
							<div className={styles.sliderValue} data-flx="user.component-gallery-tab.selections-tab.slider-value">
								{sliderValue}%
							</div>
						</div>
						<SubsectionTitle data-flx="user.component-gallery-tab.selections-tab.subsection-title--4">
							<Trans>Slider with fewer markers</Trans>
						</SubsectionTitle>
						<div className={styles.sliderRow} data-flx="user.component-gallery-tab.selections-tab.slider-row--2">
							<div
								className={styles.sliderContainer}
								data-flx="user.component-gallery-tab.selections-tab.slider-container--2"
							>
								<Slider
									defaultValue={sliderValue2}
									factoryDefaultValue={75}
									minValue={0}
									maxValue={100}
									onValueChange={(v) => setSliderValue2(Math.round(v))}
									onValueRender={(v) => `${Math.round(v)}%`}
									markers={[0, 50, 100]}
									onMarkerRender={(m) => `${m}%`}
									data-flx="user.component-gallery-tab.selections-tab.slider--2"
								/>
							</div>
							<div className={styles.sliderValue} data-flx="user.component-gallery-tab.selections-tab.slider-value--2">
								{sliderValue2}%
							</div>
						</div>
						<SubsectionTitle data-flx="user.component-gallery-tab.selections-tab.subsection-title--5">
							<Trans>Slider with step values</Trans>
						</SubsectionTitle>
						<p
							className={styles.descriptionSmall}
							data-flx="user.component-gallery-tab.selections-tab.description-small"
						>
							<Trans>Snaps to increments of 5.</Trans>
						</p>
						<div className={styles.sliderRow} data-flx="user.component-gallery-tab.selections-tab.slider-row--3">
							<div
								className={styles.sliderContainer}
								data-flx="user.component-gallery-tab.selections-tab.slider-container--3"
							>
								<Slider
									defaultValue={sliderValue3}
									factoryDefaultValue={50}
									minValue={0}
									maxValue={100}
									step={5}
									onValueChange={(v) => setSliderValue3(Math.round(v))}
									onValueRender={(v) => `${Math.round(v)}%`}
									markers={[0, 25, 50, 75, 100]}
									onMarkerRender={(m) => `${m}%`}
									data-flx="user.component-gallery-tab.selections-tab.slider--3"
								/>
							</div>
							<div className={styles.sliderValue} data-flx="user.component-gallery-tab.selections-tab.slider-value--3">
								{sliderValue3}%
							</div>
						</div>
						<SubsectionTitle data-flx="user.component-gallery-tab.selections-tab.subsection-title--6">
							<Trans>Markers below slider</Trans>
						</SubsectionTitle>
						<p
							className={styles.descriptionSmall}
							data-flx="user.component-gallery-tab.selections-tab.description-small--2"
						>
							<Trans>Alternative marker positioning.</Trans>
						</p>
						<div className={styles.sliderRow} data-flx="user.component-gallery-tab.selections-tab.slider-row--4">
							<div
								className={styles.sliderContainer}
								data-flx="user.component-gallery-tab.selections-tab.slider-container--4"
							>
								<Slider
									defaultValue={sliderValue4}
									factoryDefaultValue={75}
									minValue={0}
									maxValue={100}
									markerPosition="below"
									onValueChange={(v) => setSliderValue4(Math.round(v))}
									onValueRender={(v) => `${Math.round(v)}%`}
									markers={[0, 25, 50, 75, 100]}
									onMarkerRender={(m) => `${m}%`}
									data-flx="user.component-gallery-tab.selections-tab.slider--4"
								/>
							</div>
							<div className={styles.sliderValue} data-flx="user.component-gallery-tab.selections-tab.slider-value--4">
								{sliderValue4}%
							</div>
						</div>
						<SubsectionTitle data-flx="user.component-gallery-tab.selections-tab.subsection-title--7">
							<Trans>Disabled slider</Trans>
						</SubsectionTitle>
						<div className={styles.sliderRow} data-flx="user.component-gallery-tab.selections-tab.slider-row--5">
							<div
								className={styles.sliderContainer}
								data-flx="user.component-gallery-tab.selections-tab.slider-container--5"
							>
								<Slider
									defaultValue={sliderValue5}
									factoryDefaultValue={60}
									minValue={0}
									maxValue={100}
									disabled
									onValueRender={(v) => `${Math.round(v)}%`}
									markers={[0, 25, 50, 75, 100]}
									onMarkerRender={(m) => `${m}%`}
									data-flx="user.component-gallery-tab.selections-tab.slider--5"
								/>
							</div>
							<div
								className={styles.sliderValueDisabled}
								data-flx="user.component-gallery-tab.selections-tab.slider-value-disabled"
							>
								{sliderValue5}%
							</div>
						</div>
					</SettingsTabSection>
				</SettingsTabContent>
			</SettingsTabContainer>
		);
	},
);
