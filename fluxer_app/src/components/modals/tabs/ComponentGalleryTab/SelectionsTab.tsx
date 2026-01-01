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

import {t} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Select} from '~/components/form/Select';
import {Switch} from '~/components/form/Switch';
import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '~/components/modals/shared/SettingsTabLayout';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import {RadioGroup, type RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {Slider} from '~/components/uikit/Slider';
import styles from './SelectionsTab.module.css';
import {SubsectionTitle} from './shared';

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
	setSliderValue5: (value: number) => void;
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
		return (
			<SettingsTabContainer>
				<SettingsTabContent>
					<SettingsTabSection
						title={<Trans>Select Dropdown</Trans>}
						description={<Trans>Click to open the dropdown menu and select different options.</Trans>}
					>
						<div className={styles.gridDouble}>
							<Select<string>
								label={t`Choose an option`}
								value={selectValue}
								onChange={(value) => {
									setSelectValue(value);
								}}
								options={[
									{value: 'opt1', label: t`Option One`},
									{value: 'opt2', label: t`Option Two`},
									{value: 'opt3', label: t`Option Three`},
									{value: 'opt4', label: t`Option Four`},
								]}
							/>
							<Select<string>
								label={t`Size Selection`}
								value={selectValue2}
								onChange={(value) => {
									setSelectValue2(value);
								}}
								options={[
									{value: 'size-sm', label: t`Small`},
									{value: 'size-md', label: t`Medium`},
									{value: 'size-lg', label: t`Large`},
									{value: 'size-xl', label: t`Extra Large`},
								]}
							/>
						</div>
						<div className={styles.gridSingle}>
							<Select
								label={t`Disabled Select`}
								value="disabled-opt"
								onChange={() => {}}
								disabled
								options={[{value: 'disabled-opt', label: t`This is disabled`}]}
							/>
						</div>
					</SettingsTabSection>

					<SettingsTabSection
						title={<Trans>Switches</Trans>}
						description={<Trans>Toggle switches on and off to see state changes.</Trans>}
					>
						<div className={styles.contentList}>
							<Switch
								label={t`Enable Notifications`}
								description={t`Receive notifications when someone mentions you`}
								value={primarySwitch}
								onChange={(value) => {
									setPrimarySwitch(value);
								}}
							/>
							<Switch
								label={t`Dark Mode`}
								description={t`Use dark theme across the application`}
								value={dangerSwitch}
								onChange={(value) => {
									setDangerSwitch(value);
								}}
							/>
							<Switch
								label={t`Disabled Switch`}
								description={t`This switch is disabled and cannot be toggled`}
								value={false}
								onChange={() => {}}
								disabled
							/>
							<Switch
								label={t`Disabled (Checked)`}
								description={t`This switch is disabled in the checked state`}
								value={true}
								onChange={() => {}}
								disabled
							/>
						</div>
					</SettingsTabSection>

					<SettingsTabSection
						title={<Trans>Checkboxes</Trans>}
						description={<Trans>Click to check and uncheck. Available in square and round styles.</Trans>}
					>
						<SubsectionTitle>
							<Trans>Square Checkboxes</Trans>
						</SubsectionTitle>
						<div className={styles.contentList}>
							<Checkbox
								checked={checkboxChecked}
								onChange={(checked) => {
									setCheckboxChecked(checked);
								}}
							>
								<Trans>Interactive Checkbox</Trans>
							</Checkbox>
							<Checkbox
								checked={checkboxChecked2}
								onChange={(checked) => {
									setCheckboxChecked2(checked);
								}}
							>
								<Trans>Another Checkbox</Trans>
							</Checkbox>
							<Checkbox checked={true} disabled>
								<Trans>Disabled (Checked)</Trans>
							</Checkbox>
							<Checkbox checked={false} disabled>
								<Trans>Disabled (Unchecked)</Trans>
							</Checkbox>
						</div>

						<SubsectionTitle>
							<Trans>Round Checkboxes</Trans>
						</SubsectionTitle>
						<div className={styles.contentList}>
							<Checkbox checked={checkboxChecked} onChange={(checked) => setCheckboxChecked(checked)} type="round">
								<Trans>Round Style Checkbox</Trans>
							</Checkbox>
							<Checkbox checked={checkboxChecked2} onChange={(checked) => setCheckboxChecked2(checked)} type="round">
								<Trans>Another Round Checkbox</Trans>
							</Checkbox>
							<Checkbox checked={true} disabled type="round">
								<Trans>Disabled Round (Checked)</Trans>
							</Checkbox>
						</div>
					</SettingsTabSection>

					<SettingsTabSection
						title={<Trans>Radio Group</Trans>}
						description={<Trans>Radio buttons allow selecting one option from a group.</Trans>}
					>
						<RadioGroup
							aria-label={t`Select an option from the radio group`}
							options={radioOptions}
							value={radioGroupValue}
							onChange={(value) => {
								setRadioGroupValue(value);
							}}
						/>
					</SettingsTabSection>

					<SettingsTabSection
						title={<Trans>Sliders</Trans>}
						description={
							<Trans>Drag the slider handles to adjust values. Click markers to jump to specific values.</Trans>
						}
					>
						<SubsectionTitle>
							<Trans>Standard Slider with Markers</Trans>
						</SubsectionTitle>
						<div className={styles.sliderRow}>
							<div className={styles.sliderContainer}>
								<Slider
									defaultValue={sliderValue}
									factoryDefaultValue={42}
									minValue={0}
									maxValue={100}
									onValueChange={(v) => setSliderValue(Math.round(v))}
									onValueRender={(v) => `${Math.round(v)}%`}
									markers={[0, 25, 50, 75, 100]}
									onMarkerRender={(m) => `${m}%`}
								/>
							</div>
							<div className={styles.sliderValue}>{sliderValue}%</div>
						</div>

						<SubsectionTitle>
							<Trans>Slider with Fewer Markers</Trans>
						</SubsectionTitle>
						<div className={styles.sliderRow}>
							<div className={styles.sliderContainer}>
								<Slider
									defaultValue={sliderValue2}
									factoryDefaultValue={75}
									minValue={0}
									maxValue={100}
									onValueChange={(v) => setSliderValue2(Math.round(v))}
									onValueRender={(v) => `${Math.round(v)}%`}
									markers={[0, 50, 100]}
									onMarkerRender={(m) => `${m}%`}
								/>
							</div>
							<div className={styles.sliderValue}>{sliderValue2}%</div>
						</div>

						<SubsectionTitle>
							<Trans>Slider with Step Values</Trans>
						</SubsectionTitle>
						<p className={styles.descriptionSmall}>
							<Trans>Snaps to increments of 5.</Trans>
						</p>
						<div className={styles.sliderRow}>
							<div className={styles.sliderContainer}>
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
								/>
							</div>
							<div className={styles.sliderValue}>{sliderValue3}%</div>
						</div>

						<SubsectionTitle>
							<Trans>Markers Below Slider</Trans>
						</SubsectionTitle>
						<p className={styles.descriptionSmall}>
							<Trans>Alternative marker positioning.</Trans>
						</p>
						<div className={styles.sliderRow}>
							<div className={styles.sliderContainer}>
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
								/>
							</div>
							<div className={styles.sliderValue}>{sliderValue4}%</div>
						</div>

						<SubsectionTitle>
							<Trans>Disabled Slider</Trans>
						</SubsectionTitle>
						<div className={styles.sliderRow}>
							<div className={styles.sliderContainer}>
								<Slider
									defaultValue={sliderValue5}
									factoryDefaultValue={60}
									minValue={0}
									maxValue={100}
									disabled
									onValueRender={(v) => `${Math.round(v)}%`}
									markers={[0, 25, 50, 75, 100]}
									onMarkerRender={(m) => `${m}%`}
								/>
							</div>
							<div className={styles.sliderValueDisabled}>{sliderValue5}%</div>
						</div>
					</SettingsTabSection>
				</SettingsTabContent>
			</SettingsTabContainer>
		);
	},
);
