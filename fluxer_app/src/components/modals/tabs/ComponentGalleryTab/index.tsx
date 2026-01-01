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

import {Trans, useLingui} from '@lingui/react/macro';
import {
	DotsThreeOutlineIcon,
	GearIcon,
	LinkSimpleIcon,
	PlayIcon,
	PlusIcon,
	ShareFatIcon,
	TrashIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {SettingsSection} from '~/components/modals/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '~/components/modals/shared/SettingsTabLayout';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuGroups} from '~/components/uikit/ContextMenu/MenuGroups';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import {MenuItemCheckbox} from '~/components/uikit/ContextMenu/MenuItemCheckbox';
import {MenuItemRadio} from '~/components/uikit/ContextMenu/MenuItemRadio';
import {MenuItemSlider} from '~/components/uikit/ContextMenu/MenuItemSlider';
import {MenuItemSubmenu} from '~/components/uikit/ContextMenu/MenuItemSubmenu';
import type {RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {ButtonsTab} from './ButtonsTab';
import {IndicatorsTab} from './IndicatorsTab';
import {InputsTab} from './InputsTab';
import {MarkdownTab} from './MarkdownTab';
import {OverlaysTab} from './OverlaysTab';
import {SelectionsTab} from './SelectionsTab';

const ComponentGalleryTab: React.FC = observer(() => {
	const {t} = useLingui();
	const [primarySwitch, setPrimarySwitch] = React.useState(true);
	const [dangerSwitch, setDangerSwitch] = React.useState(false);
	const [selectValue, setSelectValue] = React.useState('opt1');
	const [selectValue2, setSelectValue2] = React.useState('size-md');
	const [sliderValue, setSliderValue] = React.useState(42);
	const [sliderValue2, setSliderValue2] = React.useState(75);
	const [sliderValue3, setSliderValue3] = React.useState(50);

	const [sliderValue4, setSliderValue4] = React.useState(75);
	const [sliderValue5, setSliderValue5] = React.useState(60);
	const [color, setColor] = React.useState(0x3b82f6);
	const [color2, setColor2] = React.useState(0xff5733);
	const [radioValue, setRadioValue] = React.useState<'a' | 'b' | 'c'>('a');
	const [checkOne, setCheckOne] = React.useState(true);
	const [checkTwo, setCheckTwo] = React.useState(false);
	const [checkboxChecked, setCheckboxChecked] = React.useState(false);
	const [checkboxChecked2, setCheckboxChecked2] = React.useState(true);
	const [radioGroupValue, setRadioGroupValue] = React.useState<string>('option1');
	const [inputValue1, setInputValue1] = React.useState('');
	const [inputValue2, setInputValue2] = React.useState('');
	const [inputValue3, setInputValue3] = React.useState('');
	const [searchValue, setSearchValue] = React.useState('');
	const [emailValue, setEmailValue] = React.useState('');
	const [passwordValue, setPasswordValue] = React.useState('');
	const [textareaValue1, setTextareaValue1] = React.useState('');
	const [textareaValue2, setTextareaValue2] = React.useState('This is some example text in the textarea.');
	const [inlineEditValue, setInlineEditValue] = React.useState('EditableText');

	const radioOptions: Array<RadioOption<string>> = [
		{value: 'option1', name: t`First Option`, desc: t`This is the first option description`},
		{value: 'option2', name: t`Second Option`, desc: t`This is the second option description`},
		{value: 'option3', name: t`Third Option`, desc: t`This is the third option description`},
	];

	const openContextMenu = React.useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<MenuGroups>
					<MenuGroup>
						<MenuItem icon={<GearIcon size={16} />} onClick={() => onClose()}>
							<Trans>Settings</Trans>
						</MenuItem>
						<MenuItem icon={<ShareFatIcon size={16} />} onClick={() => onClose()}>
							<Trans>Share</Trans>
						</MenuItem>
						<MenuItem icon={<LinkSimpleIcon size={16} weight="bold" />} onClick={() => onClose()}>
							<Trans>Copy Link</Trans>
						</MenuItem>
					</MenuGroup>

					<MenuGroup>
						<MenuItemCheckbox icon={<PlusIcon size={16} weight="bold" />} checked={checkOne} onChange={setCheckOne}>
							<Trans>Enable Extra Option</Trans>
						</MenuItemCheckbox>
						<MenuItemCheckbox icon={<PlusIcon size={16} weight="bold" />} checked={checkTwo} onChange={setCheckTwo}>
							<Trans>Enable Beta Feature</Trans>
						</MenuItemCheckbox>
					</MenuGroup>

					<MenuGroup>
						<MenuItemRadio
							icon={<PlayIcon size={16} />}
							selected={radioValue === 'a'}
							onSelect={() => setRadioValue('a')}
						>
							<Trans>Mode A</Trans>
						</MenuItemRadio>
						<MenuItemRadio
							icon={<PlayIcon size={16} />}
							selected={radioValue === 'b'}
							onSelect={() => setRadioValue('b')}
						>
							<Trans>Mode B</Trans>
						</MenuItemRadio>
						<MenuItemRadio
							icon={<PlayIcon size={16} />}
							selected={radioValue === 'c'}
							onSelect={() => setRadioValue('c')}
						>
							<Trans>Mode C</Trans>
						</MenuItemRadio>
					</MenuGroup>

					<MenuGroup>
						<MenuItemSlider
							label={t`Opacity`}
							value={sliderValue}
							minValue={0}
							maxValue={100}
							onChange={(v: number) => setSliderValue(Math.round(v))}
							onFormat={(v: number) => `${Math.round(v)}%`}
						/>
					</MenuGroup>

					<MenuGroup>
						<MenuItemSubmenu
							label={t`More Actions`}
							icon={<DotsThreeOutlineIcon size={16} />}
							render={() => (
								<>
									<MenuItem onClick={() => onClose()}>
										<Trans>Duplicate</Trans>
									</MenuItem>
									<MenuItem onClick={() => onClose()}>
										<Trans>Archive</Trans>
									</MenuItem>
								</>
							)}
						/>
						<MenuItem icon={<TrashIcon size={16} />} danger onClick={() => onClose()}>
							<Trans>Delete</Trans>
						</MenuItem>
					</MenuGroup>
				</MenuGroups>
			));
		},
		[checkOne, checkTwo, radioValue, sliderValue],
	);

	return (
		<SettingsTabContainer>
			<SettingsTabContent>
				<SettingsSection id="buttons" title={t`Buttons`}>
					<ButtonsTab openContextMenu={openContextMenu} />
				</SettingsSection>

				<SettingsSection id="inputs" title={t`Inputs & Text`}>
					<InputsTab
						inputValue1={inputValue1}
						setInputValue1={setInputValue1}
						inputValue2={inputValue2}
						setInputValue2={setInputValue2}
						inputValue3={inputValue3}
						setInputValue3={setInputValue3}
						searchValue={searchValue}
						setSearchValue={setSearchValue}
						emailValue={emailValue}
						setEmailValue={setEmailValue}
						passwordValue={passwordValue}
						setPasswordValue={setPasswordValue}
						textareaValue1={textareaValue1}
						setTextareaValue1={setTextareaValue1}
						textareaValue2={textareaValue2}
						setTextareaValue2={setTextareaValue2}
						inlineEditValue={inlineEditValue}
						setInlineEditValue={setInlineEditValue}
						color={color}
						setColor={setColor}
						color2={color2}
						setColor2={setColor2}
					/>
				</SettingsSection>

				<SettingsSection id="selections" title={t`Selections`}>
					<SelectionsTab
						selectValue={selectValue}
						setSelectValue={setSelectValue}
						selectValue2={selectValue2}
						setSelectValue2={setSelectValue2}
						primarySwitch={primarySwitch}
						setPrimarySwitch={setPrimarySwitch}
						dangerSwitch={dangerSwitch}
						setDangerSwitch={setDangerSwitch}
						checkboxChecked={checkboxChecked}
						setCheckboxChecked={setCheckboxChecked}
						checkboxChecked2={checkboxChecked2}
						setCheckboxChecked2={setCheckboxChecked2}
						radioGroupValue={radioGroupValue}
						setRadioGroupValue={setRadioGroupValue}
						radioOptions={radioOptions}
						sliderValue={sliderValue}
						setSliderValue={setSliderValue}
						sliderValue2={sliderValue2}
						setSliderValue2={setSliderValue2}
						sliderValue3={sliderValue3}
						setSliderValue3={setSliderValue3}
						sliderValue4={sliderValue4}
						setSliderValue4={setSliderValue4}
						sliderValue5={sliderValue5}
						setSliderValue5={setSliderValue5}
					/>
				</SettingsSection>

				<SettingsSection id="overlays" title={t`Overlays & Menus`}>
					<OverlaysTab openContextMenu={openContextMenu} />
				</SettingsSection>

				<SettingsSection id="indicators" title={t`Indicators & Status`}>
					<IndicatorsTab />
				</SettingsSection>

				<SettingsSection
					id="status"
					title={t`Status Slate`}
					description={t`A reusable component for empty states, errors, and status messages.`}
				>
					<StatusSlate
						Icon={WarningCircleIcon}
						title={<Trans>Lorem ipsum dolor sit amet</Trans>}
						description={
							<Trans>
								Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et
								dolore magna aliqua.
							</Trans>
						}
						actions={[
							{
								text: <Trans>Primary Action</Trans>,
								onClick: () => {},
							},
							{
								text: <Trans>Secondary Action</Trans>,
								onClick: () => {},
								variant: 'secondary',
							},
						]}
					/>
				</SettingsSection>

				<SettingsSection id="markdown" title={t`Markdown`}>
					<MarkdownTab />
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default ComponentGalleryTab;
