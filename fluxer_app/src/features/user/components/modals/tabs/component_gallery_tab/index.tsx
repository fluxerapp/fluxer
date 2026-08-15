// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {Accordion} from '@app/features/ui/accordion/Accordion';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuGroups} from '@app/features/ui/action_menu/MenuGroups';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import {MenuItemSlider} from '@app/features/ui/action_menu/MenuItemSlider';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {ButtonsTab} from '@app/features/user/components/modals/tabs/component_gallery_tab/ComponentGalleryTabButtonsTab';
import {InputsTab} from '@app/features/user/components/modals/tabs/component_gallery_tab/ComponentGalleryTabInputsTab';
import {IndicatorsTab} from '@app/features/user/components/modals/tabs/component_gallery_tab/IndicatorsTab';
import {MarkdownTab} from '@app/features/user/components/modals/tabs/component_gallery_tab/MarkdownTab';
import {OverlaysTab} from '@app/features/user/components/modals/tabs/component_gallery_tab/OverlaysTab';
import {SelectionsTab} from '@app/features/user/components/modals/tabs/component_gallery_tab/SelectionsTab';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	GearIcon,
	LinkSimpleIcon,
	PlayIcon,
	PlusIcon,
	ShareFatIcon,
	TrashIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import type React from 'react';
import {useCallback, useState} from 'react';

const THIS_IS_SOME_EXAMPLE_TEXT_IN_THE_TEXTAREA_DESCRIPTOR = msg({
	message: 'This is some example text in the textarea.',
	comment: 'Description text in the index.',
});
const EDITABLE_TEXT_DESCRIPTOR = msg({
	message: 'Editable text',
	comment: 'Button or menu action label in the index. Keep it concise.',
});
const OPACITY_DESCRIPTOR = msg({
	message: 'Opacity',
	comment: 'Short label in the index. Keep it concise.',
});
const MORE_ACTIONS_DESCRIPTOR = msg({
	message: 'More actions',
	comment: 'Short label in the index. Keep it concise.',
});

interface GalleryAccordionSectionProps {
	id: string;
	title: React.ReactNode;
	description?: React.ReactNode;
	defaultExpanded?: boolean;
	children: React.ReactNode;
}

const GalleryAccordionSection: React.FC<GalleryAccordionSectionProps> = ({
	id,
	title,
	description,
	defaultExpanded = false,
	children,
}) => {
	return (
		<Accordion
			id={id}
			title={title}
			description={description}
			defaultExpanded={defaultExpanded}
			data-flx="user.component-gallery-tab.gallery-accordion-section.accordion"
		>
			{children}
		</Accordion>
	);
};
const ComponentGalleryTab: React.FC = () => {
	const {i18n} = useLingui();
	const [primarySwitch, setPrimarySwitch] = useState(true);
	const [dangerSwitch, setDangerSwitch] = useState(false);
	const [selectValue, setSelectValue] = useState('opt1');
	const [selectValue2, setSelectValue2] = useState('size-md');
	const [sliderValue, setSliderValue] = useState(42);
	const [sliderValue2, setSliderValue2] = useState(75);
	const [sliderValue3, setSliderValue3] = useState(50);
	const [sliderValue4, setSliderValue4] = useState(75);
	const [sliderValue5, setSliderValue5] = useState(60);
	const [color, setColor] = useState(0x3b82f6);
	const [color2, setColor2] = useState(0xff5733);
	const [radioValue, setRadioValue] = useState<'a' | 'b' | 'c'>('a');
	const [checkOne, setCheckOne] = useState(true);
	const [checkTwo, setCheckTwo] = useState(false);
	const [checkboxChecked, setCheckboxChecked] = useState(false);
	const [checkboxChecked2, setCheckboxChecked2] = useState(true);
	const [radioGroupValue, setRadioGroupValue] = useState<string>('option1');
	const [inputValue1, setInputValue1] = useState('');
	const [inputValue2, setInputValue2] = useState('');
	const [inputValue3, setInputValue3] = useState('');
	const [searchValue, setSearchValue] = useState('');
	const [emailValue, setEmailValue] = useState('');
	const [passwordValue, setPasswordValue] = useState('');
	const [textareaValue1, setTextareaValue1] = useState('');
	const [textareaEdit, setTextareaValue2] = useState<string | null>(null);
	const textareaValue2 = textareaEdit ?? i18n._(THIS_IS_SOME_EXAMPLE_TEXT_IN_THE_TEXTAREA_DESCRIPTOR);
	const [textareaValue3, setTextareaValue3] = useState('');
	const [inlineEdit, setInlineEditValue] = useState<string | null>(null);
	const inlineEditValue = inlineEdit ?? i18n._(EDITABLE_TEXT_DESCRIPTOR);
	const radioOptions: Array<RadioOption<string>> = [
		{value: 'option1', name: <Trans>First option</Trans>, desc: <Trans>This is the first option description</Trans>},
		{value: 'option2', name: <Trans>Second option</Trans>, desc: <Trans>This is the second option description</Trans>},
		{value: 'option3', name: <Trans>Third option</Trans>, desc: <Trans>This is the third option description</Trans>},
	];
	const openContextMenu = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<MenuGroups data-flx="user.component-gallery-tab.open-context-menu.menu-groups">
					<MenuGroup data-flx="user.component-gallery-tab.open-context-menu.menu-group">
						<MenuItem
							icon={<GearIcon size={16} data-flx="user.component-gallery-tab.open-context-menu.gear-icon" />}
							onClick={() => onClose()}
							data-flx="user.component-gallery-tab.open-context-menu.menu-item.close"
						>
							<Trans>Settings</Trans>
						</MenuItem>
						<MenuItem
							icon={<ShareFatIcon size={16} data-flx="user.component-gallery-tab.open-context-menu.share-fat-icon" />}
							onClick={() => onClose()}
							data-flx="user.component-gallery-tab.open-context-menu.menu-item.close--2"
						>
							<Trans>Share</Trans>
						</MenuItem>
						<MenuItem
							icon={
								<LinkSimpleIcon
									size={16}
									weight="bold"
									data-flx="user.component-gallery-tab.open-context-menu.link-simple-icon"
								/>
							}
							onClick={() => onClose()}
							data-flx="user.component-gallery-tab.open-context-menu.menu-item.close--3"
						>
							<Trans>Copy link</Trans>
						</MenuItem>
					</MenuGroup>
					<MenuGroup data-flx="user.component-gallery-tab.open-context-menu.menu-group--2">
						<CheckboxItem
							icon={
								<PlusIcon size={16} weight="bold" data-flx="user.component-gallery-tab.open-context-menu.plus-icon" />
							}
							checked={checkOne}
							onCheckedChange={setCheckOne}
							data-flx="user.component-gallery-tab.open-context-menu.checkbox-item"
						>
							<Trans>Enable extra option</Trans>
						</CheckboxItem>
						<CheckboxItem
							icon={
								<PlusIcon
									size={16}
									weight="bold"
									data-flx="user.component-gallery-tab.open-context-menu.plus-icon--2"
								/>
							}
							checked={checkTwo}
							onCheckedChange={setCheckTwo}
							data-flx="user.component-gallery-tab.open-context-menu.checkbox-item--2"
						>
							<Trans>Enable beta feature</Trans>
						</CheckboxItem>
					</MenuGroup>
					<MenuGroup data-flx="user.component-gallery-tab.open-context-menu.menu-group--3">
						<MenuItemRadio
							icon={<PlayIcon size={16} data-flx="user.component-gallery-tab.open-context-menu.play-icon" />}
							selected={radioValue === 'a'}
							onSelect={() => setRadioValue('a')}
							data-flx="user.component-gallery-tab.open-context-menu.menu-item-radio.set-radio-value"
						>
							Mode A
						</MenuItemRadio>
						<MenuItemRadio
							icon={<PlayIcon size={16} data-flx="user.component-gallery-tab.open-context-menu.play-icon--2" />}
							selected={radioValue === 'b'}
							onSelect={() => setRadioValue('b')}
							data-flx="user.component-gallery-tab.open-context-menu.menu-item-radio.set-radio-value--2"
						>
							Mode B
						</MenuItemRadio>
						<MenuItemRadio
							icon={<PlayIcon size={16} data-flx="user.component-gallery-tab.open-context-menu.play-icon--3" />}
							selected={radioValue === 'c'}
							onSelect={() => setRadioValue('c')}
							data-flx="user.component-gallery-tab.open-context-menu.menu-item-radio.set-radio-value--3"
						>
							Mode C
						</MenuItemRadio>
					</MenuGroup>
					<MenuGroup data-flx="user.component-gallery-tab.open-context-menu.menu-group--4">
						<MenuItemSlider
							label={i18n._(OPACITY_DESCRIPTOR)}
							value={sliderValue}
							minValue={0}
							maxValue={100}
							onChange={(v: number) => setSliderValue(Math.round(v))}
							onFormat={(v: number) => `${Math.round(v)}%`}
							data-flx="user.component-gallery-tab.open-context-menu.menu-item-slider.set-slider-value"
						/>
					</MenuGroup>
					<MenuGroup data-flx="user.component-gallery-tab.open-context-menu.menu-group--5">
						<MenuItemSubmenu
							label={i18n._(MORE_ACTIONS_DESCRIPTOR)}
							render={() => (
								<>
									<MenuItem
										onClick={() => onClose()}
										data-flx="user.component-gallery-tab.open-context-menu.menu-item.close--4"
									>
										<Trans>Duplicate</Trans>
									</MenuItem>
									<MenuItem
										onClick={() => onClose()}
										data-flx="user.component-gallery-tab.open-context-menu.menu-item.close--5"
									>
										<Trans>Archive</Trans>
									</MenuItem>
								</>
							)}
							data-flx="user.component-gallery-tab.open-context-menu.menu-item-submenu"
						/>
						<MenuItem
							icon={<TrashIcon size={16} data-flx="user.component-gallery-tab.open-context-menu.trash-icon" />}
							danger
							onClick={() => onClose()}
							data-flx="user.component-gallery-tab.open-context-menu.menu-item.close--6"
						>
							<Trans>Delete</Trans>
						</MenuItem>
					</MenuGroup>
				</MenuGroups>
			));
		},
		[checkOne, checkTwo, radioValue, sliderValue, i18n],
	);
	return (
		<SettingsTabContainer data-flx="user.component-gallery-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.component-gallery-tab.settings-tab-content">
				<GalleryAccordionSection
					id="buttons"
					title={<Trans>Buttons</Trans>}
					defaultExpanded
					data-flx="user.component-gallery-tab.buttons"
				>
					<ButtonsTab openContextMenu={openContextMenu} data-flx="user.component-gallery-tab.buttons-tab" />
				</GalleryAccordionSection>
				<GalleryAccordionSection
					id="inputs"
					title={<Trans>Inputs & text</Trans>}
					data-flx="user.component-gallery-tab.inputs"
				>
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
						textareaValue3={textareaValue3}
						setTextareaValue3={setTextareaValue3}
						inlineEditValue={inlineEditValue}
						setInlineEditValue={setInlineEditValue}
						color={color}
						setColor={setColor}
						color2={color2}
						setColor2={setColor2}
						data-flx="user.component-gallery-tab.inputs-tab"
					/>
				</GalleryAccordionSection>
				<GalleryAccordionSection
					id="selections"
					title={<Trans>Selections</Trans>}
					data-flx="user.component-gallery-tab.selections"
				>
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
						data-flx="user.component-gallery-tab.selections-tab"
					/>
				</GalleryAccordionSection>
				<GalleryAccordionSection
					id="overlays"
					title={<Trans>Overlays & menus</Trans>}
					data-flx="user.component-gallery-tab.overlays"
				>
					<OverlaysTab openContextMenu={openContextMenu} data-flx="user.component-gallery-tab.overlays-tab" />
				</GalleryAccordionSection>
				<GalleryAccordionSection
					id="indicators"
					title={<Trans>Indicators & status</Trans>}
					data-flx="user.component-gallery-tab.indicators"
				>
					<IndicatorsTab data-flx="user.component-gallery-tab.indicators-tab" />
				</GalleryAccordionSection>
				<GalleryAccordionSection
					id="status"
					title={<Trans>Status slate</Trans>}
					description={<Trans>A reusable component for empty states, errors, and status messages.</Trans>}
					data-flx="user.component-gallery-tab.status"
				>
					<StatusSlate
						Icon={WarningCircleIcon}
						title="Lorem ipsum dolor sit amet"
						description="Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
						actions={[
							{
								text: <Trans>Primary action</Trans>,
								onClick: () => {},
							},
							{
								text: <Trans>Secondary action</Trans>,
								onClick: () => {},
								variant: 'secondary',
							},
						]}
						data-flx="user.component-gallery-tab.status-slate"
					/>
				</GalleryAccordionSection>
				<GalleryAccordionSection
					id="markdown"
					title={<Trans>Markdown</Trans>}
					data-flx="user.component-gallery-tab.markdown"
				>
					<MarkdownTab data-flx="user.component-gallery-tab.markdown-tab" />
				</GalleryAccordionSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
};

export default ComponentGalleryTab;
