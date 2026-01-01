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
import {MagnifyingGlassIcon, UserIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {ColorPickerField} from '~/components/form/ColorPickerField';
import {Input, Textarea} from '~/components/form/Input';
import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '~/components/modals/shared/SettingsTabLayout';
import {InlineEdit} from '~/components/uikit/InlineEdit';
import styles from './InputsTab.module.css';

interface InputsTabProps {
	inputValue1: string;
	setInputValue1: (value: string) => void;
	inputValue2: string;
	setInputValue2: (value: string) => void;
	inputValue3: string;
	setInputValue3: (value: string) => void;
	searchValue: string;
	setSearchValue: (value: string) => void;
	emailValue: string;
	setEmailValue: (value: string) => void;
	passwordValue: string;
	setPasswordValue: (value: string) => void;
	textareaValue1: string;
	setTextareaValue1: (value: string) => void;
	textareaValue2: string;
	setTextareaValue2: (value: string) => void;
	inlineEditValue: string;
	setInlineEditValue: (value: string) => void;
	color: number;
	setColor: (value: number) => void;
	color2: number;
	setColor2: (value: number) => void;
}

export const InputsTab: React.FC<InputsTabProps> = observer(
	({
		inputValue1,
		setInputValue1,
		inputValue2,
		setInputValue2,
		inputValue3,
		setInputValue3,
		searchValue,
		setSearchValue,
		emailValue,
		setEmailValue,
		passwordValue,
		setPasswordValue,
		textareaValue1,
		setTextareaValue1,
		textareaValue2,
		setTextareaValue2,
		inlineEditValue,
		setInlineEditValue,
		color,
		setColor,
		color2,
		setColor2,
	}) => {
		const {t} = useLingui();
		return (
			<SettingsTabContainer>
				<SettingsTabContent>
					<SettingsTabSection
						title={<Trans>Basic Text Inputs</Trans>}
						description={<Trans>All inputs are fully interactive - type to test them out!</Trans>}
					>
						<div className={styles.grid}>
							<Input
								label={t`Display Name`}
								placeholder={t`Enter your name`}
								value={inputValue1}
								onChange={(e) => setInputValue1(e.target.value)}
							/>
							<Input
								label={t`Username`}
								placeholder={t`@username`}
								value={inputValue2}
								onChange={(e) => setInputValue2(e.target.value)}
							/>
							<Input
								label={t`Email Address`}
								type="email"
								placeholder={t`your@email.com`}
								value={emailValue}
								onChange={(e) => setEmailValue(e.target.value)}
							/>
							<Input
								label={t`Password`}
								type="password"
								placeholder={t`Enter a secure password`}
								value={passwordValue}
								onChange={(e) => setPasswordValue(e.target.value)}
							/>
						</div>
					</SettingsTabSection>

					<SettingsTabSection title={<Trans>Inputs with Icons</Trans>}>
						<div className={styles.grid}>
							<Input
								label={t`Search`}
								placeholder={t`Search for anything...`}
								leftIcon={<MagnifyingGlassIcon size={16} weight="bold" />}
								value={searchValue}
								onChange={(e) => setSearchValue(e.target.value)}
							/>
							<Input
								label={t`User Profile`}
								placeholder={t`Enter username`}
								leftIcon={<UserIcon size={16} />}
								value={inputValue3}
								onChange={(e) => setInputValue3(e.target.value)}
							/>
						</div>
					</SettingsTabSection>

					<SettingsTabSection title={<Trans>Input States</Trans>}>
						<div className={styles.grid}>
							<Input
								label={t`With Error`}
								placeholder={t`This field has an error`}
								error={t`This is an error message`}
							/>
							<Input label={t`Disabled Input`} placeholder={t`Cannot be edited`} disabled value="Disabled value" />
						</div>
					</SettingsTabSection>

					<SettingsTabSection title={<Trans>Textarea</Trans>}>
						<div className={styles.grid}>
							<Textarea
								label={t`About You`}
								placeholder={t`Write a short bio (max 280 characters)`}
								maxLength={280}
								showCharacterCount
								value={textareaValue1}
								onChange={(e) => setTextareaValue1(e.target.value)}
							/>
							<Textarea
								label={t`Message`}
								placeholder={t`Type your message here...`}
								minRows={3}
								value={textareaValue2}
								onChange={(e) => setTextareaValue2(e.target.value)}
							/>
						</div>
						<div className={styles.gridSingle}>
							<Textarea
								label={t`Long Form Content`}
								placeholder={t`Write your content here... This textarea expands as you type.`}
								minRows={4}
								maxRows={12}
								value={textareaValue1}
								onChange={(e) => setTextareaValue1(e.target.value)}
								footer={
									<p className={styles.inlineEditLabel}>
										<Trans>This textarea auto-expands between 4-12 rows as you type.</Trans>
									</p>
								}
							/>
						</div>
					</SettingsTabSection>

					<SettingsTabSection
						title={<Trans>Inline Edit</Trans>}
						description={
							<Trans>Click the text below to edit it inline. Press Enter to save or Escape to cancel.</Trans>
						}
					>
						<div className={styles.inlineEditWrapper}>
							<span className={styles.inlineEditCaption}>
								<Trans>Editable Text:</Trans>
							</span>
							<InlineEdit
								value={inlineEditValue}
								onSave={(newValue) => {
									setInlineEditValue(newValue);
									ToastActionCreators.createToast({type: 'success', children: t`Value saved: ${newValue}`});
								}}
								placeholder={t`Enter text`}
								maxLength={50}
							/>
						</div>
					</SettingsTabSection>

					<SettingsTabSection
						title={<Trans>Color Pickers</Trans>}
						description={<Trans>Click to open the color picker and choose a new color.</Trans>}
					>
						<div className={styles.colorPickersGrid}>
							<ColorPickerField label={t`Primary Accent Color`} value={color} onChange={setColor} />
							<ColorPickerField label={t`Secondary Accent Color`} value={color2} onChange={setColor2} />
						</div>
					</SettingsTabSection>
				</SettingsTabContent>
			</SettingsTabContainer>
		);
	},
);
