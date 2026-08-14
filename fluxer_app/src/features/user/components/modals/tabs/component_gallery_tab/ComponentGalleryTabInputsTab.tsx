// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {EXAMPLE_EMAIL, EXAMPLE_USERNAME_MENTION} from '@app/features/app/config/I18nDisplayConstants';
import {PASSWORD_DESCRIPTOR, USERNAME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {ColorPickerField} from '@app/features/ui/components/form/ColorPickerField';
import {Input, Textarea} from '@app/features/ui/components/form/FormInput';
import {InlineEdit} from '@app/features/ui/components/InlineEdit';
import styles from '@app/features/user/components/modals/tabs/component_gallery_tab/ComponentGalleryTabInputsTab.module.css';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, UserIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DISPLAY_NAME_DESCRIPTOR = msg({
	message: 'Display name',
	comment: 'Short label in the inputs tab. Keep it concise.',
});
const ENTER_YOUR_NAME_DESCRIPTOR = msg({
	message: 'Enter your name',
	comment: 'Short label in the inputs tab. Keep it concise.',
});
const EMAIL_ADDRESS_DESCRIPTOR = msg({
	message: 'Email address',
	comment: 'Short label in the inputs tab. Keep it concise.',
});
const ENTER_A_SECURE_PASSWORD_DESCRIPTOR = msg({
	message: 'Enter a secure password',
	comment: 'Label in the inputs tab. Keep the tone plain and specific.',
});
const SEARCH_DESCRIPTOR = msg({
	message: 'Search',
	comment: 'Button or menu action label in the inputs tab. Keep it concise.',
});
const SEARCH_FOR_ANYTHING_DESCRIPTOR = msg({
	message: 'Search for anything...',
	comment: 'Button or menu action label in the inputs tab. Keep it concise.',
});
const USER_PROFILE_DESCRIPTOR = msg({
	message: 'User profile',
	comment: 'Short label in the inputs tab. Keep it concise.',
});
const ENTER_USERNAME_DESCRIPTOR = msg({
	message: 'Enter username',
	comment: 'Short label in the inputs tab. Keep it concise.',
});
const WITH_ERROR_DESCRIPTOR = msg({
	message: 'With error',
	comment: 'Error message in the inputs tab.',
});
const THIS_FIELD_HAS_AN_ERROR_DESCRIPTOR = msg({
	message: 'This field has an error',
	comment: 'Error message in the inputs tab.',
});
const THIS_IS_AN_ERROR_MESSAGE_DESCRIPTOR = msg({
	message: 'This is an error message',
	comment: 'Error message in the inputs tab.',
});
const DISABLED_INPUT_DESCRIPTOR = msg({
	message: 'Disabled input',
	comment: 'Button or menu action label in the inputs tab. Keep it concise.',
});
const CANNOT_BE_EDITED_DESCRIPTOR = msg({
	message: 'Cannot be edited',
	comment: 'Error message in the inputs tab.',
});
const DISABLED_VALUE_DESCRIPTOR = msg({
	message: 'Disabled value',
	comment: 'Button or menu action label in the inputs tab. Keep it concise.',
});
const ABOUT_YOU_DESCRIPTOR = msg({
	message: 'About you',
	comment: 'Short label in the inputs tab. Keep it concise.',
});
const WRITE_A_SHORT_BIO_MAX_280_CHARACTERS_DESCRIPTOR = msg({
	message: 'Write a short bio (max 280 characters)',
	comment: 'Label in the inputs tab.',
});
const MESSAGE_DESCRIPTOR = msg({
	message: 'Message',
	comment: 'Short label in the inputs tab. Keep it concise.',
});
const TYPE_YOUR_MESSAGE_HERE_DESCRIPTOR = msg({
	message: 'Type your message here…',
	comment: 'Label in the inputs tab.',
});
const LONG_FORM_CONTENT_DESCRIPTOR = msg({
	message: 'Long-form content',
	comment: 'Short label in the inputs tab. Keep it concise.',
});
const WRITE_YOUR_CONTENT_HERE_THIS_TEXTAREA_EXPANDS_AS_DESCRIPTOR = msg({
	message: 'Write your content here. This textarea expands as you type.',
	comment: 'Description text in the inputs tab.',
});
const VALUE_SAVED_DESCRIPTOR = msg({
	message: 'Value saved: {newValue}',
	comment: 'Short label in the inputs tab. Keep it concise. Preserve {newValue}; it is inserted by code.',
});
const ENTER_TEXT_DESCRIPTOR = msg({
	message: 'Enter text',
	comment: 'Short label in the inputs tab. Keep it concise.',
});
const PRIMARY_ACCENT_COLOR_DESCRIPTOR = msg({
	message: 'Primary accent color',
	comment: 'Short label in the inputs tab. Keep it concise.',
});
const SECONDARY_ACCENT_COLOR_DESCRIPTOR = msg({
	message: 'Secondary accent color',
	comment: 'Short label in the inputs tab. Keep it concise.',
});

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
	textareaValue3: string;
	setTextareaValue3: (value: string) => void;
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
		textareaValue3,
		setTextareaValue3,
		inlineEditValue,
		setInlineEditValue,
		color,
		setColor,
		color2,
		setColor2,
	}) => {
		const {i18n} = useLingui();
		return (
			<SettingsTabContainer data-flx="user.component-gallery-tab.inputs-tab.settings-tab-container">
				<SettingsTabContent data-flx="user.component-gallery-tab.inputs-tab.settings-tab-content">
					<SettingsTabSection
						title={<Trans>Basic text inputs</Trans>}
						description={<Trans>All inputs are fully interactive, so type away.</Trans>}
						data-flx="user.component-gallery-tab.inputs-tab.settings-tab-section"
					>
						<div className={styles.grid} data-flx="user.component-gallery-tab.inputs-tab.grid">
							<Input
								label={i18n._(DISPLAY_NAME_DESCRIPTOR)}
								placeholder={i18n._(ENTER_YOUR_NAME_DESCRIPTOR)}
								value={inputValue1}
								onChange={(e) => setInputValue1(e.target.value)}
								data-flx="user.component-gallery-tab.inputs-tab.input"
							/>
							<Input
								label={i18n._(USERNAME_DESCRIPTOR)}
								placeholder={EXAMPLE_USERNAME_MENTION}
								value={inputValue2}
								onChange={(e) => setInputValue2(e.target.value)}
								data-flx="user.component-gallery-tab.inputs-tab.input--2"
							/>
							<Input
								label={i18n._(EMAIL_ADDRESS_DESCRIPTOR)}
								type="email"
								placeholder={EXAMPLE_EMAIL}
								value={emailValue}
								onChange={(e) => setEmailValue(e.target.value)}
								data-flx="user.component-gallery-tab.inputs-tab.input.set-email-value"
							/>
							<Input
								label={i18n._(PASSWORD_DESCRIPTOR)}
								type="password"
								placeholder={i18n._(ENTER_A_SECURE_PASSWORD_DESCRIPTOR)}
								value={passwordValue}
								onChange={(e) => setPasswordValue(e.target.value)}
								data-flx="user.component-gallery-tab.inputs-tab.input.set-password-value"
							/>
						</div>
					</SettingsTabSection>
					<SettingsTabSection
						title={<Trans>Inputs with icons</Trans>}
						data-flx="user.component-gallery-tab.inputs-tab.settings-tab-section--2"
					>
						<div className={styles.grid} data-flx="user.component-gallery-tab.inputs-tab.grid--2">
							<Input
								label={i18n._(SEARCH_DESCRIPTOR)}
								placeholder={i18n._(SEARCH_FOR_ANYTHING_DESCRIPTOR)}
								leftIcon={
									<MagnifyingGlassIcon
										size={remFromPx(16)}
										weight="bold"
										data-flx="user.component-gallery-tab.inputs-tab.magnifying-glass-icon"
									/>
								}
								value={searchValue}
								onChange={(e) => setSearchValue(e.target.value)}
								data-flx="user.component-gallery-tab.inputs-tab.input.set-search-value"
							/>
							<Input
								label={i18n._(USER_PROFILE_DESCRIPTOR)}
								placeholder={i18n._(ENTER_USERNAME_DESCRIPTOR)}
								leftIcon={<UserIcon size={remFromPx(16)} data-flx="user.component-gallery-tab.inputs-tab.user-icon" />}
								value={inputValue3}
								onChange={(e) => setInputValue3(e.target.value)}
								data-flx="user.component-gallery-tab.inputs-tab.input--3"
							/>
						</div>
					</SettingsTabSection>
					<SettingsTabSection
						title={<Trans>Input states</Trans>}
						data-flx="user.component-gallery-tab.inputs-tab.settings-tab-section--3"
					>
						<div className={styles.grid} data-flx="user.component-gallery-tab.inputs-tab.grid--3">
							<Input
								label={i18n._(WITH_ERROR_DESCRIPTOR)}
								placeholder={i18n._(THIS_FIELD_HAS_AN_ERROR_DESCRIPTOR)}
								error={i18n._(THIS_IS_AN_ERROR_MESSAGE_DESCRIPTOR)}
								data-flx="user.component-gallery-tab.inputs-tab.input--4"
							/>
							<Input
								label={i18n._(DISABLED_INPUT_DESCRIPTOR)}
								placeholder={i18n._(CANNOT_BE_EDITED_DESCRIPTOR)}
								disabled
								value={i18n._(DISABLED_VALUE_DESCRIPTOR)}
								data-flx="user.component-gallery-tab.inputs-tab.input--5"
							/>
						</div>
					</SettingsTabSection>
					<SettingsTabSection
						title={<Trans>Textarea</Trans>}
						data-flx="user.component-gallery-tab.inputs-tab.settings-tab-section--4"
					>
						<div className={styles.grid} data-flx="user.component-gallery-tab.inputs-tab.grid--4">
							<Textarea
								label={i18n._(ABOUT_YOU_DESCRIPTOR)}
								placeholder={i18n._(WRITE_A_SHORT_BIO_MAX_280_CHARACTERS_DESCRIPTOR)}
								maxLength={280}
								showCharacterCount
								value={textareaValue1}
								onChange={(e) => setTextareaValue1(e.target.value)}
								data-flx="user.component-gallery-tab.inputs-tab.textarea"
							/>
							<Textarea
								label={i18n._(MESSAGE_DESCRIPTOR)}
								placeholder={i18n._(TYPE_YOUR_MESSAGE_HERE_DESCRIPTOR)}
								minRows={3}
								value={textareaValue2}
								onChange={(e) => setTextareaValue2(e.target.value)}
								data-flx="user.component-gallery-tab.inputs-tab.textarea--2"
							/>
						</div>
						<div className={styles.gridSingle} data-flx="user.component-gallery-tab.inputs-tab.grid-single">
							<Textarea
								label={i18n._(LONG_FORM_CONTENT_DESCRIPTOR)}
								placeholder={i18n._(WRITE_YOUR_CONTENT_HERE_THIS_TEXTAREA_EXPANDS_AS_DESCRIPTOR)}
								minRows={4}
								maxRows={12}
								value={textareaValue3}
								onChange={(e) => setTextareaValue3(e.target.value)}
								footer={
									<p
										className={styles.inlineEditLabel}
										data-flx="user.component-gallery-tab.inputs-tab.inline-edit-label"
									>
										<Trans>This textarea auto-expands between 4 and 12 rows as you type.</Trans>
									</p>
								}
								data-flx="user.component-gallery-tab.inputs-tab.textarea--3"
							/>
						</div>
					</SettingsTabSection>
					<SettingsTabSection
						title={<Trans>Inline edit</Trans>}
						description={
							<Trans>Click the text below to edit it inline. Press Enter to save or Escape to cancel.</Trans>
						}
						data-flx="user.component-gallery-tab.inputs-tab.settings-tab-section--5"
					>
						<div
							className={styles.inlineEditWrapper}
							data-flx="user.component-gallery-tab.inputs-tab.inline-edit-wrapper"
						>
							<span
								className={styles.inlineEditCaption}
								data-flx="user.component-gallery-tab.inputs-tab.inline-edit-caption"
							>
								<Trans>Editable text:</Trans>
							</span>
							<InlineEdit
								value={inlineEditValue}
								onSave={(newValue) => {
									setInlineEditValue(newValue);
									ToastCommands.createToast({type: 'success', children: i18n._(VALUE_SAVED_DESCRIPTOR, {newValue})});
								}}
								placeholder={i18n._(ENTER_TEXT_DESCRIPTOR)}
								maxLength={50}
								data-flx="user.component-gallery-tab.inputs-tab.inline-edit"
							/>
						</div>
					</SettingsTabSection>
					<SettingsTabSection
						title={<Trans>Color pickers</Trans>}
						description={<Trans>Click to open the color picker and choose a new color.</Trans>}
						data-flx="user.component-gallery-tab.inputs-tab.settings-tab-section--6"
					>
						<div
							className={styles.colorPickersGrid}
							data-flx="user.component-gallery-tab.inputs-tab.color-pickers-grid"
						>
							<ColorPickerField
								label={i18n._(PRIMARY_ACCENT_COLOR_DESCRIPTOR)}
								value={color}
								onChange={setColor}
								data-flx="user.component-gallery-tab.inputs-tab.color-picker-field.set-color"
							/>
							<ColorPickerField
								label={i18n._(SECONDARY_ACCENT_COLOR_DESCRIPTOR)}
								value={color2}
								onChange={setColor2}
								data-flx="user.component-gallery-tab.inputs-tab.color-picker-field.set-color2"
							/>
						</div>
					</SettingsTabSection>
				</SettingsTabContent>
			</SettingsTabContainer>
		);
	},
);
