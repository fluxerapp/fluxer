// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {I18N_WEBLATE_DOMAIN, I18N_WEBLATE_URL, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import Spellcheck from '@app/features/messaging/state/Spellcheck';
import type {SpellcheckEngine} from '@app/features/platform/types/Electron';
import {Button} from '@app/features/ui/button/Button';
import {Combobox, type ComboboxFilterOption, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as NativeUtils from '@app/features/ui/utils/NativeUtils';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import styles from '@app/features/user/components/modals/tabs/LanguageTab.module.css';
import UserSettings from '@app/features/user/state/UserSettings';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {TimeFormatTypes} from '@fluxer/constants/src/UserConstants';
import {getFormattedTime} from '@fluxer/date_utils/src/DateFormatting';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useState} from 'react';

const SYSTEM_LOCALE_DESCRIPTOR = msg({
	message: 'System locale: {format}',
	comment: 'Label in the language tab. Preserve {format}.',
});
const BROWSER_LOCALE_DESCRIPTOR = msg({
	message: 'Browser locale: {format}',
	comment: 'Label in the language tab. Preserve {format}.',
});
const APP_LANGUAGE_DESCRIPTOR = msg({
	message: 'App language: {format}',
	comment: 'Label in the language tab. Preserve {format}.',
});
const AUTO_DESCRIPTOR = msg({
	message: 'Auto',
	comment: 'Short label in the language tab. Keep it concise.',
});
const MESSAGE_12_HOUR_DESCRIPTOR = msg({
	message: '12-hour',
	comment: 'Short label in the language tab. Keep it concise.',
});
const MESSAGE_24_HOUR_DESCRIPTOR = msg({
	message: '24-hour',
	comment: 'Short label in the language tab. Keep it concise.',
});
const TIME_FORMAT_SELECTION_DESCRIPTOR = msg({
	message: 'Time format selection',
	comment: 'Short label in the language tab. Keep it concise.',
});
const USE_SYSTEM_LOCALE_FOR_TIME_FORMAT_DESCRIPTOR = msg({
	message: 'Use system locale for time format',
	comment: 'Label in the language tab.',
});
const USE_BROWSER_LOCALE_FOR_TIME_FORMAT_DESCRIPTOR = msg({
	message: 'Use browser locale for time format',
	comment: 'Label in the language tab.',
});
const SEARCH_LANGUAGES_DESCRIPTOR = msg({
	message: 'Search languages…',
	comment: 'Button or menu action label in the language tab. Keep it concise.',
});
const SELECT_INTERFACE_LANGUAGE_DESCRIPTOR = msg({
	message: 'Select interface language',
	comment: 'Button or menu action label in the language tab. Keep it concise.',
});
const INTERFACE_LANGUAGE_DESCRIPTOR = msg({
	message: 'Interface language',
	comment: 'Settings section title in the language tab. Keep it concise.',
});
const ENABLE_SPELLCHECK_DESCRIPTOR = msg({
	message: 'Enable spellcheck',
	comment: 'Button or menu action label in the language tab. Keep it concise.',
});
const RECOMMENDED_DESCRIPTOR = msg({
	message: 'Recommended',
	comment: 'Spellcheck engine option in the language tab. Lets the app choose the best available spellcheck engine.',
});
const USE_THE_IN_APP_HUNSPELL_ENGINE_WHEN_A_DESCRIPTOR = msg({
	message:
		"Use {productName}'s in-app dictionaries when available. Otherwise, use your operating system's spellchecker.",
	comment:
		'Description for the recommended spellcheck engine option in the language tab. Preserve {productName}; it is inserted by code and must appear verbatim in the translation.',
});
const IN_APP_HUNSPELL_DESCRIPTOR = msg({
	message: 'In-app dictionaries',
	comment:
		'Spellcheck engine option in the language tab. Uses the bundled in-app Hunspell dictionaries. Keep it concise.',
});
const ALWAYS_USE_S_HUNSPELL_DICTIONARIES_DOWNLOADED_ON_DEMAND_DESCRIPTOR = msg({
	message: "Always use {productName}'s Hunspell dictionaries, downloaded on demand.",
	comment:
		'Description for the in-app dictionary spellcheck engine option in the language tab. Preserve {productName}; it is inserted by code and must appear verbatim in the translation. Hunspell is the spellcheck engine name; keep it as a product/protocol term.',
});
const OPERATING_SYSTEM_DESCRIPTOR = msg({
	message: 'Operating system',
	comment: 'Short label in the language tab. Keep it concise.',
});
const USE_THE_OS_LEVEL_SPELLCHECKER_NSSPELLCHECKER_ON_MACOS_DESCRIPTOR = msg({
	message: 'Use the spellchecker built into your operating system.',
	comment: 'Description for the operating-system spellcheck engine option in the language tab.',
});
const SPELLCHECK_ENGINE_DESCRIPTOR = msg({
	message: 'Spellcheck engine',
	comment: 'Short label in the language tab. Keep it concise.',
});
const SPELLCHECK_LANGUAGES_DESCRIPTOR = msg({
	message: 'Languages',
	comment: 'Subsection title for spellcheck language settings. Keep it concise.',
});
const AUTO_DETECT_LANGUAGE_DESCRIPTOR = msg({
	message: 'Detect while typing',
	comment:
		'Spellcheck language mode label in the language tab. The app detects the draft message language and switches dictionaries as the user types.',
});
const PERSONAL_DICTIONARY_DESCRIPTOR = msg({
	message: 'Personal dictionary',
	comment: 'Subsection title for personal dictionary settings. Keep it concise.',
});
const PICK_ONE_OR_MORE_DICTIONARIES_DESCRIPTOR = msg({
	message:
		'Pick one or more dictionaries. Words from any selected dictionary are accepted, which is useful for multilingual chats.',
	comment: 'Help text above the manual spellcheck dictionary picker in the language tab.',
});
const ADD_A_WORD_DESCRIPTOR = msg({
	message: 'Add a word…',
	comment: 'Button or menu action label in the language tab. Keep it concise.',
});
const REMOVE_FROM_DICTIONARY_DESCRIPTOR = msg({
	message: 'Remove "{word}" from dictionary',
	comment:
		'Button or menu action label in the language tab. Keep it concise. Preserve {word}; it is inserted by code. Keep the tone plain and specific.',
});

interface LanguageSelectOption extends ComboboxOption<string> {
	code: string;
	name: string;
	nativeName: string;
	flag: string;
	searchText: string;
}

function normalizeLanguageSearchText(value: string): string {
	return value
		.normalize('NFD')
		.replace(/[\u0300-\u036f]/g, '')
		.toLowerCase();
}

interface LanguageSelectorProps {
	value: string;
	onChange: (locale: string) => void;
	className?: string;
	openMenuOnFocus?: boolean;
	maxMenuHeight?: number | null;
	menuPlacement?: 'auto' | 'bottom' | 'top';
}

export const LanguageSelector = observer(function LanguageSelector({
	value,
	onChange,
	className,
	openMenuOnFocus = true,
	maxMenuHeight = null,
	menuPlacement = 'auto',
}: LanguageSelectorProps) {
	const {i18n} = useLingui();
	const availableLocales = LocaleUtils.getSortedLocales();
	const localeOptions = useMemo<ReadonlyArray<LanguageSelectOption>>(
		() =>
			availableLocales.map((locale) => ({
				value: locale.code,
				label: `${locale.nativeName} ${locale.name}`,
				code: locale.code,
				name: locale.name,
				nativeName: locale.nativeName,
				flag: locale.flag,
				searchText: normalizeLanguageSearchText(`${locale.nativeName} ${locale.name} ${locale.code}`),
			})),
		[availableLocales],
	);
	const renderLanguageContent = useCallback((option: LanguageSelectOption, selected: boolean, compact = false) => {
		const isEnGB = option.code === 'en-GB';
		const flagUrl = EmojiUtils.getEmojiURL(option.flag);
		const flagImg = flagUrl ? (
			<img
				src={flagUrl}
				alt={`${option.name} flag`}
				aria-hidden={true}
				className={styles.flagImage}
				draggable={false}
				data-flx="user.language-selector.render-language-content.flag-image"
			/>
		) : (
			<span
				className={styles.flagImageText}
				role="img"
				aria-label={`${option.name} flag`}
				data-flx="user.language-selector.render-language-content.flag-image-text"
			>
				{option.flag}
			</span>
		);
		return (
			<div
				className={clsx(
					styles.languageOption,
					selected && styles.languageOptionSelected,
					!compact && styles.languageOptionMenu,
					compact && styles.languageOptionCompact,
				)}
				data-flx="user.language-selector.render-language-content.language-option"
			>
				<span className={styles.languageName} data-flx="user.language-selector.render-language-content.language-name">
					{option.nativeName}
				</span>
				<div
					className={styles.languageDetails}
					data-flx="user.language-selector.render-language-content.language-details"
				>
					<span className={styles.languageCode} data-flx="user.language-selector.render-language-content.language-code">
						{option.name}
					</span>
					{isEnGB ? (
						<Tooltip
							text={() => (
								<span
									className={styles.tooltipContent}
									data-flx="user.language-selector.render-language-content.tooltip-content"
								>
									<span
										className={styles.tooltipText}
										data-flx="user.language-selector.render-language-content.tooltip-text"
									>
										<Trans>For british eyes only...</Trans>
									</span>
								</span>
							)}
							data-flx="user.language-selector.render-language-content.tooltip"
						>
							{flagImg}
						</Tooltip>
					) : (
						flagImg
					)}
				</div>
			</div>
		);
	}, []);
	const filterLanguageOption = useCallback((option: ComboboxFilterOption<LanguageSelectOption>, inputValue: string) => {
		const query = normalizeLanguageSearchText(inputValue.trim());
		return query.length === 0 || option.data.searchText.includes(query);
	}, []);
	return (
		<Combobox<string, false, LanguageSelectOption>
			options={localeOptions}
			value={value}
			onChange={onChange}
			renderOption={(option, selected) => renderLanguageContent(option, selected)}
			renderValue={(option) => (option ? renderLanguageContent(option, false, true) : null)}
			filterOption={filterLanguageOption}
			placeholder={i18n._(SEARCH_LANGUAGES_DESCRIPTOR)}
			isSearchable={true}
			openMenuOnFocus={openMenuOnFocus}
			density="compact"
			maxMenuHeight={maxMenuHeight}
			menuPlacement={menuPlacement}
			className={clsx(styles.languageSelect, className)}
			aria-label={i18n._(SELECT_INTERFACE_LANGUAGE_DESCRIPTOR)}
			data-flx="user.language-selector.locale-change"
		/>
	);
});

const LanguageTab = observer(() => {
	const {i18n} = useLingui();
	const currentLocale = LocaleUtils.getCurrentLocale();
	const {timeFormat} = UserSettings;
	const isDesktop = NativeUtils.isDesktop();
	const getAutoTimeFormatDescription = () => {
		const appLocale = UserSettings.getLocale();
		const browserLocale = navigator.language;
		const effectiveLocale = Accessibility.useBrowserLocaleForTimeFormat ? browserLocale : appLocale;
		const localeUses12Hour = (locale: string): boolean => {
			const lang = locale.toLowerCase();
			const twelveHourLocales = [
				'en-us',
				'en-ca',
				'en-au',
				'en-nz',
				'en-ph',
				'en-in',
				'en-pk',
				'en-bd',
				'en-za',
				'es-mx',
				'es-co',
				'ar',
				'hi',
				'bn',
				'ur',
				'fil',
				'tl',
			];
			return twelveHourLocales.some((l) => lang.startsWith(l));
		};
		const uses12Hour = localeUses12Hour(effectiveLocale);
		const sampleDate = new Date(2025, 0, 1, 14, 30, 0);
		const format = getFormattedTime(sampleDate, effectiveLocale, uses12Hour);
		if (Accessibility.useBrowserLocaleForTimeFormat) {
			return isDesktop ? i18n._(SYSTEM_LOCALE_DESCRIPTOR, {format}) : i18n._(BROWSER_LOCALE_DESCRIPTOR, {format});
		}
		return i18n._(APP_LANGUAGE_DESCRIPTOR, {format});
	};
	const get12HourExample = () => {
		const locale = UserSettings.getLocale();
		const sampleDate = new Date(2025, 0, 1, 14, 30, 0);
		return getFormattedTime(sampleDate, locale, true);
	};
	const get24HourExample = () => {
		const locale = UserSettings.getLocale();
		const sampleDate = new Date(2025, 0, 1, 14, 30, 0);
		return getFormattedTime(sampleDate, locale, false);
	};
	const timeFormatOptions: ReadonlyArray<RadioOption<number>> = [
		{value: TimeFormatTypes.AUTO, name: i18n._(AUTO_DESCRIPTOR), desc: getAutoTimeFormatDescription()},
		{value: TimeFormatTypes.TWELVE_HOUR, name: i18n._(MESSAGE_12_HOUR_DESCRIPTOR), desc: get12HourExample()},
		{value: TimeFormatTypes.TWENTY_FOUR_HOUR, name: i18n._(MESSAGE_24_HOUR_DESCRIPTOR), desc: get24HourExample()},
	];
	const handleLocaleChange = (newLocale: string) => {
		LocaleUtils.setLocale(newLocale);
	};
	return (
		<SettingsTabContainer data-flx="user.language-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.language-tab.settings-tab-content">
				<SettingsSection
					id="language-settings"
					title={i18n._(INTERFACE_LANGUAGE_DESCRIPTOR)}
					data-flx="user.language-tab.settings-tab-section.language-settings"
				>
					<div className={styles.languageControls} data-flx="user.language-tab.language-controls">
						<LanguageSelector
							value={currentLocale}
							onChange={handleLocaleChange}
							data-flx="user.language-tab.language-selector.locale-change"
						/>
						<div className={styles.notice} data-flx="user.language-tab.notice">
							<p className={styles.noticeText} data-flx="user.language-tab.notice-text">
								<Trans>
									Help translate {PRODUCT_NAME} into your language on{' '}
									<ExternalLink href={I18N_WEBLATE_URL} className={styles.link} data-flx="user.language-tab.link">
										{I18N_WEBLATE_DOMAIN}
									</ExternalLink>
									.
								</Trans>
							</p>
						</div>
					</div>
				</SettingsSection>
				<SettingsSection
					id="time-format"
					title={<Trans>Time format</Trans>}
					data-flx="user.language-tab.settings-tab-section.time-format"
				>
					<div className={styles.timeControls} data-flx="user.language-tab.time-controls">
						<RadioGroup
							options={timeFormatOptions}
							value={timeFormat}
							onChange={(value) => UserSettingsCommands.update({timeFormat: value})}
							aria-label={i18n._(TIME_FORMAT_SELECTION_DESCRIPTOR)}
							data-flx="user.language-tab.radio-group.update"
						/>
						{timeFormat === TimeFormatTypes.AUTO && (
							<div className={styles.switchWrapper} data-flx="user.language-tab.switch-wrapper">
								<Switch
									label={
										isDesktop
											? i18n._(USE_SYSTEM_LOCALE_FOR_TIME_FORMAT_DESCRIPTOR)
											: i18n._(USE_BROWSER_LOCALE_FOR_TIME_FORMAT_DESCRIPTOR)
									}
									value={Accessibility.useBrowserLocaleForTimeFormat}
									onChange={(value) => AccessibilityCommands.update({useBrowserLocaleForTimeFormat: value})}
									data-flx="user.language-tab.switch.update"
								/>
							</div>
						)}
					</div>
				</SettingsSection>
				<SpellcheckSettingsSection data-flx="user.language-tab.spellcheck-settings-section" />
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});
const SpellcheckSettingsSection = observer(() => {
	const {i18n} = useLingui();
	const isDesktop = NativeUtils.isDesktop();
	const [newWord, setNewWord] = useState('');
	if (!isDesktop) {
		return (
			<SettingsSection
				id="spellcheck"
				title={<Trans>Spellcheck</Trans>}
				data-flx="user.language-tab.spellcheck-settings-section.settings-tab-section"
			>
				<Switch
					label={i18n._(ENABLE_SPELLCHECK_DESCRIPTOR)}
					value={Spellcheck.enabled}
					onChange={(value) => Spellcheck.setEnabled(value)}
					data-flx="user.language-tab.spellcheck-settings-section.switch.set-enabled"
				/>
			</SettingsSection>
		);
	}
	const engineOptions: ReadonlyArray<RadioOption<SpellcheckEngine>> = [
		{
			value: 'auto',
			name: i18n._(RECOMMENDED_DESCRIPTOR),
			desc: i18n._(USE_THE_IN_APP_HUNSPELL_ENGINE_WHEN_A_DESCRIPTOR, {productName: PRODUCT_NAME}),
		},
		{
			value: 'hunspell',
			name: i18n._(IN_APP_HUNSPELL_DESCRIPTOR),
			desc: i18n._(ALWAYS_USE_S_HUNSPELL_DICTIONARIES_DOWNLOADED_ON_DEMAND_DESCRIPTOR, {productName: PRODUCT_NAME}),
		},
		{
			value: 'system',
			name: i18n._(OPERATING_SYSTEM_DESCRIPTOR),
			desc: i18n._(USE_THE_OS_LEVEL_SPELLCHECKER_NSSPELLCHECKER_ON_MACOS_DESCRIPTOR),
		},
	];
	const isHunspellActive =
		Spellcheck.resolvedEngine?.mode === 'hunspell' ||
		(Spellcheck.resolvedEngine === null && (Spellcheck.engine === 'auto' || Spellcheck.engine === 'hunspell'));
	const selectedTagSet = new Set(Spellcheck.languages.map((l) => l.toLowerCase()));
	const handleAddWord = () => {
		const trimmed = newWord.trim();
		if (!trimmed) return;
		Spellcheck.addPersonalWord(trimmed);
		setNewWord('');
	};
	return (
		<SettingsSection
			id="spellcheck"
			title={<Trans>Spellcheck</Trans>}
			data-flx="user.language-tab.spellcheck-settings-section.settings-tab-section--2"
		>
			<Switch
				label={i18n._(ENABLE_SPELLCHECK_DESCRIPTOR)}
				value={Spellcheck.enabled}
				onChange={(value) => Spellcheck.setEnabled(value)}
				data-flx="user.language-tab.spellcheck-settings-section.switch.set-enabled--2"
			/>
			{Spellcheck.enabled && (
				<>
					<SettingsTabSection
						title={i18n._(SPELLCHECK_ENGINE_DESCRIPTOR)}
						data-flx="user.language-tab.spellcheck-settings-section.spellcheck-engine-section"
					>
						<RadioGroup
							options={engineOptions}
							value={Spellcheck.engine}
							onChange={(value) => Spellcheck.setEngine(value)}
							aria-label={i18n._(SPELLCHECK_ENGINE_DESCRIPTOR)}
							data-flx="user.language-tab.spellcheck-settings-section.radio-group.set-engine"
						/>
					</SettingsTabSection>
					{Spellcheck.reloadRequired && (
						<div
							className={styles.spellcheckRestartBanner}
							data-flx="user.language-tab.spellcheck-settings-section.spellcheck-restart-banner"
						>
							<Trans>
								Reload {PRODUCT_NAME} to fully apply the engine change. (switching between in-app and system spellcheck
								requires a renderer reload because Electron can't swap providers in-flight.)
							</Trans>{' '}
							<Button
								small
								variant="secondary"
								onClick={() => {
									if (typeof window !== 'undefined') window.location.reload();
								}}
								data-flx="user.language-tab.spellcheck-settings-section.button"
							>
								<Trans>Reload now</Trans>
							</Button>
						</div>
					)}
					<SettingsTabSection
						title={i18n._(SPELLCHECK_LANGUAGES_DESCRIPTOR)}
						data-flx="user.language-tab.spellcheck-settings-section.spellcheck-languages-section"
					>
						<Switch
							label={i18n._(AUTO_DETECT_LANGUAGE_DESCRIPTOR)}
							value={Spellcheck.autoDetect}
							onChange={(value) => Spellcheck.setAutoDetect(value)}
							data-flx="user.language-tab.spellcheck-settings-section.switch.set-auto-detect"
						/>
						{!Spellcheck.autoDetect && (
							<div
								className={styles.manualDictionaryBlock}
								data-flx="user.language-tab.spellcheck-settings-section.manual-dictionary-block"
							>
								<div
									className={styles.spellcheckHint}
									data-flx="user.language-tab.spellcheck-settings-section.spellcheck-hint"
								>
									{i18n._(PICK_ONE_OR_MORE_DICTIONARIES_DESCRIPTOR)}
								</div>
								<div
									className={styles.dictionaryGrid}
									data-flx="user.language-tab.spellcheck-settings-section.dictionary-grid"
								>
									{Spellcheck.bundledDictionaries.map((dict) => {
										const checked = selectedTagSet.has(dict.tag.toLowerCase());
										return (
											<label
												key={`${dict.package}-${dict.tag}`}
												className={clsx(styles.dictionaryRow, checked && styles.dictionaryRowChecked)}
												data-flx="user.language-tab.spellcheck-settings-section.dictionary-row"
											>
												<input
													type="checkbox"
													checked={checked}
													onChange={() => Spellcheck.toggleLanguage(dict.tag)}
													data-flx="user.language-tab.spellcheck-settings-section.input.toggle-language.checkbox"
												/>
												<span
													className={styles.dictionaryName}
													data-flx="user.language-tab.spellcheck-settings-section.dictionary-name"
												>
													{dict.nativeName}
												</span>
												<span
													className={styles.dictionaryTag}
													data-flx="user.language-tab.spellcheck-settings-section.dictionary-tag"
												>
													{dict.tag}
												</span>
											</label>
										);
									})}
								</div>
								{Spellcheck.engine === 'system' && (
									<div
										className={styles.spellcheckHint}
										data-flx="user.language-tab.spellcheck-settings-section.spellcheck-hint--2"
									>
										<Trans>
											In "Operating system" mode, dictionaries listed above are advisory. {PRODUCT_NAME} will pass these
											language tags to your OS spellchecker when supported. Tags it doesn't recognize are ignored.
										</Trans>
									</div>
								)}
							</div>
						)}
					</SettingsTabSection>
					{isHunspellActive && (
						<SettingsTabSection
							title={i18n._(PERSONAL_DICTIONARY_DESCRIPTOR)}
							data-flx="user.language-tab.spellcheck-settings-section.personal-dictionary-section"
						>
							<div
								className={styles.personalDictAddRow}
								data-flx="user.language-tab.spellcheck-settings-section.personal-dict-add-row"
							>
								<input
									type="text"
									className={styles.personalDictInput}
									placeholder={i18n._(ADD_A_WORD_DESCRIPTOR)}
									value={newWord}
									onChange={(event) => setNewWord(event.target.value)}
									onKeyDown={(event) => {
										if (event.key === 'Enter') {
											event.preventDefault();
											handleAddWord();
										}
									}}
									data-flx="user.language-tab.spellcheck-settings-section.personal-dict-input.set-new-word.text"
								/>
								<Button
									small
									variant="secondary"
									onClick={handleAddWord}
									disabled={!newWord.trim()}
									data-flx="user.language-tab.spellcheck-settings-section.button.add-word"
								>
									<Trans>Add</Trans>
								</Button>
							</div>
							{Spellcheck.personalDictionary.length === 0 ? (
								<div
									className={styles.personalDictEmpty}
									data-flx="user.language-tab.spellcheck-settings-section.personal-dict-empty"
								>
									<Trans>No personal words yet.</Trans>
								</div>
							) : (
								<div
									className={styles.personalDictList}
									data-flx="user.language-tab.spellcheck-settings-section.personal-dict-list"
								>
									{Spellcheck.personalDictionary.map((word) => (
										<span
											key={word}
											className={styles.personalDictChip}
											data-flx="user.language-tab.spellcheck-settings-section.personal-dict-chip"
										>
											{word}
											<button
												type="button"
												className={styles.personalDictChipRemove}
												aria-label={i18n._(REMOVE_FROM_DICTIONARY_DESCRIPTOR, {word})}
												onClick={() => Spellcheck.removePersonalWord(word)}
												data-flx="user.language-tab.spellcheck-settings-section.personal-dict-chip-remove.remove-personal-word.button"
											>
												×
											</button>
										</span>
									))}
								</div>
							)}
						</SettingsTabSection>
					)}
				</>
			)}
		</SettingsSection>
	);
});

export default LanguageTab;
