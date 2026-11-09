// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {HelpCenterArticleSlug} from '@app/features/app/config/HelpCenterConstants';
import {I18N_WEBLATE_DOMAIN, I18N_WEBLATE_URL} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import SystemLocale from '@app/features/i18n/state/SystemLocale';
import {getCachedCollator, getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import Spellcheck from '@app/features/messaging/state/Spellcheck';
import {Button} from '@app/features/ui/button/Button';
import {Combobox, type ComboboxFilterOption, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as HelpCenterUtils from '@app/features/ui/utils/HelpCenterUtils';
import * as NativeUtils from '@app/features/ui/utils/NativeUtils';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import styles from '@app/features/user/components/modals/tabs/LanguageTab.module.css';
import {
	DATE_FORMAT_DESCRIPTOR,
	REGIONAL_FORMAT_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import UserSettings from '@app/features/user/state/UserSettings';
import {resolveHour12} from '@app/features/user/utils/DateFormatting';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {
	INTERFACE_LANGUAGE_DESCRIPTOR,
	PERSONAL_DICTIONARY_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import type {SpellcheckEngine} from '@app/types/electron.d';
import {REGIONAL_FORMAT_SYSTEM, RegionalFormatLocales, resolveFormattingLocale} from '@fluxer/constants/src/Locales';
import {TimeFormatTypes, TimestampFormatTypes} from '@fluxer/constants/src/UserConstants';
import {formatPresetDateTime, toDateFormatPreset} from '@fluxer/date_utils/src/DateFormatPresets';
import {getFormattedDateTime, getFormattedTime} from '@fluxer/date_utils/src/DateFormatting';
import {msg, ph} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {TranslateIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type ReactElement, useCallback, useMemo, useState} from 'react';

const LANGUAGE_FLAG_DESCRIPTOR = msg({message: '{languageName} flag'});
const MISSING_LANGUAGE_DESCRIPTOR = msg({
	message: "Don't see your language?",
	comment:
		'Link under the Weblate line in the language settings. It opens a help article on fluxer.com about suggesting a new language and helping translate the app. Keep it a short question.',
});

const REGIONAL_FORMAT_EXAMPLE_DESCRIPTOR = msg({
	message: 'Regional format: {format}',
	comment:
		'Description of the automatic choice in the time format and date format settings. {format} is an example time or date written in the chosen regional format. Preserve {format}.',
});
const REGIONAL_FORMAT_HELP_DESCRIPTOR = msg({
	message: 'Dates, times and numbers follow this format. Prices stay in the currency of your billing country.',
	comment: 'Help text under the regional format picker in the language settings.',
});
const SAME_AS_LANGUAGE_DESCRIPTOR = msg({
	message: 'Same as language',
	comment:
		'Regional format option that writes dates, times and numbers the way the interface language does. Keep it concise.',
});
const SYSTEM_FORMAT_DESCRIPTOR = msg({
	message: 'System format',
	comment:
		'Regional format option that uses the date, time and number settings of the device or browser. Keep it concise.',
});
const SEARCH_REGIONAL_FORMATS_DESCRIPTOR = msg({
	message: 'Search formats…',
	comment: 'Placeholder in the regional format picker in the language settings. Keep it concise.',
});
const SELECT_REGIONAL_FORMAT_DESCRIPTOR = msg({
	message: 'Select regional format',
	comment: 'Accessible label for the regional format picker in the language settings. Keep it concise.',
});
const DATE_FORMAT_SELECTION_DESCRIPTOR = msg({
	message: 'Date format selection',
	comment: 'Accessible label for the date format choices in the language settings. Keep it concise.',
});
const ISO_8601_DESCRIPTOR = msg({
	message: 'ISO 8601',
	comment: 'Date format option named after the international standard. Keep "ISO 8601" as is.',
});
const DAY_FIRST_DESCRIPTOR = msg({
	message: 'Day first',
	comment: 'Date format option that writes day/month/year. Keep it concise.',
});
const MONTH_FIRST_DESCRIPTOR = msg({
	message: 'Month first',
	comment: 'Date format option that writes month/day/year. Keep it concise.',
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
const SEARCH_LANGUAGES_DESCRIPTOR = msg({
	message: 'Search languages…',
	comment: 'Button or menu action label in the language tab. Keep it concise.',
});
const SELECT_INTERFACE_LANGUAGE_DESCRIPTOR = msg({
	message: 'Select interface language',
	comment: 'Button or menu action label in the language tab. Keep it concise.',
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
		"Use your operating system's spellchecker when available. Otherwise, use {productName}'s in-app dictionaries.",
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

const REGIONAL_FORMAT_LANGUAGE = 'language';
const FORMAT_SAMPLE_DATE = new Date(2026, 10, 24, 14, 30, 0);
const FORMAT_SAMPLE_NUMBER = 1234.56;

interface RegionalFormatSelectOption extends ComboboxOption<string> {
	name: string;
	sample: string;
	searchText: string;
}

function getRegionalFormatSample(locale: string): string {
	const date = getFormattedDateTime(FORMAT_SAMPLE_DATE, locale);
	const number = getCachedNumberFormat(locale, {minimumFractionDigits: 2}).format(FORMAT_SAMPLE_NUMBER);
	return `${date} · ${number}`;
}

interface LanguageSelectOption extends ComboboxOption<string> {
	code: string;
	name: string;
	nativeName: string;
	flag: string | null;
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

function renderLanguageMark(flag: string | null, flagLabel: string): ReactElement {
	if (flag === null) {
		return (
			<TranslateIcon
				aria-hidden={true}
				className={styles.languageMark}
				data-flx="user.language-selector.render-language-content.language-mark"
			/>
		);
	}
	const flagUrl = EmojiUtils.getEmojiURL(flag);
	if (flagUrl) {
		return (
			<img
				src={flagUrl}
				alt={flagLabel}
				aria-hidden={true}
				className={styles.flagImage}
				draggable={false}
				data-flx="user.language-selector.render-language-content.flag-image"
			/>
		);
	}
	return (
		<span
			className={styles.flagImageText}
			role="img"
			aria-label={flagLabel}
			data-flx="user.language-selector.render-language-content.flag-image-text"
		>
			{flag}
		</span>
	);
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
	const renderLanguageContent = useCallback(
		(option: LanguageSelectOption, selected: boolean, compact = false) => {
			const isEnGB = option.code === 'en-GB';
			const flagImg = renderLanguageMark(option.flag, i18n._(LANGUAGE_FLAG_DESCRIPTOR, {languageName: option.name}));
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
						<span
							className={styles.languageCode}
							data-flx="user.language-selector.render-language-content.language-code"
						>
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
											<Trans>For British eyes only...</Trans>
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
		},
		[i18n],
	);
	const filterLanguageOption = useCallback((option: ComboboxFilterOption<LanguageSelectOption>, inputValue: string) => {
		const query = normalizeLanguageSearchText(inputValue.trim());
		return query.length === 0 || option.data.searchText.includes(query);
	}, []);
	return (
		<Combobox<string, LanguageSelectOption>
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

const RegionalFormatSelector = observer(function RegionalFormatSelector() {
	const {i18n} = useLingui();
	const regionalFormat = UserSettings.regionalFormat;
	const value = regionalFormat ?? REGIONAL_FORMAT_LANGUAGE;
	const languageLocale = resolveFormattingLocale(LocaleUtils.getCurrentLocale());
	const systemLocale = resolveFormattingLocale(SystemLocale.locale);
	const options = useMemo<ReadonlyArray<RegionalFormatSelectOption>>(() => {
		const displayLocale = resolveFormattingLocale(i18n.locale);
		const displayNames = new Intl.DisplayNames([displayLocale], {type: 'language'});
		const collator = getCachedCollator(displayLocale, {sensitivity: 'base'});
		const toOption = (code: string): RegionalFormatSelectOption => {
			const name = displayNames.of(code) ?? code;
			return {
				value: code,
				label: name,
				name,
				sample: getRegionalFormatSample(code),
				searchText: normalizeLanguageSearchText(`${name} ${code}`),
			};
		};
		const codes =
			regionalFormat === null ||
			regionalFormat === REGIONAL_FORMAT_SYSTEM ||
			RegionalFormatLocales.includes(regionalFormat)
				? RegionalFormatLocales
				: [...RegionalFormatLocales, regionalFormat];
		const regions = codes.map(toOption).sort((a, b) => collator.compare(a.name, b.name));
		const sameAsLanguage = i18n._(SAME_AS_LANGUAGE_DESCRIPTOR);
		const systemFormat = i18n._(SYSTEM_FORMAT_DESCRIPTOR);
		return [
			{
				value: REGIONAL_FORMAT_LANGUAGE,
				label: sameAsLanguage,
				name: sameAsLanguage,
				sample: getRegionalFormatSample(languageLocale),
				searchText: normalizeLanguageSearchText(sameAsLanguage),
			},
			{
				value: REGIONAL_FORMAT_SYSTEM,
				label: systemFormat,
				name: systemFormat,
				sample: getRegionalFormatSample(systemLocale),
				searchText: normalizeLanguageSearchText(`${systemFormat} ${SystemLocale.locale}`),
			},
			...regions,
		];
	}, [i18n, i18n.locale, languageLocale, regionalFormat, systemLocale]);
	const renderRegionalFormat = useCallback(
		(option: RegionalFormatSelectOption, selected: boolean, compact = false) => (
			<div
				className={clsx(
					styles.languageOption,
					selected && styles.languageOptionSelected,
					!compact && styles.languageOptionMenu,
					compact && styles.languageOptionCompact,
				)}
				data-flx="user.regional-format-selector.option"
			>
				<span className={styles.languageName} data-flx="user.regional-format-selector.option-name">
					{option.name}
				</span>
				<div className={styles.languageDetails} data-flx="user.regional-format-selector.option-details">
					<span className={styles.languageCode} data-flx="user.regional-format-selector.option-sample">
						{option.sample}
					</span>
				</div>
			</div>
		),
		[],
	);
	const filterRegionalFormat = useCallback(
		(option: ComboboxFilterOption<RegionalFormatSelectOption>, inputValue: string) => {
			const query = normalizeLanguageSearchText(inputValue.trim());
			return query.length === 0 || option.data.searchText.includes(query);
		},
		[],
	);
	return (
		<Combobox<string, RegionalFormatSelectOption>
			options={options}
			value={value}
			onChange={(next) =>
				UserSettingsCommands.update({regionalFormat: next === REGIONAL_FORMAT_LANGUAGE ? null : next})
			}
			renderOption={(option, selected) => renderRegionalFormat(option, selected)}
			renderValue={(option) => (option ? renderRegionalFormat(option, false, true) : null)}
			filterOption={filterRegionalFormat}
			placeholder={i18n._(SEARCH_REGIONAL_FORMATS_DESCRIPTOR)}
			isSearchable={true}
			openMenuOnFocus={true}
			density="compact"
			className={styles.languageSelect}
			aria-label={i18n._(SELECT_REGIONAL_FORMAT_DESCRIPTOR)}
			data-flx="user.regional-format-selector.combobox"
		/>
	);
});

const LanguageTab = observer(() => {
	const {i18n} = useLingui();
	const currentLocale = LocaleUtils.getCurrentLocale();
	const {timeFormat, timestampFormat} = UserSettings;
	const formattingLocale = LocaleUtils.getFormattingLocale();
	const autoTime = getFormattedTime(
		FORMAT_SAMPLE_DATE,
		formattingLocale,
		resolveHour12(formattingLocale, TimeFormatTypes.AUTO, timestampFormat),
	);
	const timeFormatOptions: ReadonlyArray<RadioOption<number>> = [
		{
			value: TimeFormatTypes.AUTO,
			name: i18n._(AUTO_DESCRIPTOR),
			desc: i18n._(REGIONAL_FORMAT_EXAMPLE_DESCRIPTOR, {format: autoTime}),
		},
		{
			value: TimeFormatTypes.TWELVE_HOUR,
			name: i18n._(MESSAGE_12_HOUR_DESCRIPTOR),
			desc: getFormattedTime(FORMAT_SAMPLE_DATE, formattingLocale, true),
		},
		{
			value: TimeFormatTypes.TWENTY_FOUR_HOUR,
			name: i18n._(MESSAGE_24_HOUR_DESCRIPTOR),
			desc: getFormattedTime(FORMAT_SAMPLE_DATE, formattingLocale, false),
		},
	];
	const getPresetExample = (format: number): string => {
		const hour12 = resolveHour12(formattingLocale, timeFormat, format);
		const preset = toDateFormatPreset(format);
		return preset === null
			? getFormattedDateTime(FORMAT_SAMPLE_DATE, formattingLocale, hour12)
			: formatPresetDateTime(FORMAT_SAMPLE_DATE, preset, formattingLocale, hour12);
	};
	const dateFormatOptions: ReadonlyArray<RadioOption<number>> = [
		{
			value: TimestampFormatTypes.AUTO,
			name: i18n._(AUTO_DESCRIPTOR),
			desc: i18n._(REGIONAL_FORMAT_EXAMPLE_DESCRIPTOR, {format: getPresetExample(TimestampFormatTypes.AUTO)}),
		},
		{
			value: TimestampFormatTypes.ISO_8601,
			name: i18n._(ISO_8601_DESCRIPTOR),
			desc: getPresetExample(TimestampFormatTypes.ISO_8601),
		},
		{
			value: TimestampFormatTypes.DAY_MONTH_YEAR,
			name: i18n._(DAY_FIRST_DESCRIPTOR),
			desc: getPresetExample(TimestampFormatTypes.DAY_MONTH_YEAR),
		},
		{
			value: TimestampFormatTypes.MONTH_DAY_YEAR,
			name: i18n._(MONTH_FIRST_DESCRIPTOR),
			desc: getPresetExample(TimestampFormatTypes.MONTH_DAY_YEAR),
		},
	];
	const translateHelpUrl = HelpCenterUtils.getURL(HelpCenterArticleSlug.TranslateFluxer);
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
									Help translate {ph({PRODUCT_NAME: RuntimeConfig.productName})} into your language on{' '}
									<ExternalLink href={I18N_WEBLATE_URL} className={styles.link} data-flx="user.language-tab.link">
										{I18N_WEBLATE_DOMAIN}
									</ExternalLink>
									.
								</Trans>
							</p>
							{translateHelpUrl && (
								<p className={styles.noticeText} data-flx="user.language-tab.missing-language">
									<ExternalLink
										href={translateHelpUrl}
										className={styles.link}
										data-flx="user.language-tab.missing-language-link"
									>
										{i18n._(MISSING_LANGUAGE_DESCRIPTOR)}
									</ExternalLink>
								</p>
							)}
						</div>
					</div>
				</SettingsSection>
				<SettingsSection
					id="regional-format"
					title={i18n._(REGIONAL_FORMAT_DESCRIPTOR)}
					data-flx="user.language-tab.settings-tab-section.regional-format"
				>
					<div className={styles.languageControls} data-flx="user.language-tab.regional-format-controls">
						<RegionalFormatSelector data-flx="user.language-tab.regional-format-selector" />
						<p className={styles.noticeText} data-flx="user.language-tab.regional-format-help">
							{i18n._(REGIONAL_FORMAT_HELP_DESCRIPTOR)}
						</p>
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
					</div>
				</SettingsSection>
				<SettingsSection
					id="date-format"
					title={i18n._(DATE_FORMAT_DESCRIPTOR)}
					data-flx="user.language-tab.settings-tab-section.date-format"
				>
					<RadioGroup
						options={dateFormatOptions}
						value={timestampFormat}
						onChange={(value) => UserSettingsCommands.update({timestampFormat: value})}
						aria-label={i18n._(DATE_FORMAT_SELECTION_DESCRIPTOR)}
						data-flx="user.language-tab.radio-group.date-format"
					/>
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
			desc: i18n._(USE_THE_IN_APP_HUNSPELL_ENGINE_WHEN_A_DESCRIPTOR, {productName: RuntimeConfig.productName}),
		},
		{
			value: 'hunspell',
			name: i18n._(IN_APP_HUNSPELL_DESCRIPTOR),
			desc: i18n._(ALWAYS_USE_S_HUNSPELL_DICTIONARIES_DOWNLOADED_ON_DEMAND_DESCRIPTOR, {
				productName: RuntimeConfig.productName,
			}),
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
								Reload {ph({PRODUCT_NAME: RuntimeConfig.productName})} to fully apply the engine change. Switching
								between in-app and system spellcheck requires a renderer reload because Electron can't swap providers
								while running.
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
											In "Operating system" mode, dictionaries listed above are advisory.{' '}
											{ph({PRODUCT_NAME: RuntimeConfig.productName})} will pass these language tags to your OS
											spellchecker when supported. Tags it doesn't recognize are ignored.
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
