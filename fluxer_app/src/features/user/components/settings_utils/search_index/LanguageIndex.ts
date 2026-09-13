// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {msg} from '@lingui/core/macro';

const LANGUAGE_DESCRIPTOR = msg({
	message: 'Language',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const LOCALE_DESCRIPTOR = msg({
	message: 'Locale',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TRANSLATION_DESCRIPTOR = msg({
	message: 'Translation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LOCALIZATION_DESCRIPTOR = msg({
	message: 'Localization',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_YOUR_LANGUAGE_DESCRIPTOR = msg({
	message: 'Choose your language',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const TIME_FORMAT_DESCRIPTOR = msg({
	message: 'Time format',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const TIME_DESCRIPTOR = msg({
	message: 'Time',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TWELVE_HOUR_DESCRIPTOR = msg({
	message: 'Twelve-hour',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TWENTY_FOUR_HOUR_DESCRIPTOR = msg({
	message: 'Twenty-four-hour',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MORNING_TIME_DESCRIPTOR = msg({
	message: 'Morning time',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EVENING_TIME_DESCRIPTOR = msg({
	message: 'Evening time',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CLOCK_DESCRIPTOR = msg({
	message: 'Clock',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MILITARY_TIME_DESCRIPTOR = msg({
	message: 'Military time',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYSTEM_TIME_FORMAT_DESCRIPTOR = msg({
	message: 'System time format',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BROWSER_TIME_FORMAT_DESCRIPTOR = msg({
	message: 'Browser time format',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LOCALE_TIME_FORMAT_DESCRIPTOR = msg({
	message: 'Locale time format',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_HOW_TIMES_ARE_DISPLAYED_THROUGHOUT_THE_APP_DESCRIPTOR = msg({
	message: 'Choose how times are displayed throughout the app',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const SPELLCHECK_DESCRIPTOR = msg({
	message: 'Spellcheck',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const SPELL_CHECK_DESCRIPTOR = msg({
	message: 'Spellcheck',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SPELLING_DESCRIPTOR = msg({
	message: 'Spelling',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DICTIONARY_DESCRIPTOR = msg({
	message: 'Dictionary',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DICTIONARIES_DESCRIPTOR = msg({
	message: 'Dictionaries',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PERSONAL_DICTIONARY_DESCRIPTOR = msg({
	message: 'Personal dictionary',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTOMATIC_LANGUAGE_DETECTION_DESCRIPTOR = msg({
	message: 'Automatic language detection',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MISSPELLED_WORDS_DESCRIPTOR = msg({
	message: 'Misspelled words',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONFIGURE_SPELLCHECK_DICTIONARIES_AND_PERSONAL_WORDS_DESCRIPTOR = msg({
	message: 'Configure spellcheck, dictionaries, and personal words',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const languageIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'language-locale',
		tabType: 'language',
		sectionId: 'language-settings',
		label: LANGUAGE_DESCRIPTOR,
		keywords: [LANGUAGE_DESCRIPTOR, LOCALE_DESCRIPTOR, TRANSLATION_DESCRIPTOR, LOCALIZATION_DESCRIPTOR],
		description: CHOOSE_YOUR_LANGUAGE_DESCRIPTOR,
	},
	{
		id: 'language-time-format',
		tabType: 'language',
		sectionId: 'time-format',
		label: TIME_FORMAT_DESCRIPTOR,
		keywords: [
			TIME_FORMAT_DESCRIPTOR,
			TIME_DESCRIPTOR,
			TWELVE_HOUR_DESCRIPTOR,
			TWENTY_FOUR_HOUR_DESCRIPTOR,
			MORNING_TIME_DESCRIPTOR,
			EVENING_TIME_DESCRIPTOR,
			CLOCK_DESCRIPTOR,
			MILITARY_TIME_DESCRIPTOR,
			SYSTEM_TIME_FORMAT_DESCRIPTOR,
			BROWSER_TIME_FORMAT_DESCRIPTOR,
			LOCALE_TIME_FORMAT_DESCRIPTOR,
		],
		description: CHOOSE_HOW_TIMES_ARE_DISPLAYED_THROUGHOUT_THE_APP_DESCRIPTOR,
	},
	{
		id: 'language-spellcheck',
		tabType: 'language',
		sectionId: 'spellcheck',
		label: SPELLCHECK_DESCRIPTOR,
		keywords: [
			SPELLCHECK_DESCRIPTOR,
			SPELL_CHECK_DESCRIPTOR,
			SPELLING_DESCRIPTOR,
			DICTIONARY_DESCRIPTOR,
			DICTIONARIES_DESCRIPTOR,
			PERSONAL_DICTIONARY_DESCRIPTOR,
			AUTOMATIC_LANGUAGE_DETECTION_DESCRIPTOR,
			MISSPELLED_WORDS_DESCRIPTOR,
		],
		description: CONFIGURE_SPELLCHECK_DICTIONARIES_AND_PERSONAL_WORDS_DESCRIPTOR,
	},
];
