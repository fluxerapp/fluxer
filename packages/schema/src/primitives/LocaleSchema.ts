// SPDX-License-Identifier: AGPL-3.0-or-later

import {REGIONAL_FORMAT_PATTERN} from '@fluxer/constants/src/Locales';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {createNamedStringLiteralUnion, withOpenApiType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const LocaleSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['ar', 'AR', 'Arabic'],
			['be', 'BE', 'Belarusian'],
			['bg', 'BG', 'Bulgarian'],
			['bn', 'BN', 'Bangla'],
			['ca', 'CA', 'Catalan'],
			['cs', 'CS', 'Czech'],
			['da', 'DA', 'Danish'],
			['de', 'DE', 'German'],
			['el', 'EL', 'Greek'],
			['en-GB', 'EN_GB', 'English (United Kingdom)'],
			['en-US', 'EN_US', 'English (United States)'],
			['es-ES', 'ES_ES', 'Spanish (Spain)'],
			['es-419', 'ES_419', 'Spanish (Latin America)'],
			['fi', 'FI', 'Finnish'],
			['fil', 'FIL', 'Filipino'],
			['fr', 'FR', 'French'],
			['he', 'HE', 'Hebrew'],
			['hi', 'HI', 'Hindi'],
			['hr', 'HR', 'Croatian'],
			['hu', 'HU', 'Hungarian'],
			['id', 'ID', 'Indonesian'],
			['it', 'IT', 'Italian'],
			['ja', 'JA', 'Japanese'],
			['ko', 'KO', 'Korean'],
			['lt', 'LT', 'Lithuanian'],
			['lv', 'LV', 'Latvian'],
			['nl', 'NL', 'Dutch'],
			['no', 'NO', 'Norwegian'],
			['pl', 'PL', 'Polish'],
			['pt-BR', 'PT_BR', 'Portuguese (Brazil)'],
			['pt-PT', 'PT_PT', 'Portuguese (Portugal)'],
			['ro', 'RO', 'Romanian'],
			['ru', 'RU', 'Russian'],
			['sk', 'SK', 'Slovak'],
			['sl', 'SL', 'Slovenian'],
			['sq', 'SQ', 'Albanian'],
			['sv-SE', 'SV_SE', 'Swedish'],
			['th', 'TH', 'Thai'],
			['tok', 'TOK', 'Toki Pona'],
			['tr', 'TR', 'Turkish'],
			['uk', 'UK', 'Ukrainian'],
			['vi', 'VI', 'Vietnamese'],
			['zh-CN', 'ZH_CN', 'Chinese (Simplified)'],
			['zh-TW', 'ZH_TW', 'Chinese (Traditional)'],
		] as const,
		'The locale code for the user interface language',
	),
	'Locale',
);

export const RegionalFormatSchema = z
	.string()
	.regex(REGIONAL_FORMAT_PATTERN, ValidationErrorCodes.INVALID_FORMAT)
	.nullable()
	.describe(
		'The locale used to write dates, times and numbers, such as de-DE, or "system" for the device locale. Null follows the interface language. Currency follows the billing country either way.',
	);
