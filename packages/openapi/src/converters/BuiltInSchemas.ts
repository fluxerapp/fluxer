// SPDX-License-Identifier: AGPL-3.0-or-later
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ErrorCodeToI18nKey} from '@fluxer/errors/src/i18n/ErrorCodeMappings';
import {ERROR_I18N_MESSAGES} from '@fluxer/errors/src/i18n/ErrorI18nMessages';
import type {OpenAPISchemaWithExtensions} from '@fluxer/openapi/src/converters/OpenAPIExtensions';
import type {OpenAPIRef, OpenAPISchema} from '@fluxer/openapi/src/Types';
export const SnowflakeTypeSchema: OpenAPISchema = {
	type: 'string',
	pattern: '^(0|[1-9][0-9]*)$',
	format: 'snowflake',
};
export const SnowflakeTypeRef: OpenAPIRef = {$ref: '#/components/schemas/SnowflakeType'};
export const Int32TypeSchema: OpenAPISchema = {
	type: 'integer',
	minimum: 0,
	maximum: 2147483647,
	format: 'int32',
};
export const Int32TypeRef: OpenAPIRef = {$ref: '#/components/schemas/Int32Type'};
export const Int64TypeSchema: OpenAPISchema = {
	type: 'string',
	format: 'int64',
	pattern: '^-?[0-9]+$',
};
export const Int64TypeRef: OpenAPIRef = {$ref: '#/components/schemas/Int64Type'};
export const Int64StringTypeSchema: OpenAPISchema = {
	type: 'string',
	format: 'int64',
	pattern: '^-?[0-9]+$',
};
export const Int64StringTypeRef: OpenAPIRef = {$ref: '#/components/schemas/Int64StringType'};
export const UnsignedInt64TypeSchema: OpenAPISchema = {
	type: 'string',
	format: 'int64',
	pattern: '^[0-9]+$',
};
export const UnsignedInt64TypeRef: OpenAPIRef = {$ref: '#/components/schemas/UnsignedInt64Type'};
export const UsernameTypeSchema: OpenAPISchema = {
	type: 'string',
	minLength: 1,
	maxLength: 32,
	pattern: '^[a-zA-Z0-9_]+$',
};
export const UsernameTypeRef: OpenAPIRef = {$ref: '#/components/schemas/UsernameType'};
export const DiscriminatorTypeSchema: OpenAPISchema = {
	type: 'string',
	pattern: '^\\d{1,4}$',
};
export const DiscriminatorTypeRef: OpenAPIRef = {$ref: '#/components/schemas/DiscriminatorType'};
export const EmailTypeSchema: OpenAPISchema = {
	type: 'string',
	format: 'email',
};
export const EmailTypeRef: OpenAPIRef = {$ref: '#/components/schemas/EmailType'};
export const PasswordTypeSchema: OpenAPISchema = {
	type: 'string',
	minLength: 8,
	maxLength: 256,
};
export const PasswordTypeRef: OpenAPIRef = {$ref: '#/components/schemas/PasswordType'};
export const PhoneNumberTypeSchema: OpenAPISchema = {
	type: 'string',
	pattern: '^\\+[1-9]\\d{1,14}$',
};
export const PhoneNumberTypeRef: OpenAPIRef = {$ref: '#/components/schemas/PhoneNumberType'};
export const Base64ImageTypeSchema: OpenAPISchema = {
	type: 'string',
	format: 'byte',
	description: 'Base64-encoded image data',
};
export const Base64ImageTypeRef: OpenAPIRef = {$ref: '#/components/schemas/Base64ImageType'};
export const LocaleSchema = {
	type: 'string',
	enum: [
		'ar',
		'bg',
		'cs',
		'da',
		'de',
		'el',
		'en-GB',
		'en-US',
		'es-ES',
		'es-419',
		'fi',
		'fr',
		'he',
		'hi',
		'hr',
		'hu',
		'id',
		'it',
		'ja',
		'ko',
		'lt',
		'nl',
		'no',
		'pl',
		'pt-BR',
		'ro',
		'ru',
		'sv-SE',
		'th',
		'tr',
		'uk',
		'vi',
		'zh-CN',
		'zh-TW',
	],
	'x-enumNames': [
		'AR',
		'BG',
		'CS',
		'DA',
		'DE',
		'EL',
		'EN_GB',
		'EN_US',
		'ES_ES',
		'ES_419',
		'FI',
		'FR',
		'HE',
		'HI',
		'HR',
		'HU',
		'ID',
		'IT',
		'JA',
		'KO',
		'LT',
		'NL',
		'NO',
		'PL',
		'PT_BR',
		'RO',
		'RU',
		'SV_SE',
		'TH',
		'TR',
		'UK',
		'VI',
		'ZH_CN',
		'ZH_TW',
	],
	'x-enumDescriptions': [
		'Arabic',
		'Bulgarian',
		'Czech',
		'Danish',
		'German',
		'Greek',
		'English (United Kingdom)',
		'English (United States)',
		'Spanish (Spain)',
		'Spanish (Latin America)',
		'Finnish',
		'French',
		'Hebrew',
		'Hindi',
		'Croatian',
		'Hungarian',
		'Indonesian',
		'Italian',
		'Japanese',
		'Korean',
		'Lithuanian',
		'Dutch',
		'Norwegian',
		'Polish',
		'Portuguese (Brazil)',
		'Romanian',
		'Russian',
		'Swedish',
		'Thai',
		'Turkish',
		'Ukrainian',
		'Vietnamese',
		'Chinese (Simplified)',
		'Chinese (Traditional)',
	],
	description: 'The locale code for the user interface language',
} satisfies OpenAPISchemaWithExtensions;
export const LocaleRef: OpenAPIRef = {$ref: '#/components/schemas/Locale'};
const apiErrorCodeValues = Object.values(APIErrorCodes);
function hasOwnKey<TObject extends object>(object: TObject, key: PropertyKey): key is keyof TObject {
	return Object.hasOwn(object, key);
}
const apiErrorCodeDescriptions = apiErrorCodeValues.map((code) => {
	const i18nKey = hasOwnKey(ErrorCodeToI18nKey, code) ? ErrorCodeToI18nKey[code] : undefined;
	const message = i18nKey && hasOwnKey(ERROR_I18N_MESSAGES, i18nKey) ? ERROR_I18N_MESSAGES[i18nKey] : undefined;
	return message ?? '';
});
export const APIErrorCodeSchema = {
	type: 'string',
	enum: apiErrorCodeValues,
	'x-enumDescriptions': apiErrorCodeDescriptions,
	description: 'Error codes returned by API operations',
} satisfies OpenAPISchemaWithExtensions;
export const ValidationErrorItemSchema: OpenAPISchema = {
	type: 'object',
	properties: {
		field: {
			type: 'string',
			description: 'Field path that failed validation',
		},
		code: {
			type: 'string',
			description: 'Machine-readable validation error code',
		},
		message: {
			type: 'string',
			description: 'Human-readable validation error message',
		},
	},
	required: ['field', 'code', 'message'],
};
