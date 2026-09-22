// SPDX-License-Identifier: AGPL-3.0-or-later
import {ErrorCodeToI18nKey} from '@fluxer/errors/src/i18n/ErrorCodeMappings';
import {ERROR_I18N_MESSAGES} from '@fluxer/errors/src/i18n/ErrorI18nMessages';
import type {OpenAPISchema} from '@fluxer/openapi/src/OpenAPITypes';
import {APIErrorCodeSchema as KnownAPIErrorCodeSchema} from '@fluxer/schema/src/domains/common/ErrorSchemas';
import {z} from 'zod';

const apiErrorCodeValues = KnownAPIErrorCodeSchema.options;
function hasOwnKey<TObject extends object>(object: TObject, key: PropertyKey): key is keyof TObject {
	return Object.hasOwn(object, key);
}
const apiErrorCodeDescriptions = apiErrorCodeValues.map((code) => {
	const i18nKey = hasOwnKey(ErrorCodeToI18nKey, code) ? ErrorCodeToI18nKey[code] : undefined;
	const message = i18nKey && hasOwnKey(ERROR_I18N_MESSAGES, i18nKey) ? ERROR_I18N_MESSAGES[i18nKey] : undefined;
	return message ?? '';
});
export const APIErrorCodeSchema = {
	...z.toJSONSchema(KnownAPIErrorCodeSchema, {target: 'openapi-3.0'}),
	'x-enumDescriptions': apiErrorCodeDescriptions,
} satisfies OpenAPISchema;
