// SPDX-License-Identifier: AGPL-3.0-or-later
import {SnowflakeTypeRef} from '@fluxer/openapi/src/converters/BuiltInSchemas';
import type {OpenAPIParameter} from '@fluxer/openapi/src/Types';

const COMMON_PATH_PARAMETERS: Record<string, OpenAPIParameter> = {
	guild_id: {
		name: 'guild_id',
		in: 'path',
		required: true,
		schema: SnowflakeTypeRef,
		description: 'The ID of the guild',
	},
	channel_id: {
		name: 'channel_id',
		in: 'path',
		required: true,
		schema: SnowflakeTypeRef,
		description: 'The ID of the channel',
	},
	user_id: {
		name: 'user_id',
		in: 'path',
		required: true,
		schema: SnowflakeTypeRef,
		description: 'The ID of the user',
	},
	message_id: {
		name: 'message_id',
		in: 'path',
		required: true,
		schema: SnowflakeTypeRef,
		description: 'The ID of the message',
	},
	role_id: {
		name: 'role_id',
		in: 'path',
		required: true,
		schema: SnowflakeTypeRef,
		description: 'The ID of the role',
	},
	emoji_id: {
		name: 'emoji_id',
		in: 'path',
		required: true,
		schema: SnowflakeTypeRef,
		description: 'The ID of the emoji',
	},
	sticker_id: {
		name: 'sticker_id',
		in: 'path',
		required: true,
		schema: SnowflakeTypeRef,
		description: 'The ID of the sticker',
	},
	invite_code: {
		name: 'invite_code',
		in: 'path',
		required: true,
		schema: {type: 'string'},
		description: 'The invite code',
	},
	webhook_id: {
		name: 'webhook_id',
		in: 'path',
		required: true,
		schema: SnowflakeTypeRef,
		description: 'The ID of the webhook',
	},
	webhook_token: {
		name: 'webhook_token',
		in: 'path',
		required: true,
		schema: {type: 'string'},
		description: 'The webhook token',
	},
	application_id: {
		name: 'application_id',
		in: 'path',
		required: true,
		schema: SnowflakeTypeRef,
		description: 'The ID of the OAuth2 application',
	},
	key_id: {
		name: 'key_id',
		in: 'path',
		required: true,
		schema: SnowflakeTypeRef,
		description: 'The ID of the key',
	},
};
export function extractPathParameters(path: string): Array<OpenAPIParameter> {
	const paramRegex = /:(\w+)/g;
	const parameters: Array<OpenAPIParameter> = [];
	let match: RegExpExecArray | null;
	while ((match = paramRegex.exec(path)) !== null) {
		const paramName = match[1];
		const commonParam = COMMON_PATH_PARAMETERS[paramName];
		if (commonParam) {
			parameters.push({...commonParam});
		} else {
			parameters.push({
				name: paramName,
				in: 'path',
				required: true,
				schema: {type: 'string'},
				description: `The ${paramName.replace(/_/g, ' ')}`,
			});
		}
	}
	return parameters;
}
export function convertPathToOpenAPI(path: string): string {
	return path.replace(/:(\w+)/g, '{$1}');
}
