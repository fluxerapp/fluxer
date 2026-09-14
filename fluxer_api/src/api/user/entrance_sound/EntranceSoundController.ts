// SPDX-License-Identifier: AGPL-3.0-or-later

import {createEntranceSoundID} from '@app/api/BrandedTypes';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import type {EntranceSoundSelection} from '@app/api/models/EntranceSound';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import type {EntranceSoundLibraryEntry, EntranceSoundService} from '@app/api/user/entrance_sound/EntranceSoundService';
import {Validator} from '@app/api/Validator';
import {
	EntranceSoundIdParam,
	EntranceSoundLibraryResponse,
	EntranceSoundRenameRequest,
	EntranceSoundResponse,
	EntranceSoundSelectionRequest,
	EntranceSoundUploadRequest,
} from '@fluxer/schema/src/domains/user/EntranceSoundSchemas';

function serializeSound(entry: EntranceSoundLibraryEntry) {
	return {
		id: entry.sound.soundId.toString(),
		name: entry.sound.name,
		hash: entry.sound.hash,
		extension: entry.sound.extension,
		content_type: entry.sound.contentType,
		duration_ms: entry.sound.durationMs,
		size_bytes: entry.sound.sizeBytes,
		url: entry.url,
		created_at: entry.sound.createdAt.toISOString(),
	};
}

function serializeSelection(selection: EntranceSoundSelection) {
	return {
		scope_id: selection.scopeId,
		sound_id: selection.soundId.toString(),
	};
}

export function EntranceSoundController(app: HonoApp) {
	app.get(
		'/users/@me/entrance-sounds',
		RateLimitMiddleware(RateLimitConfigs.USER_ENTRANCE_SOUND_LIST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_entrance_sounds',
			summary: "List the user's entrance sound library",
			description: 'Returns the saved entrance sounds owned by the user plus the per-scope active selections.',
			responseSchema: EntranceSoundLibraryResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const service: EntranceSoundService = ctx.get('entranceSoundService');
			const [library, selections] = await Promise.all([service.listLibrary(userId), service.listSelections(userId)]);
			return ctx.json(
				{
					sounds: library.map(serializeSound),
					selections: selections.map(serializeSelection),
				},
				200,
			);
		},
	);
	app.post(
		'/users/@me/entrance-sounds',
		RateLimitMiddleware(RateLimitConfigs.USER_ENTRANCE_SOUND_UPLOAD),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', EntranceSoundUploadRequest),
		OpenAPI({
			operationId: 'upload_entrance_sound',
			summary: 'Upload an entrance sound',
			description:
				"Uploads a short audio clip to the user's entrance sound library. Validates format, duration, and size server-side.",
			requestSchema: EntranceSoundUploadRequest,
			responseSchema: EntranceSoundResponse,
			statusCode: 201,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const service: EntranceSoundService = ctx.get('entranceSoundService');
			const {name, audio} = ctx.req.valid('json');
			const created = await service.upload({userId, name, base64Audio: audio});
			return ctx.json(serializeSound(created), 201);
		},
	);
	app.patch(
		'/users/@me/entrance-sounds/:sound_id',
		RateLimitMiddleware(RateLimitConfigs.USER_ENTRANCE_SOUND_MUTATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', EntranceSoundIdParam),
		Validator('json', EntranceSoundRenameRequest),
		OpenAPI({
			operationId: 'rename_entrance_sound',
			summary: 'Rename an entrance sound',
			description: "Updates the display label for a sound in the user's library. Audio bytes are unchanged.",
			requestSchema: EntranceSoundRenameRequest,
			responseSchema: EntranceSoundResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const service: EntranceSoundService = ctx.get('entranceSoundService');
			const soundId = createEntranceSoundID(BigInt(ctx.req.valid('param').sound_id));
			const {name} = ctx.req.valid('json');
			const updated = await service.rename({userId, soundId, name});
			return ctx.json(serializeSound(updated), 200);
		},
	);
	app.delete(
		'/users/@me/entrance-sounds/:sound_id',
		RateLimitMiddleware(RateLimitConfigs.USER_ENTRANCE_SOUND_MUTATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', EntranceSoundIdParam),
		OpenAPI({
			operationId: 'delete_entrance_sound',
			summary: 'Delete an entrance sound',
			description: 'Removes the sound from the library and clears any per-scope selections that pointed at it.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const service: EntranceSoundService = ctx.get('entranceSoundService');
			const soundId = createEntranceSoundID(BigInt(ctx.req.valid('param').sound_id));
			await service.delete(userId, soundId);
			return ctx.body(null, 204);
		},
	);
	app.put(
		'/users/@me/entrance-sound-selections',
		RateLimitMiddleware(RateLimitConfigs.USER_ENTRANCE_SOUND_MUTATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', EntranceSoundSelectionRequest),
		OpenAPI({
			operationId: 'set_entrance_sound_selection',
			summary: 'Set the active entrance sound for a scope',
			description:
				"Assigns one of the user's library sounds to a scope (global, guilds, dms, or guild:<id>). Pass sound_id null to clear.",
			requestSchema: EntranceSoundSelectionRequest,
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const service: EntranceSoundService = ctx.get('entranceSoundService');
			const {scope_id, sound_id} = ctx.req.valid('json');
			await service.setSelection({
				userId,
				scopeId: scope_id,
				soundId: sound_id ? createEntranceSoundID(BigInt(sound_id)) : null,
			});
			return ctx.body(null, 204);
		},
	);
}
