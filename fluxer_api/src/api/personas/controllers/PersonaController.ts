import { createPersonaID, createUserID, type PersonaID } from "@app/api/BrandedTypes";
import { getPngDataUrl } from "@app/api/emoji/tests/EmojiTestUtils";
import { LimitConfigService } from "@app/api/limits/LimitConfigService";
import { Logger } from "@app/api/Logger";
import { LoginRequired } from "@app/api/middleware/AuthMiddleware";
import { RateLimitMiddleware } from "@app/api/middleware/RateLimitMiddleware";
import { OpenAPI } from "@app/api/middleware/ResponseTypeMiddleware";
import { getLimitConfigService } from "@app/api/middleware/ServiceSingletons";
import { RateLimitConfigs } from "@app/api/RateLimitConfig";
import type { HonoApp } from "@app/api/types/HonoEnv";
import { Validator } from "@app/api/Validator";
import { HttpStatus } from "@fluxer/constants/src/HttpConstants.js";
import { AVATAR_MAX_SIZE } from "@fluxer/constants/src/LimitConstants.js";
import { RelationshipTypes } from "@fluxer/constants/src/UserConstants.js";
import { createJsonErrorResponse } from "@fluxer/errors/src/error_handling/ErrorResponse.js";
import { PersonaIdParam, UserPersonaIdParam } from "@fluxer/schema/src/domains/common/CommonParamSchemas.js";
import { ErrorResponse } from "@fluxer/schema/src/domains/common/ErrorSchemas.js";
import { OwnPersonaResponse, PersonaCreateRequest, PersonaPatchRequest, PersonaResponse } from "@fluxer/schema/src/domains/persona/PersonaSchemas.js";
import { stream } from "hono/streaming";
import { encodeBase64Url } from "hono/utils/encode";
import { z } from "zod";

export function PersonaController(app: HonoApp) {
	app.get(
		'/users/@me/personas',
		RateLimitMiddleware(RateLimitConfigs.USER_GET),
		LoginRequired,
		//Validator('param', PersonaIdParam),
		//Validator('query', GuildMemberListQuery), // TODO: query list
		OpenAPI({
			operationId: 'list_own_personas',
			summary: 'List own personas',
			responseSchema: z.array(OwnPersonaResponse),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Personas'],
			description:
				// Took out 'Supports pagination with limit and after cursor.' after the first sentence cause it doesn't do that yet
				'List personas for the current user. Returns a list of all personas that belong to the signed-in user.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			//const requestCache = ctx.get('requestCache');
			const personaService = ctx.get('personaService');
			return ctx.json(await personaService.getUserPersonas(userId));
			//return ctx.json([]);
		}
	);

	app.get(
		'/users/:user_id/personas/:persona_id',
		RateLimitMiddleware(RateLimitConfigs.USER_PERSONA_GET),
		LoginRequired,
		Validator('param', UserPersonaIdParam),
		//Validator('query', GuildMemberListQuery), // TODO: query list
		OpenAPI({
			operationId: 'get_persona',
			summary: 'Get the profile for a persona.',
			responseSchema: PersonaResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Personas'],
			description:
				// Took out 'Supports pagination with limit and after cursor.' after the first sentence cause it doesn't do that yet
				'Retrieves the profile of a persona by its ID.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {user_id, persona_id} = ctx.req.valid('param');
			const userService = ctx.get('userService');
			if (userId.toString() !== user_id.toString() && user_id !== "@me") {
				// pretend that the persona doesn't exist to people who the persona owner has blocked
				const relationship = await userService.relationshipService.getRelationship({userId, targetId: createUserID(user_id), type: RelationshipTypes.BLOCKED});
				if (relationship) return ctx.notFound();
			}
			const targetId = user_id !== "@me" ? createUserID(user_id) : undefined;
			const targetUser = targetId && await userService.accountService.lookupService.findUnique(targetId)
			targetId && user_id.valueOf() !== userId.valueOf() && targetUser && await userService.accountService.lookupService.validateProfileAccess(userId, targetId, targetUser);
			const personaService = ctx.get('personaService');
			const uid = user_id === "@me" ? userId : user_id;
			const response = await personaService.getPersona(createUserID(uid), createPersonaID(persona_id));
			if (!response) return ctx.notFound();
			console.log(response);
			return ctx.json(response);
		}
	);

	app.post(
		'/users/@me/personas',
		RateLimitMiddleware(RateLimitConfigs.USER_PERSONA_NEW),
		LoginRequired,
		//Validator('param', PersonaIdParam),
		//Validator('query', GuildMemberListQuery), // TODO: query list
		Validator('json', PersonaCreateRequest),
		OpenAPI({
			operationId: 'create_persona',
			summary: 'Create a new persona',
			responseSchema: OwnPersonaResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Personas'],
			description:
				'Creates a new persona with the specified properties. Returns a list of all personas that belong to the signed-in user.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const data = ctx.req.valid('json');
			const personaService = ctx.get('personaService');
			const persona = await personaService.createPersona(user, data);
			return ctx.json(await personaService.getOwnPersona(user.id, persona));
		}
	);

	app.patch(
		'/users/@me/personas/:persona_id',
		RateLimitMiddleware(RateLimitConfigs.USER_PERSONA_UPDATE),
		LoginRequired,
		Validator('param', PersonaIdParam),
		Validator('json', PersonaPatchRequest),
		OpenAPI({
			operationId: 'update_persona',
			summary: 'Update a persona.',
			requestSchema: PersonaPatchRequest,
			responseSchema: OwnPersonaResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Personas'],
			description:
				'Updates a persona by its ID. Included fields will be patched onto the existing data.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {persona_id} = ctx.req.valid('param');
			const data = ctx.req.valid('json');
			//const requestCache = ctx.get('requestCache');
			const personaService = ctx.get('personaService');
			const personaId = createPersonaID(persona_id);
			const response = await personaService.checkPersonaExists(userId, personaId);
			if (!response) return ctx.notFound();
			console.log(data);
			await personaService.updatePersona(userId, personaId, data);
			const finalResponse = await personaService.getOwnPersona(userId, personaId);
			return ctx.json(finalResponse);
		}
	);

	app.delete(
		'/users/@me/personas/:persona_id',
		RateLimitMiddleware(RateLimitConfigs.USER_PERSONA_UPDATE),
		LoginRequired,
		Validator('param', PersonaIdParam),
		OpenAPI({
			operationId: 'delete_persona',
			summary: 'Delete a persona',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Personas'],
			description:
				'Deletes the persona by its ID.',
		}),
		async (ctx) => {
			const {persona_id} = ctx.req.valid('param');
			const user = ctx.get('user');
			const personaService = ctx.get('personaService');
			// const persona = await personaService.createPersona(user, data);
			// return ctx.json(await personaService.getPersona(user.id, persona));
			const persona = createPersonaID(persona_id);
			await personaService.deletePersona(user.id, persona);
			const check = await personaService.getPersona(user.id, persona);
			if (check) throw new Error("Persona deletion failed");
			return ctx.body(null, 204);
		}
	);

	app.post(
		'/users/@me/personas/import',
		RateLimitMiddleware(RateLimitConfigs.USER_PERSONA_BULK_IMPORT),
		LoginRequired,
		//Validator('param', PersonaIdParam),
		//Validator('query', GuildMemberListQuery), // TODO: query list
		Validator('json', PersonaCreateRequest.array().min(1)),
		OpenAPI({
			operationId: 'import_personas',
			summary: 'Import personas',
			responseSchema: null,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Personas'],
			description:
				'Creates several personas at once. This may be useful if you are importing personas from another service.',
				/**
				 * This returns NDJSON where each line is a JSON object which always contains the "event" key.
				 * Progress events that this endpoint returns these, generally in this order for each index:
				 * * "Start" - also comes with "length", which is a numeric value representing the length
				 * * "PersonaBuilt" - comes with the "index"
				 * * "PersonaSaved" - comes with the "index"; the final event for each persona,
				 * 			indicating it has been written to the database
				 * * "Done" - comes with nothing, the stream closes immediately after
				 */
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const data = ctx.req.valid('json');
			const personaService = ctx.get('personaService');
			// TODO: pre-flight checks; i.e. make sure the length would not exceed this user's global persona limit, banners, etc
			return stream(ctx, async (str) => {
				str.writeln(JSON.stringify({"event": "Start", "length": data.length}));
				const personaFutures: Array<Promise<PersonaID>> = [];
				for (const i in data) {
					const datum = data[i];
					const persona = personaService.createPersona(user, datum).finally(async () => {
						await str.writeln(JSON.stringify({"event": "PersonaSaved", "index": parseInt(i, 10)}));
					})
					personaFutures.push(persona);
					await str.writeln(JSON.stringify({"event": "PersonaBuilt", "index": parseInt(i, 10)}));
				}
				await Promise.all(personaFutures.map((v, i) => v.catch(async (e) => {
					await str.writeln(JSON.stringify({"event": "SaveFailed", "index": i, "message": e.toString(), "error": e}));
				})));
				await (await str.writeln(JSON.stringify({"event": "Done"}))).close();
			});
		}
	);

	app.get(
		'/import_image_proxy',
		RateLimitMiddleware(RateLimitConfigs.USER_PERSONA_FETCH_AVATAR),
		LoginRequired,
		//Validator('param', PersonaIdParam),
		//Validator('query', GuildMemberListQuery), // TODO: query list
		Validator('query', z.object({"url": z.httpUrl()})),
		OpenAPI({
			operationId: 'import_persona_avatar',
			summary: 'Get persona avatar or banner for import',
			responseSchema: null,
			statusCode: 200,
			security: ['sessionToken'],
			tags: ['Personas'],
			description:
				'Gets an avatar for bulk persona imports. Returns an image file. Intended only for use by users in the web-app; you can get the images directly otherwise.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const tokenType = ctx.get('authTokenType');
			const url = ctx.req.query('url');
			if (user.isBot || tokenType === "bearer" || tokenType === "bot") {
				// Do it yourself, don't hammer the server if you're not the web app.
				ctx.status(HttpStatus.UNAUTHORIZED)
				return;
			}
			// TODO: only accept connections from web browsers on the same domain(s)
			// TODO: abort if file is too large
			const remoteFile = await fetch(String(url), {
				headers: [
					["Accept", "image/*"]
				],
			});
			const type = remoteFile.headers.get("Content-Type");
			if (!type?.startsWith("image/")) {
				ctx.status(HttpStatus.BAD_GATEWAY);
				return;
			}
			if (!remoteFile.ok) {
				ctx.status(remoteFile.status as any || HttpStatus.INTERNAL_SERVER_ERROR);
				return;
			}
			ctx.header("Content-Type", type);
			return remoteFile.body ? ctx.body(remoteFile.body!) : ctx.status(HttpStatus.NOT_FOUND);
		}
	)
}
