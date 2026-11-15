// SPDX-License-Identifier: AGPL-3.0-or-later

import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getVoiceServerHealth} from '@app/api/middleware/ServiceRegistry';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {getRequestClientIp} from '@app/api/utils/RequestClientIp';
import {Validator} from '@app/api/Validator';
import {VOICE_CONFIGURATION_CHANNEL} from '@app/api/voice/VoiceConstants';
import {VoiceJoinTokenRepository} from '@app/api/voice/VoiceJoinTokenRepository';
import {VOICE_NODE_PATH, VoiceNodeService} from '@app/api/voice/VoiceNodeService';
import {VoiceRepository} from '@app/api/voice/VoiceRepository';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {
	UpdateVoiceNodeRequest,
	VoiceJoinPreviewRequest,
	VoiceJoinPreviewResponse,
	VoiceJoinRequest,
	VoiceJoinResponse,
	VoiceNodeResponse,
} from '@fluxer/schema/src/domains/voice/VoiceNodeSchemas';
import type {Context} from 'hono';

function nodeService(ctx: Context<HonoEnv>): VoiceNodeService {
	const cache = ctx.get('apiContext').services.cache;
	return new VoiceNodeService({
		voiceRepository: new VoiceRepository(),
		joinTokenRepository: new VoiceJoinTokenRepository(),
		auditService: ctx.get('adminService').auditService,
		health: getVoiceServerHealth(),
		publish: (message) => cache.publish(VOICE_CONFIGURATION_CHANNEL, message),
	});
}

function parseNodeBody(text: string): UpdateVoiceNodeRequest {
	let value: unknown;
	try {
		value = text === '' ? {} : JSON.parse(text);
	} catch {
		throw InputValidationError.create('body', 'The body is not JSON');
	}
	const parsed = UpdateVoiceNodeRequest.safeParse(value);
	if (!parsed.success) {
		throw InputValidationError.createMultiple(
			parsed.error.issues.map((issue) => ({path: issue.path.join('.') || 'body', message: issue.message})),
		);
	}
	return parsed.data;
}

const NODE_AUTH_DESCRIPTION =
	'Send Authorization: Bearer with an HS256 JWT signed with the API secret of the server, with iss set to its API key, aud set to fluxer-voice-node, method and path of the request, an exp at most 5 minutes ahead and sha256 holding the base64 SHA-256 of the body, or an empty string for a request without a body.';

export function VoiceNodeController(app: HonoApp) {
	app.post(
		'/voice/join/preview',
		RateLimitMiddleware(RateLimitConfigs.VOICE_JOIN),
		Validator('json', VoiceJoinPreviewRequest),
		OpenAPI({
			operationId: 'preview_voice_join',
			summary: 'Preview joining a voice server',
			description:
				'Shows the server id, region, coordinates and webhook URL a new voice server would get from a join token. Send Authorization: VoiceJoin with the token. The call changes nothing and does not use the token.',
			requestSchema: VoiceJoinPreviewRequest,
			responseSchema: VoiceJoinPreviewResponse,
			statusCode: 200,
			security: [],
			tags: ['Voice'],
		}),
		async (ctx) => {
			const service = nodeService(ctx);
			const row = await service.authenticateJoin(ctx.req.header('Authorization'));
			return ctx.json(await service.preview(row, ctx.req.valid('json'), getRequestClientIp(ctx)));
		},
	);
	app.post(
		'/voice/join',
		RateLimitMiddleware(RateLimitConfigs.VOICE_JOIN),
		Validator('json', VoiceJoinRequest),
		OpenAPI({
			operationId: 'create_voice_join',
			summary: 'Join a voice server to the instance',
			description:
				'Registers a new voice server with a join token. Send Authorization: VoiceJoin with the token. Fluxer checks that the endpoint answers and that the key and secret match before it saves the server, then the token stops working.',
			requestSchema: VoiceJoinRequest,
			responseSchema: VoiceJoinResponse,
			statusCode: 200,
			security: [],
			tags: ['Voice'],
		}),
		async (ctx) => {
			const service = nodeService(ctx);
			const row = await service.authenticateJoin(ctx.req.header('Authorization'));
			return ctx.json(await service.join(row, ctx.req.valid('json'), getRequestClientIp(ctx)));
		},
	);
	app.get(
		VOICE_NODE_PATH,
		RateLimitMiddleware(RateLimitConfigs.VOICE_NODE),
		OpenAPI({
			operationId: 'get_voice_node',
			summary: 'Get the calling voice server',
			description: `Returns the voice server that signed the request, with its health as the API sees it. ${NODE_AUTH_DESCRIPTION}`,
			responseSchema: VoiceNodeResponse,
			statusCode: 200,
			security: [],
			tags: ['Voice'],
		}),
		async (ctx) => {
			const service = nodeService(ctx);
			const server = await service.authenticateNode(ctx.req.header('Authorization'), 'GET', '');
			return ctx.json(service.describe(server));
		},
	);
	app.patch(
		VOICE_NODE_PATH,
		RateLimitMiddleware(RateLimitConfigs.VOICE_NODE),
		OpenAPI({
			operationId: 'update_voice_node',
			summary: 'Update the calling voice server',
			description: `Changes the endpoint, draining flag, coordinates or soft connection limit of the voice server that signed the request. A new endpoint is checked before it is saved. ${NODE_AUTH_DESCRIPTION}`,
			requestSchema: UpdateVoiceNodeRequest,
			responseSchema: VoiceNodeResponse,
			statusCode: 200,
			security: [],
			tags: ['Voice'],
		}),
		async (ctx) => {
			const service = nodeService(ctx);
			const text = await ctx.req.text();
			const server = await service.authenticateNode(ctx.req.header('Authorization'), 'PATCH', text);
			return ctx.json(await service.update(server, parseNodeBody(text)));
		},
	);
	app.delete(
		VOICE_NODE_PATH,
		RateLimitMiddleware(RateLimitConfigs.VOICE_NODE),
		OpenAPI({
			operationId: 'delete_voice_node',
			summary: 'Remove the calling voice server',
			description: `Deletes the voice server that signed the request. ${NODE_AUTH_DESCRIPTION}`,
			responseSchema: null,
			statusCode: 204,
			security: [],
			tags: ['Voice'],
		}),
		async (ctx) => {
			const service = nodeService(ctx);
			const server = await service.authenticateNode(ctx.req.header('Authorization'), 'DELETE', '');
			await service.remove(server);
			return ctx.body(null, 204);
		},
	);
}
