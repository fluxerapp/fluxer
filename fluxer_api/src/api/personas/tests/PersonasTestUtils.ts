import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas.js';
import type {OwnPersonaResponse, PersonaSnapshot} from '@fluxer/schema/src/domains/persona/PersonaSchemas.js';

export async function createPersona(harness: ApiTestHarness, token: string, name: string): Promise<OwnPersonaResponse> {
	await ensureSessionStarted(harness, token);
	const msg = await createBuilder<OwnPersonaResponse>(harness, token)
		.post(`/users/@me/personas`)
		.body({
			internal_name: name,
		})
		.execute();
	if (!msg.id) {
		throw new Error('Message response missing id');
	}
	return msg;
}

export async function sendMessageWithPersona(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	content: string,
	persona: PersonaSnapshot,
): Promise<MessageResponse> {
	await ensureSessionStarted(harness, token);
	const msg = await createBuilder<MessageResponse>(harness, token)
		.post(`/channels/${channelId}/messages`)
		.body({
			content,
			persona,
		})
		.execute();
	if (!msg.id) {
		throw new Error('Message response missing id');
	}
	return msg;
}
