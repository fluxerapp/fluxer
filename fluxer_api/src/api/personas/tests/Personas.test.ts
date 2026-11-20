import { createFriendship, setupTestGuildWithMembers } from "@app/api/channel/tests/ChannelTestUtils";
import { type ApiTestHarness, createApiTestHarness } from "@app/api/test/ApiTestHarness";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";
import { createPersona, sendMessageWithPersona } from "./PersonasTestUtils";
import { createTestUserWithPremium } from "@app/api/stripe/tests/StripeWebhookTestUtils";
import { createTestAccount } from "@app/api/auth/tests/AuthTestUtils";
import { createBuilder } from "@app/api/test/TestRequestBuilder";
import type { OwnPersonaListResponse, OwnPersonaResponse, PersonaPatchRequest } from "@fluxer/schema/src/domains/persona/PersonaSchemas.js";
import { createDMChannel, getMessages } from "@app/api/message/tests/MessageTestUtils";
import { messageDesc } from "@bufbuild/protobuf/codegenv1";
import type { MessageResponse } from "@fluxer/schema/src/domains/message/MessageResponseSchemas.js";
import type { MessageUpdateRequest } from "@app/api/channel/MessageTypes";

describe('Personas', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	it('creates a persona', async () => {
		const member = await createTestAccount(harness);
		const persona = await createPersona(harness, member.token, "Alice");
		expect(persona.internal_name).toEqual("Alice");
		expect(persona.display_name).toEqual("Alice");
		const personas = await createBuilder<OwnPersonaListResponse>(harness, member.token)
			.get("/users/@me/personas")
			.expect(200)
			.execute();
		expect(personas).length(1);
		expect(personas[0]?.display_name).toEqual("Alice");
	});

	it('updates a persona', async () => {
		const member = await createTestAccount(harness);
		const persona = await createPersona(harness, member.token, "Alice");
		expect(persona.display_name).toEqual("Alice");
		const updateResponse = await createBuilder<OwnPersonaResponse>(harness, member.token)
			.patch(`/users/@me/personas/${persona.id}`)
			.body<PersonaPatchRequest>({
				display_name: "Alice <3"
			})
			.expect(200)
			.execute();
		expect(updateResponse?.internal_name).toEqual("Alice");
		expect(updateResponse?.display_name).toEqual("Alice <3");
		const personas = await createBuilder<OwnPersonaListResponse>(harness, member.token)
			.get("/users/@me/personas")
			.expect(200)
			.execute();
		expect(personas).length(1);
		expect(personas[0]?.internal_name).toEqual("Alice");
		expect(personas[0]?.display_name).toEqual("Alice <3");
	});

	it('can change personas on messages', async () => {
		const sender = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		await createFriendship(harness, sender!, recipient!);
		const channel = await createDMChannel(harness, sender.token, recipient.userId);
		const alice = await createPersona(harness, sender.token, "Alice");
		const bob = await createPersona(harness, sender.token, "Bob");

		const personaMessage = await sendMessageWithPersona(harness, sender.token, channel.id, "Hello world", {id: alice.id});
		expect(personaMessage.persona?.id, "Initial message response should contain persona data").toBe(alice.id);
		expect(personaMessage.persona?.name, "Initial message response should contain persona data").toBe("Alice");
		const channelMessages = await getMessages(harness, sender.token, channel.id);
		expect(channelMessages).toHaveLength(1);
		expect(channelMessages[0]?.persona?.name).toBe("Alice");
		expect(channelMessages[0]?.persona?.id).toBe(alice.id);

		const editResponse = await createBuilder<MessageResponse>(harness, sender.token)
			.patch(`/channels/${channel.id}/messages/${personaMessage.id}`)
			.body<MessageUpdateRequest>({
				persona: {id: bob.id}
			})
			.expect(200)
			.execute();
		expect(editResponse.persona?.name).toBe("Bob");
		expect(editResponse.persona?.id).toBe(bob.id);
	});
});
