// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomUUID} from 'node:crypto';
import {once} from 'node:events';
import net, {type AddressInfo} from 'node:net';
import {Readable} from 'node:stream';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {Config, getConfig} from '@app/api/Config';
import {createChannel, createGuild} from '@app/api/channel/tests/AttachmentTestUtils';
import {THREADS_FEATURE, THREADS_FEATURE_HEADER} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {setInjectedMediaService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {TestMediaService} from '@app/api/test/TestMediaService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ATTACHMENT_MAX_SIZE_BOT} from '@fluxer/constants/src/LimitConstants';
import type {WebhookCreateResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import {serve} from '@hono/node-server';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const MIB = 1024 * 1024;

type Part =
	| {name: string; value: string}
	| {headers: string; value: string}
	| {name: string; filename: string; size: number; fill?: number};

interface StreamedBody {
	boundary: string;
	pieces: Array<Buffer | {size: number; fill: number}>;
	stream: ReadableStream<Uint8Array>;
	contentType: string;
	length: number;
	bytesRead: () => number;
}

function streamedMultipart(parts: Array<Part>): StreamedBody {
	const boundary = `----Boundary${randomUUID()}`;
	const pieces: Array<Buffer | {size: number; fill: number}> = [];
	for (const part of parts) {
		if ('headers' in part) {
			pieces.push(Buffer.from(`--${boundary}\r\n${part.headers}\r\n\r\n${part.value}\r\n`));
			continue;
		}
		if ('value' in part) {
			pieces.push(
				Buffer.from(`--${boundary}\r\nContent-Disposition: form-data; name="${part.name}"\r\n\r\n${part.value}\r\n`),
			);
			continue;
		}
		pieces.push(
			Buffer.from(
				`--${boundary}\r\nContent-Disposition: form-data; name="${part.name}"; filename="${part.filename}"\r\nContent-Type: application/octet-stream\r\n\r\n`,
			),
		);
		pieces.push({size: part.size, fill: part.fill ?? 0x61});
		pieces.push(Buffer.from('\r\n'));
	}
	pieces.push(Buffer.from(`--${boundary}--\r\n`));
	const length = pieces.reduce((sum, piece) => sum + (Buffer.isBuffer(piece) ? piece.length : piece.size), 0);
	let read = 0;
	let pieceIndex = 0;
	let offset = 0;
	const stream = new ReadableStream<Uint8Array>(
		{
			pull(controller) {
				const piece = pieces[pieceIndex];
				if (!piece) {
					controller.close();
					return;
				}
				if (Buffer.isBuffer(piece)) {
					pieceIndex++;
					read += piece.length;
					controller.enqueue(new Uint8Array(piece));
					return;
				}
				const chunkSize = Math.min(MIB, piece.size - offset);
				offset += chunkSize;
				read += chunkSize;
				if (offset >= piece.size) {
					pieceIndex++;
					offset = 0;
				}
				controller.enqueue(new Uint8Array(chunkSize).fill(piece.fill));
			},
		},
		{highWaterMark: 0},
	);
	return {
		boundary,
		pieces,
		stream,
		contentType: `multipart/form-data; boundary=${boundary}`,
		length,
		bytesRead: () => read,
	};
}

function sha256OfFill(size: number, fill: number): string {
	return createHash('sha256').update(Buffer.alloc(size, fill)).digest('hex');
}

describe('streamed multipart attachment uploads', () => {
	let harness: ApiTestHarness;
	let owner: TestAccount;
	let guildId: string;
	let channelId: string;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		setInjectedMediaService(new TestMediaService(harness.storageService));
		owner = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		const guild = await createGuild(harness, owner.token, 'Streaming Guild');
		guildId = guild.id;
		channelId = guild.system_channel_id ?? (await createChannel(harness, owner.token, guild.id, 'general')).id;
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function post(
		path: string,
		body: StreamedBody,
		authorization: string | null,
		headers: Record<string, string> = {},
		method = 'POST',
	): Promise<Response> {
		return harness.app.request(path, {
			method,
			headers: {
				...(authorization ? {Authorization: authorization} : {}),
				'Content-Type': body.contentType,
				'x-forwarded-for': '127.0.0.1',
				...headers,
			},
			body: body.stream,
			duplex: 'half',
		} as RequestInit);
	}

	async function botAuthorization(): Promise<string> {
		const bot = await createTestBotAccount(harness);
		await authorizeBot(harness, owner.token, bot.appId, ['bot'], guildId, Permissions.ADMINISTRATOR.toString());
		return `Bot ${bot.botToken}`;
	}

	function uploadedKeys(): Array<string> {
		return harness.storageService.uploadObjectSpy.mock.calls
			.map(([params]) => params as {bucket: string; key: string})
			.filter((params) => params.bucket === Config.s3.buckets.uploads)
			.map((params) => params.key);
	}

	it('reads the rest of a slightly oversized bot upload before answering', async () => {
		const authorization = await botAuthorization();
		const body = streamedMultipart([
			{name: 'payload_json', value: JSON.stringify({content: 'big'})},
			{name: 'files[0]', filename: 'big.bin', size: 60 * MIB},
		]);

		const response = await post(`/channels/${channelId}/messages`, body, authorization);

		expect(response.status).toBe(400);
		expect(await response.json()).toMatchObject({code: 'FILE_SIZE_TOO_LARGE'});
		expect(body.bytesRead()).toBe(body.length);
		for (const key of uploadedKeys()) {
			expect(harness.storageService.hasObject(Config.s3.buckets.uploads, key)).toBe(false);
		}
	});

	it('stops reading a bot upload that runs far past the bot limit', async () => {
		const authorization = await botAuthorization();
		const body = streamedMultipart([{name: 'files[0]', filename: 'huge.bin', size: 200 * MIB}]);

		const response = await post(`/channels/${channelId}/messages`, body, authorization);

		expect(response.status).toBe(400);
		expect(await response.json()).toMatchObject({code: 'FILE_SIZE_TOO_LARGE'});
		expect(body.bytesRead()).toBeLessThan(ATTACHMENT_MAX_SIZE_BOT + 70 * MIB);
	});

	it('answers a client that keeps sending past the limit with the JSON error', async () => {
		const authorization = await botAuthorization();
		const body = streamedMultipart([
			{name: 'payload_json', value: JSON.stringify({content: 'big'})},
			{name: 'files[0]', filename: 'big.bin', size: 60 * MIB},
		]);
		const server = serve({fetch: harness.app.fetch, port: 0, hostname: '127.0.0.1'});
		await once(server, 'listening');
		const {port} = server.address() as AddressInfo;

		const outcome = await new Promise<string>((resolve) => {
			const socket = net.connect(port, '127.0.0.1');
			let received = '';
			socket.on('data', (chunk) => {
				received += chunk.toString('latin1');
			});
			socket.on('error', (error: NodeJS.ErrnoException) => resolve(`socket error ${error.code}`));
			socket.on('end', () => resolve(received));
			socket.write(
				[
					`POST /channels/${channelId}/messages HTTP/1.1`,
					'Host: 127.0.0.1',
					`Authorization: ${authorization}`,
					`Content-Type: ${body.contentType}`,
					`Content-Length: ${body.length}`,
					'x-forwarded-for: 127.0.0.1',
					'Connection: close',
					'',
					'',
				].join('\r\n'),
			);
			const pieces = [...body.pieces];
			const pump = () => {
				for (let piece = pieces.shift(); piece; piece = pieces.shift()) {
					const chunk = Buffer.isBuffer(piece) ? piece : Buffer.alloc(piece.size, piece.fill);
					if (!socket.write(chunk)) {
						socket.once('drain', pump);
						return;
					}
				}
			};
			pump();
		});
		server.close();

		expect(outcome).toMatch(/^HTTP\/1\.1 400 /);
		expect(outcome).toContain('"code":"FILE_SIZE_TOO_LARGE"');
	});

	it('rejects a declared length above the upload budget before reading the body', async () => {
		const authorization = await botAuthorization();
		const body = streamedMultipart([{name: 'files[0]', filename: 'big.bin', size: 600 * MIB}]);

		const response = await post(`/channels/${channelId}/messages`, body, authorization, {
			'content-length': String(body.length),
		});

		expect(response.status).toBe(413);
		expect(body.bytesRead()).toBe(0);
	});

	it('streams an accepted file to storage without changing it', async () => {
		const size = 20 * MIB;
		const body = streamedMultipart([
			{
				name: 'payload_json',
				value: JSON.stringify({content: 'ok', attachments: [{id: 0, filename: 'data.bin'}]}),
			},
			{name: 'files[0]', filename: 'data.bin', size, fill: 0x42},
		]);

		const response = await post(`/channels/${channelId}/messages`, body, owner.token);

		expect(response.status).toBe(200);
		const [params] = harness.storageService.uploadObjectSpy.mock.calls.find(
			([call]) => (call as {bucket: string}).bucket === Config.s3.buckets.uploads,
		)!;
		expect((params as {body: unknown}).body).toBeInstanceOf(Readable);
		const message = (await response.json()) as {attachments: Array<{size: number; filename: string}>};
		expect(message.attachments[0]).toMatchObject({size, filename: 'data.bin'});
		const copied = harness.storageService.getCopiedObjects().find((copy) => copy.destinationKey.endsWith('data.bin'));
		expect(copied).toBeDefined();
		const stored = await harness.storageService.readObject(Config.s3.buckets.cdn, copied!.destinationKey);
		expect(createHash('sha256').update(stored).digest('hex')).toBe(sha256OfFill(size, 0x42));
	});

	it('deletes a stored file when payload_json after it is invalid', async () => {
		const body = streamedMultipart([
			{name: 'files[0]', filename: 'first.bin', size: MIB},
			{name: 'payload_json', value: '{not json'},
		]);

		const response = await post(`/channels/${channelId}/messages`, body, owner.token);

		expect(response.status).toBe(400);
		const keys = uploadedKeys();
		expect(keys).toHaveLength(1);
		expect(harness.storageService.hasObject(Config.s3.buckets.uploads, keys[0]!)).toBe(false);
	});

	it('deletes a stored file when the merged payload fails validation', async () => {
		const body = streamedMultipart([
			{name: 'files[0]', filename: 'first.bin', size: MIB},
			{name: 'payload_json', value: JSON.stringify({flags: 'not a number'})},
		]);

		const response = await post(`/channels/${channelId}/messages`, body, owner.token);

		expect(response.status).toBe(400);
		const keys = uploadedKeys();
		expect(keys).toHaveLength(1);
		expect(harness.storageService.hasObject(Config.s3.buckets.uploads, keys[0]!)).toBe(false);
	});

	it('rejects a text channel thread body above the request body limit', async () => {
		const body = streamedMultipart([
			{name: 'payload_json', value: JSON.stringify({name: 'thread', type: ChannelTypes.PUBLIC_THREAD})},
			{name: 'files[0]', filename: 'big.bin', size: 20 * MIB},
		]);

		const response = await post(`/channels/${channelId}/threads`, body, owner.token, {
			[THREADS_FEATURE_HEADER]: THREADS_FEATURE,
		});

		expect(response.status).toBe(413);
		expect(await response.json()).toMatchObject({code: 'FILE_SIZE_TOO_LARGE'});
	});

	it('ignores a small file part when creating a text channel thread', async () => {
		const body = streamedMultipart([
			{name: 'payload_json', value: JSON.stringify({name: 'thread', type: ChannelTypes.PUBLIC_THREAD})},
			{name: 'files[0]', filename: 'small.bin', size: 1024},
		]);

		const response = await post(`/channels/${channelId}/threads`, body, owner.token, {
			[THREADS_FEATURE_HEADER]: THREADS_FEATURE,
		});

		expect(response.status).toBe(201);
		expect(uploadedKeys()).toHaveLength(0);
	});

	it('reads an octet-stream part without a filename as a field', async () => {
		const body = streamedMultipart([
			{
				headers: 'Content-Disposition: form-data; name="payload_json"\r\nContent-Type: application/octet-stream',
				value: JSON.stringify({content: 'octet'}),
			},
			{
				headers: 'Content-Disposition: form-data; name="files[0]"\r\nContent-Type: application/octet-stream',
				value: 'not a file',
			},
		]);

		const response = await post(`/channels/${channelId}/messages`, body, owner.token);

		expect(response.status).toBe(200);
		const message = (await response.json()) as {content: string; attachments?: Array<unknown>};
		expect(message.content).toBe('octet');
		expect(message.attachments ?? []).toHaveLength(0);
		expect(uploadedKeys()).toHaveLength(0);
	});

	it('does not count unrecognised file parts toward the attachment limit', async () => {
		const parts: Array<Part> = [{name: 'payload_json', value: JSON.stringify({content: 'ten'})}];
		for (let index = 0; index < 10; index++) {
			parts.push({name: `files[${index}]`, filename: `f${index}.bin`, size: 1024});
		}
		parts.push({name: 'thumbnail', filename: 'thumb.bin', size: 1024});
		parts.push({name: 'attachment', filename: 'extra.bin', size: 1024});

		const response = await post(`/channels/${channelId}/messages`, streamedMultipart(parts), owner.token);

		expect(response.status).toBe(200);
		expect(((await response.json()) as {attachments: Array<unknown>}).attachments).toHaveLength(10);
	});

	it('rejects a part without a name on every multipart route', async () => {
		const webhook = await createBuilder<WebhookCreateResponse>(harness, owner.token)
			.post(`/channels/${channelId}/webhooks`)
			.body({name: 'Nameless'})
			.execute();
		const nameless: Array<Part> = [
			{headers: 'Content-Disposition: form-data', value: 'value'},
			{headers: 'Content-Disposition: form-data; filename="a.bin"', value: 'abc'},
		];
		const statuses: Array<number> = [];
		for (const part of nameless) {
			statuses.push((await post(`/channels/${channelId}/messages`, streamedMultipart([part]), owner.token)).status);
			statuses.push((await post(`/webhooks/${webhook.id}/${webhook.token}`, streamedMultipart([part]), null)).status);
			statuses.push(
				(
					await post(`/channels/${channelId}/threads`, streamedMultipart([part]), owner.token, {
						[THREADS_FEATURE_HEADER]: THREADS_FEATURE,
					})
				).status,
			);
		}

		expect(statuses).toEqual([400, 400, 400, 400, 400, 400]);
	});

	it('limits how many files stream to storage at once', async () => {
		const config = getConfig();
		const previousLimit = config.maxConcurrentFormUploads;
		config.maxConcurrentFormUploads = 2;
		const storage = harness.storageService;
		const uploadObject = storage.uploadObject.bind(storage);
		let inflight = 0;
		let peak = 0;
		storage.uploadObject = async (params) => {
			inflight++;
			peak = Math.max(peak, inflight);
			try {
				return await uploadObject(params);
			} finally {
				inflight--;
			}
		};
		try {
			const statuses = await Promise.all(
				Array.from({length: 6}, async (_, index) => {
					const body = streamedMultipart([
						{name: 'payload_json', value: JSON.stringify({content: `upload ${index}`})},
						{name: 'files[0]', filename: 'part.bin', size: 4 * MIB},
					]);
					return (await post(`/channels/${channelId}/messages`, body, owner.token)).status;
				}),
			);

			expect(statuses).toEqual([200, 200, 200, 200, 200, 200]);
			expect(peak).toBe(2);
		} finally {
			storage.uploadObject = uploadObject;
			config.maxConcurrentFormUploads = previousLimit;
		}
	});

	it('still accepts a file on message edit', async () => {
		const created = await createBuilder<{id: string}>(harness, owner.token)
			.post(`/channels/${channelId}/messages`)
			.body({content: 'before'})
			.execute();
		const body = streamedMultipart([
			{
				name: 'payload_json',
				value: JSON.stringify({content: 'after', attachments: [{id: 0, filename: 'edit.bin'}]}),
			},
			{name: 'files[0]', filename: 'edit.bin', size: MIB},
		]);

		const response = await post(`/channels/${channelId}/messages/${created.id}`, body, owner.token, {}, 'PATCH');

		expect(response.status).toBe(200);
		expect(((await response.json()) as {attachments: Array<unknown>}).attachments).toHaveLength(1);
	});

	it('still accepts a file on webhook execute', async () => {
		const webhook = await createBuilder<WebhookCreateResponse>(harness, owner.token)
			.post(`/channels/${channelId}/webhooks`)
			.body({name: 'Uploader'})
			.execute();
		const body = streamedMultipart([
			{name: 'payload_json', value: JSON.stringify({content: 'hook'})},
			{name: 'files[0]', filename: 'hook.bin', size: MIB},
		]);

		const response = await post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`, body, null);

		expect(response.status).toBe(200);
		expect(((await response.json()) as {attachments: Array<unknown>}).attachments).toHaveLength(1);
	});
});
