/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {ILogger} from '@fluxer/api/src/ILogger';
import {RequestCacheMiddleware} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import {ServiceMiddleware} from '@fluxer/api/src/middleware/ServiceMiddleware';
import type {HonoEnv} from '@fluxer/api/src/types/HonoEnv';
import {Validator} from '@fluxer/api/src/Validator';
import {AppErrorHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import type {INatsConnectionManager} from '@fluxer/nats/src/INatsConnectionManager';
import {RpcRequest} from '@fluxer/schema/src/domains/rpc/RpcSchemas';
import {Hono} from 'hono';
import {type Msg, StringCodec, type Subscription} from 'nats';

const RPC_SUBJECT = 'rpc.api';
const QUEUE_GROUP = 'api';

export class NatsApiRpcListener {
	private readonly connectionManager: INatsConnectionManager;
	private readonly logger: ILogger;
	private readonly rpcApp: Hono<HonoEnv>;
	private readonly codec = StringCodec();
	private subscription: Subscription | null = null;
	private running = false;

	constructor(connectionManager: INatsConnectionManager, logger: ILogger) {
		this.connectionManager = connectionManager;
		this.logger = logger;

		this.rpcApp = new Hono<HonoEnv>({strict: true});
		this.rpcApp.onError(AppErrorHandler);
		this.rpcApp.use(RequestCacheMiddleware);
		this.rpcApp.use(ServiceMiddleware);
		this.rpcApp.post('/', Validator('json', RpcRequest), async (ctx) => {
			const request = ctx.req.valid('json');
			const rpcService = ctx.get('rpcService');
			const requestCache = ctx.get('requestCache');
			const response = await rpcService.handleRpcRequest({request, requestCache});
			return ctx.json(response);
		});
	}

	async start(): Promise<void> {
		await this.connectionManager.connect();
		const connection = this.connectionManager.getConnection();
		this.subscription = connection.subscribe(RPC_SUBJECT, {queue: QUEUE_GROUP});
		this.running = true;
		this.logger.info(`NATS API RPC listener started, subscribed to ${RPC_SUBJECT} with queue group ${QUEUE_GROUP}`);
		this.processMessages();
	}

	async stop(): Promise<void> {
		this.running = false;
		if (this.subscription) {
			this.subscription.unsubscribe();
			this.subscription = null;
		}
		await this.connectionManager.drain();
		this.logger.info('NATS API RPC listener stopped');
	}

	private async processMessages(): Promise<void> {
		if (!this.subscription) return;

		for await (const msg of this.subscription) {
			if (!this.running) break;
			this.handleMessage(msg).catch((error) => {
				this.logger.error(
					{error: error instanceof Error ? error.message : String(error)},
					'NATS API RPC handler error',
				);
			});
		}
	}

	private async handleMessage(msg: Msg): Promise<void> {
		const payload = this.codec.decode(msg.data);

		try {
			const response = await this.rpcApp.request('/', {
				method: 'POST',
				headers: {'Content-Type': 'application/json'},
				body: payload,
			});

			if (!msg.reply) return;

			const responseBody = await response.text();
			if (response.ok) {
				msg.respond(this.codec.encode(responseBody));
			} else {
				msg.respond(this.codec.encode(JSON.stringify({_error: true, status: response.status, message: responseBody})));
			}
		} catch (error) {
			if (!msg.reply) return;
			const message = error instanceof Error ? error.message : 'internal_error';
			msg.respond(this.codec.encode(JSON.stringify({_error: true, status: 500, message})));
		}
	}
}
