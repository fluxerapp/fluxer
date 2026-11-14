// SPDX-License-Identifier: AGPL-3.0-or-later

import {chmod, mkdir, rm} from 'node:fs/promises';
import {createServer, type Server} from 'node:http';
import path from 'node:path';
import {isOperatorPath, markOperatorRequest} from '@app/api/admin/operator/OperatorRequests';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {getRequestListener, type Http2Bindings, type HttpBindings} from '@hono/node-server';

function createOperatorFetch(app: HonoApp) {
	return async (request: Request, env?: HttpBindings | Http2Bindings): Promise<Response> => {
		if (!isOperatorPath(new URL(request.url).pathname)) {
			return Response.json(
				{code: APIErrorCodes.NOT_FOUND, message: 'Only admin routes are served here.'},
				{status: 404},
			);
		}
		markOperatorRequest(request);
		return await app.fetch(request, env);
	};
}

export function createOperatorServer(app: HonoApp): Server {
	return createServer(getRequestListener(createOperatorFetch(app)));
}

export async function listenOperatorSocket(
	app: HonoApp,
	socketPath: string,
	logger: LoggerInterface,
): Promise<Server | null> {
	try {
		await mkdir(path.dirname(socketPath), {recursive: true, mode: 0o700});
		await rm(socketPath, {force: true});
		const server = createOperatorServer(app);
		await new Promise<void>((resolve, reject) => {
			server.once('error', reject);
			server.listen(socketPath, () => {
				server.off('error', reject);
				resolve();
			});
		});
		await chmod(socketPath, 0o600);
		server.on('error', (error) => {
			logger.error({error, socketPath}, 'Operator socket failed');
		});
		logger.info({socketPath}, 'Operator socket listening');
		return server;
	} catch (error) {
		logger.warn({error, socketPath}, 'Operator socket is unavailable');
		return null;
	}
}
