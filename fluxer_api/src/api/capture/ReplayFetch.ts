// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import type {Consumer, FetchOptions, JsMsg} from '@nats-io/jetstream';

export async function fetchOnce(
	consumer: Pick<Consumer, 'fetch'>,
	options: Partial<FetchOptions>,
	onMessage: (msg: JsMsg) => Promise<boolean>,
): Promise<void> {
	try {
		const messages = await consumer.fetch(options);
		for await (const msg of messages) {
			if (!(await onMessage(msg))) {
				messages.stop();
				break;
			}
		}
	} catch (error) {
		Logger.warn({error}, 'Capture fetch ended early, fetching again');
	}
}
